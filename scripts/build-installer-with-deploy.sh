#!/usr/bin/env bash
# Build Systole installer ISO with SSH deploy key support
# Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

# Default values
SSH_DEPLOY_KEY=""
DEPLOY_KEY=""          # DEPRECATED: backward compatibility
CHANNELS_FILE=""
SIGNING_KEY=""
HOST_KEY_PRIVATE=""
HOST_KEY_PUBLIC=""
OUTPUT=""
USE_TIME_MACHINE=true
REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CHANNELS_LOCK="$REPO_DIR/channels-lock.scm"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

usage() {
    cat <<EOF
Usage: $0 [OPTIONS]

Build a Systole installer ISO with deployment configuration for remote workflows.
Uses channels-lock.scm for reproducible builds via guix time-machine.

OPTIONS:
    --ssh-deploy-key KEY           SSH public key string (e.g., "ssh-ed25519 AAAA...")
    --ssh-deploy-key-file FILE     Path to SSH public key file
    --key KEY                      DEPRECATED: Use --ssh-deploy-key
    --key-file FILE                DEPRECATED: Use --ssh-deploy-key-file
    --channels-file FILE           Path to channels.scm file (default: channels-lock.scm)
    --signing-key-file FILE        Path to Guix signing key file (.pub)
    --host-key-file FILE           Path to SSH host private key (expects .pub sibling)
    --output FILE                  Output ISO file path (default: auto-generated)
    --no-time-machine              Skip guix time-machine (use current channels)
    -h, --help                     Show this help message

ENVIRONMENT VARIABLES:
    SYSTOLE_SSH_DEPLOY_KEY          SSH public key string
    SYSTOLE_SSH_DEPLOY_KEY_FILE     Path to SSH public key file
    SYSTOLE_DEPLOY_KEY              DEPRECATED: Use SYSTOLE_SSH_DEPLOY_KEY
    SYSTOLE_DEPLOY_KEY_FILE         DEPRECATED: Use SYSTOLE_SSH_DEPLOY_KEY_FILE
    SYSTOLE_CHANNELS_FILE           Path to channels file
    SYSTOLE_SIGNING_KEY_FILE        Path to Guix signing key file
    SYSTOLE_HOST_KEY_FILE           Path to SSH host private key file

Priority: command-line arguments take precedence over environment variables.

EXAMPLES:
    # Full deployment setup (all flags)
    $0 --ssh-deploy-key-file ~/.ssh/id_ed25519.pub \\
       --channels-file channels-lock.scm \\
       --signing-key-file /etc/guix/signing-key.pub \\
       --host-key-file /etc/ssh/ssh_host_ed25519_key

    # Just SSH access
    $0 --ssh-deploy-key-file ~/.ssh/id_ed25519.pub

    # SSH + signing key (enables immediate guix deploy)
    $0 --ssh-deploy-key-file ~/.ssh/id_ed25519.pub \\
       --signing-key-file /etc/guix/signing-key.pub

    # Using environment variables
    SYSTOLE_SSH_DEPLOY_KEY="\$(cat ~/.ssh/id_ed25519.pub)" \\
    SYSTOLE_SIGNING_KEY_FILE="/etc/guix/signing-key.pub" \\
      $0

NOTES:
    - ssh-deploy-key: Authorizes root SSH access in installer
    - channels-file: Embeds channel specs for reproducible deployments
    - signing-key: Authorizes Guix signing key (enables guix deploy without manual auth)
    - host-key: Provides predictable SSH host fingerprints across reinstalls

EOF
    exit 0
}

error() {
    echo -e "${RED}ERROR: $*${NC}" >&2
    exit 1
}

info() {
    echo -e "${GREEN}INFO: $*${NC}"
}

warn() {
    echo -e "${YELLOW}WARNING: $*${NC}"
}

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --ssh-deploy-key)
            if [[ -z "${2:-}" ]]; then
                error "No key specified after --ssh-deploy-key"
            fi
            SSH_DEPLOY_KEY="$2"
            shift 2
            ;;
        --ssh-deploy-key-file)
            if [[ -z "${2:-}" ]]; then
                error "No key file specified after --ssh-deploy-key-file"
            fi
            if [[ ! -f "$2" ]]; then
                error "Key file not found: $2"
            fi
            SSH_DEPLOY_KEY="$(cat "$2")"
            shift 2
            ;;
        --key)
            warn "--key is deprecated, use --ssh-deploy-key instead"
            if [[ -z "${2:-}" ]]; then
                error "No key specified after --key"
            fi
            DEPLOY_KEY="$2"
            shift 2
            ;;
        --key-file)
            warn "--key-file is deprecated, use --ssh-deploy-key-file instead"
            if [[ -z "${2:-}" ]]; then
                error "No key file specified after --key-file"
            fi
            if [[ ! -f "$2" ]]; then
                error "Key file not found: $2"
            fi
            DEPLOY_KEY="$(cat "$2")"
            shift 2
            ;;
        --channels-file)
            if [[ -z "${2:-}" ]]; then
                error "No channels file specified after --channels-file"
            fi
            if [[ ! -f "$2" ]]; then
                error "Channels file not found: $2"
            fi
            CHANNELS_FILE="$2"
            shift 2
            ;;
        --signing-key-file)
            if [[ -z "${2:-}" ]]; then
                error "No signing key file specified after --signing-key-file"
            fi
            if [[ ! -f "$2" ]]; then
                error "Signing key file not found: $2"
            fi
            SIGNING_KEY="$(cat "$2")"
            shift 2
            ;;
        --host-key-file)
            if [[ -z "${2:-}" ]]; then
                error "No host key file specified after --host-key-file"
            fi
            if [[ ! -f "$2" ]]; then
                error "Host key private file not found: $2"
            fi
            if [[ ! -f "$2.pub" ]]; then
                error "Host key public file not found: $2.pub"
            fi
            HOST_KEY_PRIVATE="$2"
            HOST_KEY_PUBLIC="$2.pub"
            shift 2
            ;;
        --output)
            OUTPUT="$2"
            shift 2
            ;;
        --no-time-machine)
            USE_TIME_MACHINE=false
            shift 1
            ;;
        -h|--help)
            usage
            ;;
        *)
            error "Unknown option: $1\nUse --help for usage information."
            ;;
    esac
done

# Fall back to environment variables if not provided via command line
if [[ -z "$SSH_DEPLOY_KEY" && -z "$DEPLOY_KEY" ]]; then
    if [[ -n "${SYSTOLE_SSH_DEPLOY_KEY:-}" ]]; then
        SSH_DEPLOY_KEY="$SYSTOLE_SSH_DEPLOY_KEY"
        info "Using SSH deploy key from SYSTOLE_SSH_DEPLOY_KEY environment variable"
    elif [[ -n "${SYSTOLE_SSH_DEPLOY_KEY_FILE:-}" ]]; then
        if [[ ! -f "$SYSTOLE_SSH_DEPLOY_KEY_FILE" ]]; then
            error "Key file not found: $SYSTOLE_SSH_DEPLOY_KEY_FILE"
        fi
        SSH_DEPLOY_KEY="$(cat "$SYSTOLE_SSH_DEPLOY_KEY_FILE")"
        info "Using SSH deploy key from file: $SYSTOLE_SSH_DEPLOY_KEY_FILE"
    elif [[ -n "${SYSTOLE_DEPLOY_KEY:-}" ]]; then
        warn "SYSTOLE_DEPLOY_KEY is deprecated, use SYSTOLE_SSH_DEPLOY_KEY instead"
        DEPLOY_KEY="$SYSTOLE_DEPLOY_KEY"
    elif [[ -n "${SYSTOLE_DEPLOY_KEY_FILE:-}" ]]; then
        warn "SYSTOLE_DEPLOY_KEY_FILE is deprecated, use SYSTOLE_SSH_DEPLOY_KEY_FILE instead"
        if [[ ! -f "$SYSTOLE_DEPLOY_KEY_FILE" ]]; then
            error "Key file not found: $SYSTOLE_DEPLOY_KEY_FILE"
        fi
        DEPLOY_KEY="$(cat "$SYSTOLE_DEPLOY_KEY_FILE")"
    fi
fi

if [[ -z "$CHANNELS_FILE" && -n "${SYSTOLE_CHANNELS_FILE:-}" ]]; then
    CHANNELS_FILE="$SYSTOLE_CHANNELS_FILE"
    info "Using channels file from SYSTOLE_CHANNELS_FILE: $CHANNELS_FILE"
fi

if [[ -z "$SIGNING_KEY" && -n "${SYSTOLE_SIGNING_KEY_FILE:-}" ]]; then
    if [[ ! -f "$SYSTOLE_SIGNING_KEY_FILE" ]]; then
        error "Signing key file not found: $SYSTOLE_SIGNING_KEY_FILE"
    fi
    SIGNING_KEY="$(cat "$SYSTOLE_SIGNING_KEY_FILE")"
    info "Using signing key from SYSTOLE_SIGNING_KEY_FILE: $SYSTOLE_SIGNING_KEY_FILE"
fi

if [[ -z "$HOST_KEY_PRIVATE" && -n "${SYSTOLE_HOST_KEY_FILE:-}" ]]; then
    if [[ ! -f "$SYSTOLE_HOST_KEY_FILE" ]]; then
        error "Host key file not found: $SYSTOLE_HOST_KEY_FILE"
    fi
    if [[ ! -f "$SYSTOLE_HOST_KEY_FILE.pub" ]]; then
        error "Host key public file not found: $SYSTOLE_HOST_KEY_FILE.pub"
    fi
    HOST_KEY_PRIVATE="$SYSTOLE_HOST_KEY_FILE"
    HOST_KEY_PUBLIC="$SYSTOLE_HOST_KEY_FILE.pub"
    info "Using host key from SYSTOLE_HOST_KEY_FILE: $SYSTOLE_HOST_KEY_FILE"
fi

# Prioritize ssh-deploy-key over deprecated deploy-key
if [[ -n "$SSH_DEPLOY_KEY" ]]; then
    FINAL_DEPLOY_KEY="$SSH_DEPLOY_KEY"
elif [[ -n "$DEPLOY_KEY" ]]; then
    warn "deploy-key parameter is deprecated, use ssh-deploy-key instead"
    FINAL_DEPLOY_KEY="$DEPLOY_KEY"
else
    FINAL_DEPLOY_KEY=""
fi

# Validate SSH deploy key if provided
if [[ -n "$FINAL_DEPLOY_KEY" ]]; then
    if [[ ! "$FINAL_DEPLOY_KEY" =~ ^ssh-(rsa|dsa|ecdsa|ed25519)[[:space:]] ]]; then
        error "Invalid SSH key format. Expected key to start with ssh-rsa, ssh-dsa, ssh-ecdsa, or ssh-ed25519"
    fi
    KEY_TYPE=$(echo "$FINAL_DEPLOY_KEY" | awk '{print $1}')
    KEY_COMMENT=$(echo "$FINAL_DEPLOY_KEY" | awk '{print $3}')
    info "SSH deploy key type: $KEY_TYPE${KEY_COMMENT:+ ($KEY_COMMENT)}"
fi

# Validate signing key format if provided
if [[ -n "$SIGNING_KEY" ]]; then
    if [[ ! "$SIGNING_KEY" =~ ^\(public-key ]]; then
        error "Invalid signing key format. Expected S-expression starting with (public-key"
    fi
    info "Guix signing key provided"
fi

# Validate host key pair if provided
if [[ -n "$HOST_KEY_PRIVATE" ]]; then
    info "SSH host key pair provided: $HOST_KEY_PRIVATE"
fi

# Check if at least one configuration option is provided
if [[ -z "$FINAL_DEPLOY_KEY" && -z "$CHANNELS_FILE" && -z "$SIGNING_KEY" && -z "$HOST_KEY_PRIVATE" ]]; then
    error "No configuration provided. Specify at least one of:\n  --ssh-deploy-key-file, --channels-file, --signing-key-file, or --host-key-file\nSee --help for more information."
fi

# Set default channels file if not provided
if [[ -z "$CHANNELS_FILE" ]]; then
    if [[ -f "$CHANNELS_LOCK" ]]; then
        CHANNELS_FILE="$CHANNELS_LOCK"
        info "Using default channels file: $CHANNELS_FILE"
    else
        warn "No channels file specified and channels-lock.scm not found"
        warn "Installed systems will not have pinned channel specifications"
    fi
elif [[ -f "$CHANNELS_FILE" ]]; then
    info "Using channels file: $CHANNELS_FILE"
fi

# Generate output filename if not specified
if [[ -z "$OUTPUT" ]]; then
    TIMESTAMP=$(date +%Y%m%d-%H%M%S)
    OUTPUT="$REPO_DIR/systole-installer-deploy-$TIMESTAMP.iso"
fi

info "Building installer ISO with configuration..."
info "Output: $OUTPUT"

# Build the ISO using guix system image
cd "$REPO_DIR"

# Build Scheme expression with all provided parameters
EXPRESSION="(use-modules (os install)) ((@ (os install) systole-os-installation-with-deploy-key)"

if [[ -n "$FINAL_DEPLOY_KEY" ]]; then
    ESCAPED_KEY="${FINAL_DEPLOY_KEY//\\/\\\\}"
    ESCAPED_KEY="${ESCAPED_KEY//\"/\\\"}"
    EXPRESSION="$EXPRESSION #:ssh-deploy-key \"$ESCAPED_KEY\""
fi

if [[ -n "$CHANNELS_FILE" && -f "$CHANNELS_FILE" ]]; then
    ESCAPED_CHANNELS="${CHANNELS_FILE//\\/\\\\}"
    ESCAPED_CHANNELS="${ESCAPED_CHANNELS//\"/\\\"}"
    EXPRESSION="$EXPRESSION #:channels-file \"$ESCAPED_CHANNELS\""
fi

if [[ -n "$SIGNING_KEY" ]]; then
    ESCAPED_SIGNING="${SIGNING_KEY//\\/\\\\}"
    ESCAPED_SIGNING="${ESCAPED_SIGNING//\"/\\\"}"
    EXPRESSION="$EXPRESSION #:signing-key \"$ESCAPED_SIGNING\""
fi

if [[ -n "$HOST_KEY_PRIVATE" && -f "$HOST_KEY_PRIVATE" ]]; then
    ESCAPED_PRIVATE="${HOST_KEY_PRIVATE//\\/\\\\}"
    ESCAPED_PRIVATE="${ESCAPED_PRIVATE//\"/\\\"}"
    ESCAPED_PUBLIC="${HOST_KEY_PUBLIC//\\/\\\\}"
    ESCAPED_PUBLIC="${ESCAPED_PUBLIC//\"/\\\"}"
    EXPRESSION="$EXPRESSION #:host-key-private \"$ESCAPED_PRIVATE\" #:host-key-public \"$ESCAPED_PUBLIC\""
fi

EXPRESSION="$EXPRESSION)"

# Build with time-machine for reproducibility (uses channels-lock.scm)
if [[ "$USE_TIME_MACHINE" == "true" ]]; then
    if [[ ! -f "$CHANNELS_LOCK" ]]; then
        error "channels-lock.scm not found at: $CHANNELS_LOCK\nRun 'guix describe -f channels > channels-lock.scm' to create it."
    fi
    info "Using guix time-machine with channels-lock.scm for reproducible build"
    info "Channel lock file: $CHANNELS_LOCK"
    STORE_PATH=$(guix time-machine -C "$CHANNELS_LOCK" -- system image -t iso9660 -L "$REPO_DIR/systole" -L "$REPO_DIR/system" -e "$EXPRESSION")
else
    info "Building with current channels (--no-time-machine specified)"
    STORE_PATH=$(guix system image -t iso9660 -L "$REPO_DIR/systole" -L "$REPO_DIR/system" -e "$EXPRESSION")
fi

if [[ $? -eq 0 && -n "$STORE_PATH" ]]; then
    info "Build completed successfully!"
    info "Image built at: $STORE_PATH"

    # Copy the ISO from the store to the desired output location
    info "Copying ISO to: $OUTPUT"
    if cp "$STORE_PATH" "$OUTPUT"; then
        info "ISO copied successfully!"
        echo ""
        echo -e "${GREEN}===== Next Steps =====${NC}"
        echo "1. Boot the ISO on your target machine:"
        echo "   - Physical: Write to USB with 'dd if=$OUTPUT of=/dev/sdX bs=4M status=progress'"
        echo "   - VM: Use the ISO directly with your hypervisor"
        echo ""
        echo "2. Ensure the target has network connectivity"
        echo ""
        echo "3. SSH to the installer as root:"
        echo "   ssh -i /path/to/your/private/key root@<target-ip>"
        echo ""
        echo "4. Deploy your system configuration:"
        echo "   guix deploy your-deployment.scm"
        echo ""
        echo -e "${YELLOW}Note: The deploy key only works in the installer environment.${NC}"
        echo -e "${YELLOW}After installation, the target system uses its own SSH configuration.${NC}"
    else
        error "Failed to copy ISO from store to $OUTPUT"
    fi
else
    error "Build failed. Check the output above for errors."
fi
