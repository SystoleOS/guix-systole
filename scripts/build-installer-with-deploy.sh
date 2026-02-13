#!/usr/bin/env bash
# Build Systole installer ISO with SSH deploy key support
# Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

# Default values
DEPLOY_KEY=""
CHANNELS_FILE=""
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

Build a Systole installer ISO with SSH deploy key for remote deployment.
Uses channels-lock.scm for reproducible builds via guix time-machine.

OPTIONS:
    --key KEY              SSH public key string (e.g., "ssh-ed25519 AAAA...")
    --key-file FILE        Path to SSH public key file
    --channels-file FILE   Path to channels.scm file (default: channels-lock.scm)
    --output FILE          Output ISO file path (default: auto-generated)
    --no-time-machine      Skip guix time-machine (use current channels)
    -h, --help            Show this help message

ENVIRONMENT VARIABLES:
    SYSTOLE_DEPLOY_KEY          SSH public key string
    SYSTOLE_DEPLOY_KEY_FILE     Path to SSH public key file

The SSH public key can be provided via:
  1. --key command-line argument
  2. --key-file command-line argument
  3. SYSTOLE_DEPLOY_KEY environment variable
  4. SYSTOLE_DEPLOY_KEY_FILE environment variable

Priority: command-line arguments take precedence over environment variables.

EXAMPLES:
    # Using key file
    $0 --key-file ~/.ssh/id_ed25519.pub

    # Using key string
    $0 --key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5... user@host"

    # Using environment variable
    SYSTOLE_DEPLOY_KEY="\$(cat ~/.ssh/id_ed25519.pub)" $0

    # Custom output path
    $0 --key-file ~/.ssh/id_ed25519.pub --output /tmp/systole-deploy.iso

NOTES:
    - The SSH key authorizes root user access in the installer environment only
    - After installation, the target system uses its own configuration
    - Only key-based authentication is enabled (password auth disabled)
    - The installer ISO will have network support for SSH access

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
        --key)
            if [[ -z "${2:-}" ]]; then
                error "No key specified after --key"
            fi
            DEPLOY_KEY="$2"
            shift 2
            ;;
        --key-file)
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

# Fall back to environment variables if no key provided
if [[ -z "$DEPLOY_KEY" ]]; then
    if [[ -n "${SYSTOLE_DEPLOY_KEY:-}" ]]; then
        DEPLOY_KEY="$SYSTOLE_DEPLOY_KEY"
        info "Using deploy key from SYSTOLE_DEPLOY_KEY environment variable"
    elif [[ -n "${SYSTOLE_DEPLOY_KEY_FILE:-}" ]]; then
        if [[ ! -f "$SYSTOLE_DEPLOY_KEY_FILE" ]]; then
            error "Key file not found: $SYSTOLE_DEPLOY_KEY_FILE"
        fi
        DEPLOY_KEY="$(cat "$SYSTOLE_DEPLOY_KEY_FILE")"
        info "Using deploy key from file: $SYSTOLE_DEPLOY_KEY_FILE"
    fi
fi

# Validate that a deploy key was provided
if [[ -z "$DEPLOY_KEY" ]]; then
    error "No deploy key provided. Use --key, --key-file, or environment variables.\nSee --help for more information."
fi

# Basic validation of key format
if [[ ! "$DEPLOY_KEY" =~ ^ssh-(rsa|dsa|ecdsa|ed25519)[[:space:]] ]]; then
    error "Invalid SSH key format. Expected key to start with ssh-rsa, ssh-dsa, ssh-ecdsa, or ssh-ed25519"
fi

# Extract key type for info message
KEY_TYPE=$(echo "$DEPLOY_KEY" | awk '{print $1}')
KEY_COMMENT=$(echo "$DEPLOY_KEY" | awk '{print $3}')
info "Deploy key type: $KEY_TYPE${KEY_COMMENT:+ ($KEY_COMMENT)}"

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

info "Building installer ISO with deploy key..."
info "Output: $OUTPUT"

# Escape the deploy key for embedding in Scheme expression
# Replace backslashes and quotes
ESCAPED_KEY="${DEPLOY_KEY//\\/\\\\}"
ESCAPED_KEY="${ESCAPED_KEY//\"/\\\"}"

# Build the ISO using guix system image
cd "$REPO_DIR"

# Build expression as single line for proper passing to guix
if [[ -n "$CHANNELS_FILE" && -f "$CHANNELS_FILE" ]]; then
    # Escape the channels file path for embedding in Scheme expression
    ESCAPED_CHANNELS_FILE="${CHANNELS_FILE//\\/\\\\}"
    ESCAPED_CHANNELS_FILE="${ESCAPED_CHANNELS_FILE//\"/\\\"}"
    EXPRESSION="(use-modules (os install)) ((@ (os install) systole-os-installation-with-deploy-key) #:deploy-key \"$ESCAPED_KEY\" #:channels-file \"$ESCAPED_CHANNELS_FILE\")"
else
    EXPRESSION="(use-modules (os install)) ((@ (os install) systole-os-installation-with-deploy-key) #:deploy-key \"$ESCAPED_KEY\")"
fi

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
