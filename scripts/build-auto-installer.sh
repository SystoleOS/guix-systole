#!/usr/bin/env bash
# Build automatic installer ISO for testing
# Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

DEPLOY_KEY=""
OUTPUT=""
REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

usage() {
    cat <<EOF
Usage: $0 --key-file KEY_FILE [--output FILE]

Build an automatic installer ISO that will:
  1. Boot automatically
  2. Partition /dev/vda
  3. Install a minimal Systole system with SSH key
  4. Reboot into installed system

OPTIONS:
    --key-file FILE        SSH public key file for deployment
    --output FILE          Output ISO file path (default: auto-generated)
    -h, --help            Show this help

EXAMPLE:
    $0 --key-file ~/.ssh/id_ed25519.pub

WARNING:
    This ISO will AUTOMATICALLY INSTALL to /dev/vda on boot!
    Only use for testing in VMs!

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

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --key-file)
            if [[ ! -f "$2" ]]; then
                error "Key file not found: $2"
            fi
            DEPLOY_KEY="$(cat "$2")"
            shift 2
            ;;
        --output)
            OUTPUT="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            error "Unknown option: $1\nUse --help for usage information."
            ;;
    esac
done

# Validate key provided
if [[ -z "$DEPLOY_KEY" ]]; then
    error "No deploy key provided. Use --key-file to specify."
fi

# Validate key format
if [[ ! "$DEPLOY_KEY" =~ ^ssh-(rsa|dsa|ecdsa|ed25519)[[:space:]] ]]; then
    error "Invalid SSH key format."
fi

# Generate output filename
if [[ -z "$OUTPUT" ]]; then
    TIMESTAMP=$(date +%Y%m%d-%H%M%S)
    OUTPUT="$REPO_DIR/systole-auto-installer-$TIMESTAMP.iso"
fi

KEY_TYPE=$(echo "$DEPLOY_KEY" | awk '{print $1}')
KEY_COMMENT=$(echo "$DEPLOY_KEY" | awk '{print $3}')
info "Deploy key type: $KEY_TYPE${KEY_COMMENT:+ ($KEY_COMMENT)}"

warn "This ISO will AUTOMATICALLY INSTALL to /dev/vda on boot!"
warn "Only use for testing in VMs!"
echo ""

info "Building automatic installer ISO..."
info "Output: $OUTPUT"

cd "$REPO_DIR"

# Escape the deploy key
ESCAPED_KEY="${DEPLOY_KEY//\\/\\\\}"
ESCAPED_KEY="${ESCAPED_KEY//\"/\\\"}"

# Build expression
EXPRESSION="(use-modules (os auto-install)) ((@ (os auto-install) systole-auto-install-os) #:deploy-key \"$ESCAPED_KEY\")"

info "Running guix system image..."
STORE_PATH=$(guix system image -t iso9660 -L "$REPO_DIR/systole" -L "$REPO_DIR/system" -e "$EXPRESSION")

if [[ $? -eq 0 && -n "$STORE_PATH" ]]; then
    info "Build completed successfully!"
    info "Image built at: $STORE_PATH"

    info "Copying ISO to: $OUTPUT"
    if cp "$STORE_PATH" "$OUTPUT"; then
        info "ISO copied successfully!"
        echo ""
        echo -e "${GREEN}===== Automatic Installer ISO Ready =====${NC}"
        echo ""
        echo -e "${YELLOW}⚠️  WARNING: This ISO will automatically install to /dev/vda on boot!${NC}"
        echo ""
        echo "What it does:"
        echo "  1. Boots automatically"
        echo "  2. Partitions /dev/vda (512MB EFI + rest as root)"
        echo "  3. Installs minimal Systole system"
        echo "  4. Configures SSH with your deploy key"
        echo "  5. Reboots into installed system"
        echo ""
        echo "To test:"
        echo "  1. Create VM disk: qemu-img create -f qcow2 test.qcow2 20G"
        echo "  2. Boot ISO: qemu-system-x86_64 -m 4096 -cdrom $OUTPUT \\"
        echo "                 -drive file=test.qcow2,format=qcow2 \\"
        echo "                 -net user,hostfwd=tcp::2223-:22 -enable-kvm"
        echo "  3. Wait 10-30 minutes for automatic installation"
        echo "  4. System will reboot automatically"
        echo "  5. SSH to installed system: ssh -i your-key -p 2223 root@localhost"
        echo "  6. Test guix deploy!"
        echo ""
    else
        error "Failed to copy ISO from store to $OUTPUT"
    fi
else
    error "Build failed. Check the output above for errors."
fi
