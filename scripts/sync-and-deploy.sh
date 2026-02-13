#!/usr/bin/env bash
# Sync channels and deploy Systole system
# Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CHANNELS_LOCK="$REPO_DIR/channels-lock.scm"
DEPLOYMENT_FILE=""
SKIP_SYNC=false
USE_TIME_MACHINE=true

usage() {
    cat <<EOF
Usage: $0 [OPTIONS] DEPLOYMENT_FILE

Sync channels and deploy a Systole system configuration.

This script ensures channel compatibility by:
1. Copying channels-lock.scm to the target system
2. Running 'guix pull' on the target to sync channels
3. Deploying the system configuration

OPTIONS:
    --skip-sync            Skip channel synchronization (uses time-machine only)
    --sync                 Force channel synchronization before deploy
    --no-time-machine      Don't use guix time-machine (use current channels)
    -h, --help            Show this help message

ARGUMENTS:
    DEPLOYMENT_FILE       Path to the deployment.scm file

EXAMPLES:
    # Full sync and deploy
    $0 /tmp/deployment.scm

    # Deploy without syncing (if channels already match)
    $0 --skip-sync /tmp/deployment.scm

NOTES:
    - The target must be accessible via SSH as specified in deployment.scm
    - Channel synchronization can take several minutes
    - Requires channels-lock.scm in repository root

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
        --skip-sync)
            SKIP_SYNC=true
            shift 1
            ;;
        -h|--help)
            usage
            ;;
        -*)
            error "Unknown option: $1\nUse --help for usage information."
            ;;
        *)
            DEPLOYMENT_FILE="$1"
            shift 1
            ;;
    esac
done

# Validate deployment file
if [[ -z "$DEPLOYMENT_FILE" ]]; then
    error "No deployment file specified.\nUse --help for usage information."
fi

if [[ ! -f "$DEPLOYMENT_FILE" ]]; then
    error "Deployment file not found: $DEPLOYMENT_FILE"
fi

# Validate channels-lock.scm exists
if [[ ! -f "$CHANNELS_LOCK" ]]; then
    error "channels-lock.scm not found at: $CHANNELS_LOCK\nRun 'guix describe -f channels > channels-lock.scm' to create it."
fi

info "Deployment file: $DEPLOYMENT_FILE"
info "Channel lock: $CHANNELS_LOCK"

# Extract SSH connection details from deployment.scm
# This is a simple extraction - assumes standard format
HOST=$(grep -oP "host-name\s+\"\K[^\"]+(?=\")" "$DEPLOYMENT_FILE" || echo "")
PORT=$(grep -oP "port\s+\K[0-9]+" "$DEPLOYMENT_FILE" || echo "22")
USER=$(grep -oP "user\s+\"\K[^\"]+(?=\")" "$DEPLOYMENT_FILE" || echo "root")
IDENTITY=$(grep -oP "identity\s+\"\K[^\"]+(?=\")" "$DEPLOYMENT_FILE" || echo "")

if [[ -z "$HOST" ]]; then
    error "Could not extract host-name from deployment file.\nPlease check deployment.scm format."
fi

info "Target: $USER@$HOST:$PORT"

# Build SSH command
SSH_OPTS="-o StrictHostKeyChecking=no"
if [[ -n "$IDENTITY" ]]; then
    SSH_OPTS="$SSH_OPTS -i $IDENTITY"
fi
SSH_CMD="ssh $SSH_OPTS -p $PORT $USER@$HOST"

# Test SSH connection
info "Testing SSH connection..."
if ! $SSH_CMD "echo 'SSH connection successful'" &>/dev/null; then
    error "Failed to connect via SSH to $USER@$HOST:$PORT\nPlease check your deployment configuration and SSH key."
fi
info "SSH connection verified"

# Sync channels if requested
if [[ "$SKIP_SYNC" == "false" ]]; then
    info "Syncing channels on target system..."

    # Ensure .config/guix directory exists on target
    info "Ensuring ~/.config/guix directory exists on target..."
    if ! $SSH_CMD 'mkdir -p ~/.config/guix'; then
        error "Failed to create ~/.config/guix directory on target"
    fi

    # Copy channels-lock.scm to target
    info "Copying channels-lock.scm to target..."
    SCP_OPTS="-o StrictHostKeyChecking=no"
    if [[ -n "$IDENTITY" ]]; then
        SCP_OPTS="$SCP_OPTS -i $IDENTITY"
    fi
    if ! scp $SCP_OPTS -P "$PORT" "$CHANNELS_LOCK" "$USER@$HOST:~/.config/guix/channels.scm"; then
        error "Failed to copy channels-lock.scm to target"
    fi

    # Run guix pull on target
    info "Running 'guix pull' on target (this may take several minutes)..."
    if ! $SSH_CMD 'guix pull && hash guix'; then
        error "Failed to run 'guix pull' on target"
    fi

    # Verify channels
    info "Verifying channel versions..."
    $SSH_CMD 'guix describe'

    info "Channel synchronization complete!"
else
    info "Skipping channel synchronization (--skip-sync specified)"
fi

# Deploy
info "Deploying system configuration..."
info "Using time-machine with channels-lock.scm for consistent channel versions..."
echo ""
if guix time-machine -C "$CHANNELS_LOCK" -- deploy "$DEPLOYMENT_FILE"; then
    echo ""
    info "Deployment completed successfully!"
    info "Your system has been reconfigured remotely."
else
    error "Deployment failed. Check the output above for errors."
fi
