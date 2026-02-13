#!/usr/bin/env bash
# Deploy using time-machine with locked channels
# Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Simple deployment wrapper that uses the same channels for deployment
# as were used to build the installer ISO.

set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CHANNELS_LOCK="$REPO_DIR/channels-lock.scm"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

if [[ $# -eq 0 || "$1" == "-h" || "$1" == "--help" ]]; then
    cat <<EOF
Usage: $0 DEPLOYMENT_FILE

Deploy a Guix system using locked channel versions.

This ensures the deployment uses the same channels as the installer ISO,
preventing channel mismatch errors.

EXAMPLE:
    $0 deployment.scm

EQUIVALENT TO:
    guix time-machine -C channels-lock.scm -- deploy deployment.scm

EOF
    exit 0
fi

DEPLOYMENT_FILE="$1"

if [[ ! -f "$DEPLOYMENT_FILE" ]]; then
    echo -e "${RED}ERROR: Deployment file not found: $DEPLOYMENT_FILE${NC}" >&2
    exit 1
fi

if [[ ! -f "$CHANNELS_LOCK" ]]; then
    echo -e "${RED}ERROR: channels-lock.scm not found at: $CHANNELS_LOCK${NC}" >&2
    exit 1
fi

echo -e "${GREEN}Deploying with locked channels from: $CHANNELS_LOCK${NC}"
echo ""

exec guix time-machine -C "$CHANNELS_LOCK" -- deploy "$DEPLOYMENT_FILE"
