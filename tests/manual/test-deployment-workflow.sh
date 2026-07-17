#!/usr/bin/env bash
# Test complete deployment workflow
# Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

# Configuration
TEST_DIR="/tmp/systole-deployment-test"
SSH_KEY="$TEST_DIR/test-deploy-key"
VM_DISK="$TEST_DIR/test-disk.qcow2"
VM_PORT=2223
VM_PID_FILE="$TEST_DIR/qemu.pid"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

step() {
    echo -e "${BLUE}==>${NC} $*"
}

info() {
    echo -e "${GREEN}INFO:${NC} $*"
}

error() {
    echo -e "${RED}ERROR:${NC} $*" >&2
    cleanup
    exit 1
}

success() {
    echo -e "${GREEN}✓${NC} $*"
}

cleanup() {
    step "Cleaning up..."
    if [[ -f "$VM_PID_FILE" ]]; then
        PID=$(cat "$VM_PID_FILE")
        if kill -0 "$PID" 2>/dev/null; then
            info "Stopping VM (PID: $PID)"
            kill "$PID" 2>/dev/null || true
            sleep 2
            kill -9 "$PID" 2>/dev/null || true
        fi
        rm -f "$VM_PID_FILE"
    fi
}

trap cleanup EXIT

echo "========================================="
echo "  Systole Deployment Workflow Test"
echo "========================================="
echo ""

# Step 1: Setup test environment
step "Setting up test environment"
mkdir -p "$TEST_DIR"
cd "$(dirname "$0")/.."

# Step 2: Generate SSH key if needed
if [[ ! -f "$SSH_KEY" ]]; then
    step "Generating test SSH key"
    ssh-keygen -t ed25519 -f "$SSH_KEY" -N "" -C "systole-test"
    success "SSH key generated"
else
    info "Using existing SSH key: $SSH_KEY"
fi

# Step 3: Build installer ISO
step "Building installer ISO with deploy key"
if ! ./scripts/build-installer-with-deploy.sh --key-file "$SSH_KEY.pub"; then
    error "Failed to build installer ISO"
fi

# Find the generated ISO
ISO_PATH=$(ls -t systole-installer-deploy-*.iso 2>/dev/null | head -1)
if [[ -z "$ISO_PATH" || ! -f "$ISO_PATH" ]]; then
    error "Installer ISO not found"
fi
success "Installer ISO built: $ISO_PATH"

# Step 4: Create VM disk
step "Creating VM disk"
if [[ -f "$VM_DISK" ]]; then
    info "Removing existing disk: $VM_DISK"
    rm -f "$VM_DISK"
fi
qemu-img create -f qcow2 "$VM_DISK" 20G
success "VM disk created: $VM_DISK"

# Step 5: Boot VM
step "Booting VM from installer ISO"
qemu-system-x86_64 \
  -m 4096 \
  -smp 2 \
  -cdrom "$ISO_PATH" \
  -boot d \
  -net nic,model=virtio \
  -net user,hostfwd=tcp::${VM_PORT}-:22 \
  -drive file="$VM_DISK",format=qcow2,if=virtio \
  -enable-kvm \
  -display none \
  -daemonize \
  -pidfile "$VM_PID_FILE"

if [[ ! -f "$VM_PID_FILE" ]]; then
    error "Failed to start VM"
fi
VM_PID=$(cat "$VM_PID_FILE")
success "VM started (PID: $VM_PID, SSH port: $VM_PORT)"

# Step 6: Wait for SSH
step "Waiting for SSH to become available (this may take 2-3 minutes)..."
MAX_ATTEMPTS=60
ATTEMPT=0
while ! ssh -i "$SSH_KEY" -p "$VM_PORT" -o StrictHostKeyChecking=no -o ConnectTimeout=5 root@localhost "echo Connected" &>/dev/null; do
    ATTEMPT=$((ATTEMPT + 1))
    if [[ $ATTEMPT -ge $MAX_ATTEMPTS ]]; then
        error "SSH connection timed out after $MAX_ATTEMPTS attempts"
    fi
    echo -n "."
    sleep 5
done
echo ""
success "SSH connection established"

# Step 7: Verify installer environment
step "Verifying installer environment"
ssh -i "$SSH_KEY" -p "$VM_PORT" -o StrictHostKeyChecking=no root@localhost << 'EOF'
echo "=== System Info ==="
uname -a
echo ""
echo "=== Guix Version ==="
guix --version | head -1
echo ""
echo "=== Available Disks ==="
lsblk
echo ""
echo "=== Network ==="
ip addr show | grep "inet "
EOF
success "Installer environment verified"

# Step 8: Copy automation script
step "Copying automated installer script"
scp -i "$SSH_KEY" -P "$VM_PORT" -o StrictHostKeyChecking=no \
    ./scripts/systole-auto-install.sh \
    root@localhost:/tmp/
success "Automation script copied"

# Step 9: Get deployer signing key
step "Getting deployer signing key"
if [[ -f /etc/guix/signing-key.pub ]]; then
    DEPLOYER_KEY=$(cat /etc/guix/signing-key.pub)
    info "Using guix signing key from /etc/guix/signing-key.pub"
else
    warn "No guix signing key found - deployer authorization will not be configured"
    DEPLOYER_KEY=""
fi

# Step 10: Run automated installation
step "Running automated installation"
echo ""
info "This will perform unattended installation:"
info "  - Partition /dev/vda"
info "  - Install system"
info "  - Configure SSH and deployment keys"
echo ""

DEPLOY_KEY_CONTENT=$(cat "$SSH_KEY.pub")

ssh -i "$SSH_KEY" -p "$VM_PORT" -o StrictHostKeyChecking=no root@localhost << EOF
chmod +x /tmp/systole-auto-install.sh
/tmp/systole-auto-install.sh \
  --disk /dev/vda \
  --hostname test-systole \
  --timezone UTC \
  --deploy-key "$DEPLOY_KEY_CONTENT" \
  ${DEPLOYER_KEY:+--deployer-key "$DEPLOYER_KEY"} << 'CONFIRM'
yes
CONFIRM
EOF

if [[ $? -eq 0 ]]; then
    success "Automated installation completed successfully!"
else
    error "Automated installation failed"
fi

# Step 11: Test results
step "Testing installation results"
ssh -i "$SSH_KEY" -p "$VM_PORT" -o StrictHostKeyChecking=no root@localhost << 'EOF'
echo "=== Checking mounted filesystems ==="
df -h /mnt
ls -la /mnt/etc/config.scm
echo ""
echo "=== Configuration preview ==="
head -30 /mnt/etc/config.scm
EOF

echo ""
echo "========================================="
echo -e "${GREEN}  Test Workflow Complete!${NC}"
echo "========================================="
echo ""
echo "Results:"
echo "  ✓ Installer ISO built successfully"
echo "  ✓ VM booted and SSH accessible"
echo "  ✓ Automated installation executed"
echo "  ✓ System installed to disk"
echo ""
echo "VM is still running (PID: $VM_PID, Port: $VM_PORT)"
echo ""
echo "Next steps:"
echo "  1. SSH to installed system:"
echo "     ssh -i $SSH_KEY -p $VM_PORT root@localhost"
echo ""
echo "  2. Stop VM when done:"
echo "     kill $VM_PID"
echo ""
echo "  3. To boot installed system:"
echo "     qemu-system-x86_64 -m 4096 -smp 2 \\"
echo "       -drive file=$VM_DISK,format=qcow2,if=virtio \\"
echo "       -net nic,model=virtio \\"
echo "       -net user,hostfwd=tcp::${VM_PORT}-:22 \\"
echo "       -enable-kvm"
echo ""
