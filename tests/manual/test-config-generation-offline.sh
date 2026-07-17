#!/usr/bin/env bash
# Offline config generation test - no VM or ISO needed
# Tests SSH deploy key injection in generated config.scm

set -e

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_DIR="/tmp/systole-config-test"
TEST_KEY="ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBtESyRjo6GOaeM0YjbtOQ8L3qZpQ8eFple/nt0tkfRd test@test"

echo "============================================="
echo " Offline Config Generation Test"
echo " (No VM or ISO needed)"
echo "============================================="
echo

# Cleanup
rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR"

# Step 1: Create test deploy key
echo "[1/7] Creating test deploy key..."
mkdir -p "$TEST_DIR/etc"
echo "$TEST_KEY" > "$TEST_DIR/etc/systole-deploy-key.pub"
echo "  ✓ Created test key"

# Step 2: Temporarily patch steps.scm
echo
echo "[2/7] Patching installer code for testing..."
cp "$REPO_DIR/system/installer/steps.scm" "$TEST_DIR/steps.scm.backup"

sed -i "s|/etc/systole-deploy-key.pub|$TEST_DIR/etc/systole-deploy-key.pub|g" \
    "$REPO_DIR/system/installer/steps.scm"

echo "  ✓ Patched to use test path"

# Step 3: Generate config
echo
echo "[3/7] Generating config.scm..."

cat > "$TEST_DIR/test-generate.scm" << 'GENEOF'
(add-to-load-path "/home/rafael/src/guix-systole/system")
(add-to-load-path "/home/rafael/src/guix-systole/systole")

(use-modules (installer steps)
             (gnu installer steps)
             (gnu installer record)
             (guix read-print))

(define test-steps
  (list
   (installer-step
    (id 'locale)
    (compute (lambda () #t))
    (configuration-formatter (lambda (r) '((locale "en_US.utf8")))))
   (installer-step
    (id 'timezone)
    (compute (lambda () #t))
    (configuration-formatter (lambda (r) '((timezone "UTC")))))
   (installer-step
    (id 'hostname)
    (compute (lambda () #t))
    (configuration-formatter (lambda (r) '((host-name "test-deploy")))))
   (installer-step
    (id 'bootloader)
    (compute (lambda () #t))
    (configuration-formatter
     (lambda (r) '((bootloader (bootloader-configuration
                                (bootloader grub-efi-bootloader)
                                (targets '("/boot/efi"))))))))
   (installer-step
    (id 'file-systems)
    (compute (lambda () #t))
    (configuration-formatter
     (lambda (r) '((file-systems
                   (cons* (file-system
                           (device (file-system-label "root"))
                           (mount-point "/")
                           (type "ext4"))
                          (file-system
                           (device (file-system-label "boot"))
                           (mount-point "/boot/efi")
                           (type "vfat"))
                          %base-file-systems))))))
   (installer-step
    (id 'services)
    (compute (lambda () #t))
    (configuration-formatter
     (lambda (r) '((services (append (list (service openssh-service-type)
                                           (service dhcp-client-service-type))
                                     %base-services))))))))

(define test-results '((locale . #t) (timezone . #t) (hostname . #t)
                       (bootloader . #t) (file-systems . #t) (services . #t)))

(let ((config (systole-format-configuration test-steps test-results)))
  (call-with-output-file "/tmp/systole-config-test/generated.scm"
    (lambda (port) (pretty-print-with-comments port config)))
  (display "Generated\n"))
GENEOF

if guile "$TEST_DIR/test-generate.scm" 2>&1 | tee "$TEST_DIR/gen.log"; then
    echo "  ✓ Config generated"
else
    echo "  ✗ FAILED"
    cat "$TEST_DIR/gen.log"
    mv "$TEST_DIR/steps.scm.backup" "$REPO_DIR/system/installer/steps.scm"
    exit 1
fi

# Step 4: Restore original
echo
echo "[4/7] Restoring original code..."
mv "$TEST_DIR/steps.scm.backup" "$REPO_DIR/system/installer/steps.scm"
echo "  ✓ Restored"

# Step 5: Check for SSH service with deploy key injection
echo
echo "[5/7] Verifying SSH service with deploy key..."
if grep -q "openssh-configuration" "$TEST_DIR/generated.scm"; then
    echo "  ✓ SSH configuration FOUND"
    echo
    echo "  Generated SSH config:"
    grep -B 2 -A 6 "openssh-configuration" "$TEST_DIR/generated.scm" | sed 's/^/      /'
else
    echo "  ✗ SSH configuration NOT found!"
    echo
    echo "Full config:"
    cat "$TEST_DIR/generated.scm"
    exit 1
fi

if grep -q "deploy-key.pub" "$TEST_DIR/generated.scm"; then
    echo "  ✓ Deploy key file reference found"
else
    echo "  ✗ Deploy key file reference NOT found!"
    exit 1
fi

if grep -q "authorized-keys" "$TEST_DIR/generated.scm"; then
    echo "  ✓ Authorized-keys field found"
else
    echo "  ✗ Authorized-keys field NOT found!"
    exit 1
fi

# Step 6: Validate syntax
echo
echo "[6/7] Validating Scheme syntax..."
if guile -c "(call-with-input-file \"$TEST_DIR/generated.scm\" read)" >/dev/null 2>&1; then
    echo "  ✓ Valid syntax"
else
    echo "  ✗ SYNTAX ERROR:"
    guile -c "(call-with-input-file \"$TEST_DIR/generated.scm\" read)"
    exit 1
fi

# Step 7: Test with guix system
echo
echo "[7/7] Testing with 'guix system build'..."
echo "      (May take 30-60 seconds...)"

if timeout 120 guix system build "$TEST_DIR/generated.scm" 2>&1 | tee "$TEST_DIR/build.log" | tail -3; then
    echo "  ✓ BUILD SUCCESSFUL!"
else
    echo "  ✗ BUILD FAILED!"
    echo
    echo "Last 30 lines of build output:"
    tail -30 "$TEST_DIR/build.log"
    exit 1
fi

echo
echo "============================================="
echo " ✓ ALL TESTS PASSED!"
echo "============================================="
echo
echo "Files:"
echo "  Config: $TEST_DIR/generated.scm"
echo "  Logs:   $TEST_DIR/"
echo
echo "View config:"
echo "  cat $TEST_DIR/generated.scm"
