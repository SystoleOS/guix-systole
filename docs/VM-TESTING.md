# VM Testing Guide

This document describes the VM-based system tests for Guix-Systole, which use the Guix system testing framework (Marionette) to boot and test full systems in QEMU.

## Overview

VM tests verify that:
- The Systole installer ISO boots successfully
- System services are running correctly
- The SSH deploy key feature works as expected
- Network configuration is correct
- Security settings are properly applied

## Quick Start

```bash
# Run all VM tests
./scripts/run-vm-tests.sh

# Run specific test
./scripts/run-vm-tests.sh basic
./scripts/run-vm-tests.sh deploy-key
./scripts/run-vm-tests.sh no-ssh
```

## Available Tests

### 1. Basic Installer Test (`systole-installer-basic`)

**What it tests:**
- Installer boots successfully
- System is running and accessible
- Kernel is loaded
- Required packages (git, vim, guix) are available
- Installer modules can be loaded

**Run with:**
```bash
./scripts/run-vm-tests.sh basic
```

**What happens:**
1. Builds the basic Systole installer OS
2. Creates a VM and boots it
3. Runs automated tests inside the VM
4. Verifies system functionality

### 2. Deploy Key Test (`systole-installer-deploy-key`)

**What it tests:**
- Installer with deploy key boots successfully
- SSH daemon (sshd) is running
- Authorized keys file exists and has correct permissions
- Deploy key is configured for root user
- Password authentication is disabled
- Root login is restricted to key-based auth only
- Network interface is up

**Run with:**
```bash
./scripts/run-vm-tests.sh deploy-key
```

**What happens:**
1. Builds installer with a test SSH key
2. Boots the VM
3. Verifies SSH service is running
4. Checks SSH configuration
5. Validates security settings

### 3. No SSH Without Key Test (`systole-installer-no-ssh-without-key`)

**What it tests:**
- Basic installer (without deploy key) does NOT have SSH running
- No sshd process exists
- System is still fully functional
- Deploy key is truly optional

**Run with:**
```bash
./scripts/run-vm-tests.sh no-ssh
```

**Purpose:** Ensures that SSH is only enabled when explicitly requested via deploy key.

## Test Architecture

### Guix System Tests Framework

The tests use Guix's built-in system testing framework:

```
┌─────────────────────────────────────────┐
│  Test Definition (systole/tests/...)   │
│  - Defines OS to test                   │
│  - Specifies test assertions            │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│  Virtual Machine (QEMU)                 │
│  - Boots the OS                         │
│  - Runs test code via Marionette       │
│  - Returns results                      │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│  Test Results                           │
│  - Pass/Fail status                     │
│  - Detailed output                      │
│  - Logs in /tmp/test-*.log             │
└─────────────────────────────────────────┘
```

### Marionette

Marionette is a Guix tool that allows:
- Running Scheme code inside a QEMU VM
- Controlling the VM from test code
- Reading files, checking processes, testing services
- Full system introspection

Example:
```scheme
(marionette-eval
  '(file-exists? "/root/.ssh/authorized_keys")
  marionette)
```

## Test Implementation

Tests are defined in `systole/systole/tests/installer.scm`:

```scheme
(define %test-systole-installer-basic
  (system-test
   (name "systole-installer-basic")
   (description "Test basic installer boots")
   (value ...)))
```

Each test:
1. Defines the OS to boot
2. Creates a virtual machine
3. Specifies test assertions
4. Returns a derivation that can be built

## Running Tests Manually

### Using Guix Directly

```bash
# Build and run a specific test
guix build -L systole -L system --expression \
  '(use-modules (systole tests installer)) %test-systole-installer-basic'

# The result is a derivation in /gnu/store
# Output contains test results
```

### Debugging Failed Tests

1. **Check test logs:**
   ```bash
   cat /tmp/test-systole-installer-*.log
   ```

2. **Run test interactively:**
   ```bash
   # Start guix repl
   guix repl -L systole -L system

   # Load test module
   ,use (systole tests installer)

   # Examine test definition
   ,pp %test-systole-installer-basic
   ```

3. **Boot VM manually:**
   ```bash
   # Build the VM
   guix system vm -L systole -L system system/os/install.scm

   # Run the VM
   /gnu/store/...-run-vm.sh
   ```

## Performance Considerations

### Test Duration

- **Basic test:** ~2-3 minutes
- **Deploy key test:** ~3-4 minutes
- **No SSH test:** ~2-3 minutes
- **Total (all tests):** ~8-10 minutes

### Resource Requirements

- **Memory:** 2GB RAM per VM (allocated by QEMU)
- **Disk:** Temporary space for VM images and test outputs
- **CPU:** Tests benefit from KVM acceleration

### KVM Acceleration

For faster tests, ensure KVM is available:

```bash
# Check if KVM is available
ls -l /dev/kvm

# If not available, install KVM
# (Distro-specific, e.g., apt install qemu-kvm)
```

Without KVM, tests will be significantly slower but still functional.

## CI/CD Integration

### GitHub Actions

Example workflow:

```yaml
name: VM Tests

on: [push, pull_request]

jobs:
  vm-tests:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v2

      - name: Install Guix
        run: |
          # Install Guix

      - name: Enable KVM
        run: |
          # Enable nested virtualization if available

      - name: Run VM Tests
        run: ./scripts/run-vm-tests.sh
        timeout-minutes: 30
```

**Note:** VM tests in CI may be slower and require nested virtualization support.

### GitLab CI

```yaml
vm-tests:
  stage: test
  image: guix/guix:latest
  script:
    - ./scripts/run-vm-tests.sh
  timeout: 30m
  only:
    - merge_requests
    - main
```

## Writing New Tests

### Adding a New Test

1. **Edit `systole/systole/tests/installer.scm`:**

```scheme
(define %test-my-new-feature
  (system-test
   (name "my-new-feature")
   (description "Test my new feature works")
   (value
    (mlet* %store-monad ((os -> systole-os-installation)
                         (vm (virtual-machine os)))
      (define test
        (with-imported-modules '((gnu build marionette))
          #~(begin
              (use-modules (gnu build marionette)
                           (srfi srfi-64))

              (define marionette
                (make-marionette (list #$vm)))

              (test-runner-current (system-test-runner #$output))
              (test-begin "my-new-feature")

              (test-assert "my test condition"
                (marionette-eval
                 '(file-exists? "/some/file")
                 marionette))

              (test-end)
              (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

      (gexp->derivation "my-new-feature" test)))))
```

2. **Export the test:**

```scheme
#:export (%test-systole-installer-basic
          %test-systole-installer-deploy-key
          %test-my-new-feature)  ; Add this
```

3. **Add to test runner script:**

Edit `scripts/run-vm-tests.sh` and add your test case.

### Test Assertions

Common patterns:

```scheme
;; Check file exists
(test-assert "file exists"
  (marionette-eval '(file-exists? "/path/to/file") marionette))

;; Check process running
(test-assert "process running"
  (marionette-eval '(zero? (system* "pgrep" "processname")) marionette))

;; Check service status
(test-assert "service running"
  (marionette-eval
   '(begin
      (use-modules (gnu services herd))
      (live-service-running
       (find (lambda (live)
               (memq 'service-name (live-service-provision live)))
             (current-services))))
   marionette))

;; Check file contents
(test-assert "file contains text"
  (marionette-eval
   '(string-contains
     (call-with-input-file "/path/to/file"
       (lambda (port) (read-string port)))
     "expected text")
   marionette))
```

## Troubleshooting

### Tests Timeout

**Problem:** Tests hang or timeout.

**Solutions:**
- Increase timeout in test definition
- Check if KVM is available for acceleration
- Verify enough RAM is allocated to VM

### Tests Fail to Boot

**Problem:** VM fails to boot.

**Solutions:**
- Check that the OS definition is valid
- Try building the ISO manually first
- Check QEMU is installed: `which qemu-system-x86_64`

### SSH Tests Fail

**Problem:** SSH tests can't connect.

**Solutions:**
- Verify SSH service is in the OS services list
- Check firewall settings aren't blocking SSH
- Ensure network is configured in VM

### Permission Denied

**Problem:** Can't access /dev/kvm.

**Solutions:**
```bash
# Add user to kvm group
sudo usermod -a -G kvm $USER

# Re-login or use:
newgrp kvm
```

## Best Practices

1. **Keep tests fast:** Only test what's necessary
2. **Make tests independent:** Each test should be self-contained
3. **Use descriptive names:** Clear test and assertion names
4. **Test both positive and negative cases:** Verify features work AND don't work when they shouldn't
5. **Add documentation:** Comment complex test logic
6. **Run locally before CI:** Catch issues early

## Related Documentation

- [Testing Guide](../README.testing.md) - Overview of all testing approaches
- [Contributing Guide](../CONTRIBUTING.md) - How to contribute tests
- [Guix Manual: Running the Test Suite](https://guix.gnu.org/manual/en/html_node/Running-the-Test-Suite.html)
- [Guix Manual: System Tests](https://guix.gnu.org/manual/en/html_node/System-Tests.html)

## Future Improvements

Potential enhancements for VM tests:

- [ ] Test actual SSH connectivity with real key pair
- [ ] Test full deployment workflow with `guix deploy`
- [ ] Test installer UI in non-interactive mode
- [ ] Performance benchmarking
- [ ] Network configuration testing
- [ ] Installer scripted installation test
- [ ] Multi-VM tests (client/server scenarios)
