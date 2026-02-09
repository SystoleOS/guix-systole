# Testing Infrastructure

This directory contains the testing infrastructure for Guix-Systole.

## Quick Start

```bash
# Run all tests (except slow build tests)
./scripts/run-tests.sh

# Run specific test category
./scripts/run-tests.sh packages
./scripts/run-tests.sh installer
./scripts/run-tests.sh lint

# Run build tests (WARNING: very slow!)
./scripts/run-tests.sh build

# See all options
./scripts/run-tests.sh --help
```

## Test Categories

### Package Tests (`tests/packages/`)
- **Module loading tests:** Verify all Guile modules load without errors
- **Package definitions:** Test package syntax and structure
- **Build tests:** Actually build packages (slow, not run by default in CI)
- **Functionality tests:** Test that packages work as expected

### Installer Tests (`tests/installer/`)
- **Module loading:** Verify installer modules load correctly
- **Dry-run tests:** Test installer logic without actual installation
- **VM tests:** Test full installer in QEMU virtual machines (to be implemented)

### Lint Tests
- Run `guix lint` on all packages
- Check for common issues and Guix standards compliance
- Already integrated in CI

## Directory Structure

```
tests/
├── packages/          # Package-specific tests
│   └── test-modules-load.sh
├── installer/         # Installer tests
│   └── test-dry-run.sh
├── system/            # System configuration tests (to be added)
└── common/            # Shared test utilities (to be added)
```

## Writing New Tests

### Shell Script Tests

Create a test script in the appropriate directory:

```bash
#!/usr/bin/env bash
# tests/packages/test-mypackage.sh

set -e

echo "Testing my package..."

# Your test logic here
if guix build -L . mypackage; then
    echo "✓ Build succeeded"
    exit 0
else
    echo "✗ Build failed"
    exit 1
fi
```

Make it executable:
```bash
chmod +x tests/packages/test-mypackage.sh
```

The test runner will automatically discover and run it.

### Guile/Scheme Tests

For more complex tests, use Guile:

```scheme
;; tests/packages/test-mypackage.scm
(define-module (tests packages mypackage)
  #:use-module (systole packages mypackage)
  #:use-module (srfi srfi-64))

(test-begin "mypackage")

(test-assert "package builds"
  ;; Test logic here
  #t)

(test-end "mypackage")
```

## CI Integration

Tests run automatically in GitHub Actions:
- **On every PR:** Module loading and lint tests
- **On push to main/dev:** Full test suite (except builds)
- **Build tests:** Run manually due to time constraints

See `.github/workflows/` for CI configuration.

## Adding Tests for Refactoring

Before refactoring code:

1. **Add tests for current behavior**
   ```bash
   # Create test that validates current functionality
   vim tests/packages/test-before-refactor.sh
   ./scripts/run-tests.sh packages
   ```

2. **Refactor the code**

3. **Run tests to verify nothing broke**
   ```bash
   ./scripts/run-tests.sh
   ```

4. **Add tests for new behavior**

## Test Best Practices

1. **Fast by default:** Don't include slow tests in the main suite
2. **Isolated:** Tests should not depend on each other
3. **Deterministic:** Same input = same output, every time
4. **Clear output:** Make it obvious what passed and what failed
5. **Documentation:** Comment why you're testing something, not just what

## Current Test Coverage

- [x] Package module loading
- [x] Lint checks for all packages
- [ ] Package builds (too slow for CI, run manually)
- [ ] Package functionality tests
- [x] Installer module loading
- [ ] Installer dry-run full workflow
- [ ] Installer VM tests
- [ ] System transformation tests

## Future Enhancements

- [ ] Add pytest-based test framework for better reporting
- [ ] Add coverage metrics
- [ ] Add performance benchmarking
- [ ] Add integration tests for package interactions
- [ ] Add automated VM testing for installer
- [ ] Add snapshot testing for generated configurations
