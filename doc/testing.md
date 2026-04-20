# Testing Guide

## Quick Reference

```bash
./scripts/run-tests.sh                 # all fast tests (CI runs this)
./scripts/run-tests.sh packages        # module-load + package-definition tests
./scripts/run-tests.sh installer       # installer module tests
./scripts/run-tests.sh lint            # guix lint over the channel
```

## Test Categories

### Package Tests

**Module loading** (`tests/packages/test-modules-load.sh`): verifies that
all `(systole packages ...)` modules load without errors via `guix repl`.

**Package definitions** (`tests/packages/test-package-definitions.sh`):
checks that all exported `define-public` packages are accessible and
have valid metadata.

Run individually:

```bash
tests/packages/test-modules-load.sh
tests/packages/test-package-definitions.sh
```

Or smoke-test a single module:

```bash
guix repl -L systole <<< ',m (systole packages slicer)'
```

### Installer Tests

**Module loading** (`tests/installer/test-dry-run.sh`): verifies that
the installer modules load and the OS definitions evaluate.

### Lint

Runs `guix lint` on all channel packages. Excludes system modules that
depend on nonguix (which may not be available in CI).

### VM Tests (slow)

Boot real VMs using Guix's Marionette framework. See
[VM Testing](vm-testing.md) for details.

```bash
./scripts/run-vm-tests.sh              # all VM tests
./scripts/run-vm-tests.sh basic        # installer boot test
./scripts/run-vm-tests.sh deploy-key   # SSH deploy-key feature
```

### Build Tests (manual)

Full package builds are not run in CI due to time constraints.
Test locally:

```bash
guix build -L systole slicer           # builds Slicer + all deps
guix build -L systole sofa-framework   # builds SOFA Framework
```

## CI Integration

GitHub Actions runs on every PR and push to main:

- **commit-message-check** — enforces `[Prefix][Component] Title` format
- **package-tests** — module loading + package definitions
- **installer-tests** — installer module loading
- **guix-lint-check** — lint all channel packages

See `.github/workflows/` for configuration.

## Writing New Tests

When adding a new package module:

1. Add the module name to the list in `tests/packages/test-modules-load.sh`
2. Add the package name(s) to `tests/packages/test-package-definitions.sh`
3. If the package needs patches, ensure the patch directory is registered
   in `systole/systole/packages.scm` `%patch-path`
4. Run `./scripts/run-tests.sh` to verify
