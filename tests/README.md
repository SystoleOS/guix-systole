# Testing Infrastructure

This directory contains the test suite for Guix-Systole. The full
guide, including CI details and the `GUIX` override for pinned runs,
is in [doc/testing.md](../doc/testing.md).

## Quick Start

```bash
# Run all fast tests (packages + installer + lint)
./scripts/run-tests.sh

# Run a specific category
./scripts/run-tests.sh packages
./scripts/run-tests.sh installer
./scripts/run-tests.sh lint

# Run build tests (WARNING: very slow!)
./scripts/run-tests.sh build

# Run against the pinned channels, as CI does
GUIX="guix time-machine -C channels-lock.scm --" ./scripts/run-tests.sh

# See all options
./scripts/run-tests.sh --help
```

## Directory Structure

```
tests/
├── packages/
│   ├── test-modules-load.sh    # wrapper: runs check-packages.scm in script mode
│   └── check-packages.scm      # module discovery + package sweep + API contract
├── installer/
│   ├── test-dry-run.sh         # wrapper: runs check-installer.scm in script mode
│   ├── check-installer.scm     # installer/OS module + entry-point checks
│   └── installer.scm           # VM tests, module (tests installer installer)
├── manual/                     # interactive end-to-end scripts (see its README)
└── lint-allowlist.regex        # guix lint warnings accepted as technical debt
```

## Test Categories

- **Package tests** (`tests/packages/`): load every `(systole ...)`
  module, touch every exported package, and verify the channel's public
  API contract. New modules are discovered automatically.
- **Installer tests** (`tests/installer/`): load all installer/OS
  modules and verify the ISO-build entry points. Needs
  `guix shell guile guile-newt guile-parted guile-webutils` (the
  wrapper handles this).
- **Lint**: `guix lint -L systole` over the `LINT_PACKAGES` list in
  `scripts/run-tests.sh`, gated on `lint-allowlist.regex`.
- **VM tests**: Marionette system tests booting full QEMU VMs; run with
  `./scripts/run-vm-tests.sh [basic|deploy-key|no-ssh|all]`.
- **Manual tests** (`tests/manual/`): deployment workflows that need
  real hardware or human interaction.

## Writing New Tests

Shell tests named `tests/packages/test-*.sh` or
`tests/installer/test-*.sh` are discovered and run automatically by
`run-tests.sh`. Keep them:

1. **Fast by default** — slow things go in `build`, VM, or manual tests
2. **Honoring `$GUIX`** — so CI can pin them via time-machine
3. **Exit-status-clean** — non-zero on failure; prefer `guix repl`
   *script mode* over heredoc REPL sessions (which always exit 0)
4. **Using `-L systole`** — never `-L .`; the channel root is the
   `systole/` subdirectory

Example build check inside a test:

```bash
guix build -L "$REPO_ROOT/systole" mypackage
```
