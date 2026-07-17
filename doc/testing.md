# Testing Guide

## Quick Reference

```bash
./scripts/run-tests.sh                 # packages + installer + lint (default: "all")
./scripts/run-tests.sh packages        # module discovery + package sweep + API contract
./scripts/run-tests.sh installer       # installer/OS module checks
./scripts/run-tests.sh lint            # guix lint gated by the allowlist
./scripts/run-tests.sh build           # actually compile key packages (very slow)
./scripts/run-vm-tests.sh              # VM integration tests (slow, needs KVM)
```

## The GUIX override

All test wrappers honor the `GUIX` environment variable, which replaces
the plain `guix` invocation. CI uses it to run every check against the
pinned channel set instead of whatever Guix happens to be installed:

```bash
GUIX="guix time-machine -C channels-lock.scm --" ./scripts/run-tests.sh
```

## Test Categories

### Package tests (`packages`)

`tests/packages/test-modules-load.sh` runs
`tests/packages/check-packages.scm` via `guix repl -L systole` in
*script mode* (so failures propagate as a non-zero exit status, unlike
heredoc REPL sessions which always exit 0). The check makes three
passes:

1. **Module discovery**: find every module under `systole/` and load it.
2. **Package sweep**: walk each module's public interface and touch
   every exported package (name + version) — currently ~340 packages —
   so malformed definitions surface.
3. **API contract**: verify a curated list of load-bearing public names
   (e.g. `slicer-5.8`, `slicer-all-5.10`, the factory functions) still
   exists.

Run it directly:

```bash
tests/packages/test-modules-load.sh
# or, equivalently:
guix repl -L systole -- tests/packages/check-packages.scm
```

Smoke-test a single module:

```bash
guix repl -L systole <<< ',m (systole packages slicer)'
```

(Always `-L systole` — the channel root is the `systole/` subdirectory;
`-L .` silently resolves modules from the *pulled* channel instead of
the checkout.)

### Installer tests (`installer`)

`tests/installer/test-dry-run.sh` runs
`tests/installer/check-installer.scm`, which loads every installer/OS
module and verifies the ISO-build entry points. Because
`(gnu installer newt ...)` imports the `(newt)`, `(parted)`, and
`(webutils multipart)` Guile bindings at load time, the wrapper runs
inside `guix shell guile guile-newt guile-parted guile-webutils`.
`(os install)` also imports nonguix modules, so the invoking Guix must
have the channels from `channels-lock.scm` available (in CI this comes
from the `GUIX` time-machine override).

### Lint (`lint`)

`run-tests.sh` lints a fixed package list (the `LINT_PACKAGES` array in
the script — single source of truth, shared with CI) using
`guix lint -L systole --exclude=archival`. Since `guix lint` exits 0
even when it emits warnings, the runner gates on output instead: any
line not matching `tests/lint-allowlist.regex` (the allowlist of
documented technical debt) fails the check. To accept a new warning,
add a pattern to the allowlist in the same commit that introduces it.

### Build tests (`build`, manual)

Actually compiles `vtk-slicer`, `itk-slicer`, and `ctk` (2h timeout
each). Not part of `all` and not run in CI. For broader coverage,
`manifest.scm` enumerates every public package:

```bash
guix build -m manifest.scm
```

### VM tests (slow)

`tests/installer/installer.scm` defines Marionette-based system tests
in the module `(tests installer installer)` (namespaced so it cannot
collide with `system/installer/installer.scm`). They boot full QEMU
VMs and verify installer boot, the SSH deploy-key feature, and that SSH
is absent without a key. Run via:

```bash
./scripts/run-vm-tests.sh              # all
./scripts/run-vm-tests.sh basic        # installer boots
./scripts/run-vm-tests.sh deploy-key   # SSH deploy-key feature
./scripts/run-vm-tests.sh no-ssh       # no key => no sshd
```

See [VM Testing](vm-testing.md) for details. These need KVM and long
build times; run locally or on a self-hosted runner, not in GitHub CI.

### Manual tests

`tests/manual/` holds interactive end-to-end scripts (config
generation, full deployment workflow) that need real hardware, network,
or human judgment. See `tests/manual/README.md`.

## CI Integration

GitHub Actions runs on PRs and pushes. Each testing workflow bootstraps
the Ubuntu `apt` Guix and then runs the relevant `run-tests.sh`
category through `guix time-machine -C channels-lock.scm`, i.e. against
the exact Guix + channel commits the project ships with:

- **package-tests** — `./scripts/run-tests.sh packages`
- **installer-tests** — `./scripts/run-tests.sh installer`
- **guix-lint-check** — `./scripts/run-tests.sh lint`
- **commit-message-check** — enforces `[Prefix][Component] Title` format

See `.github/workflows/` for configuration.

## Writing New Tests

- New package modules are picked up automatically: the package check
  discovers modules by walking `systole/` — there is no list to update.
  If the new module exports a load-bearing public name, add it to the
  API-contract list in `tests/packages/check-packages.scm`.
- If the package needs patches, put them in a subdirectory of
  `systole/systole/packages/patches/` and register it in `%patch-path`
  in `systole/systole/packages.scm` (Slicer module patches instead go
  under `patches/slicer-<version>/` and are resolved by the
  `slicer-patch` helper).
- New shell tests dropped in `tests/packages/test-*.sh` or
  `tests/installer/test-*.sh` are discovered and run automatically by
  `run-tests.sh`. Honor the `GUIX` variable in new wrappers.
- Run `./scripts/run-tests.sh` before committing.
