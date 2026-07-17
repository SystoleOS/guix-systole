# GitHub Actions Workflows

CI for guix-systole. The three testing workflows share one design:
they bootstrap the Ubuntu `apt` Guix as a launcher only, then run the
relevant `scripts/run-tests.sh` category through
`guix time-machine -C channels-lock.scm` — so every check executes
against the exact Guix + channel commits the project actually ships
with, not whatever Ubuntu packaged. Locally you reproduce a CI run
with:

```bash
GUIX="guix time-machine -C channels-lock.scm --" ./scripts/run-tests.sh <category>
```

## Workflows

### package-tests.yml
**Triggers:** PR and push to main/dev when `systole/`, `tests/`,
`scripts/run-tests.sh`, or `channels-lock.scm` change.
**Runs:** `./scripts/run-tests.sh packages` —
`tests/packages/check-packages.scm` in `guix repl` script mode:
discovers and loads every `(systole ...)` module, touches every
exported package (name + version), and verifies the channel's public
API contract (load-bearing names like `slicer-5.8`, `slicer-all-5.10`).

### installer-tests.yml
**Triggers:** PR and push when `system/`, `systole/`,
`tests/installer/`, or `channels-lock.scm` change (installer modules
import `(systole ...)` modules, so channel changes re-run these too).
**Runs:** `./scripts/run-tests.sh installer` —
`tests/installer/check-installer.scm` inside
`guix shell guile guile-newt guile-parted guile-webutils`, loading all
installer/OS modules and verifying the ISO-build entry points.
`(os install)` imports nonguix modules, which is why the pinned channel
set is required — the apt Guix alone could never load them.

### guix-lint-check.yml
**Triggers:** PR and push to main/dev when `systole/`,
`tests/lint-allowlist.regex`, `scripts/run-tests.sh`, or
`channels-lock.scm` change.
**Runs:** `./scripts/run-tests.sh lint`. The package list lives in the
`LINT_PACKAGES` array in `scripts/run-tests.sh` and the accepted
warnings in `tests/lint-allowlist.regex` — single sources of truth
shared with local runs. `guix lint` exits 0 even on warnings, so the
runner gates on filtered output: anything not matching the allowlist
fails the check.

### commit-message-check.yml
**Triggers:** every PR event and push to main.
**Runs:** the `SystoleOS/guix-systole-check-commit-message-action`,
enforcing the `[Prefix][Component] Title` commit format
(see CONTRIBUTING.md).

## What is *not* run in CI

- **Package builds** — hours per package; run locally with
  `./scripts/run-tests.sh build` or `guix build -m manifest.scm`.
- **VM installer tests** — `tests/installer/installer.scm` (module
  `(tests installer installer)`) boots full QEMU VMs and needs KVM plus
  long build times; run locally or on a self-hosted runner:
  `./scripts/run-vm-tests.sh [basic|deploy-key|no-ssh|all]`.
- **Installer ISO builds** — multi-gigabyte artifacts; built locally
  with `scripts/build-installer-with-deploy.sh` (which uses the same
  `channels-lock.scm` pin by default).

## Maintenance notes

- Add lint packages to `LINT_PACKAGES` in `scripts/run-tests.sh`, not
  to the workflow files.
- Accept a new lint warning by adding a pattern to
  `tests/lint-allowlist.regex` in the same commit that introduces it.
- Bump `channels-lock.scm` and the `.guix-channel` dependency pins
  together; the workflows re-run automatically on lock changes.
