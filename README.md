# Systole OS GNU Guix Channel for Medical Image Computing

A [GNU Guix](https://guix.gnu.org) channel providing packages for
medical image computing: 3D Slicer and its ecosystem (VTK, ITK, CTK,
OpenIGTLink, PlusToolkit, SlicerIGT, SOFA Framework, ROS 2, ...),
plus a custom SystoleOS installer and `guix deploy` workflow.

Developed at Oslo University Hospital, NTNU, and collaborating
institutions. Most packages aim for eventual upstream inclusion.

## Quick Start

Add the channel to `~/.config/guix/channels.scm`:

```scheme
(cons (channel
        (name 'systole)
        (url "https://github.com/SystoleOS/guix-systole.git")
        (branch "main")
        ;; Authenticate pulls against the channel introduction:
        (introduction
         (make-channel-introduction
          "0898d41446ecbd6eddcbf5d78beae63f7e8a4069"
          (openpgp-fingerprint
           "53A2 D043 E0DC 9249 DF75  ABB6 65D8 96E0 0C10 1DDF"))))
      %default-channels)
```

Then pull and build:

```bash
guix pull
guix build slicer-5.8          # Slicer application (Python-enabled)
guix build slicer-all-5.8      # Slicer + all module packages
```

Or from a local checkout (the channel root is the `systole/`
subdirectory, so always pass `-L systole` — not `-L .`):

```bash
git clone https://github.com/SystoleOS/guix-systole.git
cd guix-systole
guix build -L systole slicer-all-5.8
```

Search for channel packages by name:

```bash
guix package -L systole -A slicer
```

[`manifest.scm`](manifest.scm) at the repository root enumerates every
public package in the channel — one target for full builds and
substitute-coverage checks:

```bash
guix build -m manifest.scm                    # build everything
guix weather -m manifest.scm                  # substitute coverage
```

## Available Packages

### Core Medical Imaging
- **3D Slicer 5.8 and 5.10** — medical visualization and computing
  platform, packaged as parallel stacks (`slicer-5.8`/`slicer-all-5.8`
  and `slicer-5.10`/`slicer-all-5.10`); the application plus per-module
  packages (loadable, scripted, and CLI modules). See
  [doc/architecture.md](doc/architecture.md) for how the modularization
  works.
- **VTK / ITK** (`vtk-slicer`, `vtkaddon`, `itk-slicer`) — visualization
  and image processing, Slicer variants
- **CTK** (`ctk`, `ctkapplauncher`) — Common Toolkit for biomedical
  computing

### Simulation and Robotics
- **SOFA Framework** (`sofa-framework`) — real-time biomechanical simulation
- **SlicerSOFA** — SOFA integration for 3D Slicer
- **SlicerROS2** — ROS 2 Jazzy bridge for Slicer
- **ROS 2 Jazzy** (`ros-jazzy-...`) — ROS 2 distribution with cisst/SAW stack

### Image-Guided Therapy
- **OpenIGTLink / OpenIGTLinkIO** — network protocol for IGT
- **PlusToolkit** (`pluslib`, `plusapp`) — data acquisition for IGT
- **SlicerIGT** (`slicer-igt`) — image-guided therapy extension
- **IGSIO** (`igsio`) — IO library for IGT

### Supporting Libraries
- `libarchive-slicer` (Slicer's 3.8.1 security-fix fork),
  `teem-slicer` (Slicer's r7265 fork), `qrestapi`, `pythonqt-commontk`

## Substitutes

Systems built with the Systole transformations authorize the
[nonguix](https://gitlab.com/nonguix/nonguix) substitute server by
default. The community-run `cache-cdn.guix.moe` mirror is **opt-in**:
pass `#:community-substitutes? #t` to `systole-transformation-guix` to
authorize its key and add its URL (the published installer images opt
in so Slicer-stack substitutes are available out of the box).

## Channel Authentication

The channel is **authenticated**: every commit from the introduction
(`0898d41`) onward is signed by a key in `.guix-authorizations`
(Rafael Palomar's signing subkeys; primary key on the `keyring`
branch).  Configure the channel with the introduction shown above so
`guix pull` verifies the history; see
[doc/channel-authentication.md](doc/channel-authentication.md) for key
management.

## Testing

```bash
./scripts/run-tests.sh                 # packages + installer + lint (CI runs these)
./scripts/run-tests.sh packages        # module loading + package sweep
./scripts/run-tests.sh installer       # installer module checks
./scripts/run-tests.sh lint            # guix lint with allowlist gating
./scripts/run-vm-tests.sh              # VM integration tests (slow)
```

## Documentation

Full documentation is in [`doc/`](doc/):

- [Architecture](doc/architecture.md) — the Slicer modularization and channel design
- [Channel Management](doc/channel-management.md) — versioning and reproducible builds
- [Channel Authentication](doc/channel-authentication.md) — activation runbook
- [Remote Deployment](doc/remote-deployment.md) — `guix deploy` workflow
- [VM Testing](doc/vm-testing.md) — Marionette-based system tests
- [Testing Guide](doc/testing.md) — how to run and write tests

See also:
- [CONTRIBUTING.md](CONTRIBUTING.md) — contribution guidelines and commit format
- [CLAUDE.md](CLAUDE.md) — guide for AI-assisted development

## Development

All commits must follow the format:

```
[Prefix][Component] Title
```

Prefixes: `BUG`, `COMP`, `DOC`, `ENH`, `PERF`, `STYLE`, `WIP`.
See [CONTRIBUTING.md](CONTRIBUTING.md) for details.

Always run `./scripts/run-tests.sh` before committing.

## License

GNU General Public License v3.0 or later. See [LICENSE.md](LICENSE.md).
