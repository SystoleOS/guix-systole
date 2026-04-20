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
        (branch "main"))
      %default-channels)
```

Then pull and build:

```bash
guix pull
guix build slicer
```

Or from a local checkout:

```bash
git clone https://github.com/SystoleOS/guix-systole.git
guix build -L guix-systole/systole slicer
```

List all available packages:

```bash
guix package -L guix-systole/systole -A systole
```

## Available Packages

### Core Medical Imaging
- **3D Slicer 5.8** — medical visualization and computing platform
- **VTK / ITK** (Slicer variants) — visualization and image processing
- **CTK** — Common Toolkit for biomedical computing

### Simulation and Robotics
- **SOFA Framework** — real-time biomechanical simulation
- **SlicerSOFA** — SOFA integration for 3D Slicer
- **SlicerROS2** — ROS 2 Jazzy bridge for Slicer
- **ROS 2 Jazzy** — full ROS 2 distribution with cisst/SAW stack

### Image-Guided Therapy
- **OpenIGTLink / OpenIGTLinkIO** — network protocol for IGT
- **PlusToolkit / PlusApp** — data acquisition for IGT
- **SlicerIGT** — image-guided therapy extension
- **IGSIO** — IO library for IGT

### Supporting Libraries
- libarchive-slicer, teem, qRestAPI, PythonQt

## Testing

```bash
./scripts/run-tests.sh                 # all fast tests (CI runs this)
./scripts/run-tests.sh packages        # package definition tests
./scripts/run-tests.sh installer       # installer module tests
./scripts/run-tests.sh lint            # guix lint
./scripts/run-vm-tests.sh             # VM integration tests (slow)
```

## Documentation

Full documentation is in [`doc/`](doc/):

- [Channel Management](doc/channel-management.rst) — versioning and reproducible builds
- [Remote Deployment](doc/remote-deployment.rst) — `guix deploy` workflow
- [VM Testing](doc/vm-testing.rst) — Marionette-based system tests
- [Testing Guide](doc/testing.rst) — how to run and write tests

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
