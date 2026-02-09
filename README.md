# Systole OS GNU Guix Channel for Medical Image Computing

Welcome! This repository contains package recipes and extensions of the
[GNU Guix package manager](https://guix.gnu.org) tailored for medical image computing. It offers a collection of packages developed at Oslo University Hospital, NTNU, and other institutions.

The primary aim is to contribute most packages upstream. This repository serves as a staging area for work in progress or packages that are not yet ready for public use.

For inquiries or further information, please feel free to reach out directly via email to our contributors.

## Getting Started

To utilize this repository, you must have [Guix](https://guix.gnu.org) installed on your system. Refer to the
[installation instructions](https://guix.gnu.org/manual/en/html_node/Binary-Installation.html).

## How Does It Work?

The package definitions in this repository _extend_ [the default packages provided by Guix](https://hpc.guix.info/browse). To integrate them with the `guix` command-line tools, create the `~/.config/guix/channels.scm` file using the following snippet to include the `systole` _channel_:

```scheme
(cons (channel
        (name 'systole)
        (url "https://github.com/SystoleOS/guix-systole.git")
        (branch "main"))
      %default-channels)
```

This configuration ensures that `guix pull` will fetch both Guix and the Systole channel.

## Package Installation

As this repository does not currently provide pre-built substitutes or binaries, users are encouraged to build packages from source. You can clone the Systole repository and compile the packages you need:

```bash
mkdir -p ~/src
cd ~/src
git clone https://github.com/SystoleOS/guix-systole.git
```

After cloning, you may build packages using:

```bash
guix build -L ~/src/guix-systole <package-name>
```

When you are satisfied with your changes to the packages, you can push them to the repository for others to access.

## Testing

This repository includes comprehensive testing infrastructure to ensure package quality and enable safe refactoring.

### Quick Test

```bash
# Run all tests
./scripts/run-tests.sh

# Run specific test categories
./scripts/run-tests.sh packages    # Test package definitions
./scripts/run-tests.sh installer   # Test installer modules
./scripts/run-tests.sh lint        # Run guix lint checks
```

### Detailed Testing Guide

For comprehensive testing instructions, see [README.testing.md](README.testing.md), which covers:
- How to run different types of tests
- Understanding test output
- Writing new tests
- Troubleshooting common issues
- CI/CD integration

### Test Coverage

Current tests verify:
- ✅ All 10 package modules load correctly
- ✅ All 13 package definitions are accessible
- ✅ All installer modules load correctly
- ✅ Packages pass guix lint checks
- ⏳ Build tests (manual only - very slow)

## Development

### Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on:
- How to submit pull requests
- Commit message format (required!)
- Code review process
- Testing requirements

### Commit Message Format

All commits must follow this format:

```
[Prefix][Component] Title

Body explaining what, why, and how.
```

**Prefixes:** BUG, COMP, DOC, ENH, PERF, STYLE, WIP
**Example:** `[ENH][packages/slicer] Add Volumes module support`

See [CONTRIBUTING.md](CONTRIBUTING.md) for details.

### Before Committing

Always run tests before committing:

```bash
./scripts/run-tests.sh
```

Tests run automatically in CI on every PR.

## Documentation

- **[CLAUDE.md](CLAUDE.md)** - Guide for AI-assisted development
- **[ROADMAP.md](ROADMAP.md)** - Project roadmap and priorities
- **[TESTING.md](TESTING.md)** - Comprehensive testing strategy
- **[README.testing.md](README.testing.md)** - How to run tests
- **[CONTRIBUTING.md](CONTRIBUTING.md)** - Contribution guidelines

## Guidelines for Modifications

New modules defining packages should be stored in `systole/packages`, belonging to the Guile `(systole packages)` namespace. When adding new packages, please place them in the relevant module or create a new module if necessary.

New modules defining services should be stored in `systole/services`, belonging to the Guile `(systole services)` namespace. When adding new services, please place them in the appropriate module or create a new module if needed.

Note that we are in the process of migrating older modules to this new namespace, so some packages may not adhere to these guidelines yet.

## Available Packages

The Systole channel provides specialized packages for medical imaging:

### Core Medical Imaging
- **3D Slicer 5.8** - Medical visualization and computing platform
- **VTK (Slicer variant)** - Visualization Toolkit optimized for Slicer
- **ITK (Slicer variant)** - Insight Toolkit with medical imaging modules
- **CTK** - Common Toolkit for biomedical image computing

### Supporting Libraries
- **libarchive-slicer** - Archive handling for Slicer
- **teem-slicer** - Scientific raster data library
- **OpenIGTLink** - Network communication for image-guided therapy
- **qRestAPI** - Qt REST API library

To see all available packages:

```bash
guix package -L ~/src/guix-systole -A systole
```

## System Configuration

This repository also provides:

- **Custom SystoleOS installer** - Medical imaging workstation installer
- **System transformations** - NVIDIA GPU support, kernel configuration
- **GRUB theme** - SystoleOS branding

See the `system/` directory for system configuration examples.

## More Information

The Guix manual provides valuable resources, including:

- Information on [getting started](https://guix.gnu.org/manual/en/html_node/Getting-Started.html),
- Details on the [channels mechanism](https://www.gnu.org/software/guix/manual/en/html_node/Channels.html) that allows integration of Systole packages with Guix,
- Guidance on the [GUIX_PACKAGE_PATH environment variable](https://guix.gnu.org/manual/en/html_node/Package-Modules.html#index-GUIX_005fPACKAGE_005fPATH), which enables you to extend the visible package set in Guix.

## Getting Help

- **Issues:** [GitHub Issues](https://github.com/SystoleOS/guix-systole/issues)
- **Documentation:** See docs in this repository
- **Testing Problems:** See [README.testing.md](README.testing.md)
- **Development Questions:** See [CLAUDE.md](CLAUDE.md)

## License

This project is licensed under the GNU General Public License v3.0 or later.
See [LICENSE.md](LICENSE.md) for details.
