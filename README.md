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
# Run unit tests (fast)
./scripts/run-tests.sh

# Run specific test categories
./scripts/run-tests.sh packages    # Test package definitions
./scripts/run-tests.sh installer   # Test installer modules
./scripts/run-tests.sh lint        # Run guix lint checks

# Run VM tests (comprehensive, slower)
./scripts/run-vm-tests.sh          # Boot full VMs to test installer
./scripts/run-vm-tests.sh basic    # Test basic installer boot
./scripts/run-vm-tests.sh deploy-key  # Test SSH deploy key feature
```

### Detailed Testing Guide

For comprehensive testing instructions, see:
- **[README.testing.md](README.testing.md)** - Unit tests, lint checks, and package tests
- **[docs/VM-TESTING.md](docs/VM-TESTING.md)** - VM-based system tests using Marionette

Topics covered:
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
- ✅ Installer boots successfully in VM (Marionette tests)
- ✅ SSH deploy key configuration works correctly
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

### Building Installer ISO with Deploy Key

For remote deployment scenarios using `guix deploy`, you can build an installer ISO with SSH access enabled:

```bash
# Using a key file (recommended)
./scripts/build-installer-with-deploy.sh --key-file ~/.ssh/id_ed25519.pub

# Using a key string
./scripts/build-installer-with-deploy.sh --key "ssh-ed25519 AAAAC3Nza... user@host"

# Using environment variable
SYSTOLE_DEPLOY_KEY="$(cat ~/.ssh/id_ed25519.pub)" ./scripts/build-installer-with-deploy.sh
```

This creates an installer ISO with:
- SSH daemon enabled in the live installer environment
- Your public key authorized for root user access
- Key-based authentication only (password auth disabled)

**Use cases:**
- Remote installation in data centers or cloud environments
- Automated deployment in CI/CD pipelines
- Testing and development in virtualized environments

**Workflow:**

1. **Build ISO with your deploy key:**
   ```bash
   ./scripts/build-installer-with-deploy.sh --key-file ~/.ssh/id_ed25519.pub
   ```

2. **Boot the ISO on target machine** (via USB, network boot, or VM)

3. **SSH to the installer:**
   ```bash
   ssh -i ~/.ssh/id_ed25519 root@<target-ip>
   ```

4. **Deploy your system remotely:**
   ```bash
   guix deploy deployment.scm
   ```

**Note:** The deploy key only authorizes access to the live installer environment. After installation, the target system uses its own SSH configuration.

For advanced usage, you can also build directly with `guix system image`:

```bash
guix system image -t iso9660 -L . --expression \
  '(use-modules (os install))
   ((@ (os install) systole-os-installation-with-deploy-key)
    #:deploy-key "ssh-ed25519 AAAAC3Nza... user@host")'
```

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
