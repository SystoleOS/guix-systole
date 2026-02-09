# GitHub Actions Workflows

This directory contains CI/CD workflows for automated testing.

## Available Workflows

### package-tests.yml
**Triggers:** PR and push to main/dev when package files change
**Tests:**
- Module loading (all package modules)
- Package definitions verification (13 packages)

**Note:** Full package linting of all packages is disabled in CI because some system modules require the `nonguix` channel. Medical imaging packages are linted via guix-lint-check.yml. Run full lint locally:
```bash
./scripts/run-tests.sh lint
```

### installer-tests.yml
**Triggers:** PR and push when installer files change
**Tests:**
- Installer module loading
- Module import verification

### guix-lint-check.yml
**Triggers:** PR and push to main/dev
**Tests:**
- Module syntax validation
- Package linting for all 13 medical imaging packages
- Excludes system modules that require nonguix

**Packages linted:** vtk-slicer, vtkaddon, itk-slicer, slicer-5.8, slicer-volumes-5.8, ctk, ctkapplauncher, teem-slicer, netcdf-slicer, libarchive-slicer, qrestapi, openigtlink, slicer-openigtlink

**Note:** Full linting of system modules (transformations, installer) requires local setup with nonguix channel.

## Why Some Tests Are Disabled in CI

### Full Lint Tests
**Problem:** System modules (transformations, installer) depend on external channels:
- `nonguix` - For Linux kernel, NVIDIA drivers
- `systole-artwork` - For branding assets

**What works in CI:**
- ✅ Medical imaging packages (slicer, vtk, itk, ctk, qrestapi, etc.) - don't need nonguix
- ❌ System modules (transformations.scm, installer modules) - require nonguix

**Solution:** Run full lint locally with channel configuration:
```bash
# Make sure channels are configured in ~/.config/guix/channels.scm
guix pull
./scripts/run-tests.sh lint
```

### Build Tests
**Problem:** Build tests take 2-8 hours per package
**Solution:** Run manually when needed:
```bash
./scripts/run-tests.sh build
```

## Running Full Tests Locally

For complete validation before pushing:

```bash
# Fast tests (always run these)
./scripts/run-tests.sh

# Lint tests (requires channel configuration)
guix pull  # Update channels first
./scripts/run-tests.sh lint

# Build tests (very slow!)
./scripts/run-tests.sh build
```

## CI Workflow Status

| Workflow | Enabled | Reason if Disabled |
|----------|---------|-------------------|
| Module loading | ✅ Yes | Fast, no external deps |
| Package definitions | ✅ Yes | Fast, no external deps |
| Package linting (medical imaging) | ✅ Yes | No nonguix dependency |
| Package linting (system modules) | ❌ No | Requires nonguix channel |
| Build testing | ❌ No | Too slow (2-8h per package) |
| Installer modules | ✅ Yes | Fast, basic checks only |

## Expected Warnings

When running lint locally, these warnings are expected:

1. **Module name mismatches**
   ```
   warning: module name (systole packages foo) does not match file name 'systole/systole/packages/foo.scm'
   ```
   This is due to the nested `systole/systole/` directory structure and doesn't affect functionality.

2. **Style warnings**
   - Long lines in package definitions
   - Missing upstream status in patches
   - Synopsis/description formatting

These are documented as technical debt in ROADMAP.md.

## Future Improvements

- [ ] Set up nonguix channel in CI (if possible)
- [ ] Add caching for faster test runs
- [ ] Implement VM-based installer testing
- [ ] Add performance benchmarking

See ROADMAP.md for detailed enhancement plans.
