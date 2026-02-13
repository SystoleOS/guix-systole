# Channel Management and Reproducible Deployments

This document describes the channel management strategy for Systole installer builds and deployments.

## Problem Statement

When building installer ISOs and deploying to target systems, channel version mismatches can cause deployment failures. Guix prevents channel "downgrades" for safety, which can block deployments when:

1. The installer ISO was built with certain channel versions
2. The target system inherits those channel versions after installation
3. The deployment host has different channel versions
4. Guix refuses to reconfigure because it detects a version mismatch

## Solution: Hybrid Channel Management

We use a hybrid approach combining:

1. **Locked channel versions** (`channels-lock.scm`) for reproducible builds
2. **Time-machine builds** to ensure installer ISOs use known-good channels
3. **Automatic channel synchronization** before deployment

This ensures:
- ✅ Reproducible installer builds
- ✅ Compatible deployment targets
- ✅ Explicit version control of channels
- ✅ Ability to update channels in a controlled manner

## Files and Scripts

### `channels-lock.scm`

Located in the repository root, this file pins specific channel commits:

```scheme
(list (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix.git")
        (commit "f75080db69716891c46d5795f5d29ad8cc54700e"))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (commit "6c0ea215e0bd089bf3b2097e5c59dd726fbbe304"))
      ...)
```

**Purpose:**
- Defines exact channel versions for builds
- Committed to version control
- Updated explicitly when testing new channel versions

**Last updated:** See file header for date and tested configurations

### `scripts/build-installer-with-deploy.sh`

Builds installer ISOs using `guix time-machine` for reproducibility.

**Key changes:**
- Uses `guix time-machine -C channels-lock.scm` by default
- Ensures installer ISO has exact channel versions from lock file
- Optional `--no-time-machine` flag to use current channels

**Usage:**
```bash
# Standard build with locked channels (recommended)
./scripts/build-installer-with-deploy.sh --key-file test-key.pub

# Build with current channels (testing only)
./scripts/build-installer-with-deploy.sh --key-file test-key.pub --no-time-machine
```

### `scripts/sync-and-deploy.sh`

Handles channel synchronization before deployment.

**What it does:**
1. Extracts SSH connection details from `deployment.scm`
2. Tests SSH connectivity
3. Copies `channels-lock.scm` to target as `~/.config/guix/channels.scm`
4. Runs `guix pull` on target to synchronize channels
5. Executes `guix deploy`

**Usage:**
```bash
# Full sync and deploy (recommended)
./scripts/sync-and-deploy.sh /tmp/deployment.scm

# Deploy without syncing (if you know channels match)
./scripts/sync-and-deploy.sh --skip-sync /tmp/deployment.scm
```

## Workflow

### 1. Initial Setup

Create locked channel versions:

```bash
cd /path/to/guix-systole

# Generate channels-lock.scm from your current working channels
guix describe -f channels > channels-lock.scm

# Edit to add header and comments
# Test that your system works with these channel versions
```

Commit to repository:

```bash
git add channels-lock.scm
git commit -m "[ENH][misc.] Add locked channel versions"
```

### 2. Build Installer ISO

Build with locked channels for reproducibility:

```bash
./scripts/build-installer-with-deploy.sh --key-file ~/.ssh/deploy_key.pub
```

This will:
- Use `guix time-machine` with `channels-lock.scm`
- Build an ISO with exact channel versions
- Produce `systole-installer-deploy-YYYYMMDD-HHMMSS.iso`

**Output includes:**
- ISO file path
- Next steps for booting and deployment

### 3. Install System from ISO

Boot the ISO and complete installation as normal. The installed system will have the same channel versions as `channels-lock.scm`.

### 4. Deploy Configuration

Create a deployment specification:

```scheme
;; deployment.scm
(use-modules (gnu)
             (gnu machine)
             (gnu machine ssh))

(define target-system
  (load "/path/to/config.scm"))

(list (machine
       (operating-system target-system)
       (environment managed-host-environment-type)
       (configuration
        (machine-ssh-configuration
         (system "x86_64-linux")
         (host-name "target-hostname")
         (port 22)
         (user "root")
         (identity "~/.ssh/deploy_key")
         (build-locally? #t)))))
```

Deploy with automatic channel sync:

```bash
./scripts/sync-and-deploy.sh deployment.scm
```

This will:
1. Verify SSH connectivity
2. Copy `channels-lock.scm` to target
3. Run `guix pull` on target to sync channels
4. Deploy the configuration

## Updating Channels

When you want to update to newer channel versions:

### 1. Update Your Local System

```bash
# Edit ~/.config/guix/channels.scm if needed
# Or just use default channels

guix pull
hash guix
```

### 2. Test the New Channels

```bash
# Test building packages
guix build -L . vtk-slicer
guix build -L . slicer-5.8

# Test system operations
guix system build config.scm  # if you have a test config
```

### 3. Update channels-lock.scm

Once verified working:

```bash
guix describe -f channels > channels-lock.scm

# Edit to preserve header comments and update "Last updated" date
```

### 4. Commit and Document

```bash
git add channels-lock.scm
git commit -m "[ENH][misc.] Update locked channels to YYYY-MM-DD versions

- guix: <commit>
- nonguix: <commit>
- Tested with: <what you tested>
"
```

### 5. Rebuild and Redeploy

```bash
# Rebuild installer with new channels
./scripts/build-installer-with-deploy.sh --key-file ~/.ssh/deploy_key.pub

# Sync existing systems
./scripts/sync-and-deploy.sh deployment.scm
```

## Troubleshooting

### Channel Mismatch Errors

**Error:**
```
guix deploy: error: failed to deploy: aborting reconfiguration because
commit <hash1> of channel '<channel>' is not a descendant of <hash2>
```

**Solution:**
```bash
# Option 1: Use sync-and-deploy.sh (recommended)
./scripts/sync-and-deploy.sh deployment.scm

# Option 2: Manual sync
scp channels-lock.scm root@target:~/.config/guix/channels.scm
ssh root@target 'guix pull && hash guix'
guix deploy deployment.scm
```

### Time-Machine Build Failures

**Error:**
```
guix time-machine: error: channel 'nonguix' lacks introduction
```

**Solution:**
Ensure `channels-lock.scm` includes channel introductions:

```scheme
(channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (commit "...")
  (introduction
    (make-channel-introduction
      "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
      (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
```

### Git History Issues

If nonguix or other channels have force-pushed or rebased:

```bash
# Check if commit still exists
git ls-remote https://gitlab.com/nonguix/nonguix <commit-hash>

# If not, update to a new known-good commit
guix pull
# Test your system
guix describe -f channels > channels-lock.scm
```

## Best Practices

1. **Always use channels-lock.scm for installer builds**
   - Ensures reproducibility
   - Prevents drift between build systems

2. **Test channel updates before committing**
   - Build key packages
   - Test on a development system
   - Update lock file only when verified

3. **Document channel updates in commit messages**
   - Include what was tested
   - Note any breaking changes
   - Reference relevant issues

4. **Use sync-and-deploy.sh for all deployments**
   - Automatic channel synchronization
   - Reduces manual errors
   - Consistent deployment process

5. **Version your installer ISOs**
   - Keep track of which channel versions each ISO uses
   - Useful for debugging deployment issues

6. **Periodically update channels**
   - Security patches
   - Bug fixes
   - New features
   - Balance stability vs. updates

## CI/CD Integration

For continuous integration:

```yaml
# .github/workflows/build-installer.yml
- name: Build installer with locked channels
  run: |
    ./scripts/build-installer-with-deploy.sh \
      --key-file test-key.pub \
      --output systole-installer.iso
```

This ensures CI builds use the same channel versions as local development.

## References

- [GNU Guix Manual: Channels](https://guix.gnu.org/manual/en/html_node/Channels.html)
- [GNU Guix Manual: Invoking guix time-machine](https://guix.gnu.org/manual/en/html_node/Invoking-guix-time_002dmachine.html)
- [GNU Guix Manual: Invoking guix deploy](https://guix.gnu.org/manual/en/html_node/Invoking-guix-deploy.html)
- [Nonguix Channel](https://gitlab.com/nonguix/nonguix)
