# Deployment Workflow Summary

Quick reference for building reproducible installer ISOs and deploying Systole systems.

## The Problem We Solved

Channel version mismatches between the build system, installer ISO, and target systems were causing deployment failures:

```
guix deploy: error: failed to deploy: aborting reconfiguration because
commit 6c0ea215 of channel 'nonguix' is not a descendant of 48a8706
```

## The Solution: Hybrid Channel Management

We implemented a hybrid approach combining:

1. **Locked channel versions** (`channels-lock.scm`)
2. **Time-machine builds** for reproducibility
3. **Automatic channel synchronization** before deployment

## Files Created/Modified

### New Files

1. **`channels-lock.scm`** - Pinned channel commits
   - Generated from your working system
   - Committed to version control
   - Updated explicitly when testing new versions

2. **`scripts/sync-and-deploy.sh`** - Deployment wrapper
   - Copies `channels-lock.scm` to target
   - Runs `guix pull` on target to sync channels
   - Executes `guix deploy`

3. **`docs/CHANNEL-MANAGEMENT.md`** - Complete documentation
   - Problem statement and solution
   - Workflow descriptions
   - Troubleshooting guide
   - Update procedures

### Modified Files

1. **`scripts/build-installer-with-deploy.sh`**
   - Added `--no-time-machine` option
   - Uses `guix time-machine -C channels-lock.scm` by default
   - Ensures reproducible installer builds

2. **`docs/REMOTE-DEPLOYMENT.md`**
   - Added channel management section
   - References new documentation

3. **`README.md`**
   - Updated deployment workflow
   - Added documentation links

## Quick Start

### 1. Build Reproducible Installer ISO

```bash
# Standard build with locked channels
./scripts/build-installer-with-deploy.sh --key-file ~/.ssh/deploy_key.pub

# Produces: systole-installer-deploy-YYYYMMDD-HHMMSS.iso
```

### 2. Boot Installer

```bash
# For VMs
qemu-system-x86_64 \
  -m 4096 -smp 2 \
  -cdrom systole-installer-deploy-*.iso \
  -boot d \
  -net nic,model=virtio \
  -net user,hostfwd=tcp::2223-:22 \
  -drive file=target-disk.qcow2,format=qcow2,if=virtio \
  -enable-kvm

# For physical machines: write to USB
sudo dd if=systole-installer-deploy-*.iso of=/dev/sdX bs=4M status=progress
```

### 3. Deploy with Automatic Channel Sync

```bash
# Create deployment.scm (see docs/REMOTE-DEPLOYMENT.md)
# Then deploy with automatic synchronization:
./scripts/sync-and-deploy.sh deployment.scm
```

This will:
1. ✅ Test SSH connectivity
2. ✅ Copy `channels-lock.scm` to target
3. ✅ Run `guix pull` on target
4. ✅ Verify channel versions match
5. ✅ Execute `guix deploy`

## Workflow Diagram

```
┌─────────────────────────────────────┐
│ Development Machine                  │
├─────────────────────────────────────┤
│ 1. Test new channels locally         │
│    guix pull                         │
│    guix build -L . slicer-5.8        │
│                                      │
│ 2. Update channels-lock.scm          │
│    guix describe -f channels >       │
│      channels-lock.scm               │
│                                      │
│ 3. Build installer with time-machine │
│    ./scripts/build-installer-with-   │
│      deploy.sh --key-file key.pub    │
│                                      │
│    Uses: guix time-machine -C        │
│           channels-lock.scm          │
└─────────────┬───────────────────────┘
              │
              │ ISO with exact channel versions
              ▼
┌─────────────────────────────────────┐
│ Target Machine (Installer Boot)     │
├─────────────────────────────────────┤
│ - Boots from ISO                     │
│ - Has channel versions from          │
│   channels-lock.scm baked in         │
│ - SSH daemon enabled                 │
│ - Deploy key authorized              │
└─────────────┬───────────────────────┘
              │
              │ sync-and-deploy.sh
              │
              ▼
┌─────────────────────────────────────┐
│ Channel Synchronization              │
├─────────────────────────────────────┤
│ 1. scp channels-lock.scm to target   │
│ 2. ssh target 'guix pull'            │
│ 3. Verify channels match             │
└─────────────┬───────────────────────┘
              │
              │ Channels now synchronized
              ▼
┌─────────────────────────────────────┐
│ Deployment Execution                 │
├─────────────────────────────────────┤
│ guix deploy deployment.scm           │
│ - Builds system locally              │
│ - Transfers to target                │
│ - Activates new configuration        │
│ ✅ No channel mismatch errors!       │
└─────────────────────────────────────┘
```

## Key Commands

### Build Installer
```bash
# With locked channels (recommended)
./scripts/build-installer-with-deploy.sh --key-file ~/.ssh/deploy_key.pub

# Without time-machine (testing only)
./scripts/build-installer-with-deploy.sh --key-file ~/.ssh/deploy_key.pub --no-time-machine
```

### Deploy System
```bash
# With automatic channel sync (recommended)
./scripts/sync-and-deploy.sh deployment.scm

# Skip sync if channels already match
./scripts/sync-and-deploy.sh --skip-sync deployment.scm

# Manual approach (not recommended)
guix deploy deployment.scm  # May fail with channel mismatch!
```

### Update Channels
```bash
# 1. Update local system
guix pull

# 2. Test packages
guix build -L . vtk-slicer slicer-5.8

# 3. Update lock file
guix describe -f channels > channels-lock.scm

# 4. Commit
git add channels-lock.scm
git commit -m "[ENH][misc.] Update locked channels to $(date +%Y-%m-%d)"
```

## What Each File Does

| File | Purpose | When to Use |
|------|---------|-------------|
| `channels-lock.scm` | Pins exact channel commits | Committed to repo, updated explicitly |
| `build-installer-with-deploy.sh` | Builds reproducible ISO | Every time you build installer |
| `sync-and-deploy.sh` | Syncs channels before deploy | Every deployment |
| `deployment.scm` | Defines target system config | Created per target machine |

## Benefits

✅ **Reproducibility**: Same `channels-lock.scm` produces identical ISOs
✅ **No channel conflicts**: Automatic synchronization prevents errors
✅ **Version control**: Channel versions tracked in git
✅ **Flexibility**: Can update channels in controlled manner
✅ **Documentation**: Complete workflow documented

## For More Information

- **Complete guide**: [docs/CHANNEL-MANAGEMENT.md](docs/CHANNEL-MANAGEMENT.md)
- **Deployment details**: [docs/REMOTE-DEPLOYMENT.md](docs/REMOTE-DEPLOYMENT.md)
- **CI/CD integration**: See CHANNEL-MANAGEMENT.md § "CI/CD Integration"

## Troubleshooting

**Q: Deployment fails with "not a descendant" error**
```bash
# Use sync-and-deploy.sh instead of guix deploy directly:
./scripts/sync-and-deploy.sh deployment.scm
```

**Q: Want to test with different channels without committing**
```bash
# Build installer without time-machine:
./scripts/build-installer-with-deploy.sh --no-time-machine --key-file key.pub

# Or temporarily modify channels-lock.scm
```

**Q: How to rebuild existing systems with new channels?**
```bash
# Update channels-lock.scm, then:
./scripts/sync-and-deploy.sh deployment.scm
# This syncs channels and redeploys
```

## Summary

The hybrid channel management approach ensures:
- Installer ISOs have reproducible, known-good channel versions
- Deployments automatically synchronize channels with targets
- Channel updates are explicit and version-controlled
- No more mysterious channel mismatch deployment failures!

Use `sync-and-deploy.sh` for all deployments and you're good to go!
