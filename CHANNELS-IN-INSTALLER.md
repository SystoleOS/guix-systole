# Channel Specifications in Installer - Implementation Summary

## What Was Implemented

We've implemented the **official Guix approach** for embedding channel specifications in system configurations, using the `channels` field in `guix-configuration` (added to Guix in March 2024).

## How It Works

### 1. Building the Installer

The build script now accepts a `--channels-file` parameter:

```bash
./scripts/build-installer-with-deploy.sh \
  --key-file test-key.pub \
  --channels-file channels-lock.scm  # NEW!
```

**Default behavior:** If `--channels-file` is not specified, it automatically uses `channels-lock.scm` from the repository root.

### 2. What Happens During Build

1. The installer build process reads the channels file
2. Copies it to `/etc/systole-channels.scm` in the installer ISO
3. The channels are embedded in the installer environment

### 3. What Happens During Installation

When the installer generates `/etc/config.scm` for the installed system:

1. **Checks for `/etc/systole-channels.scm`**
2. **If found**, the generated config includes:
   ```scheme
   (define %system-channels
     (load "/etc/systole-channels.scm"))

   (operating-system
     ;; ... other config ...
     (services
       (modify-services %base-services
         (guix-service-type config =>
           (guix-configuration
             (inherit config)
             (channels %system-channels))))))
   ```

3. **The `guix-configuration` service automatically:**
   - Writes `/etc/guix/channels.scm` on the installed system
   - Ensures all `guix pull` commands use these channels
   - Records the channels in system provenance

### 4. Result

✅ **Single source of truth**: `channels-lock.scm` in your repository
✅ **Automatic propagation**: Channels flow from build → ISO → installed system
✅ **Standard Guix method**: Uses official `guix-configuration` feature
✅ **No manual sync needed**: Everything is automatic
✅ **Reproducible**: Exact channel versions baked into system

## Files Modified

### Core Implementation

1. **`system/os/install.scm`**
   - Added `#:channels-file` parameter to `systole-os-installation-with-deploy-key`
   - Passes channels file to transformation

2. **`systole/systole/transformations.scm`**
   - Updated `systole-transformation-deploy` to accept `#:channels-file`
   - Copies channels file to `/etc/systole-channels.scm` in installer ISO
   - Uses `simple-service` with `etc-service-type`

3. **`system/installer/steps.scm`**
   - Updated `systole-format-configuration` to detect `/etc/systole-channels.scm`
   - Loads channels and includes them in generated config
   - Adds `(guix channels)` to module imports
   - Modifies `guix-service-type` to set `channels` field

4. **`scripts/build-installer-with-deploy.sh`**
   - Added `--channels-file FILE` option
   - Defaults to `channels-lock.scm` if file exists
   - Passes channels file path to installer build expression

## Usage Examples

### Basic Usage (Default)

```bash
# Uses channels-lock.scm automatically
./scripts/build-installer-with-deploy.sh --key-file test-key.pub
```

### Explicit Channels File

```bash
# Use a specific channels file
./scripts/build-installer-with-deploy.sh \
  --key-file test-key.pub \
  --channels-file my-channels.scm
```

### Complete Workflow

```bash
# 1. Create/update your channel lock
guix describe -f channels > channels-lock.scm

# 2. Build installer with those channels
./scripts/build-installer-with-deploy.sh --key-file deploy-key.pub

# 3. Boot installer on target machine

# 4. Complete installation (interactive or automated)

# 5. Installed system automatically has:
#    - /etc/config.scm with guix-configuration channels field
#    - /etc/guix/channels.scm written by guix-configuration
#    - System provenance records exact channels

# 6. Deploy updates using same channels
guix time-machine -C channels-lock.scm -- deploy deployment.scm

# OR just use the simple script
./scripts/deploy-with-channels.sh deployment.scm
```

## Benefits Over Manual Approaches

### Before (Manual Channel Sync)
- ❌ Had to SSH to target and manually copy channels.scm
- ❌ Had to run `guix pull` on target before deployment
- ❌ Channels could drift between systems
- ❌ Multi-step error-prone process

### After (Automatic Channel Embedding)
- ✅ Channels embedded during installer build
- ✅ Automatically propagated to installed system
- ✅ Uses official Guix `guix-configuration` feature
- ✅ `/etc/guix/channels.scm` managed automatically
- ✅ System provenance includes exact channels
- ✅ Zero manual intervention needed

## Technical Details

### The `guix-configuration` Service

When you include `channels` in `guix-configuration`:

```scheme
(guix-configuration
  (channels my-channels))
```

Guix automatically:
1. Writes `/etc/guix/channels.scm` with those channel specs
2. Backs up any existing manual `/etc/guix/channels.scm` as `.scm.bak`
3. Ensures `guix pull` uses these channels by default
4. Includes channels in system provenance

### Channel File Format

The channels file (`channels-lock.scm`) should be a list of channel objects:

```scheme
(list (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (commit "abc123...")
        (introduction ...))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (commit "def456...")
        (introduction ...)))
```

Generated with: `guix describe -f channels`

### File Locations in Installer ISO

- **`/etc/systole-channels.scm`** - Channels file copied during build
- **`/etc/systole-deploy-key.pub`** - SSH deploy key (if provided)

### File Locations in Installed System

- **`/etc/config.scm`** - Generated system config with channels embedded
- **`/etc/guix/channels.scm`** - Auto-managed by `guix-configuration`
- **`/run/current-system/channels.scm`** - Provenance record

## Compatibility

- **Guix Version Required**: Works with Guix since March 2024 (when `channels` field was added)
- **Older Guix**: Will ignore the `channels` field, no errors
- **Manual channels.scm**: Automatically backed up to `.scm.bak` on first reconfigure

## Deployment Workflow

```
┌─────────────────────────────────────┐
│ Development Machine                  │
├─────────────────────────────────────┤
│ 1. Update channels-lock.scm          │
│    guix describe -f channels >       │
│      channels-lock.scm               │
│                                      │
│ 2. Build installer with channels     │
│    ./scripts/build-installer-with-   │
│      deploy.sh --key-file key.pub    │
│      --channels-file channels-lock   │
└─────────────┬───────────────────────┘
              │
              │ ISO with channels baked in
              ▼
┌─────────────────────────────────────┐
│ Target Machine (Installation)        │
├─────────────────────────────────────┤
│ - Boots from ISO                     │
│ - Installer reads /etc/systole-      │
│   channels.scm                       │
│ - Generates /etc/config.scm with:    │
│   (guix-configuration                │
│     (channels %system-channels))     │
│ - System installed with channels     │
└─────────────┬───────────────────────┘
              │
              │ Channels automatically in config
              ▼
┌─────────────────────────────────────┐
│ Installed System                     │
├─────────────────────────────────────┤
│ - /etc/config.scm has channels       │
│ - /etc/guix/channels.scm auto-       │
│   generated by guix-configuration    │
│ - guix pull uses these channels      │
│ - System provenance records them     │
│ ✅ Ready for guix deploy!            │
└─────────────────────────────────────┘
```

## Troubleshooting

### Channels not appearing in installed system

**Check:**
1. Was `--channels-file` specified (or does `channels-lock.scm` exist)?
2. Does `/etc/systole-channels.scm` exist in the installer ISO?
3. Did the installer successfully generate `/etc/config.scm`?
4. Does `/etc/config.scm` contain `(guix-configuration (channels ...))`?

**Debug:**
```bash
# In installer ISO
ls -la /etc/systole-channels.scm

# In installed system
cat /etc/config.scm | grep -A 5 "guix-configuration"
cat /etc/guix/channels.scm
```

### Deployment still fails with channel mismatch

**Ensure:**
1. Local system has the same channels: `guix pull --channels=channels-lock.scm`
2. Use time-machine for deployment: `guix time-machine -C channels-lock.scm -- deploy`
3. Or use the wrapper: `./scripts/deploy-with-channels.sh deployment.scm`

## References

- [Customizing the System-Wide Guix (GNU Guix Manual)](https://guix.gnu.org/manual/devel/en/html_node/Customizing-the-System_002dWide-Guix.html)
- [Channels (GNU Guix Manual)](https://guix.gnu.org/manual/en/html_node/Channels.html)
- [Patch: Add channels field to guix-configuration](https://issues.guix.gnu.org/49610)

## Summary

This implementation provides a **clean, automatic, and standard way** to ensure installed systems have reproducible channel specifications, eliminating manual channel synchronization and following official Guix practices.

The key insight: **Channels are configuration, not runtime state** - they should be declared in your system config just like packages and services.
