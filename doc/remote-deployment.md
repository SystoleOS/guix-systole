# Remote Deployment Guide

Guide for using the Systole installer with `guix deploy` for remote
system installation and ongoing deployment.

## Overview

1. Build an installer ISO with an SSH deploy key (and optionally a Guix
   signing key and SSH host key)
2. Boot the installer and establish SSH access
3. Install — automated (`systole-auto-install.sh`) or via the GUI
4. Generate a deployment specification and deploy with `guix deploy`

## Quick Start: Fully Automated VM Installation

```bash
# 1. Generate keys
ssh-keygen -t ed25519 -f ~/.ssh/systole-deploy -N ""

# 2. Build installer (ISO lands in artifacts/ by default)
./scripts/build-installer-with-deploy.sh \
  --ssh-deploy-key-file ~/.ssh/systole-deploy.pub

# 3. Create VM disk
qemu-img create -f qcow2 target-disk.qcow2 50G

# 4. Boot installer
qemu-system-x86_64 \
  -m 4096 -smp 2 \
  -cdrom artifacts/systole-installer-deploy-*.iso \
  -boot d \
  -net nic,model=virtio \
  -net user,hostfwd=tcp::2223-:22 \
  -drive file=target-disk.qcow2,format=qcow2,if=virtio \
  -enable-kvm &

# 5. Wait for boot, then copy automation script
scp -P 2223 -i ~/.ssh/systole-deploy \
    scripts/systole-auto-install.sh \
    root@localhost:/tmp/

# 6. Run automated installation
ssh -i ~/.ssh/systole-deploy -p 2223 root@localhost \
  "/tmp/systole-auto-install.sh \
    --disk /dev/vda \
    --hostname test-vm \
    --deploy-key '$(cat ~/.ssh/systole-deploy.pub)' \
    --deployer-key '$(cat /etc/guix/signing-key.pub)'"

# 7. System will install and be ready for guix deploy after reboot
```

## Prerequisites

- SSH key pair for deployment (`ssh-keygen -t ed25519`)
- Guix signing key from your deployment machine (`/etc/guix/signing-key.pub`)
- Network access to the target machine

## Important: Channel Management

Before building installer ISOs or deploying systems, read the
[Channel Management Guide](channel-management.md):

- Installer builds use `guix time-machine` with `channels-lock.scm` for
  reproducibility
- Use `scripts/sync-and-deploy.sh` instead of `guix deploy` directly to
  keep channels in sync between deployment host and target
- Channel version mismatches are the most common deployment failure

## Part 1: Build the Installer ISO

`scripts/build-installer-with-deploy.sh` builds an ISO through
`guix time-machine -C channels-lock.scm`. Its current flags:

| Flag | Purpose |
|---|---|
| `--ssh-deploy-key KEY` | SSH public key string authorized for root in the installer |
| `--ssh-deploy-key-file FILE` | Same, read from a file |
| `--channels-file FILE` | Channel specs embedded in installed systems (default: `channels-lock.scm`) |
| `--signing-key-file FILE` | Guix signing key (`.pub`) to authorize — enables immediate `guix deploy` |
| `--host-key-file FILE` | SSH host *private* key (expects `FILE.pub` sibling) for stable host fingerprints |
| `--output FILE` | Output ISO path (default: `artifacts/systole-installer-deploy-<timestamp>.iso`) |
| `--no-time-machine` | Build with current channels instead of the lock file (testing only) |

The old `--key`/`--key-file` flags (and `SYSTOLE_DEPLOY_KEY*`
environment variables) are **deprecated** — use
`--ssh-deploy-key`/`--ssh-deploy-key-file` (`SYSTOLE_SSH_DEPLOY_KEY*`).
Every flag also has a `SYSTOLE_*` environment-variable equivalent; run
`./scripts/build-installer-with-deploy.sh --help` for the full list.

```bash
# Just SSH access
./scripts/build-installer-with-deploy.sh \
  --ssh-deploy-key-file ~/.ssh/systole-deploy.pub

# SSH + signing key (enables guix deploy without manual authorization)
./scripts/build-installer-with-deploy.sh \
  --ssh-deploy-key-file ~/.ssh/systole-deploy.pub \
  --signing-key-file /etc/guix/signing-key.pub
```

The ISO is registered as a GC root during the build and copied to the
output path (default under `artifacts/`, which is gitignored so
multi-gigabyte images do not pile up in the repository root).

The resulting installer has SSH enabled on boot, your key authorized
for root, and password authentication disabled.

### Security caveat: `--host-key-file`

Providing an SSH host key gives reinstalled machines a stable SSH
fingerprint, but the **private key is embedded in the image and
therefore transits the world-readable `/gnu/store`** (on the build
machine, inside the ISO, and on any substitute server it reaches).
The installed system copies the key into `/etc` with `0600`
permissions (a real copy, not a store symlink), but the store copy
itself is unavoidable with this transport. Treat that host identity as
disposable: use it only to bridge the install, and **rotate the host
key after installation**. The build script prints a warning to the
same effect.

### Write ISO to USB (for physical machines)

```bash
lsblk                                   # find the USB device
sudo dd if=artifacts/systole-installer-deploy-*.iso \
        of=/dev/sdX bs=4M status=progress
sudo sync
```

## Part 2: Boot Installer and Verify SSH Access

Physical machines: boot from USB, find the IP (DHCP server, or connect
a monitor and run `ip addr`). VMs: use the QEMU invocation from the
Quick Start with `hostfwd=tcp::2223-:22`.

```bash
ssh -i ~/.ssh/systole-deploy root@<target-ip>        # physical
ssh -i ~/.ssh/systole-deploy -p 2223 root@localhost  # VM

# Sanity checks once connected
guix --version
lsblk
ip addr && ping -c 3 8.8.8.8
```

## Part 3: Install

### Automated (optional)

Copy `scripts/systole-auto-install.sh` to the installer and run it as
in the Quick Start. It partitions the disk (512MB EFI + root), formats
filesystems, generates a system configuration embedding your deploy and
deployer keys, runs `guix system init`, and configures SSH/Guix
authorization for future deployments. Afterwards:

```bash
umount /mnt/boot/efi
umount /mnt
reboot
```

### Interactive

Use the standard Systole installer GUI. The installer writes the
generated system configuration to `/etc/config.scm` on the target.

## Part 4: Prepare the Deployment Specification

`deployment.scm` is **generated, not hand-written**, and is gitignored
at the repository root (it contains host-specific data and key paths).
After the target is installed and reachable:

```bash
./scripts/prepare-deployment.sh \
  --key ~/.ssh/systole-deploy \
  --host <target-ip> \
  [--port 22] [--output deployment.scm]
```

This fetches the target's own `/etc/config.scm` and produces a
`deployment.scm` wired for `guix deploy` / `sync-and-deploy.sh`.

For manual setups, the expected shape is documented in
[`doc/examples/deployment.scm.example`](examples/deployment.scm.example).
The critical ingredients are:

1. **SSH service** with your deploy key authorized for root
2. **Guix authorization**: your deployment machine's signing key in
   `guix-configuration`'s `authorized-keys` — without this,
   deployments fail with "unauthorized" errors. Put the key in the
   configuration, not in a one-off `guix authorize` call, so it
   persists across reconfigurations.
3. Optionally `#:community-substitutes? #t` on
   `systole-transformation-guix` if you want the community guix.moe
   substitutes (off by default; see the transformation's docstring)

## Part 5: Deploy

```bash
# Recommended: syncs channels-lock.scm to the target first
./scripts/sync-and-deploy.sh deployment.scm

# Or plainly, if you know the channels match
guix deploy deployment.scm
```

After the deployment completes:

```bash
ssh -i ~/.ssh/systole-deploy root@<target-ip>
guix system describe
```

Future deployments repeat the same command against the running system.

## Troubleshooting

### "guix deploy: error: failed to deploy"

Check, in order:
1. SSH access works: `ssh -i ~/.ssh/systole-deploy root@<target>`
2. Signing key is in the system configuration (`cat /etc/guix/acl` on
   the target should include it)
3. Target has network access and disk space

### "substitute: unauthorized" / "Permission denied" during build

The target's guix daemon does not accept your signing key. Ensure the
deployment configuration includes it via `guix-configuration`
`authorized-keys` (see the example file), then redeploy.

### Channel mismatch ("commit ... is not a descendant of ...")

Use `./scripts/sync-and-deploy.sh`, or manually sync:

```bash
scp channels-lock.scm root@target:~/.config/guix/channels.scm
ssh root@target 'guix pull && hash guix'
```

### Deployment hangs during build

Build on the deployment machine instead:

```bash
guix deploy --build-locally=no deployment.scm
```

## Security Considerations

- **Deploy key** grants root SSH — protect it, use per-environment
  keys, rotate periodically. It only works in the installer
  environment; the installed system uses its own configuration.
- **Guix signing key** authorizes substitutes and deployments;
  compromise allows deploying malicious packages. Keep
  `/etc/guix/signing-key.sec` secure.
- **Host keys** passed via `--host-key-file` transit the world-readable
  store (see Part 1) — rotate them after installation.

## References

- [Guix Manual: Invoking guix deploy](https://guix.gnu.org/manual/en/html_node/Invoking-guix-deploy.html)
- [Guix Manual: Invoking guix archive](https://guix.gnu.org/manual/en/html_node/Invoking-guix-archive.html)
- [Channel Management Guide](channel-management.md)
