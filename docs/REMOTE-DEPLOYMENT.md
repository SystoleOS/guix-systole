# Remote Deployment Guide

Complete guide for using the Systole installer with `guix deploy` for remote system installation and deployment.

## Overview

This guide covers:
1. Building an installer ISO with SSH deploy key
2. Booting the installer and establishing SSH access
3. **Automated installation** (optional - skip GUI installer)
4. Deploying a system remotely using `guix deploy`
5. Configuring the deployed system for future deployments

## Quick Start: Fully Automated VM Installation

For testing or automated deployments:

```bash
# 1. Generate keys
ssh-keygen -t ed25519 -f ~/.ssh/systole-deploy -N ""

# 2. Build installer
./scripts/build-installer-with-deploy.sh --key-file ~/.ssh/systole-deploy.pub

# 3. Create VM disk
qemu-img create -f qcow2 target-disk.qcow2 50G

# 4. Boot installer
qemu-system-x86_64 \
  -m 4096 -smp 2 \
  -cdrom systole-installer-deploy-*.iso \
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

# 7. System will install and be ready for guix deploy after reboot!
```

## Prerequisites

- SSH key pair for deployment (`ssh-keygen -t ed25519`)
- Guix signing key from your deployment machine
- Target system configuration file
- Network access to target machine

## Important: Channel Management

Before building installer ISOs or deploying systems, read the [Channel Management Guide](CHANNEL-MANAGEMENT.md) to understand:

- Why channel version mismatches cause deployment failures
- How to use `channels-lock.scm` for reproducible builds
- How to sync channels between deployment host and target systems
- Using `sync-and-deploy.sh` for automated channel synchronization

**Quick version:**
- Installer builds use `guix time-machine` with `channels-lock.scm` for reproducibility
- Use `scripts/sync-and-deploy.sh` instead of `guix deploy` directly
- This ensures channel compatibility and prevents deployment errors

## Part 1: Build Installer with Deploy Key

### Generate Deploy Key (if needed)

```bash
# Generate SSH key for deployment
ssh-keygen -t ed25519 -f ~/.ssh/systole-deploy -N "" -C "systole-deploy"
```

### Build Installer ISO

```bash
# Build installer with your SSH public key
./scripts/build-installer-with-deploy.sh \
  --key-file ~/.ssh/systole-deploy.pub \
  --output systole-installer-deploy.iso
```

The resulting ISO will have:
- SSH daemon enabled on boot
- Your SSH key authorized for root
- Password authentication disabled
- Ready for remote deployment

### Write ISO to USB (for physical machines)

```bash
# Find USB device
lsblk

# Write ISO (replace /dev/sdX with your device)
sudo dd if=systole-installer-deploy.iso of=/dev/sdX bs=4M status=progress
sudo sync
```

## Part 2: Boot Installer and Verify SSH Access

### For Physical Machines

1. Boot from USB on target machine
2. Wait for system to boot
3. Find IP address (check DHCP server, or connect monitor and run `ip addr`)
4. Test SSH access:

```bash
ssh -i ~/.ssh/systole-deploy root@<target-ip>
```

### For Virtual Machines (Testing)

```bash
# Create target disk image (50GB)
qemu-img create -f qcow2 target-disk.qcow2 50G

# Boot ISO in QEMU with port forwarding
qemu-system-x86_64 \
  -m 4096 \
  -smp 2 \
  -cdrom systole-installer-deploy.iso \
  -boot d \
  -net nic,model=virtio \
  -net user,hostfwd=tcp::2223-:22 \
  -drive file=target-disk.qcow2,format=qcow2,if=virtio \
  -enable-kvm

# Test SSH access
ssh -i ~/.ssh/systole-deploy -p 2223 root@localhost
```

### Verify Installer Environment

```bash
# Once connected via SSH
ssh -i ~/.ssh/systole-deploy root@<target-ip>

# Verify guix is available
guix --version

# Check available disks
lsblk

# Verify network
ip addr
ping -c 3 8.8.8.8
```

## Part 2.5: Automated Installation (Optional)

For unattended/automated installation, you can use the `systole-auto-install.sh` script instead of the interactive installer GUI.

### Copy Script to Installer

```bash
# From your deployment machine, copy the script to the installer
scp -i ~/.ssh/systole-deploy \
    scripts/systole-auto-install.sh \
    root@<target-ip>:/tmp/

# For VM with port forwarding
scp -P 2223 -i ~/.ssh/systole-deploy \
    scripts/systole-auto-install.sh \
    root@localhost:/tmp/
```

### Run Automated Installation

```bash
# SSH to installer
ssh -i ~/.ssh/systole-deploy root@<target-ip>

# Or for VM
ssh -i ~/.ssh/systole-deploy -p 2223 root@localhost

# Run automated installation
chmod +x /tmp/systole-auto-install.sh
/tmp/systole-auto-install.sh \
  --disk /dev/vda \
  --hostname my-server \
  --timezone Europe/Oslo \
  --deploy-key "$(cat ~/.ssh/systole-deploy.pub)" \
  --deployer-key "$(cat /etc/guix/signing-key.pub)"
```

**What it does:**
- Partitions disk automatically (512MB EFI + remaining as root)
- Formats filesystems
- Generates system configuration with your keys
- Installs system with `guix system init`
- Configures SSH and guix authorization for future deployments

**After automated installation:**
```bash
# Unmount and reboot
umount /mnt/boot/efi
umount /mnt
reboot
```

The system will be ready for `guix deploy` after reboot!

### Alternative: Manual Installation

If you prefer the interactive installer GUI, skip the automated script and use the standard Systole installer interface. After manual installation, proceed to Part 3 to configure deployment keys.

## Part 3: Prepare for Deployment

### Get Your Guix Signing Key

On your **deployment machine** (not the installer):

```bash
# Your guix signing key is here
cat /etc/guix/signing-key.pub

# It looks like:
# (public-key
#  (ecc
#   (curve Ed25519)
#   (q #ABC123...#)))
```

**CRITICAL:** The deployed system needs this key to accept substitutes and deployments from your machine.

### Create Deployment Configuration

Create a deployment specification file `deployment.scm`:

```scheme
;; deployment.scm - Guix deployment specification

(use-modules (gnu) (guix))
(use-modules (gnu machine))
(use-modules (gnu machine ssh))
(use-modules (gnu services)
(use-modules (gnu services ssh))

;; Your deployment machine's signing key (replace with actual key)
(define %deployer-signing-key
  (plain-file "deployer.pub"
    "(public-key
      (ecc
       (curve Ed25519)
       (q #YOUR-KEY-HERE#)))"))

(define %target-system
  (operating-system
    (host-name "target-hostname")
    (timezone "Europe/Oslo")
    (locale "en_US.utf8")

    (keyboard-layout (keyboard-layout "us"))

    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))
      (keyboard-layout keyboard-layout)))

    (file-systems
     (cons* (file-system
              (device (uuid "YOUR-ROOT-UUID"))
              (mount-point "/")
              (type "ext4"))
            (file-system
              (device (uuid "YOUR-EFI-UUID"))
              (mount-point "/boot/efi")
              (type "vfat"))
            %base-file-systems))

    (users
     (cons* (user-account
             (name "admin")
             (group "users")
             (supplementary-groups '("wheel" "netdev" "audio" "video")))
            %base-user-accounts))

    (packages
     (append (list git curl vim)
             %base-packages))

    (services
     (append
      (list
       ;; SSH service with your deploy key
       (service openssh-service-type
                (openssh-configuration
                 (permit-root-login 'prohibit-password)
                 (password-authentication? #f)
                 (authorized-keys
                  `(("root" ,(local-file "/home/user/.ssh/systole-deploy.pub"))))))

       ;; CRITICAL: Authorize your deployment machine
       (simple-service 'deployer-key
                       guix-service-type
                       (guix-extension
                        (guix-configuration
                         (authorized-keys
                          (list %deployer-signing-key))))))
      %base-services))))

;; Deployment machine specification
(list (machine
       (operating-system %target-system)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "TARGET-IP-OR-HOSTNAME")
                       (system "x86_64-linux")
                       (user "root")
                       (identity "~/.ssh/systole-deploy")
                       (port 22)))))
```

**Key Points:**
1. **SSH Configuration:** The deployed system must have SSH with your deploy key
2. **Guix Authorization:** The `guix-extension` with `authorized-keys` is CRITICAL - without this, deployments will fail
3. **Root Access:** Deploy as root (simplest approach)

## Part 4: Perform Initial Deployment

### Deploy the System

From your **deployment machine**:

```bash
# Perform deployment
guix deploy deployment.scm

# This will:
# 1. Connect to installer via SSH
# 2. Build the system configuration
# 3. Install to target disk
# 4. Set up bootloader
# 5. Reboot target machine
```

### Common Issues and Solutions

#### "Permission Denied" During Build

**Problem:** Guix daemon on target doesn't accept your signing key.

**Solution:** Ensure the deployment configuration includes:
```scheme
(simple-service 'deployer-key
                guix-service-type
                (guix-extension
                 (guix-configuration
                  (authorized-keys
                   (list %deployer-signing-key)))))
```

#### "Could not authenticate"

**Problem:** `guix authorize` was called but key not properly configured.

**Solution:** Don't just call `guix authorize` - the key MUST be in the system configuration via `authorized-keys` in `guix-configuration`. This ensures it persists across reboots.

#### Deployment Hangs During Build

**Problem:** Building on installer is slow or out of memory.

**Solution:** Use `--build-locally=no` to build on deployment machine:
```bash
guix deploy --build-locally=no deployment.scm
```

Or configure substitutes in the deployment:
```scheme
(guix-configuration
 (authorized-keys (list %deployer-signing-key))
 (substitute-urls
  '("https://ci.guix.gnu.org"
    "https://bordeaux.guix.gnu.org")))
```

## Part 5: Verify Deployed System

After deployment completes and system reboots:

### Reconnect to Deployed System

```bash
# SSH to deployed system (may be different IP after reboot)
ssh -i ~/.ssh/systole-deploy root@<target-ip>

# Verify system
guix system describe
ls /run/current-system
```

### Test Future Deployments

```bash
# Make a change to deployment.scm (e.g., add a package)
# Then redeploy:
guix deploy deployment.scm

# Should work without issues now that authorization is set up
```

## Part 6: Deployment to Installer vs. Deployed System

### Understanding the Two Phases

**Phase 1: Deploy TO the Installer (Initial Installation)**
- SSH key in installer authorizes access
- Use `guix deploy` to install system to disk
- Installer is temporary environment

**Phase 2: Deploy TO the Deployed System (Updates)**
- SSH key in deployed system configuration authorizes access
- Use `guix deploy` to update the installed system
- This is the normal operation mode

### Configuration for Both Phases

The deployment configuration must include:
1. SSH service with deploy key (for SSH access)
2. Guix service with authorized signing key (for guix operations)

## Complete Example: VM Deployment

### Step 1: Create VM Disk

```bash
qemu-img create -f qcow2 target-disk.qcow2 50G
```

### Step 2: Boot Installer with Deploy Key

```bash
# Build installer
./scripts/build-installer-with-deploy.sh --key-file ~/.ssh/systole-deploy.pub

# Boot installer
qemu-system-x86_64 \
  -m 4096 \
  -smp 2 \
  -cdrom systole-installer-deploy.iso \
  -boot d \
  -drive file=target-disk.qcow2,format=qcow2,if=virtio \
  -net nic,model=virtio \
  -net user,hostfwd=tcp::2223-:22 \
  -enable-kvm &

# Wait for boot, then test SSH
ssh -i ~/.ssh/systole-deploy -p 2223 root@localhost
```

### Step 3: Prepare Deployment Configuration

```scheme
;; vm-deployment.scm
(use-modules (gnu) (guix))
(use-modules (gnu machine))
(use-modules (gnu machine ssh))

(define %my-signing-key
  (plain-file "deployer.pub"
    ;; YOUR ACTUAL KEY FROM /etc/guix/signing-key.pub
    "(public-key (ecc (curve Ed25519) (q #ABC...#)))"))

(define %target-os
  (operating-system
    (host-name "test-vm")
    (timezone "UTC")
    (locale "en_US.utf8")

    (bootloader
     (bootloader-configuration
      (bootloader grub-bootloader)
      (targets '("/dev/vda"))))

    (file-systems
     (cons* (file-system
              (device "/dev/vda1")
              (mount-point "/")
              (type "ext4"))
            %base-file-systems))

    (services
     (cons*
      (service openssh-service-type
               (openssh-configuration
                (permit-root-login 'prohibit-password)
                (authorized-keys
                 `(("root" ,(local-file "~/.ssh/systole-deploy.pub"))))))

      (simple-service 'my-deploy-key
                      guix-service-type
                      (guix-extension
                       (guix-configuration
                        (authorized-keys (list %my-signing-key)))))

      %base-services))))

(list (machine
       (operating-system %target-os)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "localhost")
                       (port 2223)
                       (user "root")
                       (identity "~/.ssh/systole-deploy")))))
```

### Step 4: Deploy

```bash
# Deploy to VM
guix deploy vm-deployment.scm
```

### Step 5: Verify

After reboot, the deployed system should:
- Accept SSH with your deploy key
- Accept future `guix deploy` commands
- Have guix authorized keys properly configured

## Security Considerations

### Deploy Key Security

- Deploy key grants root access - protect it carefully
- Use different keys for different environments (dev/prod)
- Consider key rotation policies

### Guix Signing Key Security

- Your signing key authorizes substitutes and deployments
- Compromise allows malicious packages to be deployed
- Keep `/etc/guix/signing-key.sec` secure on deployment machine

### Network Security

- Deploy key only active during deployment
- Use VPN or secure network for remote deployments
- Consider network-level access controls (firewall, VPN)

## Troubleshooting

### "guix deploy: error: failed to deploy"

Check:
1. SSH access works: `ssh -i ~/.ssh/systole-deploy root@target`
2. Signing key is in configuration
3. Target has network access
4. Disk space available

### "substitute: unauthorized"

Your signing key is not authorized. Ensure:
```scheme
(simple-service 'deployer-key
                guix-service-type
                (guix-extension
                 (guix-configuration
                  (authorized-keys (list %deployer-signing-key)))))
```

### Can SSH but "guix deploy" fails

- Verify guix daemon is running: `herd status guix-daemon`
- Check signing key authorization: `cat /etc/guix/acl`
- Ensure key in system configuration, not just manually authorized

## References

- [Guix Manual: Invoking guix deploy](https://guix.gnu.org/manual/en/html_node/Invoking-guix-deploy.html)
- [Guix Manual: Invoking guix archive](https://guix.gnu.org/manual/en/html_node/Invoking-guix-archive.html)
- [Guix Cookbook: Guix System Image for Deployment](https://guix.gnu.org/cookbook/en/html_node/Guix-System-Image-API.html)

## Summary

Key takeaways:
1. **SSH Deploy Key** - Gets you SSH access to installer/system
2. **Guix Signing Key** - MUST be in system configuration for deployments to work
3. **System Configuration** - Must include both SSH and Guix authorization
4. **Don't use `guix authorize`** - Put keys in configuration instead

The installer with deploy key provides remote access. The deployed system configuration with proper authorization enables ongoing deployments.
