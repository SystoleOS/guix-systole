#!/run/current-system/profile/bin/bash
# Systole Automated System Installation Script
# Performs unattended installation of Systole system
# Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

# Default values
TARGET_DISK=""
HOSTNAME="systole"
TIMEZONE="UTC"
LOCALE="en_US.utf8"
KEYBOARD="us"
ROOT_PASSWORD=""
DEPLOY_KEY=""
DEPLOYER_SIGNING_KEY=""
CONFIG_FILE=""
DRY_RUN=false

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

usage() {
    cat <<EOF
Usage: $0 --disk DEVICE [OPTIONS]

Automated Systole system installation for unattended deployments.

REQUIRED:
    --disk DEVICE              Target disk (e.g., /dev/vda, /dev/sda)

OPTIONAL:
    --hostname NAME            System hostname (default: systole)
    --timezone TZ              Timezone (default: UTC)
    --locale LOCALE            System locale (default: en_US.utf8)
    --keyboard LAYOUT          Keyboard layout (default: us)
    --root-password PASS       Root password (default: none, key-only)
    --deploy-key FILE          SSH public key file for deployment
    --deployer-key FILE        Guix signing key file for deployment
    --config FILE              Custom system configuration file
    --dry-run                  Show what would be done without doing it
    -h, --help                Show this help

EXAMPLES:
    # Basic installation to /dev/vda
    $0 --disk /dev/vda

    # With custom hostname and timezone
    $0 --disk /dev/vda --hostname lab-server --timezone Europe/Oslo

    # With deployment keys for guix deploy
    $0 --disk /dev/vda \\
       --deploy-key ~/.ssh/systole-deploy.pub \\
       --deployer-key /etc/guix/signing-key.pub

    # With custom configuration
    $0 --disk /dev/vda --config /mnt/my-config.scm

DISK LAYOUT:
    The script will create:
    - 512MB EFI partition (/dev/vda1)
    - Remaining space as root partition (/dev/vda2)

NOTES:
    - ALL DATA ON TARGET DISK WILL BE DESTROYED
    - Requires root privileges
    - Designed for automated/remote installation
    - After installation, system can be updated via 'guix deploy'

EOF
    exit 0
}

error() {
    echo -e "${RED}ERROR: $*${NC}" >&2
    exit 1
}

info() {
    echo -e "${GREEN}INFO: $*${NC}"
}

warn() {
    echo -e "${YELLOW}WARNING: $*${NC}"
}

step() {
    echo -e "${BLUE}==>${NC} $*"
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --disk)
            TARGET_DISK="$2"
            shift 2
            ;;
        --hostname)
            HOSTNAME="$2"
            shift 2
            ;;
        --timezone)
            TIMEZONE="$2"
            shift 2
            ;;
        --locale)
            LOCALE="$2"
            shift 2
            ;;
        --keyboard)
            KEYBOARD="$2"
            shift 2
            ;;
        --root-password)
            ROOT_PASSWORD="$2"
            shift 2
            ;;
        --deploy-key)
            if [[ ! -f "$2" ]]; then
                error "Deploy key file not found: $2"
            fi
            DEPLOY_KEY="$(cat "$2")"
            shift 2
            ;;
        --deployer-key)
            if [[ ! -f "$2" ]]; then
                error "Deployer signing key file not found: $2"
            fi
            DEPLOYER_SIGNING_KEY="$(cat "$2")"
            shift 2
            ;;
        --config)
            if [[ ! -f "$2" ]]; then
                error "Configuration file not found: $2"
            fi
            CONFIG_FILE="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        -h|--help)
            usage
            ;;
        *)
            error "Unknown option: $1\nUse --help for usage information."
            ;;
    esac
done

# Validate required parameters
if [[ -z "$TARGET_DISK" ]]; then
    error "Target disk is required. Use --disk to specify."
fi

if [[ ! -b "$TARGET_DISK" ]]; then
    error "Target disk does not exist or is not a block device: $TARGET_DISK"
fi

if [[ $EUID -ne 0 ]]; then
    error "This script must be run as root"
fi

# Display configuration
step "Installation Configuration"
echo "  Target disk:    $TARGET_DISK"
echo "  Hostname:       $HOSTNAME"
echo "  Timezone:       $TIMEZONE"
echo "  Locale:         $LOCALE"
echo "  Keyboard:       $KEYBOARD"
echo "  Deploy key:     ${DEPLOY_KEY:+configured}"
echo "  Deployer key:   ${DEPLOYER_SIGNING_KEY:+configured}"
echo "  Custom config:  ${CONFIG_FILE:-none (will generate)}"
echo "  Dry run:        $DRY_RUN"
echo ""

if [[ "$DRY_RUN" == "true" ]]; then
    info "DRY RUN MODE - No changes will be made"
    exit 0
fi

warn "ALL DATA ON $TARGET_DISK WILL BE DESTROYED"
echo -n "Continue? (yes/no): "
read -r CONFIRM
if [[ "$CONFIRM" != "yes" ]]; then
    info "Installation cancelled"
    exit 0
fi

# Partition the disk
step "Partitioning disk $TARGET_DISK"
parted -s "$TARGET_DISK" -- mklabel gpt
parted -s "$TARGET_DISK" -- mkpart ESP fat32 1MiB 512MiB
parted -s "$TARGET_DISK" -- set 1 esp on
parted -s "$TARGET_DISK" -- mkpart primary ext4 512MiB 100%

# Get partition device names
if [[ "$TARGET_DISK" =~ nvme ]]; then
    EFI_PART="${TARGET_DISK}p1"
    ROOT_PART="${TARGET_DISK}p2"
else
    EFI_PART="${TARGET_DISK}1"
    ROOT_PART="${TARGET_DISK}2"
fi

# Format partitions
step "Formatting partitions"
mkfs.fat -F 32 -n EFI "$EFI_PART"
mkfs.ext4 -L systole-root "$ROOT_PART"

# Mount filesystems
step "Mounting filesystems"
mount "$ROOT_PART" /mnt
mkdir -p /mnt/boot/efi
mount "$EFI_PART" /mnt/boot/efi

# Generate or use provided configuration
if [[ -n "$CONFIG_FILE" ]]; then
    step "Using provided configuration: $CONFIG_FILE"
    cp "$CONFIG_FILE" /mnt/etc/config.scm
else
    step "Generating system configuration"
    mkdir -p /mnt/etc

    # Build authorized keys section if deploy key provided
    AUTHORIZED_KEYS_SECTION=""
    if [[ -n "$DEPLOY_KEY" ]]; then
        # Escape the key for Scheme
        ESCAPED_KEY="${DEPLOY_KEY//\\/\\\\}"
        ESCAPED_KEY="${ESCAPED_KEY//\"/\\\"}"
        AUTHORIZED_KEYS_SECTION="
                 (authorized-keys
                  \`((\"root\" ,(plain-file \"deploy-key.pub\"
                                          \"$ESCAPED_KEY\"))))"
    fi

    # Build deployer signing key section if provided
    DEPLOYER_KEY_SECTION=""
    if [[ -n "$DEPLOYER_SIGNING_KEY" ]]; then
        DEPLOYER_KEY_SECTION="
;; Guix signing key from deployment machine
(define %deployer-signing-key
  (plain-file \"deployer.pub\"
    \"$DEPLOYER_SIGNING_KEY\"))
"
    fi

    DEPLOYER_SERVICE=""
    if [[ -n "$DEPLOYER_SIGNING_KEY" ]]; then
        DEPLOYER_SERVICE="
       ;; CRITICAL: Authorize deployment machine's signing key
       (simple-service 'deployer-key
                       guix-service-type
                       (guix-extension
                        (guix-configuration
                         (authorized-keys (list %deployer-signing-key)))))
"
    fi

    cat > /mnt/etc/config.scm <<EOF
;; Systole system configuration
;; Generated by systole-auto-install

(use-modules (gnu)
             (gnu system)
             (guix))
(use-service-modules networking ssh)
(use-package-modules screen ssh)

$DEPLOYER_KEY_SECTION
(operating-system
  (host-name "$HOSTNAME")
  (timezone "$TIMEZONE")
  (locale "$LOCALE")

  (keyboard-layout (keyboard-layout "$KEYBOARD"))

  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efi"))
    (keyboard-layout keyboard-layout)))

  (file-systems
   (cons* (file-system
            (device (file-system-label "systole-root"))
            (mount-point "/")
            (type "ext4"))
          (file-system
            (device (file-system-label "EFI"))
            (mount-point "/boot/efi")
            (type "vfat"))
          %base-file-systems))

  (users
   (cons* (user-account
           (name "systole")
           (comment "Systole User")
           (group "users")
           (supplementary-groups '("wheel" "netdev" "audio" "video")))
          %base-user-accounts))

  (packages
   (append (list screen)
           %base-packages))

  (services
   (cons*
    (service openssh-service-type
             (openssh-configuration
              (permit-root-login 'prohibit-password)
              (password-authentication? #f)$AUTHORIZED_KEYS_SECTION))

    (service dhcp-client-service-type)
$DEPLOYER_SERVICE
    %base-services)))
EOF
fi

# Perform installation
step "Installing system (this may take a while...)"
herd start cow-store /mnt

info "Running guix system init..."
guix system init /mnt/etc/config.scm /mnt

step "Installation complete!"
echo ""
echo -e "${GREEN}===== Installation Summary =====${NC}"
echo "  Target:       $TARGET_DISK"
echo "  Hostname:     $HOSTNAME"
echo "  Root FS:      $ROOT_PART (ext4)"
echo "  EFI:          $EFI_PART (vfat)"
echo "  Config:       /mnt/etc/config.scm"
echo ""
echo -e "${YELLOW}===== Next Steps =====${NC}"
echo "1. Unmount filesystems:"
echo "   umount /mnt/boot/efi"
echo "   umount /mnt"
echo ""
echo "2. Reboot the system"
echo ""
if [[ -n "$DEPLOY_KEY" ]]; then
    echo "3. SSH to the system:"
    echo "   ssh -i your-deploy-key root@<target-ip>"
    echo ""
    if [[ -n "$DEPLOYER_SIGNING_KEY" ]]; then
        echo "4. Deploy updates using 'guix deploy'"
    fi
else
    echo "3. Login at console (no SSH configured)"
fi
echo ""
