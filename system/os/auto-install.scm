;;; Automatic installer OS for testing
;;; Boots and automatically installs Systole system
;;; Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (os auto-install)
  #:use-module (gnu)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services ssh)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages admin)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (nongnu packages linux)
  #:use-module (systole transformations)
  #:export (systole-auto-install-os))

(define* (make-auto-install-script deploy-key)
  "Create a script that automatically installs the system."
  (program-file "auto-install"
                #~(begin
                    (use-modules (ice-9 ftw)
                                 (ice-9 match)
                                 (srfi srfi-1))

                    (define (log msg)
                      (display (string-append "[AUTO-INSTALL] " msg "\n"))
                      (force-output))

                    (define (run-command cmd)
                      (log (string-append "Running: " cmd))
                      (let ((status (system cmd)))
                        (unless (zero? status)
                          (log (string-append "Command failed with status: " (number->string status))))
                        status))

                    ;; Check if we're in the installer (not an installed system)
                    (unless (file-exists? "/run/current-system/profile/bin/bash")
                      (log "Not in installer environment, exiting")
                      (exit 0))

                    (log "Starting automatic installation...")
                    (sleep 10) ;; Wait for system to fully boot

                    ;; Wait for networking (installation-os should configure this automatically)
                    (log "Waiting for network initialization...")
                    (sleep 15)

                    ;; Test internet connectivity
                    (log "Testing internet connectivity...")
                    (let test-loop ((attempts 5))
                      (let ((ping-status (system (string-append #$inetutils "/bin/ping -c 1 -W 3 8.8.8.8 2>/dev/null"))))
                        (if (zero? ping-status)
                            (log "Internet connectivity: OK")
                            (if (> attempts 0)
                                (begin
                                  (log (string-append "No connectivity yet, retrying... (" (number->string attempts) " left)"))
                                  (sleep 3)
                                  (test-loop (- attempts 1)))
                                (begin
                                  (log "ERROR: No internet connectivity after multiple attempts!")
                                  (log "The installation-os networking may not be working.")
                                  (log "Try booting the regular installer first to verify QEMU networking works.")
                                  (exit 1))))))

                    ;; Auto-detect the target disk
                    (define target-disk
                      (let ((candidates '("/dev/vda" "/dev/sda" "/dev/hda" "/dev/nvme0n1")))
                        (or (find file-exists? candidates)
                            (begin
                              (log "ERROR: No suitable disk found!")
                              (log "Checked: /dev/vda /dev/sda /dev/hda /dev/nvme0n1")
                              (exit 1)))))

                    (log (string-append "Found target disk: " target-disk))

                    ;; Determine partition naming scheme
                    (define (partition-name disk n)
                      (if (string-contains disk "nvme")
                          (string-append disk "p" (number->string n))
                          (string-append disk (number->string n))))

                    ;; Check if disk is already partitioned
                    (when (file-exists? (partition-name target-disk 1))
                      (log "Disk already partitioned, skipping installation")
                      (exit 0))

                    ;; Partition the disk
                    (log (string-append "Partitioning " target-disk "..."))
                    (run-command (string-append #$parted "/sbin/parted -s " target-disk " -- mklabel gpt"))
                    (run-command (string-append #$parted "/sbin/parted -s " target-disk " -- mkpart ESP fat32 1MiB 512MiB"))
                    (run-command (string-append #$parted "/sbin/parted -s " target-disk " -- set 1 esp on"))
                    (run-command (string-append #$parted "/sbin/parted -s " target-disk " -- mkpart primary ext4 512MiB 100%"))
                    (sleep 2) ;; Wait for partitions to appear

                    ;; Format partitions
                    (log "Formatting partitions...")
                    (run-command (string-append #$dosfstools "/sbin/mkfs.fat -F 32 -n EFI " (partition-name target-disk 1)))
                    (run-command (string-append #$e2fsprogs "/sbin/mkfs.ext4 -L systole-root " (partition-name target-disk 2)))

                    ;; Mount filesystems
                    (log "Mounting filesystems...")
                    (run-command (string-append #$util-linux "/bin/mount " (partition-name target-disk 2) " /mnt"))
                    (run-command (string-append #$coreutils "/bin/mkdir -p /mnt/boot/efi"))
                    (run-command (string-append #$util-linux "/bin/mount " (partition-name target-disk 1) " /mnt/boot/efi"))

                    ;; Create system configuration
                    (log "Creating system configuration...")
                    (run-command (string-append #$coreutils "/bin/mkdir -p /mnt/etc"))

                    (with-output-to-file "/mnt/etc/config.scm"
                      (lambda ()
                        (display #$(string-append
                                    "(use-modules (gnu) (guix))\n"
                                    "(use-service-modules networking ssh)\n"
                                    "(use-package-modules screen ssh)\n\n"
                                    "(operating-system\n"
                                    "  (host-name \"systole-test\")\n"
                                    "  (timezone \"UTC\")\n"
                                    "  (locale \"en_US.utf8\")\n"
                                    "  (keyboard-layout (keyboard-layout \"us\"))\n\n"
                                    "  (bootloader\n"
                                    "   (bootloader-configuration\n"
                                    "    (bootloader grub-efi-bootloader)\n"
                                    "    (targets '(\"/boot/efi\"))\n"
                                    "    (keyboard-layout keyboard-layout)))\n\n"
                                    "  (file-systems\n"
                                    "   (cons* (file-system\n"
                                    "            (device (file-system-label \"systole-root\"))\n"
                                    "            (mount-point \"/\")\n"
                                    "            (type \"ext4\"))\n"
                                    "          (file-system\n"
                                    "            (device (file-system-label \"EFI\"))\n"
                                    "            (mount-point \"/boot/efi\")\n"
                                    "            (type \"vfat\"))\n"
                                    "          %base-file-systems))\n\n"
                                    "  (users\n"
                                    "   (cons* (user-account\n"
                                    "           (name \"systole\")\n"
                                    "           (comment \"Systole User\")\n"
                                    "           (group \"users\")\n"
                                    "           (supplementary-groups '(\"wheel\" \"netdev\")))\n"
                                    "          %base-user-accounts))\n\n"
                                    "  (packages\n"
                                    "   (append (list screen)\n"
                                    "           %base-packages))\n\n"
                                    "  (services\n"
                                    "   (cons* (service dhcpcd-service-type)\n"
                                    "          (service openssh-service-type\n"
                                    "                   (openssh-configuration\n"
                                    "                    (permit-root-login 'prohibit-password)\n"
                                    "                    (password-authentication? #f)\n"
                                    "                    (authorized-keys\n"
                                    "                     `((\"root\" ,(plain-file \"deploy-key.pub\"\n"
                                    "                                           \"" deploy-key "\"))))))\n"
                                    "          %base-services)))\n"))))

                    ;; Initialize cow-store
                    (log "Initializing cow-store...")
                    (run-command (string-append #$shepherd "/bin/herd start cow-store /mnt"))

                    ;; Install system
                    (log "Installing system (this will take 10-30 minutes)...")
                    (let ((status (run-command (string-append #$guix "/bin/guix system init /mnt/etc/config.scm /mnt"))))
                      (if (zero? status)
                          (begin
                            (log "Installation completed successfully!")
                            (log "Rebooting in 10 seconds...")
                            (sleep 10)
                            (run-command (string-append #$shepherd "/sbin/reboot")))
                          (begin
                            (log "Installation failed!")
                            (log "Dropping to shell for debugging...")
                            (system "/run/current-system/profile/bin/bash")))))))

(define* (systole-auto-install-os #:key (deploy-key #f))
  "Return an operating-system for automatic installation testing.
When DEPLOY-KEY is provided, the system will automatically install
itself with that SSH key authorized for root access."

  (unless deploy-key
    (error "deploy-key is required for auto-install-os"))

  ((compose (systole-transformation-deploy #:deploy-key deploy-key)
            (systole-transformation-guix #:guix-source? #t)
            (systole-transformation-linux #:initrd base-initrd))
   (operating-system
    (inherit installation-os)
    (kernel linux)
    (firmware (list linux-firmware))
    (keyboard-layout (keyboard-layout "us"))
    (kernel-arguments '("quiet" "net.ifnames=0"))

    (bootloader
     (bootloader-configuration
      (inherit (operating-system-bootloader installation-os))
      (targets '("/dev/null"))))

    ;; Add packages needed for automatic installation
    (packages
     (append (list util-linux     ;; For partitioning and formatting
                   gptfdisk      ;; For parted's GPT support
                   parted        ;; For disk partitioning
                   e2fsprogs     ;; For mkfs.ext4
                   dosfstools)   ;; For mkfs.fat
             (operating-system-packages installation-os)))

    ;; Modify kmscon to automatically run installation script instead of GUI
    (services
     (modify-services (operating-system-user-services installation-os)
                      (kmscon-service-type cfg =>
                                           (kmscon-configuration
                                            (inherit cfg)
                                            (login-program (make-auto-install-script deploy-key)))))))))
