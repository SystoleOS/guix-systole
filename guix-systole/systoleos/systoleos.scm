;; This operating-system image definition can be built with:
;; `guix system image -L . -t iso9660 guix-systole/systoleos/systoleos.scm`

(define-module (guix-systole systoleos systoleos)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages conky)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages xfce)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services guix)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services lightdm)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system image)
  #:use-module (gnu system install)
  #:use-module (gnu system shadow)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu image)
  #:use-module (gnu packages)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (guix-systole services dicomd-service)
  #:use-module (guix-systole packages slicer))

;; https://substitutes.nonguix.org/signing-key.pub
(define %signing-key
  (plain-file "nonguix.pub"
   "(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

;; The Nonguix channel is necessary for the Linux kernel with nonfree blobs,
;; required by most commercial hardware.
(define %channels
  (cons* (channel
           (name 'nonguix)
           (url "https://gitlab.com/nonguix/nonguix")
           ;; Enable signature verification:
           (introduction
            (make-channel-introduction
             "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
             (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
         (channel
           (name 'guix)
           (url "https://codeberg.org/guix/guix-mirror.git")
           (branch "master")
           (introduction
            (make-channel-introduction
             "c91e27c60864faa229198f6f0caf620275c429a2"
             (openpgp-fingerprint
              "2841 9AC6 5038 7440 C7E9 2FFA 2208 D209 58C1 DEB0"))))
         (channel
           (name 'guix-systole)
           (url "https://github.com/SystoleOS/guix-systole")
           ;; (branch "dev")
           (branch "SCRUM-126-Define-package-for-SystoleOS-including-Slicer"))
         %default-channels))

(define serialise-channels
  (scheme-file "channels.scm"
               #~(begin
                   (with-output-to-file %output
                     (lambda ()
                       (display (sexp->string '%
                                              (%channels)))
                       (newline))))))

;; Fluxbox configuration inspired by guix-psy-dicom
;; https://github.com/OUH-MESHLab/guix-psy-dicom/blob/enhancement/psydicom_system/config.scm
(define conkyrc
  (local-file "etc/conky/conky.conf"))
(define fluxbox-init
  (local-file "etc/fluxbox/init"))
(define fluxbox-keys
  (local-file "etc/fluxbox/keys"))
(define fluxbox-menu
  (local-file "etc/fluxbox/menu"))
(define fluxbox-startup
  (local-file "etc/fluxbox/startup"))
(define ideskrc
  (local-file "etc/idesk/ideskrc"))
(define idesk-icon-lnk
  (local-file "etc/idesk/DICOMStore.lnk"))
(define nftables-config
  (local-file "etc/misc/nftables.conf"))
(define user-home
  (home-environment
    (services
     (cons* (service home-xdg-configuration-files-service-type
                     `())
            (service home-files-service-type
                     `((".fluxbox/init" ,fluxbox-init)
                       (".fluxbox/keys" ,fluxbox-keys)
                       (".fluxbox/startup" ,fluxbox-startup)
                       (".idesktop/DICOMStore.lnk" ,idesk-icon-lnk)
                       (".conkyrc" ,conkyrc)
                       (".ideskrc" ,ideskrc))) %base-home-services))))

(define systoleos-configuration
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (firmware (list linux-firmware iucode-tool amd-microcode))

    (host-name "systole")
    (timezone "Europe/Oslo")

    ;; Use the UEFI variant of GRUB with the EFI System
    ;; Partition mounted on /boot/efi.
    (bootloader (bootloader-configuration
                  (bootloader grub-efi-bootloader)
                  (targets '("/boot/efi"))))

    ;; Assume the target root file system is labelled "root",
    ;; and the EFI System Partition has UUID 1234-ABCD.
    (file-systems (append (list (file-system
                                  (device (file-system-label "root"))
                                  (mount-point "/")
                                  (type "ext4"))
                                (file-system
                                  (device (uuid "1234-ABCD"
                                                'fat))
                                  (mount-point "/boot/efi")
                                  (type "vfat"))) %base-file-systems))

    ;; The `brainlabmirror` account must be initialised with `passwd` command
    (users (append (list (user-account
                           (name "brainlabmirror")
                           (comment "BrainLab")
                           (password "")
                           (group "users")
                           (supplementary-groups (list "dicom" "netdev"
                                                       "audio" "video")))
                         (user-account
                           (name "admin")
                           (comment "Admin")
                           (group "users")
                           (supplementary-groups (list "wheel" "netdev"
                                                       "audio" "video"))))
                   %base-user-accounts))

    (sudoers-file (plain-file "sudoers"
                              (string-append (plain-file-content
                                              %sudoers-specification)
                                             (format #f
                                              "~a ALL = NOPASSWD: ALL~%"
                                              "admin"))))

    (packages (append (list
                       ;; Slicer
                       slicer-5.8

                       ;; terminal emulator
                       xterm

                       ;; utils
                       git
                       curl
                       vim

                       ;; desktop environment
                       conky
                       feh
                       fluxbox
                       font-bitstream-vera
                       font-dejavu
                       idesk
                       oxygen-icons
                       thunar) %base-packages))

    (services
     (append (list

                   ;; LightDM display manager
                   ;; Configuration documentation: https://guix.gnu.org/manual/en/html_node/X-Window.html
                   (service lightdm-service-type
                            (lightdm-configuration (allow-empty-passwords? #t)
                                                   (debug? #t)
                                                   (xdmcp? #t)
                                                   (vnc-server? #f)
                                                   (greeters (list (lightdm-gtk-greeter-configuration
                                                                    (allow-debugging?
                                                                     #t))))
                                                   (seats (list (lightdm-seat-configuration
                                                                 (name "*")
                                                                 (autologin-user
                                                                  "brainlabmirror")
                                                                 (user-session
                                                                  ;; "xfce.desktop"
                                                                  "fluxbox"))))))

                   ;; Services for xfce desktop environment
                   ;; (service xfce-desktop-service-type)
                   
                   ;; nftables service
                   (service nftables-service-type
                            (nftables-configuration (ruleset (local-file
                                                              "etc/nftables.conf"))))

                   (service guix-home-service-type
                            `(("brainlabmirror" ,user-home)))

                   (set-xorg-configuration
                    (xorg-configuration (keyboard-layout (keyboard-layout
                                                          "altgr-intl")))
                    lightdm-service-type)

                   ;; Use Dicomd service defined in guix-systole
                   (service dicomd-service-type)

                   ;; Include the channel file so that it can be used during installation
                   (extra-special-file "/etc/guix/channels.scm"
                                       serialise-channels)

                   )

             (modify-services %desktop-services
               (delete gdm-service-type)
               (guix-service-type config =>
                                  (guix-configuration (inherit config)
                                                      (guix (guix-for-channels
                                                             %channels))
                                                      (authorized-keys (cons*
                                                                        %signing-key
                                                                        %default-authorized-guix-keys))
                                                      (substitute-urls `(,@%default-substitute-urls
                                                                         "https://substitutes.nonguix.org"))
                                                      (channels %channels))))))

    ;; Add the 'net.ifnames' argument to prevent network interfaces
    ;; from having really long names.  This can cause an issue with
    ;; wpa_supplicant when you try to connect to a wifi network.
    (kernel-arguments '("quiet" "modprobe.blacklist=radeon" "net.ifnames=0"))))

systoleos-configuration
