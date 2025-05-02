(define-module (guix-systole systoleos systoleos)
               #:use-module (guix)
               #:use-module (guix channels)
               ; #:use-module (guix packages)
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
               #:use-module (gnu services)
               #:use-module (gnu services base)
               #:use-module (gnu services desktop)
               #:use-module (gnu services xorg)
               #:use-module (gnu services lightdm)
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
               #:use-module (nongnu packages linux)
               #:use-module (nongnu system linux-initrd)
               #:use-module (guix-systole services dicomd-service)
               #:use-module (guix-systole packages slicer)
               )

;; https://substitutes.nonguix.org/signing-key.pub
(define %signing-key
  (plain-file "nonguix.pub" "\
(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

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
           (name 'guix-systole)
           (url "https://github.com/SystoleOS/guix-systole")
           ; (branch "dev")
           (branch "SCRUM-126-Define-package-for-SystoleOS-including-Slicer")
           )
         %default-channels))

;; Serialise the contents of %channels into a file at build time
(let ((serialise-channels
        (computed-file "channels.scm"
                       #~(let ((out (string-append %output "/channels.scm")))
                           (with-output-to-file out
                                                (lambda ()
                                                  (write #$%channels)
                                                  (newline)
                                                  )
                                                )
                       out)
        #:local-build? #t))))

(define systoleos-configuration
  (operating-system
    (inherit installation-os)
    ; (kernel linux-lts)
    (kernel linux)
    ; (initrd microcode-initrd)
    (firmware (list linux-firmware iucode-tool amd-microcode))

    (host-name "systole")
    (timezone "Europe/Oslo")

    ;; Use the UEFI variant of GRUB with the EFI System
    ;; Partition mounted on /boot/efi.
    (bootloader (bootloader-configuration
                  (bootloader grub-efi-bootloader)
                  (targets '("/boot/efi"))))

    ;; Assume the target root file system is labelled "my-root",
    ;; and the EFI System Partition has UUID 1234-ABCD.
    (file-systems (append
                  (list (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        (file-system
                          (device (uuid "1234-ABCD" 'fat))
                          (mount-point "/boot/efi")
                          (type "vfat")))
                  %base-file-systems))

    (users (cons (user-account
                   (name "systole")
                   (comment "SystoleOS user")
                   (password #f)
                   (group "users")
                   (supplementary-groups (list "wheel" "netdev" "audio" "video"))
                   )
                 %base-user-accounts))

    ; (guix-configuration
    ;   (channels %channels)
    ;   (authorized-keys (cons* %signing-key %default-authorized-guix-keys))
    ;   (substitute-urls (append %default-substitute-urls
    ;                            (list "https://substitutes.nonguix.org")))
    ;   )

    (packages (append (list
                        ;; Slicer
                        slicer-5.8

                        ;; terminal emulator
                        xterm

                        ;; utils
                        git
                        curl
                        vim
                        stow
                        )
                      %base-packages))

    (services
                (append (list

                    ;; LightDM display manager
                    ;; Configuration documentation: https://guix.gnu.org/manual/en/html_node/X-Window.html
                    (service lightdm-service-type
                             (lightdm-configuration
                               (allow-empty-passwords? #t)
                               (debug? #t)
                               (xdmcp? #t)
                               (vnc-server? #f)
                               (greeters (list (lightdm-gtk-greeter-configuration
                                                 (allow-debugging? #t)
                                                 )))
                               (seats (list (lightdm-seat-configuration
                                              (name "*")
                                              (user-session "xfce.desktop")
                                              )))
                               )
                             )

                    ;; Services for xfce desktop environment
                    (service xfce-desktop-service-type)
                    ; (modify-services (list (xfce-desktop-service-type))
                    ;                  (xorg-server-service-type
                    ;                    (const %desktop-services)  ;; Remove `gdm` from `xfce-desktop-service-type`
                    ;                    )
                    ;                  )
                    ; (modify-services (list (service xfce-desktop-service-type))
                    ;                  (delete xorg-server-service-type)
                    ;                  )

                    (set-xorg-configuration
                      (xorg-configuration
                        (keyboard-layout (keyboard-layout "altgr-intl"))
                        )
                      lightdm-service-type
                      )

                    ;; Use Dicomd service defined in guix-systole
                    (service dicomd-service-type)

                ;; Include the channel file so that it can be used during installation
                ; (simple-service 'channel-file etc-service-type
                ;                 (list `("channels.scm" ,(local-file "base-channels.scm")))
                ;                 )

                ;; Include the channel file so that it can be used during installation
                (extra-special-file
                  "/etc/guix/channels.scm"
                  ; (local-file "channels.scm")
                  ; (local-file "guix-systole/systoleos/channels.scm")
                  serialise-channels
                  )

                )

                ; Use nonguix channel and include the nonguix substitutes server
                ; (modify-services (operating-system-user-services installation-os)
                ;                  (guix-service-type
                ;                    config => (guix-configuration
                ;                                (inherit config)
                ;                                (guix (guix-for-channels %channels))
                ;                                (authorized-keys
                ;                                  (cons* %signing-key
                ;                                        %default-authorized-guix-keys)
                ;                                  )
                ;                                (substitute-urls
                ;                                  `(,@%default-substitute-urls
                ;                                     "https://substitutes.nonguix.org")
                ;                                  )
                ;                                (channels %channels)
                ;                                )
                ;                    )
                ;                  )

                ; %desktop-services
                (modify-services %desktop-services
                                 (delete gdm-service-type)
                                 (guix-service-type
                                   config => (guix-configuration
                                               (inherit config)
                                               (guix (guix-for-channels %channels))
                                               (authorized-keys
                                                 (cons* %signing-key
                                                       %default-authorized-guix-keys)
                                                 )
                                               (substitute-urls
                                                 `(,@%default-substitute-urls
                                                    "https://substitutes.nonguix.org")
                                                 )
                                               (channels %channels)
                                               )
                                   )
                                  ; (set-xorg-configuration
                                  ;   (xorg-configuration
                                  ;     (keyboard-layout (keyboard-layout "altgr-intl"))
                                  ;     )
                                  ;   lightdm-service-type
                                  ;   )
                                 )
                ; (modify-services %desktop-services
                ;                  (delete gdm-service-type)
                ;                  (xorg-service-type config =>
                ;                                     (xorg-configuration
                ;                                       (inherit config)
                ;                                       (keyboard-layout (keyboard-layout "altgr-intl"))
                ;                                       )
                ;                                     ; lightdm-service-type
                ;                                     )
                ;                  )
                )
              )

    ;; Add the 'net.ifnames' argument to prevent network interfaces
    ;; from having really long names.  This can cause an issue with
    ;; wpa_supplicant when you try to connect to a wifi network.
    (kernel-arguments '("quiet" "modprobe.blacklist=radeon" "net.ifnames=0"))
    )
  )

systoleos-configuration
