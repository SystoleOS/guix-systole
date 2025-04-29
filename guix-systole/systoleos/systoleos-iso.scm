;; Uses SystemCrafters' implementation as a base
;; https://github.com/SystemCrafters/guix-installer/blob/master/guix/installer.scm
;; Builds upon examples from the Guix manual
;; https://guix.gnu.org/manual/en/html_node/Using-the-Configuration-System.html

(define-module (guix-systole systoleos systoleos-installer)
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
               #:use-module (gnu system)
               #:use-module (gnu system image)
               #:use-module (gnu system install)
               #:use-module (gnu system shadow)
               #:use-module (gnu system file-systems)
               #:use-module (gnu bootloader)
               #:use-module (gnu bootloader grub)
               #:use-module (gnu image)
               #:use-module (nongnu packages linux)
               #:use-module (nongnu system linux-initrd)
               #:use-module (guix-systole services dicomd-service)
               #:use-module (guix-systole packages slicer)
               #:export (systoleos-iso
                         ; systoleos-installer-iso
                         ; systoleos-iso-installer
                         )
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
           )
         %default-channels))

(define systoleos-configuration
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))

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

    (users (cons* (user-account
                   (name "Systole")
                   (comment "SystoleOS user")
                   ; (password #f)
                   (group "users")
                   (supplementary-groups '("wheel" "netdev" "audio" "video"))
                   )
                 %base-user-accounts))

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
              (append
                ;; Services for xfce desktop environment
                (list (service mate-desktop-service-type)
                      (service xfce-desktop-service-type)
                      (set-xorg-configuration
                        (xorg-configuration
                          (keyboard-layout keyboard-layout)
                          )
                        sddm-service-type
                        )
                      )

                ;; Include the channel file so that it can be used during installation
                (simple-service 'channel-file etc-service-type
                                (list `("channels.scm" ,(local-file "base-channels.scm")))
                                )

                ;; Use nonguix channel and include the nonguix substitutes server
                (modify-services (operating-system-user-services installation-os)
                                 (guix-system-type
                                   config => (guix-configuration
                                               (inherit config)
                                               (guix (guix-for-channels %channels))
                                               (authorized-keys
                                                 (list %signing-key
                                                       %default-authorized-guix-keys)
                                                 )
                                               (substitute-urls
                                                 `(,@%default-substitute-urls
                                                    "https://substitutes.nonguix.org")
                                                 )
                                               (channels %channels)
                                               )
                                   )
                                 )

                ;; Use Dicomd service defined in guix-systole
                dicomd-service-type

                %desktop-services)
              )

    ;; Add the 'net.ifnames' argument to prevent network interfaces
    ;; from having really long names.  This can cause an issue with
    ;; wpa_supplicant when you try to connect to a wifi network.
    (kernel-arguments '("quiet" "modprobe.blacklist=radeon" "net.ifnames=0"))
    )
  )

; (define systoleos-iso-image
;   (image
;     (name 'systoleos-iso)
;     (format 'iso9660)
;     (operating-system systoleos-configuration)
;     (partitions
;       (list (partition
;               ; (size (* 2048 (expt 2 20)))   ;; 2GB
;               (file-system 'iso9660)
;               (flags '(boot))
;               ; (device (file-system-label "SYSTOLE_ISO"))
;               (label "SYSTOLE_ISO")
;               ))
;       )
;     )
;   )

; (define-public systoleos-iso-derivation
;                (image->derivation systoleos-iso-image)
;                )

(define systoleos-image
  (image
    (name "systoleos-iso")
    (format 'iso9660)
    (operating-system
      systoleos-configuration
      )
    (partitions
      (list (partition
              ; (size (* 2048 (expt 2 20)))   ;; 2GB
              ; (file-system "vfat")
              (flags '(boot))
              ; (device (file-system-label "SYSTOLE_ISO"))
              (label "SYSTOLE_ISO")
              ))
      )
    )
  )

; (define-public systoleos-iso-installer
;                (package
;                  (name "systoleos-iso-installer")
;                  (version "0.1")
;                  (source #f)
;                  (build-system trivial-build-system)
;                  ; (build-system image-derivation)
;                  (arguments
;                    `(
;                      ; #:system ,systoleos
;                      ; #:image-types (list image-iso9660)
;                      #:modules ((guix build utils)
;                                 (gnu system image)
;                                 (guix diagnostics)
;                                 (guix colors)
;                                 (guix memoization)
;                                 (guix profiling)
;                                 (guix i18n)
;                                 (guix discovery)
;                                 (guix modules)
;                                 (guix sets)
;                                 (guix combinators)
;                                 (guix gexp)
;                                 (guix store)
;                                 (guix utils)
;                                 (guix config)
;                                 (guix deprecation)
;                                 (guix serialization)
;                                 (guix monads)
;                                 (guix records)
;                                 (guix base16)
;                                 (guix base32)
;                                 (guix derivations)
;                                 (guix ui)
;                                 (guix packages)
;                                 (guix grafts)
;                                 (guix build-system)
;                                 (guix search-paths)
;                                 (guix profiles)
;                                 (guix self)
;                                 (gnu bootloader)
;                                 (gnu system file-systems)
;                                 (gnu system uuid)
;                                 (gnu bootloader grub)
;                                 (guix build union)
;                                 (gnu artwork)
;                                 (guix git-download)
;                                 (gnu build file-systems)
;                                 (guix build bournish)
;                                 (guix build syscalls)
;                                 (gnu system keyboard)
;                                 (gnu packages xorg)
;                                 (guix licenses)
;                                 (guix download)
;                                 (guix build-system copy)
;                                 (guix build-system gnu)
;                                 (guix build-system haskell)
;                                 (guix build-system meson)
;                                 (guix build-system glib-or-gtk)
;                                 (guix build glib-or-gtk-build-system)
;                                 (guix build gnu-build-system)
;                                 (guix build gremlin)
;                                 (guix elf)
;                                 (guix build-system perl)
;                                 (guix build-system python)
;                                 (gnu packages)
;                                 (guix describe)
;                                 (guix build-system pyproject)
;                                 (gnu packages aidc)
;                                 (gnu packages autotools)
;                                 (gnu packages perl)
;                                 (gnu packages base)
;                                 (gnu packages acl)
;                                 (gnu packages attr)
;                                 (gnu packages gettext)
;                                 (gnu packages bash)
;                                 (gnu packages bootstrap)
;                                 (guix platform)
;                                 (guix build-system trivial)
;                                 (gnu packages compression)
;                                 (guix build-system ant)
;                                 (guix build-system cmake)
;                                 (guix build-system go)
;                                 (gnu packages algebra)
;                                 (gnu packages bison)
;                                 (gnu packages m4)
;                                 (gnu packages flex)
;                                 (gnu packages man)
;                                 (guix build-system ruby)
;                                 (gnu packages curl)
;                                 (gnu packages check)
;                                 (gnu packages admin)
;                                 (guix build-system cargo)
;                                 (guix build-system emacs)
;                                 (guix build emacs-build-system)
;                                 (guix build emacs-utils)
;                                 (guix build-system qt)
;                                 (guix build qt-utils)
;                                 (gnu packages boost)
;                                 (gnu packages icu4c)
;                                 (gnu packages cpio)
;                                 (gnu packages java)
;                                 (guix hg-download)
;                                 (guix svn-download)
;                                 (guix build svn)
;                                 (guix build-system maven)
;                                 (gnu packages certs)
;                                 (gnu packages python)
;                                 (gnu packages crypto)
;                                 (gnu packages cpp)
;                                 (guix build-system scons)
;                                 (gnu packages assembly)
;                                 (gnu packages image)
;                                 (gnu packages build-tools)
;                                 (guix build-system guile)
;                                 )
;                      #:builder
;                      (begin
;                        (use-modules (guix build utils)
;                                     (gnu system image)
;                        )
;                        (let* (
;                          ; (image (image
;                          ;               (format 'iso9660)
;                          ;               (operating-system systoleos)
;                          ;               (partitions
;                          ;                 (list (partition
;                          ;                         (file-system 'iso9660)
;                          ;                         (flags '(boot))
;                          ;                         ))
;                          ;                 )
;                          ;               ))
;
;                          ; systoleos-iso-image
;
;                          ; (drv (image->derivation image))
;
;                          ; systoleos-iso-installer
;
;                          ; )
;                        ; (copy-file drv (string-append %output "/systoleos.iso"))
;                        ; (copy-file systoleos-iso-derivation (string-append %output "/systoleos.iso"))
;
;
;                        ; (drv (image->derivation systoleos-iso-image))
;                        ; (output (string-append %output "/systoleos.iso")))
;                        ;   (copy-file drv output)
;
;                        ; (out (assoc-ref %outputs "out"))
;                        ; (iso-file (image-file systoleos-iso-image "iso9660"))
;                        ; (target-file (string-append out "/systoleos.iso")))
;                        ;   (mkdir-p (dirname target-file))
;                        ;   (copy-file iso-file target-file)
;
;                        (image (image
;                                 (format 'iso9660)
;                                 (operating-system ,systoleos-configuration)
;                                 (partitions
;                                   (list (partition
;                                           (flags '(boot))
;                                           (file-system "iso9660")
;                                           ))
;                                   )
;                                 ))
;                        (drv (image->derivation image "systoleos"))
;                        (out (assoc-ref %outputs "out")))
;                        (copy-file drv (string-append out "/systoleos.iso"))
;                        #t)
;                      )
;                    ))
;                  ; (native-inputs (list
;                  ;                  systoleos-iso-image
;                  ;                  ))
;                  (home-page "https://SystoleOS/guix-systole")
;                  (synopsis "ISO image generator for SystoleOS")
;                  (description "This package generates an ISO live image of SystoleOS including 3D Slicer in gnu/store")
;                  (license license:gpl3)
;                  )
;                )

(define-public systoleos-iso
               (system-image
                 ; systoleos-configuration
                 ; 'iso9660
                 systoleos-image
                 )
               )

; (define-public systoleos-iso-installer
;                (operating-system-image
;                  systoleos-configuration
;                  (iso-image
;                    #:label      "systoleos-installer"
;                    #:graphical? #t
;                    )
;                  )
;                )

; (define-public systoleos-iso-installer
;                (system-image
;                  (image-type 'iso9660)
;                  (operating-system systoleos-configuration)
;                  )
;                )
