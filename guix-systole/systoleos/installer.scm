;; Uses SystemCrafters' implementation as a base
;; https://github.com/SystemCrafters/guix-installer/blob/master/guix/installer.scm
;; Builds upon examples from the Guix manual
;; https://guix.gnu.org/manual/en/html_node/Using-the-Configuration-System.html

(define-module (guix-systole systoleos installer)
               #:use-module (guix)
               #:use-module (guix channels)
               #:use-module (gnu packages version-control)
               #:use-module (gnu packages vim)
               #:use-module (gnu packages curl)
               #:use-module (gnu packages linux)
               #:use-module (gnu packages mtools)
               #:use-module (gnu packages package-management)
               #:use-module (gnu packages terminals)
               #:use-module (gnu services)
               #:use-module (gnu services base)
               #:use-module (gnu system)
               #:use-module (gnu system install)
               #:use-module (nongnu packages linux)
               #:use-module (nongnu system linux-initrd)
               #:use-module (guix-systole services dicom-service)
               #:use-module (guix-systole packages slicer)
               #:export (systoleos-installer)
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
         %default-channels))

(define systoleos-installer
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))

    (host-name "systole")
    (timezone "Europe/Oslo")

    (users (cons (user-account
                   (name "Systole")
                   (comment "SystoleOS user")
                   (group "users")
                   (supplementary-groups (list "wheel" "netdev" "audio" "video"))
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
                        )))

    (services (if (target-x86-64?)
                (append (list (service mate-desktop-service-type)
                              (service xfce-desktop-service-type)
                              (set-xorg-configuration
                                (xorg-configuration
                                  (keyboard-layout keyboard-layout)
                                )
                                sddm-service-type
                              ))
                        %desktop-services
                ))
              (append
                ;; Include the channel file so that it can be used during installation
                (simple-service 'channel-file etc-service-type
                                (list `("channels.scm" ,(local-file "base-channels.scm")))
                                )
                %desktop-services
                )
              ;; Use nonguix channel and include the nonguix substitute server
              (modify-services (operating-system-user-services installation-os)
                               (guix-system-type
                                 config => (guix-configuration
                                             (inherit config)
                                             (guix (guix-for-channels %channels))
                                             (authorized-keys
                                               (cons* %signing-key
                                                      %default-authorized-guix-keys)
                                               )
                                             (substitute-urls
                                               `(,@%default-substitute-urls
                                                  "https:substitutes.nonguix.org"
                                                  )
                                             )
                                             (channels %channels)
                                 )
                               ))
              (append dicom-service-type %desktop-services)
              )

    ;; Add the 'net.ifnames' argument to prevent network interfaces
    ;; from having really long names.  This can cause an issue with
    ;; wpa_supplicant when you try to connect to a wifi network.
    (kernel-arguments '("quiet" "modprobe.blacklist=radeon" "net.ifnames=0"))
    )
  )
