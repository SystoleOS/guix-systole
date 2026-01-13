;; Original file from https://github.com/kennyballou/dotfiles
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;; Copyright © 2023 Kenny Ballou <kb@devnulllabs.io>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Generate a bootable image (e.g. for USB sticks, etc.) with:
;; $ guix system image -t iso9660 installer.scm

;; Original file from https://github.com/kennyballou/dotfiles

(define-module (os install)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (gnu system keyboard)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages vim)
  #:use-module (gnu services base)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu system linux-initrd)
  #:autoload   (gnu packages gtk) (guile-cairo guile-rsvg)
  #:autoload   (gnu packages guile-xyz) (guile-newt)
  #:use-module (nongnu packages linux)
  #:use-module (guix)
  #:use-module (guix channels)
  #:use-module (nonguix transformations)
  #:use-module (systole)
  #:use-module (installer installer)
  #:use-module (srfi srfi-1)
  #:export (systole-os-installation))

(define %systole-installer (systole-installer-program))

(define systole-os-installation

  ((compose (nonguix-transformation-guix #:guix-source? #t)
            ;; FIXME: ‘microcode-initrd’ results in unbootable live system.
            (nonguix-transformation-linux #:initrd base-initrd))

   (operating-system
    (inherit installation-os)
    (kernel linux)
    (firmware (list linux-firmware))
    (keyboard-layout (keyboard-layout "us" #:options '("ctrl:nocaps")))
    (kernel-arguments '("quiet" "net.ifnames=0"))

    (bootloader
     (let ((base (operating-system-bootloader installation-os)))
       (bootloader-configuration
        (inherit base)
        ;; target/device is ignored for ISO builds, any string is fine
        (targets "/dev/null")
        (theme
         (grub-theme
          (resolution '(1280 . 1024))
          (color-normal '((fg . light-gray) (bg . black)))
          (color-highlight '((fg . black ) (bg . yellow)))
          (image (local-file (string-append %systole-root "assets/grub-theme/systole.png"))))))))

    (label "GNU Systole installation")

    (services
     (append
      (list
       (simple-service 'channel-file etc-service-type
                       (list `("guix/channels.scm" ,(local-file (string-append %systole-root "/system/os/channels.scm.txt"))))))
      (modify-services (operating-system-user-services installation-os)
                       (guix-service-type config => (guix-configuration
                                                     (inherit config)
                                                     (substitute-urls
                                                      (append (list "https://substitutes.nonguix.org")
                                                              %default-substitute-urls))
                                                     (authorized-keys
                                                      (append (list (plain-file "non-guix.pub"
                                                                                "(public-key
                                                                                  (ecc
                                                                                    (curve Ed25519)
                                                                                    (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                                                              %default-authorized-guix-keys))))
                       (kmscon-service-type cfg =>
                                            (kmscon-configuration
                                             (inherit cfg)
                                             (login-program %systole-installer)))
                       )))


    (packages
     (append (list git curl vim lvm2 gptfdisk xfsprogs e2fsprogs guile-gcrypt guile-newt)
             (operating-system-packages installation-os))))))




systole-os-installation
