;;; GNU Systole
;;; Modified from the original at https://github.com/kennyballou/dotfiles
;;;
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;; Copyright © 2023 Kenny Ballou <kb@devnulllabs.io>
;;; Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
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


(define-module (os install)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services ssh)
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
  #:use-module (systole)
  #:use-module (systole transformations)
  #:use-module (installer installer)
  #:use-module (srfi srfi-1)
  #:export (systole-os-installation
            systole-os-installation-with-deploy-key))

(define %systole-installer (systole-installer-program))

(define systole-os-installation

  ((compose (systole-transformation-guix #:guix-source? #t)
            ;; FIXME: ‘microcode-initrd’ results in unbootable live system.
            (systole-transformation-linux #:initrd base-initrd))

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
     (modify-services (operating-system-user-services installation-os)
                      (kmscon-service-type cfg =>
                                           (kmscon-configuration
                                            (inherit cfg)
                                            (login-program %systole-installer)))))


    (packages
     (append (list git curl vim lvm2 gptfdisk xfsprogs e2fsprogs guile-gcrypt guile-newt)
             (operating-system-packages installation-os))))))

(define* (systole-os-installation-with-deploy-key
          #:key
          (ssh-deploy-key #f)
          (deploy-key #f)          ; DEPRECATED: backward compatibility
          (channels-file #f)
          (signing-key #f)
          (host-key-private #f)
          (host-key-public #f))
  "Return an operating-system for the Systole installer ISO.

When SSH-DEPLOY-KEY is provided (as a string containing an SSH public key),
the installer will have SSH access enabled for remote deployment via
'guix deploy'. The key is authorized for the root user AND automatically
injected into the installed system configuration. DEPLOY-KEY is deprecated;
use SSH-DEPLOY-KEY instead.

When CHANNELS-FILE is provided (as a path to a channels.scm file), the
installer will embed those channel specifications in the generated system
configuration, ensuring reproducible deployments.

When SIGNING-KEY is provided (as a string containing a Guix signing key),
the installed system will authorize this key for guix deploy operations,
enabling immediate remote deployment without manual authorization.

When HOST-KEY-PRIVATE and HOST-KEY-PUBLIC are provided (as paths to SSH
host key files), the installed system will use these keys instead of
generating new ones, enabling predictable host key fingerprints.

Examples:
  (systole-os-installation-with-deploy-key
    #:ssh-deploy-key \"ssh-ed25519 AAAAC3Nza... user@host\"
    #:channels-file \"channels-lock.scm\")

  (systole-os-installation-with-deploy-key
    #:ssh-deploy-key \"ssh-ed25519 AAAAC3Nza...\"
    #:channels-file \"channels-lock.scm\"
    #:signing-key \"(public-key (ecc ...))\"
    #:host-key-private \"ssh_host_ed25519_key\"
    #:host-key-public \"ssh_host_ed25519_key.pub\")"

  ((compose (systole-transformation-deploy
             #:ssh-deploy-key (or ssh-deploy-key deploy-key)
             #:channels-file channels-file
             #:signing-key signing-key
             #:host-key-private host-key-private
             #:host-key-public host-key-public)
            (systole-transformation-guix
             #:guix-source? #t
             #:channels (and channels-file
                             (file-exists? channels-file)
                             ;; Evaluate in the (guix channels) module context
                             ;; so that 'channel', 'make-channel-introduction', etc. are bound
                             (eval (call-with-input-file channels-file read)
                                   (resolve-module '(guix channels)))))
            (systole-transformation-linux #:initrd base-initrd))

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
        (targets "/dev/null")
        (theme
         (grub-theme
          (resolution '(1280 . 1024))
          (color-normal '((fg . light-gray) (bg . black)))
          (color-highlight '((fg . black ) (bg . yellow)))
          (image (local-file (string-append %systole-root "assets/grub-theme/systole.png"))))))))

    (label "GNU Systole installation")

    (services
     ;; Just modify kmscon for custom installer
     ;; Note: deploy key and channels are handled by systole-transformation-deploy
     (modify-services (operating-system-user-services installation-os)
                      (kmscon-service-type cfg =>
                                           (kmscon-configuration
                                            (inherit cfg)
                                            (login-program %systole-installer)))))

    (packages
     (append (list git curl vim lvm2 gptfdisk xfsprogs e2fsprogs guile-gcrypt guile-newt)
             (operating-system-packages installation-os))))))

systole-os-installation
