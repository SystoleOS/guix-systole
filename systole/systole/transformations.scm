;;; GNU Systole
;;; Modified from the original in the GNU Guix project:
;;;   https://gitlab.com/nonguix/nonguix
;;;
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
;;;
;;; This file is part of GNU Systole.

;;; GNU Systole is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Systole is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Systole.  If not, see <http://www.gnu.org/licenses/>.

(define-module (systole transformations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix channels)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (gnu system)
  #:use-module (nongnu system linux-initrd)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services ssh)
  #:use-module (nongnu services nvidia)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages nvidia)
  #:export (systole-transformation-guix
            systole-transformation-linux
            systole-transformation-nvidia
            systole-transformation-deploy)
  #:re-export (replace-mesa))

(define* (systole-transformation-guix #:key (substitutes? #t)
                                      (channel? #t)
                                      (guix-source? #f)
                                      (channels #f))
  "Return a procedure that transforms an operating system, setting up Nonguix
signing key for the Guix daemon.

Additionally, SUBSTITUTES? (default: #t) sets up the substitute server,
CHANNEL? (default: #t) adds Nonguix channel specification, GUIX-SOURCE?
(default: #f) builds Nonguix channel into the default Guix, and CHANNELS
(default: #f) allows specifying custom channel list (overrides default behavior).

When CHANNELS is provided, it is used directly as the channel list. Otherwise,
the default behavior is to append nonguix and guix-systole channels.

FIXME: GUIX-SOURCE? is disabled by default due to performance issue."

  (define %nonguix-signing-key
    (plain-file "nonguix.pub" "
(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

  (define %nonguix-channel
    (channel
     (name 'nonguix)
     (url "https://gitlab.com/nonguix/nonguix")
     ;; Enable signature verification:
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))

  (define %guix-systole-channel
    (channel
     (name 'guix-systole)
     (branch "main")
     (url "https://github.com/systoleos/guix-systole")))

  (lambda (os)
    (operating-system
      (inherit os)
      (services
       (modify-services (operating-system-user-services os)
         (guix-service-type
          config => (guix-configuration
                     (inherit config)
                     (channels
                      (cond
                       ;; If explicit channels provided, use them directly
                       (channels channels)
                       ;; Otherwise, use default behavior: add nonguix + guix-systole
                       (channel?
                        (let ((configured-channels
                               (guix-configuration-channels config)))
                          (append (list %nonguix-channel %guix-systole-channel)
                                  (or configured-channels %default-channels))))
                       ;; channel? is #f, use configured channels as-is
                       (else (guix-configuration-channels config))))
                     (guix
                      ;; Don't build guix-for-channels when custom channels provided
                      ;; (the channels themselves determine which Guix to use)
                      (if (and guix-source? (not channels))
                          (guix-for-channels
                           (append (list %nonguix-channel %guix-systole-channel)
                                   (or (guix-configuration-channels config)
                                       %default-channels)))
                          (guix-configuration-guix config)))
                     (authorized-keys
                      (cons %nonguix-signing-key
                            (guix-configuration-authorized-keys config)))
                     (substitute-urls
                      (delete-duplicates
                       `(,@(guix-configuration-substitute-urls config)
                         ,@(if substitutes?
                               '("https://substitutes.nonguix.org")
                               '())))))))))))

(define* (systole-transformation-linux #:key (linux linux)
                                       (firmware (list linux-firmware))
                                       (initrd microcode-initrd))
  "Return a procedure that transforms an operating system, setting up
LINUX (default: linux) kernel, with FIRMWARE (default: (list linux-firmware))
and INITRD (default: microcode-initrd)."
  (lambda (os)
    (operating-system
      (inherit os)
      (kernel linux)
      (firmware firmware)
      (initrd initrd))))

(define* (systole-transformation-nvidia #:key (driver nvda)
                                        (kernel-mode-setting? #t)
                                        (open-source-kernel-module? #f))
  "Return a procedure that transforms an operating system, setting up
DRIVER (default: nvda) for NVIDIA graphics card.

KERNEL-MODE-SETTING? (default: #t) is required for Wayland and rootless Xorg
support.

OPEN-SOURCE-KERNEL-MODULE? (default: #f) only supports Turing and later
architectures and is expected to work with 'linux-lts'.

For application setup, use 'replace-mesa'.

TODO: Xorg configuration."
  (define %presets
    `((,nvda . ,(service nvidia-service-type
                  (nvidia-configuration
                   (driver nvda)
                   (firmware nvidia-firmware)
                   (module
                    (if open-source-kernel-module?
                        nvidia-module-open
                        nvidia-module)))))
      (,nvdb . ,(service nvidia-service-type
                  (nvidia-configuration
                   (driver nvdb)
                   (firmware nvidia-firmware-beta)
                   (module
                    (if open-source-kernel-module?
                        nvidia-module-open-beta
                        nvidia-module-beta)))))))
  (lambda (os)
    (operating-system
      (inherit os)
      (kernel-arguments
       (delete-duplicates
        (cons* "modprobe.blacklist=nouveau"
               (string-append
                "nvidia_drm.modeset=" (if kernel-mode-setting? "1" "0"))
               (remove
                (cut string-prefix? "nvidia_drm.modeset=" <>)
                (operating-system-user-kernel-arguments os)))))
      (packages
       (replace-mesa (operating-system-packages os) #:driver driver))
      (services
       (replace-mesa
        `(,(or (assoc-ref %presets driver)
               (leave
                (G_ "no NVIDIA service configuration available for '~a'~%")
                (package-name driver)))
          ,@(operating-system-user-services os))
        #:driver driver)))))

(define* (systole-transformation-deploy
          #:key
          (ssh-deploy-key #f)
          (deploy-key #f)         ; DEPRECATED: backward compatibility
          (channels-file #f)
          (signing-key #f)
          (host-key-private #f)
          (host-key-public #f))
  "Return a procedure that transforms an operating system, optionally adding
SSH access, channel specifications, signing keys, and host keys for remote
deployment via 'guix deploy'.

SSH-DEPLOY-KEY (default: #f) should be either #f (disabled) or a string
containing an SSH public key in standard format (e.g., 'ssh-ed25519 AAAA...').
DEPLOY-KEY is deprecated; use SSH-DEPLOY-KEY instead.

CHANNELS-FILE (default: #f) should be either #f (disabled) or a path to a
channels.scm file containing channel specifications to embed in the installed
system configuration.

SIGNING-KEY (default: #f) should be either #f (disabled) or a string containing
a Guix signing key in S-expression format: (public-key (ecc ...)).

HOST-KEY-PRIVATE and HOST-KEY-PUBLIC (default: #f) should be paths to SSH host
key files. When provided, the installed system will use these keys instead of
generating new ones, enabling predictable host key fingerprints.

When SSH-DEPLOY-KEY is provided, this transformation:
- Adds openssh-service-type to enable SSH daemon in the installer
- Configures the SSH key as authorized for root user
- Enables key-based authentication for remote deployment

When CHANNELS-FILE is provided, the installer will:
- Copy the channels file to /etc for access during installation
- Embed channel specifications in the generated system configuration

When SIGNING-KEY is provided, the installed system will:
- Authorize the signing key for guix daemon operations
- Enable immediate guix deploy without manual authorization

When HOST-KEY-PRIVATE and HOST-KEY-PUBLIC are provided, the installed system will:
- Use the provided host keys instead of generating new ones
- Enable predictable SSH host fingerprints across reinstalls"

  (lambda (os)
    ;; Backward compatibility: prioritize ssh-deploy-key over deploy-key
    (when (and deploy-key (not ssh-deploy-key))
      (format (current-error-port)
              "WARNING: 'deploy-key' parameter is deprecated. ~
Use 'ssh-deploy-key' instead.~%"))

    (let* ((ssh-key (or ssh-deploy-key deploy-key))
           (has-ssh-deploy-key? (and ssh-key (not (string=? ssh-key ""))))
           (has-channels? (and channels-file (file-exists? channels-file)))
           (has-signing-key? (and signing-key (not (string=? signing-key ""))))
           (has-host-key? (and host-key-private
                               (file-exists? host-key-private)
                               host-key-public
                               (file-exists? host-key-public)))
           (etc-files (append
                       (if has-ssh-deploy-key?
                           (list `("systole-ssh-deploy-key.pub"
                                   ,(plain-file "systole-ssh-deploy-key.pub" ssh-key)))
                           '())
                       ;; Note: channels are now set via guix-configuration in
                       ;; systole-transformation-guix, not via etc-service
                       (if has-signing-key?
                           (list `("systole-signing-key.pub"
                                   ,(plain-file "systole-signing-key.pub" signing-key)))
                           '())
                       (if has-host-key?
                           (list `("systole-host-key"
                                   ,(local-file host-key-private))
                                 `("systole-host-key.pub"
                                   ,(local-file host-key-public)))
                           '())))
           (host-key-activation
            (if has-host-key?
                (list (simple-service 'systole-host-key-permissions
                                      activation-service-type
                                      (with-imported-modules '((guix build utils))
                                        #~(begin
                                            (use-modules (guix build utils))
                                            (when (file-exists? "/etc/systole-host-key")
                                              (chmod "/etc/systole-host-key" #o600)
                                              (chown "/etc/systole-host-key" 0 0))
                                            (when (file-exists? "/etc/systole-host-key.pub")
                                              (chmod "/etc/systole-host-key.pub" #o644)
                                              (chown "/etc/systole-host-key.pub" 0 0))))))
                '())))
      (if (and (not has-ssh-deploy-key?)
               (not has-channels?)
               (not has-signing-key?)
               (not has-host-key?))
          ;; No configuration provided - return OS unchanged
          os
          ;; Configure SSH and/or channels and/or signing-key and/or host-key
          (operating-system
            (inherit os)
            (services
             (let ((base-services
                    (if has-ssh-deploy-key?
                        ;; Remove any existing SSH service, then add our configured one
                        (cons (service openssh-service-type
                                       (openssh-configuration
                                        (permit-root-login 'prohibit-password)
                                        (password-authentication? #f)
                                        (authorized-keys
                                         `(("root" ,(plain-file "deploy-key.pub" ssh-key))))))
                              (remove (lambda (service)
                                        (eq? (service-kind service)
                                             openssh-service-type))
                                      (operating-system-user-services os)))
                        ;; No deploy key, keep services as-is
                        (operating-system-user-services os))))
               (append (if (null? etc-files)
                           '()
                           (list (simple-service 'systole-deployment-files
                                                 etc-service-type
                                                 etc-files)))
                       host-key-activation
                       base-services))))))))

