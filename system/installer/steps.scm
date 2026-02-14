;;; GNU Systole
;;; Modified from the original in the GNU Guix project:
;;;   https://codeberg.org/guix/guix
;;;
;;; Copyright © 2018, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
;;;
;;; This file is part of GNU Systole
;;;
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

(define-module (installer steps)
  #:use-module (guix records)
  #:use-module (guix build utils)
  #:use-module (guix i18n)
  #:use-module (guix read-print)
  #:use-module (guix utils)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer record)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs io ports)
  #:export (systole-format-configuration))

(define (systole-format-configuration steps results)
  "Return the list resulting from the application of the procedure defined in
CONFIGURATION-FORMATTER field of <installer-step> on the associated result
found in RESULTS."
  (let* ((configuration
          (append-map
           (lambda (step)
             (let* ((step-id (installer-step-id step))
                    (conf-formatter
                     (installer-step-configuration-formatter step))
                    (result-step (result-step results step-id)))
               (if (and result-step conf-formatter)
                   (conf-formatter result-step)
                   '())))
           steps))
         ;; Check for all deployment files
         (deploy-key-file "/etc/systole-ssh-deploy-key.pub")
         (deploy-key
          (and (file-exists? deploy-key-file)
               (string-trim-right
                (call-with-input-file deploy-key-file
                  get-string-all))))
         (signing-key-file "/etc/systole-signing-key.pub")
         (signing-key
          (and (file-exists? signing-key-file)
               (string-trim-right
                (call-with-input-file signing-key-file
                  get-string-all))))
         (host-key-private-file "/etc/systole-host-key")
         (host-key-public-file "/etc/systole-host-key.pub")
         (has-host-key? (and (file-exists? host-key-private-file)
                             (file-exists? host-key-public-file)))
         (has-channels? (file-exists? "/etc/guix/channels.scm"))
         ;; Helper to detect SSH key type from file content
         (detect-ssh-key-type
          (lambda (key-file)
            (if (file-exists? key-file)
                (let ((first-line (call-with-input-file key-file
                                    (lambda (port) (get-line port)))))
                  (cond
                   ((string-contains first-line "RSA") "rsa")
                   ((string-contains first-line "DSA") "dsa")
                   ((string-contains first-line "ECDSA") "ecdsa")
                   ((string-contains first-line "ED25519") "ed25519")
                   (else "ed25519")))
                "ed25519")))
         ;; Helper to check if services expression contains SSH service
         (ssh-service-exists?
          (lambda (services-expr)
            (let ((expr-str (format #f "~a" services-expr)))
              (string-contains expr-str "openssh-service-type"))))
         ;; If deploy key exists AND user selected SSH, inject authorized-keys
         (final-configuration
          (if deploy-key
              (map (lambda (field)
                     (match field
                       (('services services-expr)
                        ;; Only modify if SSH service was selected by user
                        (if (ssh-service-exists? services-expr)
                            ;; Replace bare SSH service with configured one
                            `(services
                              ,(let replace-ssh ((expr services-expr))
                                 (match expr
                                   ;; Match: (append (list (service openssh-service-type) ...) rest)
                                   (('append ('list services-list ...) rest ...)
                                    `(append (list
                                              ,@(map (lambda (svc)
                                                       (match svc
                                                         (('service 'openssh-service-type)
                                                          `(service openssh-service-type
                                                                    (openssh-configuration
                                                                     (permit-root-login 'prohibit-password)
                                                                     (authorized-keys
                                                                      `(("root" ,(plain-file "deploy-key.pub"
                                                                                             ,deploy-key)))))))
                                                         (_ svc)))
                                                     services-list))
                                             ,@rest))
                                   ;; Fallback: original structure
                                   (_ services-expr))))
                            ;; SSH not selected, don't inject anything
                            field))
                       (_ field)))
                   configuration)
              ;; No deploy key, leave configuration unchanged
              configuration))
         (modules `(,(vertical-space 1)
                    ,(comment (G_ "\
;; Indicate which modules to import to access the variables
;; used in this configuration.\n"))
                    (use-modules (gnu)
                                 (gnu services base)
                                 (systole transformations)
                                 (nongnu packages linux)
                                 (nongnu system linux-initrd)
                                 (guix channels))
                    (use-service-modules cups desktop networking ssh xorg)))
         ;; Embed channels directly from installer
         (channels-spec
          (if has-channels?
              (let ((channels-content
                     (call-with-input-file "/etc/guix/channels.scm" read)))
                `(,(vertical-space 1)
                  ,(comment (G_ "\
;; Channel specifications for reproducible system updates.
;; These channels are embedded from the installer and used by 'guix pull'
;; and 'guix system reconfigure'.\n"))
                  (define %system-channels
                    ,channels-content)))
              '()))
         ;; Combine guix-service modifications (channels + signing-key)
         (final-config-with-guix-mods
          (if (or has-channels? signing-key)
              (map (lambda (field)
                     (match field
                       (('services services-expr)
                        `(services
                          (modify-services ,services-expr
                                           (guix-service-type config =>
                                                              (guix-configuration
                                                               (inherit config)
                                                               ,@(if has-channels?
                                                                     `((channels %system-channels))
                                                                     '())
                                                               ,@(if signing-key
                                                                     `((authorized-keys
                                                                        (cons (plain-file "systole-signing-key.pub"
                                                                                          ,signing-key)
                                                                              (guix-configuration-authorized-keys config))))
                                                                     '()))))))
                       (_ field)))
                   final-configuration)
              final-configuration))
         ;; Add host key installation via activation-service
         (final-config-with-host-key
          (if has-host-key?
              (let ((key-type (detect-ssh-key-type host-key-private-file)))
                (map (lambda (field)
                       (match field
                         (('services services-expr)
                          `(services
                            (cons (simple-service 'install-systole-host-keys
                                                  activation-service-type
                                                  (with-imported-modules '((guix build utils))
                                                                         #~(begin
                                                                             (use-modules (guix build utils))
                                                                             (let ((target-dir "/etc/ssh")
                                                                                   (key-type ,key-type))
                                                                               (when (file-exists? "/etc/systole-host-key")
                                                                                 (mkdir-p target-dir)
                                                                                 (let ((target-private (string-append target-dir "/ssh_host_" key-type "_key"))
                                                                                       (target-public (string-append target-dir "/ssh_host_" key-type "_key.pub")))
                                                                                   (copy-file "/etc/systole-host-key" target-private)
                                                                                   (chmod target-private #o600)
                                                                                   (chown target-private 0 0)
                                                                                   (copy-file "/etc/systole-host-key.pub" target-public)
                                                                                   (chmod target-public #o644)
                                                                                   (chown target-public 0 0)))))))
                                  ,services-expr)))
                         (_ field)))
                     final-config-with-guix-mods))
              final-config-with-guix-mods)))
    `(,@modules
      ,@channels-spec
      ,(vertical-space 1)

      ,(comment (G_ "\
;; Optional: Add Systole branding to GRUB bootloader.
;; Uncomment the following lines and add (systole packages grub-themes) to use-modules:
;; In bootloader-configuration, add:
;;   (theme (grub-theme
;;           (image (file-append systole-grub-theme
;;                  \"/share/grub/themes/systole/systole.png\"))))
"))

      ((compose (systole-transformation-guix #:guix-source? #t)
                ;; FIXME: 'microcode-initrd' results in unbootable live system.
                (systole-transformation-linux #:initrd base-initrd))
       (operating-system ,@final-config-with-host-key)))))

;;; Local Variables:
;;; eval: (put 'with-server-socket 'scheme-indent-function 0)
;;; End:
