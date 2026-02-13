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
         ;; Check for deploy key
         (deploy-key-file "/etc/systole-deploy-key.pub")
         (deploy-key
          (and (file-exists? deploy-key-file)
               (string-trim-right
                (call-with-input-file deploy-key-file
                  get-string-all))))
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
                                                              (permit-root-login 'without-password)
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
         ;; Load channels from /etc if provided by installer
         (channels-spec
          (if (file-exists? "/etc/systole-channels.scm")
              `(,(vertical-space 1)
                ,(comment (G_ "\
;; Channel specifications for reproducible system updates.
;; These channels are loaded from the installer and used by 'guix pull'
;; and 'guix system reconfigure'.\n"))
                (define %system-channels
                  (load "/etc/systole-channels.scm")))
              '()))
         ;; Modify final configuration to include channels in guix-configuration if available
         (final-config-with-channels
          (if (file-exists? "/etc/systole-channels.scm")
              ;; Add channels to guix-configuration
              (map (lambda (field)
                     (match field
                       (('services services-expr)
                        ;; Wrap services expression with modify-services to inject channels
                        `(services
                          (modify-services ,services-expr
                            (guix-service-type config =>
                              (guix-configuration
                                (inherit config)
                                (channels %system-channels))))))
                       (_ field)))
                   final-configuration)
              ;; No channels file, use configuration as-is
              final-configuration)))
    `(,@modules
      ,@channels-spec
      ,(vertical-space 1)

      ((compose (systole-transformation-guix #:guix-source? #t)
                ;; FIXME: 'microcode-initrd' results in unbootable live system.
                (systole-transformation-linux #:initrd base-initrd))
       (operating-system ,@final-config-with-channels)))))

;;; Local Variables:
;;; eval: (put 'with-server-socket 'scheme-indent-function 0)
;;; End:
