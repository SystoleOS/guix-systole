;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix-systole installer steps)
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
  (let ((configuration
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
        (modules `(,(vertical-space 1)
                   ,(comment (G_ "\
;; Indicate which modules to import to access the variables
;; used in this configuration.\n"))
                   ,@(if (target-hurd?)
                         '((use-modules (gnu) (gnu system hurd))
                           (use-package-modules hurd ssh))
                         '((use-modules (gnu))
                           ;; TODO: We should separate Libre linux from Linux here
                           (use-modules (nongnu packages linux))
                           (use-modules (nongnu system linux-initrd))))
                   (use-service-modules cups desktop networking ssh xorg))))
    `(,@modules
      ,(vertical-space 1)
      (operating-system ,@configuration))))

;;; Local Variables:
;;; eval: (put 'with-server-socket 'scheme-indent-function 0)
;;; End:
