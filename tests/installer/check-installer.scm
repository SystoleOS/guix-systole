;;; GNU Systole
;;; Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
;;;
;;; This file is part of GNU Systole.
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

;;; Commentary:
;;;
;;; Load every installer and OS module and verify the entry points the
;;; ISO build depends on.  Run in script mode so failures propagate:
;;;
;;;   guix shell guile-newt guile-parted -- \
;;;     guix repl -L system -L systole -- tests/installer/check-installer.scm
;;;
;;; (gnu installer newt ...) needs the (newt) and (parted) Guile
;;; bindings at load time, hence the guix shell wrapper.
;;;
;;; Code:

(use-modules (ice-9 format))

(define failures 0)

(define (fail! fmt . args)
  (set! failures (+ failures 1))
  (format #t "  ✗ ~a~%" (apply format #f fmt args)))

(define %modules
  '((installer steps)
    (installer final)
    (installer installer)
    (installer newt systole-welcome)
    (installer newt systole-kernel)
    (installer newt systole-final)
    (os install)
    (os auto-install)))

;; Entry points consumed by scripts/build-installer-with-deploy.sh and
;; the ISO expression -- renaming these breaks the installer build.
(define %required-exports
  '(((installer installer) systole-installer-program)
    ((os install)
     systole-os-installation
     systole-os-installation-with-deploy-key)))

(format #t "Loading installer modules...~%")

(define interfaces
  (map (lambda (mod)
         (catch #t
           (lambda ()
             (let ((iface (resolve-interface mod)))
               (format #t "  ✓ ~s~%" mod)
               (cons mod iface)))
           (lambda (key . args)
             (fail! "module ~s failed to load: ~s ~s" mod key args)
             (cons mod #f))))
       %modules))

(format #t "~%Checking installer entry points...~%")

(for-each
 (lambda (spec)
   (let* ((mod (car spec))
          (syms (cdr spec))
          (iface (assoc-ref interfaces mod)))
     (if iface
         (for-each
          (lambda (sym)
            (if (module-variable iface sym)
                (format #t "  ✓ ~a (~s)~%" sym mod)
                (fail! "~a is no longer exported from ~s" sym mod)))
          syms)
         (fail! "cannot check exports; ~s did not load" mod))))
 %required-exports)

(format #t "~%~a~%"
        (if (zero? failures)
            "All installer checks passed."
            (format #f "~a installer check(s) FAILED." failures)))

(exit (if (zero? failures) 0 1))
