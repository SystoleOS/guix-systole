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
;;; Non-free redistribution guard, run in script mode:
;;;
;;;   guix repl -L systole -- tests/packages/check-nonfree.scm
;;;
;;; Every exported package that is non-free -- or that has a non-free
;;; package anywhere among its inputs, recursively -- must carry
;;; `#:substitutable? #f'.  That flag is what keeps a package out of the
;;; build hub: Cuirass creates no job for a non-substitutable derivation
;;; (gnu/ci.scm package->job), so it is never built, published by `guix
;;; publish' or relayed to the public substitute mirror.  Proprietary EULAs
;;; such as the 3D Systems OpenHaptics ones forbid copying and
;;; distribution; serving their binaries -- or binaries of packages that
;;; link or bundle them -- would breach them (licence audit 2026-11-10).
;;;
;;; "Non-substitutable" = `#:substitutable? #f' in the arguments, or a build
;;; system named nonsubstitutable-* (which sets it on the derivation).
;;;
;;; Non-free licences recognised: (systole licenses) EULAs (names ending in
;;; "EULA") and nonguix's `nonfree' / `undistributable' (names starting
;;; with "Nonfree").  Walk: inputs, native-inputs and propagated-inputs,
;;; recursively (memoized); the implicit build-system inputs (toolchain)
;;; are free.
;;;
;;; Code:

(use-modules (guix packages)
             (guix build-system)
             (guix licenses)
             (ice-9 ftw)
             (ice-9 format)
             (ice-9 match)
             (srfi srfi-1))

(define script-dir
  (dirname (canonicalize-path (car (command-line)))))

(define channel-root
  (canonicalize-path (string-append script-dir "/../../systole")))

(define (file->module-name relative)
  (map string->symbol
       (string-split (string-drop-right relative 4) #\/)))

(define modules
  (let ((names '()))
    (ftw channel-root
         (lambda (filename statinfo flag)
           (when (and (eq? flag 'regular)
                      (string-suffix? ".scm" filename))
             (set! names
                   (cons (file->module-name
                          (string-drop filename
                                       (+ 1 (string-length channel-root))))
                         names)))
           #t))
    names))

(define channel-packages
  ;; Exported packages of every loadable module, deduplicated.
  (delete-duplicates
   (append-map
    (lambda (mod)
      (catch #t
        (lambda ()
          (let ((iface (resolve-interface mod)) (acc '()))
            (module-for-each
             (lambda (sym var)
               (let ((val (and (variable-bound? var) (variable-ref var))))
                 (when (package? val)
                   (set! acc (cons val acc)))))
             iface)
            acc))
        ;; Module load failures are check-packages.scm's job.
        (const '())))
    modules)
   eq?))

(define (nonfree-license? l)
  (and (license? l)
       (let ((name (license-name l)))
         (or (string-prefix? "Nonfree" name)
             (string-suffix? "EULA" name)))))

(define (package-licenses p)
  (match (package-license p)
    ((? list? ls) ls)
    (l (list l))))

(define (nonfree-package? p)
  (any nonfree-license? (package-licenses p)))

(define (input-packages p)
  (filter-map (match-lambda
                ((_ (? package? q) . _) q)
                (_ #f))
              (package-direct-inputs p)))

;; Memoized: package -> the non-free package found in its closure, or #f.
(define %culprit (make-hash-table))

(define (nonfree-culprit p)
  (match (hashq-get-handle %culprit p)
    ((_ . v) v)
    (#f
     ;; Mark first: guards against a (pathological) input cycle.
     (hashq-set! %culprit p #f)
     (let ((v (if (nonfree-package? p)
                  p
                  (any nonfree-culprit
                       (catch #t
                         (lambda () (input-packages p))
                         (const '()))))))
       (hashq-set! %culprit p v)
       v))))

(define (substitutable-false? p)
  ;; Either `#:substitutable? #f' literally present in the package
  ;; arguments, or a build system that marks its derivations
  ;; non-substitutable itself (systole's nonsubstitutable-trivial-build-system,
  ;; needed because trivial-build-system rejects the keyword).  Checked
  ;; statically: computing every derivation would be far heavier.
  (let ((args (catch #t (lambda () (package-arguments p)) (const '())))
        (bs   (symbol->string
               (build-system-name (package-build-system p)))))
    (or (string-prefix? "nonsubstitutable" bs)
        (and (list? args)
             (match (memq #:substitutable? args)
               ((_ value . _) (eq? value #f))
               (_ #f))))))

(format #t "Checking ~a exported packages for non-free closures...~%"
        (length channel-packages))

(define violations
  (filter-map
   (lambda (p)
     (let ((culprit (nonfree-culprit p)))
       (and culprit
            (begin
              (format #t "  ~a ~a~a~%"
                      (if (substitutable-false? p) "✓" "✗")
                      (package-full-name p)
                      (if (eq? culprit p)
                          " (non-free licence)"
                          (format #f " (closure holds ~a)"
                                  (package-full-name culprit))))
              (and (not (substitutable-false? p)) p)))))
   (sort channel-packages
         (lambda (a b) (string<? (package-full-name a)
                                 (package-full-name b))))))

(format #t "~%~a~%"
        (if (null? violations)
            "All non-free packages (and their dependents) are #:substitutable? #f."
            (format #f "~a package(s) need #:substitutable? #f -- they must \
never be built, published or relayed by the build hub."
                    (length violations))))

(exit (if (null? violations) 0 1))
