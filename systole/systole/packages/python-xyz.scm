;;
;; Copyright @ 2025 Oslo University Hospital
;;
;; This file is part of SystoleOS.
;;
;; SystoleOS is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later version.
;;
;; SystoleOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with SystoleOS. If not, see <https://www.gnu.org/licenses/>.
;;

(define-module (systole packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

;; python-3.12 is now exported by (gnu packages python) upstream.
;; We import it from there and use it for the wrapper and rewriter below.

;; Python 3.12 wrapper that provides the 'python' binary (symlink to python3),
;; suitable for use as the #:python argument in pyproject-build-system.
;; The version MUST match python-3.12 so that python-version (which extracts
;; the major.minor from the wrapper's store-path name) returns "3.12" — this
;; determines where packages are installed (lib/python3.12/site-packages).
(define-public python-3.12-wrapper
  (package
    (inherit python-sans-pip-wrapper)
    (version (package-version python-3.12))
    (propagated-inputs `(("python" ,python-3.12)))))

;; Rewriter that swaps the Python interpreter used to build Python packages
;; from the default (Python 3.11) to Python 3.12.  Applied narrowly — only
;; to packages we need at Slicer 5.10 runtime — to avoid rebuilding the
;; entire Python ecosystem (and pulling heavy chains like scipy→pythran→LLVM
;; or cryptography→Rust).
;;
;; Uses package-mapping so we can both rewrite Python inputs AND disable
;; tests recursively on every Python package in the closure.  Rewriting
;; breaks the bootstrap chain (python-pluggy → python-pytest), so tests
;; fall back to unittest, find zero tests, and exit 5 → build failure.
;; Disabling tests for rewritten Python packages avoids this without
;; affecting their default (non-rewritten) builds.
(define (python-package? pkg)
  (memq (build-system-name (package-build-system pkg))
        '(pyproject python)))

(define %with-python-3.12
  (package-mapping
   (lambda (pkg)
     (let ((name (package-name pkg)))
       (cond
        ((member name '("python-sans-pip-wrapper" "python-wrapper"))
         python-3.12-wrapper)
        ((member name '("python-sans-pip" "python"))
         python-3.12)
        ((python-package? pkg)
         (package
           (inherit pkg)
           (arguments
            (substitute-keyword-arguments (package-arguments pkg)
              ((#:tests? _ #f) #f)))))
        (else pkg))))
   (lambda (pkg)
     ;; Cut recursion only at the Python interpreter leaves themselves.
     (member (package-name pkg)
             '("python-sans-pip-wrapper" "python-wrapper"
               "python-sans-pip" "python")))
   #:deep? #t))

(define-public python-numpy-3.12
  (package
    (inherit (%with-python-3.12 python-numpy))
    (name "python-numpy-3.12")))
