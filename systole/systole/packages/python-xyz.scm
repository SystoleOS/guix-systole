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
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
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
    (propagated-inputs (list python-3.12))))

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

;; SimpleITK as the prebuilt PyPI wheel, as Slicer 5.12 ships it.
;;
;; Since upstream f87c97746 (2025-12), Slicer no longer builds SimpleITK from
;; its github.com/Slicer/SimpleITK fork against Slicer's own ITK: the
;; SuperBuild pip-installs the stock PyPI wheel (SuperBuild/
;; External_python-SimpleITK.cmake, --require-hashes), and sitkUtils moves
;; images through numpy instead of the fork's MRMLIDImageIO.  The hashes below
;; are the ones that file pins for v5.12.4.
;;
;; The wheel is used instead of Guix's from-source simpleitk on purpose: that
;; build links shared libITK*.so from insight-toolkit, which inside Slicer
;; would be resolved by soname to the already-loaded ITK fork (a different
;; version and module set).  The wheel links ITK statically with hidden
;; visibility and needs only glibc and libstdc++, i.e. exactly the pairing
;; upstream ships and tests.  The cp311-abi3 build imports on Python 3.12.
(define %simpleitk-wheel-version "2.5.5")

(define %simpleitk-wheels
  ;; system -> (url-path platform-tag nix-base32-hash)
  `(("x86_64-linux"
     "9f/68/ed67a355a62848ee04bb4f01e89d3be871052c2c3ae6d5fc0fb2f6010979/"
     "manylinux2014_x86_64.manylinux_2_17_x86_64"
     "0cw0jsq20pq4knggxzysf7kfpy0n63bz2zn853n5n3g05cwmxkla")
    ("aarch64-linux"
     "14/e5/6a77865140278c86a392a85bd2317a1097bd891f54955ebe5db623e4b37c/"
     "manylinux2014_aarch64.manylinux_2_17_aarch64"
     "0lzw9apsrvgzjibzh7vg9x8qvvnl00zds1xi45bipw3vgl2l4zw4")))

(define (simpleitk-wheel-origin system)
  (let ((entry (or (assoc-ref %simpleitk-wheels system)
                   (assoc-ref %simpleitk-wheels "x86_64-linux"))))
    (origin
      (method url-fetch)
      (uri (string-append "https://files.pythonhosted.org/packages/"
                          (car entry)
                          "simpleitk-" %simpleitk-wheel-version
                          "-cp311-abi3-" (cadr entry) ".whl"))
      (sha256 (base32 (caddr entry))))))

(define-public python-simpleitk-bin
  (package
    (name "python-simpleitk-bin")
    (version %simpleitk-wheel-version)
    ;; The wheel is selected in the (thunked) arguments rather than in
    ;; 'source', which is evaluated once at module load and would ignore
    ;; --system/--target.
    (source #f)
    (build-system copy-build-system)
    (arguments
     (list
      #:strip-binaries? #f              ;272 MB; keep upstream bytes
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (invoke "unzip" "-q"
                      #$(simpleitk-wheel-origin
                         (if (%current-target-system)
                             (gnu-triplet->nix-system (%current-target-system))
                             (%current-system))))))
          (replace 'install
            (lambda _
              (let ((site (string-append
                           #$output "/lib/python"
                           #$(version-major+minor (package-version python-3.12))
                           "/site-packages")))
                (mkdir-p site)
                (for-each (lambda (dir)
                            (copy-recursively dir (string-append site "/" dir)))
                          (append '("SimpleITK")
                                  (find-files "." "\\.dist-info$"
                                              #:directories? #t
                                              #:fail-on-error? #f))))))
          (add-after 'install 'set-rpath
            (lambda _
              (for-each
               (lambda (so)
                 (invoke "patchelf" "--set-rpath"
                         (string-append #$glibc "/lib:"
                                        #$gcc:lib "/lib")
                         so))
               (find-files #$output "\\.so$")))))))
    (native-inputs (list patchelf unzip))
    (supported-systems (map car %simpleitk-wheels))
    (home-page "https://simpleitk.org")
    (synopsis "Simplified interface to ITK (prebuilt PyPI wheel, as in Slicer)")
    (description
     "SimpleITK is a simplified layer over the Insight Toolkit (ITK) for
image analysis.  This package installs the unmodified upstream PyPI wheel,
with ITK statically linked into the extension module, matching the SimpleITK
that 3D Slicer 5.12 bundles.  Use it for Slicer's embedded Python; for a
from-source build against Guix's ITK, use @code{simpleitk}.")
    (license license:asl2.0)))
