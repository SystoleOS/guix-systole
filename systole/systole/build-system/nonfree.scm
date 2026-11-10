;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation, either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (systole build-system nonfree)
  #:use-module (guix build-system)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:export (nonsubstitutable-trivial-build-system))

;;; Commentary:
;;;
;;; Packages whose licence forbids redistribution (the OpenHaptics SDK and
;;; the Touch driver EULAs, and anything whose output contains them) must be
;;; marked non-substitutable, so that no build farm or `guix publish' instance
;;; offers them as substitutes.  The usual way is `#:substitutable? #f' in the
;;; package arguments, but `trivial-build-system' does not accept that keyword
;;; (its lowering procedure rejects unknown keys).
;;;
;;; `nonsubstitutable-trivial-build-system' is `trivial-build-system' with the
;;; resulting derivation marked non-substitutable.  Its arguments are exactly
;;; those of `trivial-build-system'.  Cross-compilation is not supported: the
;;; packages that use it repackage prebuilt x86_64 binaries.
;;;
;;; Code:

(define* (nonsubstitutable-trivial-build name inputs
                                         #:key
                                         outputs guile
                                         system builder (modules '())
                                         search-paths allowed-references)
  "Like the trivial build system's build procedure, but return a derivation
that is never substituted."
  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f))
                      (builder -> (if (pair? builder)
                                      (sexp->gexp builder)
                                      builder)))
    (gexp->derivation name (with-build-variables inputs outputs builder)
                      #:system system
                      #:target #f
                      #:graft? #f
                      #:modules modules
                      #:allowed-references allowed-references
                      #:guile-for-build guile
                      #:substitutable? #f)))

(define (lower* name . args)
  (let ((base (apply (build-system-lower trivial-build-system) name args)))
    (when (bag-target base)
      (error "nonsubstitutable-trivial-build-system: cross-compilation is \
not supported" name))
    (bag
      (inherit base)
      (build nonsubstitutable-trivial-build))))

(define nonsubstitutable-trivial-build-system
  (build-system
    (name 'nonsubstitutable-trivial)
    (description
     "Trivial build system whose derivations are never substituted, for
packages whose licence forbids redistribution")
    (lower lower*)))

;;; nonfree.scm ends here
