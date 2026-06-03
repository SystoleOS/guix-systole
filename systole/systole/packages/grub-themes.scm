;;
;; Copyright @ 2026 Oslo University Hospital
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

(define-module (systole packages grub-themes)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:))

;; Resolve assets relative to THIS module's own on-disk location, not the
;; repo root.  We locate the module's .scm file on %load-path and take its
;; directory; the `assets/' directory ships alongside the module in BOTH
;; layouts -- a git checkout (systole/systole/packages/grub-themes.scm) and
;; an installed channel (share/guile/site/3.0/systole/packages/grub-themes.scm).
;;
;; Why not `(current-filename)': it returns #f when the module is loaded
;; from its compiled .go (which is exactly how `guix pull' / Cuirass load an
;; installed channel), so a current-filename-based path throws at load time
;; and aborts channel evaluation.  `search-path %load-path' is valid from
;; compiled code -- it is the same idiom `%distro-root-directory' (in
;; (gnu packages)) and `%systole-root-directory' (in (systole packages)) use.
;;
;; The previous 4x`dirname' walked to the repo root, which only exists in a
;; checkout; under a channel it resolved to share/guile/site (which has no
;; assets/), throwing canonicalize-path and aborting the ENTIRE channel
;; evaluation -- so a Cuirass build hub pointed at guix-systole did no work.
(define %assets
  ;; `search-path' may return a path relative to a `-L' load-path entry in a
  ;; checkout (e.g. "systole/systole/packages/..."); canonicalize so the
  ;; resulting `local-file' is absolute in both the checkout and the
  ;; (already-absolute store path) installed-channel layouts.
  (string-append
   (canonicalize-path
    (dirname (search-path %load-path "systole/packages/grub-themes.scm")))
   "/assets"))

(define-public systole-grub-theme
  (package
    (name "systole-grub-theme")
    (version "1.0.0")
    (source (local-file (string-append %assets "/grub-theme")
                        #:recursive? #t))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "share/grub/themes/systole"))))
    (home-page "https://github.com/systoleos/guix-systole")
    (synopsis "Systole GRUB bootloader theme")
    (description "A GRUB theme providing Systole branding for the bootloader.")
    (license license:gpl3+)))
