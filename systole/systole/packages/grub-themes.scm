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

(define-public systole-grub-theme
  (package
    (name "systole-grub-theme")
    (version "1.0.0")
    ;; The theme assets ship INSIDE the channel, next to this module
    ;; (systole/packages/assets/grub-theme) -- never move them outside
    ;; systole/, because only systole/ is the channel root when the
    ;; channel is consumed from the store.
    ;;
    ;; A relative name makes `local-file' resolve it via
    ;; `current-source-directory': the module's file name is captured at
    ;; expansion time and searched for on %load-path at run time (then
    ;; canonicalized).  That is exactly the mechanism the previous
    ;; hand-rolled `search-path %load-path' + `canonicalize-path' code
    ;; used, and unlike `(current-filename)' it stays valid when the
    ;; module is loaded from its compiled .go -- which is how `guix pull'
    ;; and Cuirass load an installed channel.  It therefore works in both
    ;; layouts: a git checkout (with an absolute -L load path) and an
    ;; installed channel under share/guile/site/3.0/.
    (source (local-file "assets/grub-theme"
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
