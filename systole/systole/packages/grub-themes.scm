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

(define %here
  (dirname (dirname (dirname (dirname (current-filename))))))

(define-public systole-grub-theme
  (package
    (name "systole-grub-theme")
    (version "1.0.0")
    (source (local-file (string-append %here "/assets/grub-theme")
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
