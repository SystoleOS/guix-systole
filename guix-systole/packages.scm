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

(define-module (guix-systole packages)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:replace (%patch-path search-patch search-patches)
  #:export (systole-patches))

;; Define custom patch directory
(define systole-patches
  (let* ((this-file (current-filename))
         (this-dir (dirname this-file))
         (root-dir (dirname this-dir)))
    (string-append root-dir
                   "/guix-systole/packages/patches")))

;; Get the original %patch-path value and extend it
(define %original-patch-path
  (map (lambda (directory)
         (if (string=? directory
                       (string-append (dirname (dirname (search-path
                                                         %load-path
                                                         "guix/packages.scm")))
                                      "/gnu"))
             (string-append directory "/packages/patches") directory))
       %load-path))

(define %patch-path
  (make-parameter (append
                   (list systole-patches
                         (string-append systole-patches "/ctk")
                         (string-append systole-patches "/qrestapi")
                         (string-append systole-patches "/slicer")
                         (string-append systole-patches "/slicer-openigtlink")
                         )
                   %original-patch-path)))

;; Define search-patch functio
(define (search-patch file-name)
  "Search the patch FILE-NAME. Raise an error if not found."
  (or (search-path (%patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found!!!") file-name))))

;; Define search-patches macro
(define-syntax-rule (search-patches file-name ...)
  "Return the list of absolute file names corresponding to each FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))
