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

(define-module (systole packages)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:replace (%patch-path search-patch search-patches)
  #:export (systole-patches))

(define %systole-root-directory
  ;; This is like %distro-root-directory from (gnu packages), with adjusted
  ;; paths.
  (letrec-syntax ((dirname* (syntax-rules ()
                              ((_ file)
                               (dirname file))
                              ((_ file head tail ...)
                               (dirname (dirname* file tail ...)))))
                  (try      (syntax-rules ()
                              ((_ (file things ...) rest ...)
                               (match (search-path %load-path file)
                                 (#f
                                  (try rest ...))
                                 (absolute
                                  (dirname* absolute things ...))))
                              ((_)
                               #f))))
    (try ("systole/packages/slicer.scm" systole/ packages/)
         ("systole/services/dicomd-service.scm" systole/)
         ("systole/licenses.scm" systole/)
         )))

;; Define custom patch directory
(define systole-patches
   (string-append %systole-root-directory "/systole/packages/patches"))

(define %patch-path
  ;; Prepend each systole patch subdirectory to whatever (gnu packages)
  ;; already provides.  We capture the original parameter's value via
  ;; module-ref so we don't accidentally use our own replacement.
  (make-parameter (append
                   (list systole-patches
                         (string-append systole-patches "/ctk")
                         (string-append systole-patches "/qrestapi")
                         (string-append systole-patches "/slicer")
                         (string-append systole-patches "/slicer-openigtlink")
                         (string-append systole-patches "/slicerexecutionmodel"))
                   ((module-ref (resolve-module '(gnu packages)) '%patch-path)))))

;; Define search-patch functio
(define (search-patch file-name)
  "Search the patch FILE-NAME. Raise an error if not found."
  (or (search-path (%patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found!!!") file-name))))

;; Define search-patches macro
(define-syntax-rule (search-patches file-name ...)
  "Return the list of absolute file names corresponding to each FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))
