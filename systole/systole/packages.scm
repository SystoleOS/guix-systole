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
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:replace (%patch-path search-patch search-patches)
  #:export (systole-patches slicer-patch %add-missing-iostream-includes))

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
                         (string-append systole-patches "/igsio")
                         (string-append systole-patches "/qrestapi")
                         (string-append systole-patches "/slicer-5.8")
                         (string-append systole-patches "/slicer-5.10")
                         (string-append systole-patches "/slicer-igt")
                         (string-append systole-patches "/slicer-openigtlink")
                         (string-append systole-patches "/slicerexecutionmodel")
                         (string-append systole-patches "/openigtlinkio")
                         (string-append systole-patches "/plusapp")
                         (string-append systole-patches "/plustoolkit")
                         (string-append systole-patches "/vtk")
                         (string-append systole-patches "/vtk-9.5")
                         ;; Distro-specific ROS 2 patch dirs come FIRST so
                         ;; they override entries in ros2/common.
                         (string-append systole-patches "/ros2/jazzy")
                         (string-append systole-patches "/ros2/common")
                         (string-append systole-patches "/ros2")
                         ;; SOFA Framework and SlicerSOFA extension.
                         (string-append systole-patches "/sofa")
                         (string-append systole-patches "/slicer-sofa"))
                   ((module-ref (resolve-module '(gnu packages)) '%patch-path)))))

;; Version-scoped patch helper for Slicer packages.
;; Using local-file bypasses the global %patch-path so that 5.8 and 5.10
;; patches with identical relative names resolve independently.
(define (slicer-patch version name)
  "Return the patch file NAME under patches/slicer-VERSION/.
NAME may include a subdirectory prefix, e.g. \"terminologies/0001-ENH-…\"."
  (local-file (string-append systole-patches "/slicer-" version "/" name)
              (basename name)))

;; Define search-patch functio
(define (search-patch file-name)
  "Search the patch FILE-NAME. Raise an error if not found."
  (or (search-path (%patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found!!!") file-name))))

;; Define search-patches macro
(define-syntax-rule (search-patches file-name ...)
  "Return the list of absolute file names corresponding to each FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))

(define %add-missing-iostream-includes
  ;; Build-side phase procedure: (add-after 'unpack 'add-missing-iostream-includes
  ;; #$%add-missing-iostream-includes).  VTK 9.6 headers no longer include
  ;; <iostream> transitively, nor provide the global using-declarations for
  ;; cout/cerr/clog, so code written against older VTK fails with "'cerr' is
  ;; not a member of 'std'" or "'cerr' was not declared in this scope".  For
  ;; every C++ file using these streams: qualify bare uses with std:: and
  ;; prepend #include <iostream> if it is missing.  Files are read and written
  ;; as ISO-8859-1, which maps every byte to a character and back unchanged, so
  ;; sources that are not valid UTF-8 (PlusLib has Latin-1 ones) survive intact.
  #~(lambda _
      (let ((get-string-all (@ (ice-9 textual-ports) get-string-all))
            (uses-stream (make-regexp
                          "(^|[^A-Za-z0-9_])w?c(out|err|log)[^A-Za-z0-9_]"))
            (bare "(^|[^:A-Za-z0-9_])(w?c(out|err|log))([^A-Za-z0-9_]|$)"))
        (with-fluids ((%default-port-encoding "ISO-8859-1"))
          (for-each
           (lambda (file)
             (when (regexp-exec uses-stream
                                (call-with-input-file file get-string-all))
               (substitute* file
                 ((bare all pre name kind post)
                  (string-append pre "std::" name post)))
               (let ((text (call-with-input-file file get-string-all)))
                 (unless (string-contains text "<iostream>")
                   (call-with-output-file file
                     (lambda (port)
                       (display "#include <iostream>\n" port)
                       (display text port)))))))
           (find-files "." "\\.(cxx|cpp|h|hxx|txx)$"))))))
