(define-module (guix-systole packages)
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
    (try ("guix-systole/packages/slicer.scm" guix-systole/ packages/)
         ("guix-systole/services/dicomd-service.scm" guix-systole/)
         ("guix-systole/licenses.scm" guix-systole/)
         )))

;; Define custom patch directory
(define systole-patches
   (string-append %systole-root-directory "/guix-systole/packages/patches"))

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
