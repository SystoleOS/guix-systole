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
    (string-append root-dir "/guix-systole/packages/patches")))

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

;; Define a modified %patch-path that includes custom patch directory
(define %patch-path
  (make-parameter (cons systole-patches %original-patch-path)))

;; Define search-patch function
(define (search-patch file-name)
  "Search the patch FILE-NAME. Raise an error if not found."
  (or (search-path (%patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found!!!") file-name))))

;; Define search-patches macro
(define-syntax-rule (search-patches file-name ...)
  "Return the list of absolute file names corresponding to each FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))
