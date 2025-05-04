(define-module (guix-systole licenses)
               #:use-module (guix licenses)
  #:export (slul))

;; Guix does not export the license record constructor.
(define license (@@ (guix licenses) license))

(define slul
  (license "Simple Library Usage License"
           "https://svn.code.sf.net/p/teem/code/teem/trunk/LICENSE.txt"
           "A simple permissive license for library usage."))
