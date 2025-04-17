(define-module (guix-systole licenses)
  #:export (slul)
  #:export (plus-license))

;; Guix does not export the license record constructor.
(define license (@@ (guix licenses) license))

(define slul
  (license "Simple Library Usage License"
           "https://svn.code.sf.net/p/teem/code/teem/trunk/LICENSE.txt"
           "A simple permissive license for library usage."))

(define plus-license
  (license "Plus license"
           "https://github.com/PlusToolkit/PlusLib/blob/master/License.txt"
           #f))
