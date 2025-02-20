(define-module (guix-systole artwork)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:export (%systole-artwork-repository))

(define %systole-artwork-repository
  (let ((commit "ee47d119a5700f1bf281ae107285ada9cd44199f"))
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/systoleos/guix-systole-artwork.git")
             (commit commit)))
      (file-name (string-append "guix-systole-artwork-" (string-take commit 7)
                                "-checkout"))
      (sha256
       (base32
        "08xzbhifi0sw3ymjfibpdd6vyd7h6i89gh0z19yib44rd93wya21")))))
