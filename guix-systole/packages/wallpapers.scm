(define-module (guix-systole packages wallpapers)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:export (systole-wallpapers))

(define systole-wallpapers
  (package
    (name "systole-wallpapers")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/systoleos/guix-systole-artwork.git")
             (commit "d8203496e0a54e51ba74824196d9adebaee14197")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1whc64lzyvss8s4vjyspmhzl72v0v1w2q09qx7wkxfvnv6spbxhs"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules `((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((source (assoc-ref %build-inputs "source"))
                 (target (string-append #$output "/share/backgrounds/systole")))
            (mkdir-p target)
            (copy-recursively source target)))))
    (home-page "https://github.com/systoleos/guix-systole-artwork")
    (synopsis "Systole Wallpapers")
    (description "A collection of wallpapers for the Systole project.")
    (license license:cc0)))
