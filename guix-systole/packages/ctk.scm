(define-module (guix-systole packages ctk)
  #:use-module (gnu packages qt)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages))

(define-public ctkapplauncher-slicer
  (package
    (name "ctkapplauncher-slicer")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/commontk/AppLauncher/archive/8759e03985738b8a8f3eb74ab516ba4e8ef29988.tar.gz")
       (sha256
        (base32 "1lrrcg9s39n357z2dhfhv8ff99biivdnwwxaggwvnpv9knppaz83"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags (list "-DBUILD_TESTING=OFF"
                               "-DCTKAppLauncher_QT_VERSION=5"
                               "-DCTKAppLauncher_INSTALL_LauncherLibrary=ON"
                               ;; "-DCTK_INSTALL_LIB_DIR=lib64"
                               )))
    (inputs (list qtbase-5))
    (home-page "http://www.commontk.org/")
    (synopsis
     "Simple and small program allowing to set the environment of any executable.")
    (description
     "Simple and small program allowing to set the environment of any executable.")
    (license license:asl2.0)))
