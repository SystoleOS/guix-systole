(define-module (guix-systole packages teem)
  #:use-module (guix-systole packages vtk)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module ((guix-systole licenses)
                #:prefix license:))

(define-public teem-slicer
  (package
    (name "teem-slicer")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/Slicer/teem/archive/e4746083c0e1dc0c137124c41eca5d23adf73bfa.tar.gz")
       (sha256
        (base32 "0y8wwzkflj6v5nx0v8cgzryqlxii0px3mcgb3bff1nhyr5zf9yp1"))
       (patches (list (local-file
                       "patches/0001-BUG-packages-teem-fix-missing-teem-config-cmake.patch")))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DBUILD_SHARED_LIBS:BOOL=ON"
                               "-DBUILD_TESTING:BOOL=OFF")))
    ;; https://discourse.slicer.org/t/unknown-cmake-command-vtkmacrokitpythonwrap-in-snapshot-release-for-vtkteem/13838
    (inputs (list vtkaddon-slicer))
    (home-page "https://github.com/Slicer/teem/")
    (synopsis
     "A group of libraries for representing, processing and visualizing scientific raster data.")
    (description
     "Teem is a coordinated group of libraries for representing, processing, and visualizing scientific raster data. Teem includes command-line tools that permit the library functions to be quickly applied to files and streams, without having to write any code. The most important and useful libraries in Teem are: ")
    (license (list license:slul license:lgpl2.1))))
