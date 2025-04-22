(define-module (guix-systole packages plustoolkit)
               #:use-module ((guix-systole licenses)
                             #:prefix license:)
               #:use-module (guix packages)
               #:use-module (gnu packages)
               #:use-module (guix download)
               #:use-module (guix build-system cmake)
               #:use-module (gnu packages base)
               #:use-module (gnu packages qt)
               #:use-module (gnu packages gl)
               #:use-module (gnu packages xorg)
               #:use-module (gnu packages kde)
               #:use-module (gnu packages maths)
               #:use-module (gnu packages xiph)
               #:use-module (gnu packages geo)
               #:use-module (gnu packages serialization)
               #:use-module (gnu packages xml)
               #:use-module (gnu packages pdf)
               #:use-module (gnu packages image)
               #:use-module (gnu packages algebra)
               #:use-module (gnu packages mpi)
               #:use-module (gnu packages compression)
               #:use-module (gnu packages fontutils)

               ;; PlusBuild specific modules
               #:use-module (gnu packages text-utils)
               #:use-module (gnu packages version-control)
               #:use-module (gnu packages image-processing)
               ;; end PlusBuild specific modules

               #:use-module (guix-systole packages)
               #:use-module (guix-systole packages vtk)
               #:use-module (guix-systole packages itk)
               #:use-module (guix-systole packages igsio)
               #:use-module (guix-systole packages openigtlink)
               )

(define-public pluslib
               (package
                 (name "pluslib")
                 (version "2.8")
                 (source
                   (origin
                     (method url-fetch)
                     (uri
                       "https://github.com/PlusToolkit/PlusLib/archive/refs/heads/Plus-2.8.tar.gz")
                     (sha256
                       (base32
                         "0kp241w3r3bbdbkarwzyyz90qg1zy7wjbkbxbwjd660hsvh3qj0p"
                         )
                       )
                     (patches (search-patches
                                ; "PlusLib_find-vtk-slicer.patch"
                                "0016-COMP-packages-plustoolkit-fix-path-to-VTK-ITK.patch"
                                "0025-COMP-packages-plustoolkit-skip-static-library-check-for-openigtlinkio.patch"
                                ))
                     )
                   )
                 (build-system cmake-build-system)
                 (arguments
                   `(#:configure-flags (list
                                         "-DPLUS_OFFLINE_BUILD:BOOL=ON"
                                         ; "-DPLUS_USE_NVIDIA_DVP:BOOL=OFF"
                                         
                                         ; (string-append "-DOpenIGTLinkIO_DIR=" (assoc-ref %build-inputs "openigtlinkio-igsio") "/lib/cmake/OpenIGTLinkIO")
                                         ; "-DPLUS_USE_OpenIGTLink:BOOL=ON"
                                         ))
                   )
                 (inputs (list vtk-slicer
                               itk-slicer
                               qtbase-5
                               qtmultimedia-5
                               qttools-5
                               qtxmlpatterns
                               qtx11extras
                               qtdeclarative-5
                               qtwebengine-5
                               libglvnd
                               libxt

                               kdevelop
                               
                               glew
                               hdf5
                               libtheora
                               netcdf
                               proj
                               jsoncpp
                               libxml2
                               libharu
                               gl2ps
                               libpng
                               eigen
                               openmpi
                               expat
                               double-conversion
                               lz4
                               ; openjpeg
                               ijg-libjpeg
                               freetype

                               openigtlink
                               openigtlinkio-igsio
                               igsio
                               ))
                 (home-page "plustoolkit.github.io")
                 (synopsis "Software library for data acquisition, pre-processing, and calibration for navigated image-guided interventions.")
                 (description "Software library for data acquisition, pre-processing, and calibration for navigated image-guided interventions. See more information at www.plustoolkit.org.")
                 (license license:plus-license)
                )
               )

(define-public plusbuild
               (package
                 (name "plusbuild")
                 (version "2.8")
                 (source
                   (origin
                     (method url-fetch)
                     (uri
                       "https://github.com/PlusToolkit/PlusBuild/archive/refs/heads/Plus-2.8.tar.gz")
                     (sha256
                       (base32
                         "0pd3q1djxdkdfnalcpq2h5wy4v7h44j1i6yjfhs6f1i8b69sl3w0"))
                     ; (patch-flags '("--ignore-whitespace" "-F" "3"))
                     (patches (search-patches
                                "PlusBuild_test_OS_agnostic.patch"))))
                 (build-system cmake-build-system)
                 (arguments
                   `(
                     #:phases
                     (modify-phases %standard-phases
                                    (add-before 'configure 'fetch-deps
                                                   (lambda* (#:key source outputs inputs #:allow-other-keys)
                                                            ;; Prepare the Deps directory
                                                            ; (let* ((build-dir (string-append source "/build/Deps"))
                                                            (let* ((build-dir "build/Deps")
                                                                   (repos     (list
                                                                                (list "PlusLib"
                                                                                      "https://github.com/PlusToolkit/PlusLib.git"
                                                                                      "Plus-2.8")
                                                                                (list "PlusApp"
                                                                                      "https://github.com/PlusToolkit/PlusApp.git"
                                                                                      "Plus-2.8")
                                                                                (list "OpenIGTLink"
                                                                                      "https://github.com/openigtlink/OpenIGTLink.git"
                                                                                      "2081e418c48c02e920487e2284996c1e577c1024")
                                                                                (list "OpenIGTLinkIO"
                                                                                      "https://github.com/IGSIO/OpenIGTLinkIO.git"
                                                                                      "1a2eda5ddb795df8bb5bfbba589c9650095ba4cd")
                                                                                (list "OvrvisionPro"
                                                                                      "https://github.com/PlusToolkit/OvrvisionProCMake.git"
                                                                                      "761a54917e6070a8148dc373d640a2ffe191ac7d")
                                                                                (list "PlusLibData"
                                                                                      "https://github.com/PlusToolkit/PlusLibData.git"
                                                                                      "Plus-2.8")
                                                                                (list "PlusModelCatalog"
                                                                                      "https://github.com/PlusToolkit/PlusModelCatalog.git"
                                                                                      "651be1563b40a3552f18436f580d1c14d1b388f9")
                                                                                (list "Tesseract"
                                                                                      "https://github.com/PlusToolkit/leptonica.git"
                                                                                      "3130874380fcc2b0268d4f57863cbacdf9d1e9a4")
                                                                                (list "aruco"
                                                                                      "https://github.com/PlusToolkit/aruco.git"
                                                                                      "cf93865edf157aa45c64c8f2084dcef59e58dda3")
                                                                                (list "ndicapi"
                                                                                      "https://github.com/PlusToolkit/ndicapi.git"
                                                                                      "7af09f359cf713de095246bf0dae2763b1c7d135")
                                                                                (list "InfraredSeekCam"
                                                                                      "https://github.com/medtec4susdev/libseek-thermal.git"
                                                                                      "c4fa40f200ef1277675e882358884049b8934d5e")
                                                                                (list "IntersonSDKCxx"
                                                                                      "https://github.com/KitwareMedical/IntersonSDKCxx"
                                                                                      "819d620052be7e9b232e12d8946793c15cfbf5a30")
                                                                                ))
                                                                   ; (git-exec (assoc-ref inputs "git"))
                                                                   )
                                                              ;; Make sure that the Deps directory exists
                                                              (invoke "mkdir" "-p" build-dir)
                                                              (for-each
                                                                (lambda (trip)
                                                                  (let* ((name (list-ref trip 0))
                                                                         (url  (list-ref trip 1))
                                                                         (ref  (list-ref trip 2))
                                                                         (dest (string-append build-dir "/" name)))
                                                                    ;; Clone the refs
                                                                    (invoke "git"
                                                                            "clone"
                                                                            "--depth" "1" ;; Shallow clone
                                                                            "--branch" ref
                                                                            "--single-branch" ;; Only history for that ref
                                                                            url
                                                                            dest)
                                                                    ;; If ref is a commit SHA, ensure we checkout it
                                                                    (invoke "git"
                                                                            "--git-dir" (string-append dest "/.git")
                                                                            "--work-tree" dest
                                                                            "checkout"
                                                                            ref)))
                                                                repos)
                                                              #t)))
                                                        )
                    #:configure-flags (list
                                          ; "-DPLUSBUILD_USE_GIT_PROTOCOL:BOOL=ON"
                                          "-DPLUSBUILD_OFFLINE_BUILD:BOOL=ON"
                                          )))
                 (inputs (list qtbase-5
                               vtk-slicer
                               itk-slicer
                               kdevelop
                               qtmultimedia-5
                               qttools-5
                               libglvnd
                               qtxmlpatterns
                               qtx11extras
                               libxt
                               qtdeclarative-5
                               qtwebengine-5
                               opencv))
                 (native-inputs (list dos2unix
                                      sed
                                      git))
                 (home-page "plustoolkit.github.io")
                 (synopsis "CMake scripts for building Plus library, applications, and all required dependencies")
                 (description "This project contains CMake scripts for building Plus library, applications, and all required dependencies.")
                 (license license:plus-license)))
