;; ISGIO is a dependency for PlusLib

(define-module (guix-systole packages igsio)
               #:use-module ((guix licenses)
                             #:prefix license:)
               #:use-module (guix packages)
               #:use-module (guix download)
               #:use-module (guix build-system cmake)
               #:use-module (gnu packages qt)
               #:use-module (gnu packages gl)
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
               ; #:use-module (gnu packages video)
               ; #:use-module (gnu packages xdisorg)
               ; #:use-module (gnu packages xorg)
               #:use-module (guix-systole packages)
               #:use-module (guix-systole packages vtk)
               #:use-module (guix-systole packages itk)
               #:use-module (guix-systole packages openigtlink)
               )

(define-public igsio
               (package
                 (name "igsio")
                 (version "e298138")
                 (source
                   (origin
                     (method url-fetch)
                     (uri
                       "https://github.com/IGSIO/IGSIO/archive/e298138b2df0321784bfa94ffa3b79e97e79ed48.tar.gz")
                     (sha256
                       (base32
                         "1y4nr62vff5xlh2y0ypydsrqgrgchlilgvy3zigk6hdcwmj5yxjh"
                         )
                       )
                     (patches (search-patches
                                "0017-COMP-packages-igsio-fix-path-to-vtk.patch"
                                "0019-COMP-packages-igsio-fix-codecs-targets-linking.patch"
                                ))
                     )
                   )
                 (build-system cmake-build-system)
                 (arguments
                   `(#:configure-flags (list
                                         "-DIGSIO_BUILD_SEQUENCEIO:BOOL=ON"
                                         "-DIGSIO_SEQUENCEIO_ENABLE_MVK:BOOL=ON"

                                         "-DIGSIO_BUILD_VOLUMERECONSTRUCTION:BOOL=ON"
                                             
                                         "-DIGSIO_BUILD_CODECS:BOOL=ON"
                                         ; "-DIGSIO_USE_VP9:BOOL=ON"

                                         ; (string-append "-DVP9_INCLUDE_DIR="
                                         ;                (assoc-ref %build-inputs "libvpx")
                                         ;                "/include/vpx"
                                         ;                )
                                         ; (string-append "-DVP9_LIBRARY_DIR="
                                         ;                (assoc-ref %build-inputs "libvpx")
                                         ;                "/lib"
                                         ;   )

                                         ; "-DVP9_DIR=$(assoc-ref inputs \"libvpx\")"
                                         ; (string-append "-DVP9_LIBRARY="
                                         ;                (assoc-ref %build-inputs "libvpx")
                                         ;                "/lib/libvpx.so"
                                         ;                )

                                         ; "-DPLUSBUILD_USE_OpenIGTLink:BOOL=ON"

                                         (string-append "-Dfreetype-DIR="
                                                        (assoc-ref %build-inputs "freetype")
                                                        )

                                         ; (string-append "-DCMAKE_INSTALL_RPATH="
                                         ;                ; (assoc-ref %outputs "out") "/lib"
                                         ;                "$ORIGIN/../lib"
                                         ;                )

                                         "-DCMAKE_BUILD_WITH_INSTALL_RPATH:BOOL=ON"
                                         )
                    #:phases (modify-phases %standard-phases
                                (add-before 'configure 'set-cmake-paths
                                  (lambda* (#:key inputs #:allow-other-keys)
                                    ;; Make 'vtkaddon' discoverable by CMake
                                    
                                    (setenv "CMAKE_PREFIX_PATH"
                                            (string-append (assoc-ref inputs "vtkaddon")
                                                          "/lib/cmake:"

                                                          ; (assoc-ref inputs "libvpx")
                                                          ; "/lib/cmake:"

                                                          ; (assoc-ref inputs "freetype")
                                                          ; "/lib/cmake:"

                                                          ; (assoc-ref inputs
                                                          ;   "slicerexecutionmodel")
                                                          ; "/lib/CMake:"
                                                          ;
                                                          (or (getenv "CMAKE_PREFIX_PATH")
                                                              "")
                                                          )) #t))

                                ; (add-after 'install 'symlink-slicer-applauncher
                                ;   (lambda* (#:key outputs #:allow-other-keys)
                                ;     (symlink (string-append (assoc-ref outputs "out")
                                ;                             "/Slicer")
                                ;             (string-append (string-append (assoc-ref
                                ;                                             outputs "out")
                                ;                                           "/bin/Slicer")))
                                ;     #t))
                                )
                    #:tests? #f
                     )
                   )
                 (inputs (list vtk-slicer
                               itk-slicer
                               vtkaddon
                               qtbase-5
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
                               ijg-libjpeg
                               freetype
                               ; glu
                               libglvnd
                               ; svt-vp9
                               ; libvpx
                               ; libdrm
                               ;
                               ; qtmultimedia-5
                               ; qttools-5
                               ; qtxmlpatterns
                               ; qtx11extras
                               ; qtdeclarative-5
                               ; qtwebengine-5
                               ; libglvnd
                               ; libxt
                               ))
                 (home-page "github.com/IGSIO")
                 (synopsis "A collection of tools and algorithms for image guided systems")
                 (description "A collection of tools and algorithms for image guided systems")
                 (license license:bsd-3)
                 )
               )

(define-public openigtlinkio-igsio
               (package
                 (name "openigtlinkio-igsio")
                 (version "1a2eda5")  ;; version used by PlusBuild (Plus 2.8)
                 ; (version "a262c1f")
                 (source
                   (origin
                     (method url-fetch)
                     (uri
                       "https://github.com/IGSIO/OpenIGTLinkIO/archive/1a2eda5ddb795df8bb5bfbba589c9650095ba4cd.tar.gz"
                       ; "https://github.com/IGSIO/OpenIGTLinkIO/archive/a262c1f5e63c00831cbf67d5284f4734f8a7b143.tar.gz"
                       )
                     (sha256
                       (base32
                         "0z2g2kh0a9k5rmi2j0p1nf1bwz6j8rr3rydgbn2lrj376mym6mvn"
                         ; "01y6nhv7c5m57clpql8vg1g43k4k37mvb0bvasl28r90mqm4dvsm"
                         )
                     )
                     (patches (search-patches
                                "0020-BUG-packages-igsio-fix-bug-cmath-not-included.patch"
                                "0021-BUG-packages-igsio-define-VTK_OVERRIDE.patch"
                                "0022-BUG-packages-igsio-fix-missing-semicolons.patch"
                                "0023-BUG-packages-igsio-fix-vtkSmartPointer-import.patch"
                                "0024-BUG-packages-igsio-replace-mutex-lock.patch"
                                ))
                   )
                   )
                   (build-system cmake-build-system)
                   (arguments
                     `(#:configure-flags (list
                                           "-DBUILD_EXAMPLES:BOOL=OFF"
                                           "-DBUILD_TESTING:BOOL=OFF"
                                           "-DIGTLIO_USE_GUI:BOOL=OFF"

                                           ; "-DCMAKE_CXX_FLAGS=-no-pie"
                                           )
                       #:tests? #f)
                     )
                   (inputs (list vtk-slicer
                                 openigtlink
                                 qtbase-5
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
                                 ijg-libjpeg
                                 freetype
                                 ))
                   (home-page "https://github.com/IGSIO/OpenIGTLinkIO")
                   (synopsis "Library for interfacing to openigtlink/OpenIGTLink, dependent on VTK and Qt. Based on openigtlink/OpenIGTLinkIF")
                   (description "OpenIGTLink utility library.

Originally forked from OpenIGTLinkIF.

OpenIGTLinkIO contains several wrapper layers on top of OpenIGTLink. The code originates from OpenIGTLink/OpenIGTLinkIF. The main intent of the library is to share igtl code between Slicer, CustusX, IBIS, MITK and other systems.")
(license license:asl2.0)
                 )
               )
