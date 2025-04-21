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
                                ))
                     )
                   )
                 (build-system cmake-build-system)
                 (arguments
                   `(#:configure-flags (list
                                         "-DPLUS_OFFLINE_BUILD:BOOL=ON"
                                         ; "-DPLUS_USE_NVIDIA_DVP:BOOL=OFF"
                                         ))
                   )
                 (inputs (list vtk-slicer
                               itk-slicer
                               igsio
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
                               ))
                 (home-page "plustoolkit.github.io")
                 (synopsis "Software library for data acquisition, pre-processing, and calibration for navigated image-guided interventions.")
                 (description "Software library for data acquisition, pre-processing, and calibration for navigated image-guided interventions. See more information at www.plustoolkit.org.")
                 (license license:plus-license)
                )
               )
