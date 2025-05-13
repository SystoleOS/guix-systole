(define-module (guix-systole packages openigtlink)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix-systole packages slicer)
  #:use-module (guix-systole packages ctk)
  #:use-module (guix-systole packages itk)
  #:use-module (guix-systole packages libarchive)
  #:use-module (guix-systole packages qrestapi)
  #:use-module (guix-systole packages teem)
  #:use-module (guix-systole packages vtk)
  #:use-module (guix-systole packages))

(define-public openigtlink
  (package
   (name "openigtlink")
   (version "0.0.0-c512727") ;version used by PlusBuild (Plus 2.8)
   (source
    (origin
     (method url-fetch)
     (uri
      "https://github.com/openigtlink/OpenIGTLink/archive/c512727425c2b7a594fabb9cd1fbfac512bf376e.tar.gz")
     (sha256
      (base32 "0s2rxa4igs2d354205vnp57bf81yj5fpqh91hy5v3zz34gri46j5"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags (list "-DBUILD_EXAMPLES:BOOL=OFF"
                              "-DBUILD_TESTING:BOOL=OFF"
                              "-DOpenIGTLink_SUPERBUILD:BOOL=OFF"
                              "-DOpenIGTLink_PROTOCOL_VERSION_2:BOOL=OFF"
                              "-DOpenIGTLink_PROTOCOL_VERSION_3:BOOL=ON"
                              "-DOpenIGTLink_ENABLE_VIDEOSTREAMING:BOOL=ON"
                              "-DOpenIGTLink_USE_VP9:BOOL=OFF"
                              ;;"-DOpenIGTLink_INSTALL_PACKAGE_DIR:PATH=lib/cmake/OpenIGTLink"
                              "-DBUILD_SHARED_LIBS:BOOL=ON")
      #:tests? #f))
   (inputs (list glew))
   (home-page "openigtlink.org")
   (synopsis
    "Free, open-source network communication library for image-guided therapy")
   (description
    "The OpenIGTLink Library is a C/C++ implementation of The OpenIGTLink
Protocol. OpenIGTLink is an open-source network communication interface
specifically designed for image-guided interventions. It aims to provide a
plug-and-play unified real-time communications (URTC) in operating rooms (ORs)
for image-guided interventions, where imagers, sensors, surgical robots,and
computers from different vendors work cooperatively. This URTC will ensure the
seamless data flow among those components and enable a closed-loop process of
planning, control, delivery, and feedback. The specification of OpenIGTLink is
open, and can be used without any license fee; hence OpenIGTLink is suitable for
both industrial and academic developers.")
   (license license:bsd-3)))

(define-public slicer-openigtlink
  (package
   (name "slicer-openigtlink")
   (version "0.0.0-6fbdadf1")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/openigtlink/SlicerOpenIGTLink/archive/6fbdadf16d6ccee8e840d9d408422bec4c95e867.tar.gz")
     (sha256
      (base32 "0m56zqjdv87iv0p6g269kj9bjknjsaq18ijpsjawh9wi2w8ybsaj"))
     (patches (search-patches
               "0001-COMP-Add-conditional-build-of-UltrasoundRemoteContro.patch"
               "0002-COMP-Fix-include-directories.patch"))))
   (build-system cmake-build-system)
   (arguments
    (list #:tests? #f
          #:validate-runpath? #f
          #:configure-flags
          #~(list
             "-DSlicerOpenIGTLink_SUPERBUILD:BOOL=OFF"
             "-DBUILD_TESTING:BOOL=OFF"
             (string-append "-DSlicer_DIR:PATH="
                            #$(this-package-input "slicer-5.8")
                            "/lib/Slicer-5.8")
             (string-append "-DOpenIGTLink_DIR:PATH="
                            #$(this-package-input "openigtlink")
                            "/lib/igtl/cmake/igtl-3.1")
             (string-append "-DOpenIGTLinkIO_DIR:PATH="
                            #$(this-package-input "openigtlinkio")
                            "/lib/cmake/igtlio"))))
   (inputs
    (list slicer-5.8
          mesa
          ;; QT5
          qtbase-5
          qtmultimedia-5
          qtxmlpatterns
          qtdeclarative-5
          qtsvg-5
          qtx11extras
          qtwebengine-5
          qtwebchannel-5
          qttools-5
          ;;VTK
          vtk-slicer
          itk-slicer
          double-conversion
          freetype
          gl2ps
          glew
          jsoncpp
          libharu
          libtheora
          libxml++
          lz4
          mpich
          netcdf
          proj
          libxt
          eigen
          expat
          openssl-3.0
          git
          hdf5-1.10
          libffi
          libjpeg-turbo
          libxinerama
          mesa ;libGL equivalent
          rapidjson
          tbb
          ctk
          ctkapplauncher
          libarchive-slicer
          teem-slicer
          vtkaddon
          qrestapi
          openigtlink
          openigtlinkio
          ))
   (synopsis "Slicer Extension for communication of IGT data")
   (description "SlicerOpenIGTLink is a 3D Slicer extension designed to facilitate the communication between 3D Slicer and other platforms via OpenIGTLink.")
   (license license:bsd-2)
   (home-page "https://github.com/openigtlink/SlicerOpenIGTLink")))

(define-public openigtlinkio
  (package
   (name "openigtlinkio")
   (version "0.0.0-a262c1f")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/IGSIO/OpenIGTLinkIO/archive/a262c1f5e63c00831cbf67d5284f4734f8a7b143.tar.gz")
     (sha256
      (base32 "01y6nhv7c5m57clpql8vg1g43k4k37mvb0bvasl28r90mqm4dvsm"))))
   (build-system cmake-build-system)
   (arguments
    (list #:tests? #f
          #:validate-runpath? #f
          #:configure-flags
          #~(list
             (string-append "-DSlicer_DIR:PATH="
                            #$(this-package-input "slicer-5.8")
                            "/lib/Slicer-5.8")
             (string-append "-DOpenIGTLink_DIR:PATH="
                            #$(this-package-input "openigtlink")
                            "/lib/igtl/cmake/igtl-3.1"))))
  (inputs
   (list slicer-5.8
         mesa
         ;; QT5
         qtbase-5
         qtmultimedia-5
         qtxmlpatterns
         qtdeclarative-5
         qtsvg-5
         qtx11extras
         qtwebengine-5
         qtwebchannel-5
         qttools-5
         ;;VTK
         vtk-slicer
         itk-slicer
         double-conversion
         freetype
         gl2ps
         glew
         jsoncpp
         libharu
         libtheora
         libxml++
         lz4
         mpich
         netcdf
         proj
         libxt
         eigen
         expat
         openssl-3.0
         git
         hdf5-1.10
         libffi
         libjpeg-turbo
         libxinerama
         mesa ;libGL equivalent
         rapidjson
         tbb
         ctk
         ctkapplauncher
         libarchive-slicer
         teem-slicer
         vtkaddon
         qrestapi
         openigtlink))
  (synopsis "Library for interfacing to openigtlink/OpenIGTLink, dependent on VTK and Qt. Based on openigtlink/OpenIGTLinkIF")
  (description "OpenIGTLinkIO contains several wrapper layers on top of OpenIGTLink. The code originates from OpenIGTLink/OpenIGTLinkIF. The main intent of the library is to share igtl code between Slicer, CustusX, IBIS, MITK and other systems.")
  (license license:bsd-2)
  (home-page "https://github.com/IGSIO/OpenIGTLinkIO")))
