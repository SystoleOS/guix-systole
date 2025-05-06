(define-module (guix-systole packages openigtlink)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages xiph)
  #:use-module (guix-systole packages)
  #:use-module (guix-systole packages vtk)
  #:use-module (guix-systole packages ctk))

(define-public openigtlink
  (package
    (name "openigtlink")
    (version "2081e41") ;version used by PlusBuild (Plus 2.8)
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/openigtlink/OpenIGTLink/archive/2081e418c48c02e920487e2284996c1e577c1024.tar.gz")
       (sha256
        (base32 "0ss2wc2gg28m7xvwyzqkl805684x0x5xqrdwz634rjw66g2jinh0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DBUILD_EXAMPLES:BOOL=OFF"
                          "-DBUILD_TESTING:BOOL=OFF"
                          "-DOpenIGTLink_SUPERBUILD:BOOL=OFF"
                          "-DOpenIGTLink_PROTOCOL_VERSION_2:BOOL=OFF"
                          "-DOpenIGTLink_PROTOCOL_VERSION_3:BOOL=ON"
                          "-DOpenIGTLink_ENABLE_VIDEOSTREAMING:BOOL=ON"
                          "-DOpenIGTLink_USE_VP9:BOOL=OFF"

                          "-DOpenIGTLink_INSTALL_PACKAGE_DIR:PATH=lib/cmake/OpenIGTLink"

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

(define-public openigtlinkio
  (package
    (name "openigtlinkio")
    (version "a262c1f")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/IGSIO/OpenIGTLinkIO/archive/32feaaa5fcff433dec4f071cf7aaa9106e5e5124.tar.gz")
       (sha256
        (base32 "03zdzm4m007bf9cym9fgscc4jp1n94flamqjxqi67jwrxnwd4nl6"))
       (patches (search-patches
                 "0019-BUG-packages-openigtlink-Reintroduce-old-fix-with-cmath-for-openigtlinkio.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DBUILD_TESTING:BOOL=OFF"
                               "-DIGTLIO_USE_EXAMPLES:BOOL=OFF"
                               "-DBUILD_SHARED_LIBS:BOOL=ON")
       #:tests? #f))
    (inputs (list double-conversion
                  eigen
                  expat
                  ctk
                  freetype
                  glew
                  gl2ps
                  glew
                  hdf5
                  jsoncpp
                  libharu
                  libjpeg-turbo
                  libtheora
                  libxml++
                  lz4
                  mpich
                  netcdf
                  openigtlink
                  proj
                  qtbase-5
                  qtx11extras
                  tbb
                  vtk))
    (home-page "openigtlink.org")
    (synopsis
     "Library for interfacing to openigtlink/OpenIGTLink, dependent on VTK and Qt. Based on openigtlink/OpenIGTLinkIF ")
    (description
     "Library for interfacing to openigtlink/OpenIGTLink, dependent on VTK and Qt. Based on openigtlink/OpenIGTLinkIF.")
    (license license:asl2.0)))

(define-public sliceropenigtlinkif-src
  (package
    (name "openigtlinkif-src")
    (version "6fbdadf")
    (source
     (origin
       (method url-fetch)
       (uri
        ;; "https://github.com/openigtlink/OpenIGTLinkIF/archive/37db113599833e34abb3511dc63976803f724a7f.tar.gz"
        "https://github.com/openigtlink/SlicerOpenIGTLink/archive/6fbdadf16d6ccee8e840d9d408422bec4c95e867.tar.gz")
       (sha256
        (base32 "0m56zqjdv87iv0p6g269kj9bjknjsaq18ijpsjawh9wi2w8ybsaj"))
       (patches (search-patches
                 "0018-COMP-packages-openigtlink-Include-Slicer-macros-in-OpenIGTLinkIF.patch"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "/"))
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page
     "https://slicer.readthedocs.io/en/latest/user_guide/modules/plots.html")
    (synopsis "OpenIGTLink interface module for 3D Slicer 4.x ")
    (description "OpenIGTLink interface module for 3D Slicer 4.x ")
    (license license:bsd-2)))
