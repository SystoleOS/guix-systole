;; OpenIGTLink is a dependency for PlusLib

(define-module (guix-systole packages openigtlink)
               #:use-module ((guix licenses)
                             #:prefix license:)
               #:use-module (guix packages)
               #:use-module (guix download)
               #:use-module (guix build-system cmake)
               #:use-module (gnu packages gl)
               ; #:use-module (guix-systole packages)
  )

(define-public openigtlink
               (package
                 (name "openigtlink")
                 (version "2081e41")  ;; version used by PlusBuild (Plus 2.8)
                 (source
                   (origin
                     (method url-fetch)
                     (uri
                       "https://github.com/openigtlink/OpenIGTLink/archive/2081e418c48c02e920487e2284996c1e577c1024.tar.gz"
                       )
                     (sha256
                       (base32
                         "0ss2wc2gg28m7xvwyzqkl805684x0x5xqrdwz634rjw66g2jinh0")
                       )
                     )
                   )
                 (build-system cmake-build-system)
                 (arguments
                   `(#:configure-flags (list
                                         "-DBUILD_EXAMPLES:BOOL=OFF"
                                         "-DBUILD_TESTING:BOOL=OFF"
                                         "-DOpenIGTLink_SUPERBUILD:BOOL=OFF"
                                         "-DOpenIGTLink_PROTOCOL_VERSION_2:BOOL=OFF"
                                         "-DOpenIGTLink_PROTOCOL_VERSION_3:BOOL=ON"
                                         "-DOpenIGTLink_ENABLE_VIDEOSTREAMING:BOOL=ON"
                                         "-DOpenIGTLink_USE_VP9:BOOL=OFF"

                                         "-DOpenIGTLink_INSTALL_PACKAGE_DIR:PATH=lib/cmake/OpenIGTLink"

                                         ;; Build as shared library so OpenIGTLinkIO can link to shared .so instead of static archive
                                         "-DBUILD_SHARED_LIBS:BOOL=ON"
                                         )
                     #:tests? #f
                     )
                   )
                 (inputs (list glew))
                 (home-page "openigtlink.org")
                 (synopsis "Free, open-source network communication library for image-guided therapy")
                 (description "The OpenIGTLink Library is a C/C++ implementation of The OpenIGTLink Protocol.

OpenIGTLink is an open-source network communication interface specifically designed for image-guided interventions. It aims to provide a plug-and-play unified real-time communications (URTC) in operating rooms (ORs) for image-guided interventions, where imagers, sensors, surgical robots,and computers from different vendors work cooperatively. This URTC will ensure the seamless data flow among those components and enable a closed-loop process of planning, control, delivery, and feedback. The specification of OpenIGTLink is open, and can be used without any license fee; hence OpenIGTLink is suitable for both industrial and academic developers.")
(license license:bsd-3)
                 )
               )
