(define-module (guix-systole packages qrestapi)
               #:use-module (guix packages)
               #:use-module (gnu packages qt)
               #:use-module ((guix licenses) #:prefix license:)
               #:use-module (guix download)
               #:use-module (guix build-system cmake))

(define-public qrestapi
               (package
                 (name "qRestAPI")
                 (version "0.1")
                 (source
                   (origin
                     (method url-fetch)
                     (uri "https://github.com/commontk/qRestAPI/archive/88c02c5d90169dfe065fa068969e59ada314d3cb.tar.gz")
                     (sha256
                       (base32 "0jfnja3frcm4vkibi1vygdh7f4dmhqxni43bbb3rmlcl6jlyaibl"))))
                 (build-system cmake-build-system)
                 (arguments
                   '(#:tests? #f
                     #:configure-flags
                     (list
                       ; Explicitly use Qt version 5)
                       "-DqRestAPI_QT_VERSION:STRING=5"
                       "-DBUILD_SHARED_LIBS:BOOL=ON"
                       "-DBUILD_TESTING:BOOL=OFF"
                       "-DqRestAPI_STATIC:BOOL=OFF"
                       )))
                 (inputs (list
                           qtbase-5 qtdeclarative-5))
                 (home-page "https://github.com/commontk/qRestAPI")
                 (synopsis "Simple Qt library allowing to synchronously or asynchronously query a REST server.")
                 (description "qRestAPI is a cross-platform [Qt-based](https://www.qt.io/) library 
                              allowing to easily query any [RESTful](https://en.wikipedia.org/wiki/Representational_state_transfer) web services. 

                              It provides the following interfaces:

                              | Interface    | RESTful API      |
                              |--------------| -----------------|
                              | `qRestAPI`   | _any_            |
                              | `qGirderAPI` | [Girder][girder] |
                              | `qMidasAPI`  | [Midas][midas]   |

                              [girder]: https://github.com/girder/girder
                              [midas]: https://github.com/midasplatform/midas")
                (license license:asl2.0)))
