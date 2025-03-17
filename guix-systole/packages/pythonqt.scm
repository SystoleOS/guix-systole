(define-module (guix-systole packages pythonqt)
               #:use-module (gnu packages)
               #:use-module (guix packages)
               #:use-module (gnu packages qt)
               #:use-module ((guix licenses) #:prefix license:)
               #:use-module (guix download)
               #:use-module (guix build-system cmake))

(define-public pythonqt-commontk
               (package
                 (inherit python-pyqt)
                 (name "pythonqt-commontk")
                 ; (version "0.1")
                 (source
                   (origin
                     (method url-fetch)
                     (uri "https://github.com/commontk/PythonQt/archive/0580304d8119caaa6c6a985d43f7109d180af880.tar.gz")
                     (sha256
                       (base32 "0d7kijid6rj9s2wrfbfx2d5552xxxbxpgvjn6iccsa35ssgsj1hz")
                      )
                    )
                  )
                 (build-system cmake-build-system)
                 ; (arguments
                 ;   '(#:tests? #f
                 ;     #:configure-flags
                 ;     (list
                 ;       ; Enable qMRMLWidgets python wrapping
                 ;       "-DMRMLWidgets_VTK_WRAP_PYTHON:BOOL=ON"
                 ;
                 ;       )
                 ;     )
                 ;   )
                 ; (inputs
                 ;   (list
                 ;     ; qtbase-5
                 ;     )
                 ;   )
                 ; (home-page "https://github.com/commontk/PythonQt")
                 ; (synopsis "CMake-ified version of PythonQt")
                 ; (description "PythonQt is a dynamic Python binding for Qt. It offers an easy way to embed the Python scripting language into your Qt applications.")
                 (license license:lgpl2.1)
                )
              )
