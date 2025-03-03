;------------------------------------------------------------;
;                                                            ;
;    CTK (Common Toolkit) Guile Scheme package definition    ;
;                                                            ;
;------------------------------------------------------------;

; Defining module
(define-module (guix-systole packages ctk)
    #:use-module (guix packages)
    #:use-module (gnu packages qt)                  ;; For Qt dependencies (includes QtCore, QtGui, QtWidgets)
    ;#:use-module (gnu packages qtwebkit)           ;; If QtWebKit support is enabled
    ;#:use-module (gnu packages qtmultimedia)       ;; If QtMultimedia is enabled
    ;#:use-module (gnu packages vtk)                ;; For VTK (Visualization Toolkit)
    ;#:use-module (gnu packages itk)                ;; For ITK (Image Processing Toolkit)
    #:use-module (gnu packages python)              ;; For Python support (if needed)
    ;#:use-module (gnu packages python-qt)          ;; PythonQt wrapper for Qt
    ;#:use-module (gnu packages dcmtk)              ;; For DICOM support (if needed)
    #:use-module (guix git-download)                ;; Git  -> CTK is pulling from github
    #:use-module (gnu packages version-control)     ;; For Git-related dependencies
    #:use-module (gnu packages cmake)               ;; For CMake build system
    #:use-module (guix build-system cmake)          ;; Using CMake as the build system
    #:use-module (gnu packages image-processing)
    #:use-module ((guix licenses) #:prefix license:)
    #:use-module (guix download)
)

; Potential external packages
;   - CTKDATA, DCMTK, ITK, Log4QT, PythonQT, QsSOAP, QtTesting, VTK, QrestAPI, qxmlrpc

; Defining package definition for CTK (Common Toolkit).
(define-public ctk

    (package                                ; Creating a new package.
        (name "CTK")                        ; Setting name for package definition.
                                            ;   - This is the name you use when to build the package.
        (version "0.1")                     ; Using a specific slicer version for this package definition.
        ;(inherit) 
        ;   | -> Inherit is only needed when inheriting from an already made guix package.
        ;           - ctk doesn't have a guix package definition.

        (source                             ; Specifying source.
            (origin                         ; Setting origin for source definition.
                (method url-fetch)          ; Using url fetch to retrieve source.

                ; Specifying URI (location of origin source).  
                (uri "https://github.com/commontk/CTK/archive/82cae5781621845486bad2697aed095f04cfbe76.tar.gz")

                (sha256
                    (base32 "1g2jv4hjimf4baqbmpmc29ara2f8gk8604g1v8k243x882f0ls9z")
                )
            )
        )

        (build-system cmake-build-system)

        (arguments
            '(#:tests? #f
                #:configure-flags
                (list         
                    ;"DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}"
                    ;" -DCMAKE_CXX_FLAGS:STRING=${ep_common_cxx_flags}"
                    ;" -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}"
                    ;" -DCMAKE_C_FLAGS:STRING=${ep_common_c_flags}"
                    ;" -DCMAKE_CXX_STANDARD:STRING=${CMAKE_CXX_STANDARD}"
                    ;" -DCMAKE_CXX_STANDARD_REQUIRED:BOOL=${CMAKE_CXX_STANDARD_REQUIRED}"
                    ;" -DCMAKE_CXX_EXTENSIONS:BOOL=${CMAKE_CXX_EXTENSIONS}"
                    ;" -DADDITIONAL_C_FLAGS:STRING=${ADDITIONAL_C_FLAGS}"
                    ;" -DADDITIONAL_CXX_FLAGS:STRING=${ADDITIONAL_CXX_FLAGS}"
                    " -DBUILD_TESTING:BOOL=OFF"
                    ;" -DCTK_BUILD_QTDESIGNER_PLUGINS:BOOL=${Slicer_BUILD_QT_DESIGNER_PLUGINS}"
                    ;" -DCTK_INSTALL_BIN_DIR:STRING=${Slicer_INSTALL_BIN_DIR}"
                    ;" -DCTK_INSTALL_LIB_DIR:STRING=${Slicer_INSTALL_LIB_DIR}"
                    ;" -DCTK_INSTALL_QTPLUGIN_DIR:STRING=${Slicer_INSTALL_QtPlugins_DIR}"

                    " -DCTK_USE_GIT_PROTOCOL:BOOL=OFF"  ; turn off git protocol, as it is not supported by GitHub

                    ;" -DCTK_USE_SYSTEM_VTK:BOOL=${CTK_USE_SYSTEM_VTK}"
                    ;" -DVTK_DIR:PATH=${VTK_DIR}"
                    ;" -DCTK_USE_SYSTEM_ITK:BOOL=${CTK_USE_SYSTEM_ITK}"
                    ;" -DITK_DIR:PATH=${ITK_DIR}"
                    " -DCTK_LIB_Widgets:BOOL=ON"
                    " -DCTK_LIB_Visualization/VTK/Widgets:BOOL=ON"
                    " -DCTK_LIB_Visualization/VTK/Widgets_USE_TRANSFER_FUNCTION_CHARTS:BOOL=ON"
                    " -DCTK_LIB_ImageProcessing/ITK/Core:BOOL=ON"
                    " -DCTK_LIB_PluginFramework:BOOL=OFF"
                    " -DCTK_PLUGIN_org.commontk.eventbus:BOOL=OFF"
                    ;" -DCTK_APP_ctkDICOM:BOOL=${Slicer_BUILD_DICOM_SUPPORT}"
                    ;" -DCTK_LIB_DICOM/Core:BOOL=${Slicer_BUILD_DICOM_SUPPORT}"
                    ;" -DCTK_LIB_DICOM/Widgets:BOOL=${Slicer_BUILD_DICOM_SUPPORT}"
                    ;" -DCTK_USE_QTTESTING:BOOL=${Slicer_USE_QtTesting}"
                    ;" -DGIT_EXECUTABLE:FILEPATH=${GIT_EXECUTABLE}"

                    ; ---------------------- PythonQt wrapping ----------------------

                    ;" -DCTK_LIB_Scripting/Python/Core:BOOL=${Slicer_USE_PYTHONQT}"  
                    ;" -DCTK_LIB_Scripting/Python/Core_PYTHONQT_USE_VTK:BOOL=${Slicer_USE_PYTHONQT}"
                    ;" -DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTCORE:BOOL=${Slicer_USE_PYTHONQT}"
                    ;" -DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTGUI:BOOL=${Slicer_USE_PYTHONQT}"
                    ;" -DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTUITOOLS:BOOL=${Slicer_USE_PYTHONQT}"
                    ;" -DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTNETWORK:BOOL=${Slicer_USE_PYTHONQT}"
                    ;" -DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTMULTIMEDIA:BOOL=${_wrap_qtmultimedia}"
                    ;" -DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTWEBKIT:BOOL=${_wrap_qtwebkit}"
                    ;" -DCTK_LIB_Scripting/Python/Widgets:BOOL=${Slicer_USE_PYTHONQT}"
                    ;" -DCTK_ENABLE_Python_Wrapping:BOOL=${Slicer_USE_PYTHONQT}"

                    ; ---------------------------------------------------------------

                    ;" ${EXTERNAL_PROJECT_OPTIONAL_CMAKE_CACHE_ARGS}"
                )
            )
        )

        ;(inputs
        ;    ()
        ;)

        ; ---------------------------- REQUIRED INFO FIELDS -----------------------------

        ; Home page of the CTK (CommonTK) Github repository
        (home-page "github.com/commontk/CTK")      

        (synopsis "CTK (Common Toolkit) Guile scheme package definition for Guix")

        (description "The common toolkit is an open source library for... ")

        (license license:asl2.0)  

        ; -------------------------------------------------------------------------------
    )
)
