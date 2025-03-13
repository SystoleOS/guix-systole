;------------------------------------------------------------;
;                                                            ;
;    CTK (Common Toolkit) Guile Scheme package definition    ;
;                                                            ;
;------------------------------------------------------------;

; Defining modules for CTK package definition.  
(define-module (guix-systole packages ctk)              ;;
    #:use-module (guix packages)                        ;;
    #:use-module (gnu packages qt)                      ;; 
    ;#:use-module (gnu packages qtwebkit)               ;; 
    ;#:use-module (gnu packages qtmultimedia)           ;; 
    ;#:use-module (gnu packages python)                 ;; 
    ;#:use-module (gnu packages python-qt)              ;; 
    #:use-module (gnu packages image-processing)        ;; Added Dicom module 
    #:use-module (guix git-download)                    ;; 
    #:use-module (gnu packages version-control)         ;; 
    #:use-module (gnu packages cmake)                   ;; 
    #:use-module (guix build-system cmake)              ;; 
    #:use-module ((guix licenses) #:prefix license:)    ;;
    #:use-module (guix download)                        ;; 

    #:use-module (guix-systole packages vtk)            ;; Systole VTK file.
    #:use-module (guix-systole packages itk)            ;; Systole ITK file
)


; Defining package definition for CTK (Common Toolkit).
(define-public ctk-slicer
    (package                                ; Creating a new package.
        (name "ctk-slicer")                        ; Setting name for package definition.
                                            ;   - This is the name you use when to build the package.
        (version "0.1")                     ; Using a specific slicer version for this package definition.
        ;(inherit) 
        ;   | -> Inherit is only needed when inheriting from an already made guix package.
        ;           - ctk doesn't have a guix package definition.

        (source                             ; Specifying tersource.
            (origin                         ; Setting origin for source definition.
                (method url-fetch)          ; Using url fetch to retrieve source.

                ; Specifying URI (location of origin source). Reference to a specific commit. 
                (uri "https://github.com/commontk/CTK/archive/82cae5781621845486bad2697aed095f04cfbe76.tar.gz")

                ; Added hash for uri above.
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
                    ; --------------------------- Build Flags ---------------------------

                    "-DCTK_USE_GIT_PROTOCOL:BOOL=OFF"  ; turning off git protocol, as it is not supported by modern GitHub 

                    "-DCTK_SUPERBUILD:BOOL=OFF"   ; We dont want to build CTK as a superbuild.

                    "-DBUILD_TESTING:BOOL=OFF"  ; We dont want to build CTK tests. 

                    ;"-DCTK_BUILD_QTDESIGNER_PLUGINS:BOOL=${Slicer_BUILD_QT_DESIGNER_PLUGINS}"
                    ;"-DCTK_INSTALL_BIN_DIR:STRING=${Slicer_INSTALL_BIN_DIR}"
                    ;"-DCTK_INSTALL_LIB_DIR:STRING=${Slicer_INSTALL_LIB_DIR}"
                    ;"-DCTK_INSTALL_QTPLUGIN_DIR:STRING=${Slicer_INSTALL_QtPlugins_DIR}"

                    ; -------------------------- CTKdata flags --------------------------

                    "-DCTK_ENABLE_CTKDATA:BOOL=OFF" ; CTKData is only needed for testing
                                                    ; so we can disable it.

                    ; ---------------------------- VTK flags ----------------------------

                    "-DCTK_USE_SYSTEM_VTK:BOOL=ON"  ; Enabling VTK.

                    ; ---------------------------- ITK flags ----------------------------

                    "-DCTK_USE_SYSTEM_ITK:BOOL=ON" ; Enabling ITK.

                    ; --------------------------- DICOM Flags ---------------------------

                    "-DCTK_USE_SYSTEM_DCMTK:BOOL=ON"    ; Enabling Dicom and dicom libraries.

                    ; ------------------------ CTK Widgets Flags-------------------------

                    "-DCTK_LIB_Widgets:BOOL=OFF"    ; This should be ON as widgets are required to function.

                    ;"-DCTK_LIB_Visualization/VTK/Widgets:BOOL=ON"
                    ;"-DCTK_LIB_Visualization/VTK/Widgets_USE_TRANSFER_FUNCTION_CHARTS:BOOL=ON"
                    ;"-DCTK_LIB_ImageProcessing/ITK/Core:BOOL=ON"

                    "-DCTK_LIB_PluginFramework:BOOL=OFF"
                    "-DCTK_PLUGIN_org.commontk.eventbus:BOOL=OFF"
                    ;"-DCTK_APP_ctkDICOM:BOOL=${Slicer_BUILD_DICOM_SUPPORT}"
                    ;"-DCTK_LIB_DICOM/Core:BOOL=${Slicer_BUILD_DICOM_SUPPORT}"
                    ;"-DCTK_LIB_DICOM/Widgets:BOOL=${Slicer_BUILD_DICOM_SUPPORT}"
                    ;"-DCTK_USE_QTTESTING:BOOL=${Slicer_USE_QtTesting}"
                    ;"-DGIT_EXECUTABLE:FILEPATH=${GIT_EXECUTABLE}"

                    ; ---------------------- PythonQt wrapping ----------------------

                    ;"-DCTK_LIB_Scripting/Python/Core:BOOL=${Slicer_USE_PYTHONQT}"  
                    ;"-DCTK_LIB_Scripting/Python/Core_PYTHONQT_USE_VTK:BOOL=${Slicer_USE_PYTHONQT}"
                    ;"-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTCORE:BOOL=${Slicer_USE_PYTHONQT}"
                    ;"-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTGUI:BOOL=${Slicer_USE_PYTHONQT}"
                    ;"-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTUITOOLS:BOOL=${Slicer_USE_PYTHONQT}"
                    ;"-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTNETWORK:BOOL=${Slicer_USE_PYTHONQT}"
                    ;"-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTMULTIMEDIA:BOOL=${_wrap_qtmultimedia}"
                    ;"-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTWEBKIT:BOOL=${_wrap_qtwebkit}"
                    ;"-DCTK_LIB_Scripting/Python/Widgets:BOOL=${Slicer_USE_PYTHONQT}"
                    ;"-DCTK_ENABLE_Python_Wrapping:BOOL=${Slicer_USE_PYTHONQT}"

                    ; ---------------------------------------------------------------
                )

                
            )
        )

        (inputs
            (list           
                qtbase-5    ;   
                qttools-5   ;   
                qtsvg-5     ;   
                dcmtk       ; Dicom input library.
                vtk-slicer  ; Defined which vtk slicer package to input.
                itk-slicer  ; Defined which itk slicer package to input.
            )
        )

        (native-inputs
            (list
                git         ;
            )    
        )

        ; ---------------------------- REQUIRED INFO FIELDS -----------------------------

        ; Home page of the CTK (CommonTK) Github repository
        (home-page "github.com/commontk/CTK")      

        (synopsis "CTK (Common Toolkit) Guile scheme package definition for Guix")

        (description "The common toolkit is an open source library for... ")

        (license license:asl2.0)  

        ; -------------------------------------------------------------------------------
    )
)
