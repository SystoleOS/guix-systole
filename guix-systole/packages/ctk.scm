(define-module (guix-systole packages ctk)
  #:use-module (guix packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages image-processing)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix-systole packages vtk)
  #:use-module (guix-systole packages itk))

;; --------------------------- CTK ---------------------------
(define-public ctk
    (package
        (name "ctk")
        (version "0.1")
        (source
            (origin
                (method url-fetch)
                (uri "https://github.com/commontk/CTK/archive/82cae5781621845486bad2697aed095f04cfbe76.tar.gz")
                (sha256
                    (base32 "1g2jv4hjimf4baqbmpmc29ara2f8gk8604g1v8k243x882f0ls9z"))))
        (build-system cmake-build-system)
        (arguments
            '(#:tests? #f
                #:configure-flags
                (list
                    ; --------------------------- Build Flags ---------------------------
                    "-DCTK_USE_GIT_PROTOCOL:BOOL=OFF"  ; turning off git protocol, as it is not supported by modern GitHub
                    "-DCTK_SUPERBUILD:BOOL=OFF"        ; Disable Superbuild
                    "-DBUILD_TESTING:BOOL=OFF"
                    ;; NOTE: (from Slicer) These may need to change in the future.
                    ;"-DCTK_BUILD_QTDESIGNER_PLUGINS:BOOL=${Slicer_BUILD_QT_DESIGNER_PLUGINS}"
                    ;"-DCTK_INSTALL_QTPLUGIN_DIR:STRING=${Slicer_INSTALL_QtPlugins_DIR}"
                    ; -------------------------- CTKdata flags --------------------------
                    ;; NOTE: Testing should be reviewed and added at some point
                    "-DCTK_ENABLE_CTKDATA:BOOL=OFF" ; CTKData is only needed for testing
                    ; ---------------------------- VTK flags ----------------------------
                    "-DCTK_USE_SYSTEM_VTK:BOOL=ON"
                    ; ---------------------------- ITK flags ----------------------------
                    "-DCTK_USE_SYSTEM_ITK:BOOL=ON"
                    ; --------------------------- DICOM Flags ---------------------------
                    "-DCTK_USE_SYSTEM_DCMTK:BOOL=ON"
                    ; ------------------------ CTK Widgets Flags-------------------------
                    ;; NOTE: This should be ON as widgets are required to function.
                    "-DCTK_LIB_Widgets:BOOL=OFF"
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
                    "-DCTK_LIB_Scripting/Python/Core:BOOL=OFF"
                    "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_USE_VTK:BOOL=OFF"
                    "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTCORE:BOOL=OFF"
                    "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTGUI:BOOL=OFF"
                    "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTUITOOLS:BOOL=OFF"
                    "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTNETWORK:BOOL=OFF"
                    "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTMULTIMEDIA:BOOL=OFF"
                    "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTWEBKIT:BOOL=OFF"
                    "-DCTK_LIB_Scripting/Python/Widgets:BOOL=OFF"
                    "-DCTK_ENABLE_Python_Wrapping:BOOL=OFF")))
        (inputs
            (list qtbase-5
                  qttools-5
                  qtsvg-5
                  dcmtk
                  vtk-slicer
                  itk-slicer))
        (home-page "github.com/commontk/CTK")
    (synopsis "A set of common support code for medical imaging, surgical 
navigation, and related purposes. ")
    (description
     "The goal of CTK is to support biomedical image computing. CTK
code is licensed under Apache 2.0. This means that users of CTK are allowed to
use the code for academic, commercial, or other purposes without paying license
fees or being restricted in their ability to redistribute their code or keep it
private.

CTK works on topics that are not covered by existing toolkits that support the
mutual interest and needs of the CTK community. The main scope of current CTK
efforts includes the topics DICOM, DICOM Application Hosting, Widgets, and 
Plugin Framework.")
        (license license:asl2.0)))
