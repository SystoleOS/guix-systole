(define-module (guix-systole packages ctk)
  #:use-module (gnu packages algebra)           ; Eigen3
  #:use-module (gnu packages compression)       ; lz4
  #:use-module (gnu packages fontutils)         ; freetype
  #:use-module (gnu packages geo)               ; LibPROJ
  #:use-module (gnu packages gl)                ; Glew lib, gl2ps
  #:use-module (gnu packages image)             ; PNG, JPEG
  #:use-module (gnu packages image-processing)  ; for dcmtk
  #:use-module (gnu packages maths)             ; hdf5, double-conversion
  #:use-module (gnu packages mpi)               ; mpich-ofi
  #:use-module (gnu packages pdf)               ; libharu
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)     ; jsonCPP
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages xiph)              ; Theora lib.
  #:use-module (gnu packages xml)               ; libxml2, expat
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix-systole packages itk)
  #:use-module (guix-systole packages vtk))

;; --------------------------- CTK ---------------------------
(define-public ctk
  (package
    (name "ctk")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/commontk/CTK/archive/82cae5781621845486bad2697aed095f04cfbe76.tar.gz")
       (sha256
        (base32 "1g2jv4hjimf4baqbmpmc29ara2f8gk8604g1v8k243x882f0ls9z"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f
        #:parallel-build? #t    ; Scheme building using multiple threads.
        #:configure-flags
        (list ;; --------------------------- Build Flags ---------------------------
         "-DCTK_USE_GIT_PROTOCOL:BOOL=OFF" ;turning off git protocol, as it is not supported by modern GitHub
         "-DCTK_SUPERBUILD:BOOL=OFF" ;Disable Superbuild
         "-DBUILD_TESTING:BOOL=OFF"
         "-DCTK_INSTALL_LIB_DIR=lib" ; Hardcoded path for CTK install directory. Fix for CTK-Widgets.

         ;; NOTE: (from Slicer) These may need to change in the future.
         "-DCTK_BUILD_QTDESIGNER_PLUGINS:BOOL=ON"
         ;; "-DCTK_BUILD_QTDESIGNER_PLUGINS:BOOL=${Slicer_BUILD_QT_DESIGNER_PLUGINS}"
         ;; "-DCTK_INSTALL_QTPLUGIN_DIR:STRING=${Slicer_INSTALL_QtPlugins_DIR}"
         ;; -------------------------- CTKdata flags --------------------------
         ;; NOTE: Testing should be reviewed and added at some point
         "-DCTK_ENABLE_CTKDATA:BOOL=OFF" ;CTKData is only needed for testing
         ;; ---------------------------- VTK flags ----------------------------
         "-DCTK_USE_SYSTEM_VTK:BOOL=ON"
         ;; ---------------------------- ITK flags ----------------------------
         "-DCTK_USE_SYSTEM_ITK:BOOL=ON"
         ;; --------------------------- DICOM Flags ---------------------------
         "-DCTK_USE_SYSTEM_DCMTK:BOOL=ON"
         "-DCTK_APP_ctkDICOM:BOOL=ON"
         "-DCTK_LIB_DICOM/Core:BOOL=ON"
         "-DCTK_LIB_DICOM/Widgets:BOOL=ON"
         ;; ------------------------ CTK Widgets Flags-------------------------
         "-DCTK_LIB_Widgets:BOOL=ON"
         "-DCTK_LIB_Visualization/VTK/Widgets:BOOL=ON"                               ; \
         "-DCTK_LIB_Visualization/VTK/Widgets_USE_TRANSFER_FUNCTION_CHARTS:BOOL=ON"  ; -> Needs GuiSupportQT
         "-DCTK_LIB_ImageProcessing/ITK/Core:BOOL=ON"                                ; /
         "-DCTK_LIB_PluginFramework:BOOL=OFF"
         "-DCTK_PLUGIN_org.commontk.eventbus:BOOL=OFF"
         ;; ---------------------- PythonQt wrapping ----------------------
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
           itk-slicer
                                        ; --- Libraries for Visualization VTK widgets and ITK core ---
           hdf5
           python
           glew
           libtheora
           netcdf
           proj        ; LibPROJ
           jsoncpp
           libxml2
           libharu
           gl2ps
           libpng-apng
           eigen
           mpich
           expat
           double-conversion
           lz4
           libjpeg-turbo
           freetype
           tbb))
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
