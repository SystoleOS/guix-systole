(define-module (guix-systole packages slicer)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix-systole packages ctk)
  #:use-module (guix-systole packages itk)
  #:use-module (guix-systole packages qrestapi)
  #:use-module (guix-systole packages teem)
  #:use-module (guix-systole packages vtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages cmake)
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
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
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
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public slicer
  (package
    (name "slicer")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/Slicer/Slicer/archive/11eaf62e5a70b828021ff8beebbdd14d10d4f51c.tar.gz")
       (sha256
        (base32 "05rz797ddci3a2m8297zyzv2g2hp6bd6djmwa1n0gbsla8b175bx"))
       (patches (list (local-file
                       "patches/0002-COMP-Add-vtk-CommonSystem-component-as-requirement.patch")
                      (local-file
                       "patches/0003-COMP-Find-Eigen-required.patch")
                      (local-file
                       "patches/0004-COMP-Adapt-to-new-qRestAPI-cmake.patch")
                      (local-file "patches/cpack-patch.patch")))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DSlicer_USE_SYSTEM_LibFFI:BOOL=ON"
                          "-DSlicer_SUPERBUILD:BOOL=OFF"
                          "-DBUILD_TESTING:BOOL=OFF"
                          "-DBUILD_SHARED_LIBS:BOOL=ON"
                          "-DSlicer_BUILD_EXTENSIONMANAGER_SUPPORT:BOOL=OFF"
                          "-DSlicer_DONT_USE_EXTENSION:BOOL=ON"
                          "-DSlicer_BUILD_CLI_SUPPORT:BOOL=OFF"
                          "-DSlicer_BUILD_CLI:BOOL=OFF"
                          "-DCMAKE_CXX_STANDARD:STRING=17"
                          "-DSlicer_REQUIRED_QT_VERSION:STRING=5"
                          ;; "-DSlicer_BUILD_DICOM_SUPPORT:BOOL=$(usex DICOM ON OFF)"
                          "-DSlicer_BUILD_ITKPython:BOOL=OFF"
                          ;; "-DSlicer_BUILD_QTLOADABLEMODULES:BOOL=OFF"
                          ;; "-DSlicer_BUILD_QTSCRIPTEDMODULES:BOOL=OFF"
                          ;; "-DSlicer_BUILD_QT_DESIGNER_PLUGINS:BOOL=ON"
                          "-DSlicer_USE_SYSTEM_QT:BOOL=ON"
                          "-DSlicer_USE_QtTesting:BOOL=OFF"
                          "-DSlicer_USE_SlicerITK:BOOL=ON"
                          "-DSlicer_USE_CTKAPPLAUNCHER:BOOL=ON"

                          ;; "-DSlicer_USE_SimpleITK:BOOL=OFF"
                          ;; "-DSlicer_VTK_RENDERING_BACKEND:STRING=OpenGL2"
                          "-DSlicer_VTK_VERSION_MAJOR:STRING=9"
                          "-DSlicer_BUILD_vtkAddon:BOOL=ON" ;Include things like "vtkMacroKitPythonWrap"
                          
                          "-DSlicer_INSTALL_DEVELOPMENT:BOOL=OFF"
                          ;; "-DCMAKE_BUILD_WITH_INSTALL_RPATH:BOOL=ON"
                          ;; "-DTeem_DIR:STRING="
                          "-DSlicer_USE_SYSTEM_teem:BOOL=ON"
                          "-DSlicer_USE_TBB:BOOL=ON"
                          "-DSlicer_USE_SYSTEM_tbb:BOOL=ON"

                          ;; "-DCTK_INSTALL_QTPLUGIN_DIR:STRING=/usr/lib64/qt5/plugins"
                          ;; "-DQT_PLUGINS_DIR:STRING=/usr/lib64/designer"
                          ;; "-DSlicer_QtPlugins_DIR:STRING=/usr/lib64/designer"
                          ;; "-DjqPlot_DIR:STRING=/usr/share/jqPlot"
                          ;; "-DSlicer_VTK_WRAP_HIERARCHY_DIR:STRING=#{$\x7b;BUILD_DIR\x7d;}#"
                          "-DSlicer_BUILD_vtkAddon:BOOL=ON"
                          ;; "-DSlicer_USE_SimpleITK:BOOL=OFF"
                          "-DSlicer_BUILD_DICOM_SUPPORT:BOOL=OFF" ;Disabled as we do not have IODCMTK support yet
                          
                          ;; Python
                          ;; "-DPython3_INCLUDE_DIR:FILEPATH="
                          ;; "-DPython3_LIBRARY:FILEPATH="
                          ;; "-DPython3_EXECUTABLE:FILEPATH="
                          "-DVTK_WRAP_PYTHON:BOOL=OFF"
                          "-DSlicer_USE_PYTHONQT:BOOL=OFF"
                          "-DSlicer_USE_SYSTEM_python:BOOL=OFF"

                          ;; Hack to fix error "Variable Slicer_WC_LAST_CHANGED_DATE is expected to be defined."
                          "-DSlicer_WC_LAST_CHANGED_DATE:STRING=2021-12-21 22:15:05 +0800")
                  ))
    (inputs (list libxt
                  eigen
                  expat
                  openssl-3.0
                  git
                  hdf5
                  libarchive
                  libffi
                  libjpeg-turbo
                  libxinerama
                  mesa ;libGL equivalent
                  perl
                  python-3.10
                  rapidjson
                  tbb

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

                  ;; VTK
                  vtk-slicer
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

                  ;; Other Slicer modules
                  ctk
                  ctkapplauncher
                  itk-slicer
                  teem-slicer
                  qrestapi))
    (native-inputs (list gcc cmake pkg-config))
    (synopsis "3D Slicer - Medical visualization and computing environment")
    (description
     "3D Slicer is a multi-platform, free and open source software package for 
visualization and medical image computing. It provides capabilities for:
@itemize
@item Medical image processing and analysis
@item Segmentation and registration
@item Three-dimensional visualization
@item Support for various imaging modalities
@end itemize")
    (home-page "https://www.slicer.org/")
    (license license:bsd-3)))
