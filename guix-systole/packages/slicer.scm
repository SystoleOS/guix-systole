(define-module (guix-systole packages slicer)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix-systole packages ctk)
  #:use-module (guix-systole packages itk)
  #:use-module (guix-systole packages vtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
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
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public slicer
  (package
    (name "slicer")
    (version "5.8.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/Slicer/Slicer/archive/11eaf62e5a70b828021ff8beebbdd14d10d4f51c.tar.gz")
       (sha256
        (base32 "05rz797ddci3a2m8297zyzv2g2hp6bd6djmwa1n0gbsla8b175bx"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DSlicer_USE_SYSTEM_LibFFI:BOOL=ON"
                          "-DSlicer_USE_PYTHONQT:BOOL=OFF"
                          "-DSlicer_SUPERBUILD:BOOL=OFF"
                          "-DBUILD_TESTING:BOOL=OFF"
                          "-DSlicer_BUILD_EXTENSIONMANAGER_SUPPORT:BOOL=OFF"
                          "-DSlicer_DONT_USE_EXTENSION:BOOL=ON"
                          "-DSlicer_BUILD_CLI_SUPPORT:BOOL=$(usex cli ON OFF)"
                          "-DSlicer_BUILD_CLI:BOOL=OFF"
                          "-DCMAKE_CXX_STANDARD:STRING=17"
                          "-DSlicer_REQUIRED_QT_VERSION:STRING=5"
                          ;; "-DSlicer_BUILD_DICOM_SUPPORT:BOOL=$(usex DICOM ON OFF)"
                          "-DSlicer_BUILD_ITKPython:BOOL=OFF"
                          ;; "-DSlicer_BUILD_QTLOADABLEMODULES:BOOL=OFF"
                          ;; "-DSlicer_BUILD_QTSCRIPTEDMODULES:BOOL=OFF"
                          ;; "-DSlicer_BUILD_QT_DESIGNER_PLUGINS:BOOL=ON"
                          ;; "-DSlicer_USE_CTKAPPLAUNCHER:BOOL=OFF"
                          "-DSlicer_USE_QtTesting:BOOL=OFF"
                          ;; "-DSlicer_USE_SlicerITK:BOOL=OFF"
                          ;; "-DSlicer_USE_SimpleITK:BOOL=OFF"
                          ;; "-DSlicer_VTK_RENDERING_BACKEND:STRING=OpenGL2"
                          "-DSlicer_VTK_VERSION_MAJOR:STRING=9"
                          "-DSlicer_INSTALL_DEVELOPMENT:BOOL=ON"
                          "-DCMAKE_BUILD_WITH_INSTALL_RPATH:BOOL=ON"
                          "-DTeem_DIR:STRING=/usr/lib64"
                          "-DCTK_INSTALL_QTPLUGIN_DIR:STRING=/usr/lib64/qt5/plugins"
                          "-DQT_PLUGINS_DIR:STRING=/usr/lib64/designer"
                          "-DSlicer_QtPlugins_DIR:STRING=/usr/lib64/designer"
                          "-DjqPlot_DIR:STRING=/usr/share/jqPlot"
                          ;; "-DSlicer_VTK_WRAP_HIERARCHY_DIR:STRING=#{$\x7b;BUILD_DIR\x7d;}#"
                          ;; "-DSlicer_BUILD_vtkAddon:BOOL=OFF"
                          ;; "-DSlicer_USE_SimpleITK:BOOL=OFF"
                          
                          ;; Python
                          ;; "-DPython3_INCLUDE_DIR:FILEPATH="
                          ;; "-DPython3_LIBRARY:FILEPATH="
                          ;; "-DPython3_EXECUTABLE:FILEPATH="
                          ;; "-DVTK_WRAP_PYTHON:BOOL=OFF"
                          )
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;; (add-before 'build 'prepare-build-environment
       ;; (lambda _
       ;; ;; Fix shebang in setup script
       ;; (substitute* "Utilities/SetupForDevelopment.sh"
       ;; (("#!/bin/sh") "#!/bin/bash"))
       
       ;; ;; Create build directory
       ;; (mkdir-p "Slicer-SuperBuild-Debug")
       ;; (chdir "Slicer-SuperBuild-Debug")
       
       ;; ;; Set environment variables
       ;; (setenv "CMAKE_TLS_VERIFY" "0")
       ;; (setenv "EP_EXECUTE_DISABLE_CAPTURE_OUTPUTS" "1")
       ;; #t))
       
       ;; (replace 'install
       ;; (lambda* (#:key outputs #:allow-other-keys)
       ;; (let ((out (assoc-ref outputs "out")))
       ;; (install-file "Slicer-build/Slicer"
       ;; (string-append out "/bin"))
       ;; #t)))
       ))

    (inputs (list libxt
                  eigen
                  expat
                  openssl-3.0
                  git
                  hdf5
                  libffi
                  libjpeg-turbo
                  libxinerama
                  mesa ;libGL equivalent
                  perl
                  python-3.10

                  ;; wayland
                  
                  ;; nss-certs ;; to fix broken certificate validation
                  
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

                  itk-slicer
                  ctk-slicer))
    ;; ("python" ,python-3.14)
    (native-inputs `(("gcc" ,gcc)
                     ("cmake" ,cmake)
                     ("pkg-config" ,pkg-config)))
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

;; (define-public 3d-slicer-dev
;;   (package
;;     (inherit 3d-slicer)
;;     (name "3d-slicer-dev")
;;     (inputs
;;      `(("3d-slicer" ,3d-slicer)
;;        ,@(package-inputs 3d-slicer)))
;;     (native-inputs
;;      `(("debug-tools" ,gdb)
;;        ,@(package-native-inputs 3d-slicer)))
;;     (synopsis "Development environment for 3D Slicer")))
