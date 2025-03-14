(define-module (guix-systole packages vtk)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xml)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages tbb)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages image)
  #:use-module ((gnu packages image-processing)
                #:prefix imgproc:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages xiph))

(define-public vtk-slicer
  (package
    (inherit imgproc:vtk)
    (name "vtk-slicer")
    (version "9.2")
    (source
     (origin

       (method url-fetch)   
       ; URI from VTK version: slicer-v9.2.20230607-1ff325c54-2
       (uri 
            (string-append 
                "https://github.com/Slicer/VTK/archive/"
                "59ec450206012e86d4855bc669800499254bfc77.tar.gz"
            )
        )
       (sha256
        (base32 "0dmibmx170aj59qqqfgh28dhlvkakpdfnzgh4zn4hvn24i9j0id9"))))
    (arguments
     (list
      #:build-type "Release"
      #:configure-flags
      #~'("-DBUILD_TESTING:BOOL=OFF" "-DVTK_USE_EXTERNAL:BOOL=OFF"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_doubleconversion:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_eigen:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_expat:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_freetype:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_gl2ps:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_glew:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_hdf5:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_jpeg:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_jsoncpp:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_libharu:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_libproj:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_libxml2:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_lz4:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_netcdf:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_ogg:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_png:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_sqlite:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_theora:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_tiff:BOOL=ON"
          "-DVTK_MODULE_USE_EXTERNAL_VTK_zlib:BOOL=ON"
          "-DVTK_MODULE_ENABLE_VTK_RenderingExternal:STRING=YES" ;for F3D
          "-DVTK_WRAP_PYTHON:BOOL=OFF"
          "-DVTK_SMP_ENABLE_TBB:BOOL=ON"
          "-DVTK_USE_MPI:BOOL=ON"
          "-DVTK_USE_TK:BOOL=OFF"
          "-DVTK_MODULE_ENABLE_VTK_ChartsCore:STRING=YES"
          "-DVTK_MODULE_ENABLE_VTK_ViewsContext2D:STRING=YES"
          "-DVTK_MODULE_ENABLE_VTK_RenderingContext2D:STRING=YES"
          "-DVTK_MODULE_ENABLE_VTK_RenderingContextOpenGL2:STRING=YES"
          "-DVTK_MODULE_ENABLE_VTK_GUISupportQt:STRING=YES"
          "-DVTK_GROUP_ENABLE_Qt:STRING=YES"
          "-DVTK_QT_VERSION:STRING=5"
          "-DVTK_Group_Qt:BOOL=ON"
          "-DVTK_BUILD_TESTING:BOOL=OFF"
          ;Force value to prevent lib64 from being used on Linux
          "-DCMAKE_INSTALL_LIBDIR:STRING=lib"
          "-DVTK_MODULE_ENABLE_VTK_GUISupportQtQuick:STRING=NO"
          "-DVTK_LEGACY_REMOVE:BOOL=ON"
          "-DVTK_Group_Qt:BOOL=ON"
          "-DVTK_ENABLE_KITS:BOOL=ON")
      #:tests? #f))
    (inputs (modify-inputs (package-inputs imgproc:vtk)
              (append python-pyqt qtbase-5 tbb openmpi)))))

(define-public vtkaddon-slicer
  (package
    (name "vtkaddon-slicer")
    (version "b5aa061")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/Slicer/vtkAddon/archive/b5aa0615a6486b6bdceeb13bd59c2fb9f89cce42.tar.gz")
       (sha256
        (base32 "0wazsirav972mxkawfaw0lpnkylxfr19xjrd5s03blr2kid50a91"))
    (build-system cmake-build-system)
    (outputs '("out"))
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DBUILD_SHARED_LIBS:BOOL=ON"
                               "-DBUILD_TESTING:BOOL=OFF"
                               "-DvtkAddon_INSTALL_NO_DEVELOPMENT:BOOL=OFF"
                               ;; "-DvtkAddon_USE_UTF8=ON"
                               "-DvtkAddon_WRAP_PYTHON=OFF" ;Enable whenever Python support is enabled
                               "-DVTK_WRAP_PYTHON=OFF"
                               ;; "-DvtkAddon_LAUNCH_COMMAND:STRING=" ;; Needs testing
                               "-DCMAKE_BUILD_WITH_INSTALL_RPATH=ON")))

    (inputs (list vtk-slicer
                  eigen
                  expat
                  double-conversion
                  freetype
                  gl2ps
                  glew
                  libjpeg-turbo
                  libxml2
                  jsoncpp
                  libharu
                  libtheora
                  libxml++
                  lz4
                  mpich
                  netcdf
                  proj
                  python-3.10
                  qtbase-5
                  hdf5))

    (native-inputs (list coreutils))
    (home-page "https://github.com/Slicer/vtkAddon/")
    (synopsis
     "General-purpose features that may be integrated into VTK library in the future.")
    (description
     "General-purpose features that may be integrated into VTK library in the future.")
    (license license:bsd-3)))
