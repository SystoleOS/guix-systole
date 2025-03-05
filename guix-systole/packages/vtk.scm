(define-module (guix-systole packages vtk)
  #:use-module ((guix licenses)
                #:prefix license:) ;)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (gnu packages qt)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module ((gnu packages image-processing)
                #:prefix imgproc:))

(define-public vtk-slicer
  (package
    (inherit imgproc:vtk)
    (name "vtk-slicer")
    (version "9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com"
                           "/Slicer/VTK/archive/refs/heads/"
                           "slicer-v9.2.20230607-1ff325c54-2.zip"))
       (sha256
        (base32 "15f3ryx945p7d95asic9gnpz8jmais2gv1gh6vfcxm7malsl8m85"))))
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
          "-DVTK_WRAP_PYTHON:BOOL=ON"
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
              (append python-pyqt qtbase-5)))))
