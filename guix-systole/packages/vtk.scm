(define-module (guix-systole packages vtk)
	       #:use-module ((guix licenses) #:prefix license:);)
	       #:use-module (guix packages)
	       #:use-module (guix gexp)
	       #:use-module (guix utils)
	       #:use-module (guix download)
	       #:use-module (guix git-download)
	       #:use-module (guix build-system qt)
	       #:use-module (guix build-system cmake)
	       #:use-module (guix build-system gnu)
	       #:use-module (guix build-system meson)
	       #:use-module (guix build-system python)
	       #:use-module (guix build-system pyproject)
	       #:use-module (gnu packages)
	       #:use-module (gnu packages algebra)
	       #:use-module (gnu packages base)
	       #:use-module (gnu packages bash)
	       #:use-module (gnu packages bison)
	       #:use-module (gnu packages boost)
	       #:use-module (gnu packages check)
	       #:use-module (gnu packages compression)
	       #:use-module (gnu packages cpp)
	       #:use-module (gnu packages curl)
	       #:use-module (gnu packages docbook)
         #:use-module (gnu packages documentation)
         #:use-module (gnu packages flex)
         #:use-module (gnu packages fontutils)
         #:use-module (gnu packages game-development)
         #:use-module (gnu packages gcc)
         #:use-module (gnu packages gd)
         #:use-module (gnu packages geo)
         #:use-module (gnu packages ghostscript)
         #:use-module (gnu packages gimp)
         #:use-module (gnu packages gl)
         #:use-module (gnu packages glib)
         #:use-module (gnu packages gnome)
         #:use-module (gnu packages graphics)
         #:use-module (gnu packages graphviz)
         #:use-module (gnu packages gstreamer)
         #:use-module (gnu packages gtk)
         #:use-module (gnu packages icu4c)
         #:use-module (gnu packages image)
         #:use-module (gnu packages imagemagick)
         #:use-module (gnu packages linux)
         #:use-module (gnu packages maths)
         #:use-module (gnu packages mpi)
         #:use-module (gnu packages opencl)
         #:use-module (gnu packages pdf)
         #:use-module (gnu packages perl)
         #:use-module (gnu packages photo)
         #:use-module (gnu packages pkg-config)
         #:use-module (gnu packages pretty-print)
         #:use-module (gnu packages protobuf)
         #:use-module (gnu packages python)
         #:use-module (gnu packages python-build)
         #:use-module (gnu packages python-check)
         #:use-module (gnu packages python-science)
         #:use-module (gnu packages python-xyz)
         #:use-module (gnu packages qt)
         #:use-module (gnu packages sdl)
         #:use-module (gnu packages serialization)
         #:use-module (gnu packages sphinx)
         #:use-module (gnu packages sqlite)
         #:use-module (gnu packages swig)
         #:use-module (gnu packages tbb)
         #:use-module (gnu packages textutils)
         #:use-module (gnu packages tls)
         #:use-module (gnu packages version-control)
         #:use-module (gnu packages video)
         #:use-module (gnu packages xiph)
         #:use-module (gnu packages xml)
         #:use-module (gnu packages xorg)
         #:use-module (ice-9 match)
         #:use-module (srfi srfi-1)
         #:use-module ((gnu packages image-processing) #:prefix imgproc:))

(define-public vtk
               (package
                 (inherit imgproc:vtk)
                 (version "slicer-9.2.20230607-1ff325c54-2")
                 (source (origin
                           (method git-fetch)
                           (uri (git-reference
                                  (url "https://github.com/slicer/VTK.git")
                                  (commit "492821449a5f2a9a1f5c73c3c6dd4389f1059d66")))
                           (sha256
                             (base32
                               "1lnfx7qf7l64klysv98qsik4fnd8hb538w7ppvxdv4kxxc0f5k6q"))
                           (modules '((guix build utils)))
                           (snippet
                             '(begin
                                (for-each
                                  (lambda (dir)
                                    (delete-file-recursively
                                      (string-append "ThirdParty/" dir "/vtk" dir)))
                                  ;; pugixml depended upon unconditionally
                                  '("doubleconversion" "eigen" "expat" "freetype" "gl2ps"
                                    "glew" "hdf5" "jpeg" "jsoncpp" "libharu" "libproj"
                                    "libxml2" "lz4" "netcdf" "ogg" "png" "sqlite" "theora"
                                    "tiff" "zlib"))
                                (substitute* "IO/ExportPDF/vtkPDFContextDevice2D.cxx"
                                             (("\\bHPDF_UINT16 (noPen|dash|dot|denseDot|dashDot|dashDotDot)\\b"
                                               _ var)
                                              (string-append "HPDF_REAL " var)))))))
                 (build-system cmake-build-system)
                 (arguments
                   (list #:build-type "Release"           ;Build without '-g' to save space.
                         #:configure-flags
                         #~'( ;;"-DBUILD_TESTING=TRUE"  ;not honored
                              "-DVTK_USE_EXTERNAL=OFF"           ;default
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_doubleconversion=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_eigen=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_expat=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_freetype=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_gl2ps=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_glew=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_hdf5=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_jpeg=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_jsoncpp=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_libharu=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_libproj=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_libxml2=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_lz4=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_netcdf=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_ogg=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_png=ON"
                              ;;"-DVTK_MODULE_USE_EXTERNAL_VTK_pugixml=ON" ;breaks IO/CityGML
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_sqlite=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_theora=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_tiff=ON"
                              "-DVTK_MODULE_USE_EXTERNAL_VTK_zlib=ON"
                              "-DVTK_MODULE_ENABLE_VTK_RenderingExternal=YES" ;for F3D
                              "-DVTK_WRAP_PYTHON=ON"
                              "-DVTK_PYTHON_VERSION=3"

                              ; "-DVTK_SMP_ENABLE_OPENNMP=ON"
                              "-DVTK_SMP_ENABLE_TBB=ON"
                              "-DVTK_USE_MPI=ON"
                              ;   #$@(if (target-riscv64?)
                              ; '("-DCMAKE_SHARED_LINKER_FLAGS=-latomic"
                              ;   "-DCMAKE_EXE_LINKER_FLAGS=-latomic")
                              ; '())

                              ; Optional dependencies for PythonQt
                              ; "-DPYTHON_EXECUTABLE=${PYTHON_EXECUTABLE}"
                              ; "-DPYTHON_INCLUDE_DIR=${PYTHON_INCLUDE_DIR}"
                              ; "-DPYTHON_LIBRARY=${PYTHON_LIBRARY}"
                              ; "-DPython3_ROOT_DIR=${Python3_ROOT_DIR}"
                              ; "-DPython3_INCLUDE_DIR=${Python3_INCLUDE_DIR}"
                              ; "-DPython3_LIBRARY=${Python3_LIBRARY}"
                              ; "-DPython3_LIBRARY_DEBUG=${Python3_LIBRARY_DEBUG}"
                              ; "-DPython3_LIBRARY_RELEASE=${Python3_LIBRARY_RELEASE}"
                              ; "-DPython3_EXECUTABLE=${Python3_EXECUTABLE}"
                              "-DVTK_USE_TK=OFF"

                              ; External project optional VTK9 CMake cache args
                              ; "-DVTK_MODULE_ENABLE_VTK_SplineDrivenImageSlicer=YES"
                              "-DVTK_MODULE_ENABLE_VTK_ChartsCore=YES"
                              "-DVTK_MODULE_ENABLE_VTK_ViewsContext2D=YES"
                              "-DVTK_MODULE_ENABLE_VTK_RenderingContext2D=YES"
                              "-DVTK_MODULE_ENABLE_VTK_RenderingContextOpenGL2=YES"

                              "-DVTK_MODULE_ENABLE_VTK_GUISupportQt=YES"
                              "-DVTK_GROUP_ENABLE_Qt=YES"

                              "-DVTK_QT_VERSION=5"
                              "-DVTK_Group_Qt=ON"
                              ; "-DQt5_DIR=${Qt5_DIR}"

                              ; If use TBB
                              ; "-DTBB_DIR=${TBB_DIR}"

                              ; if UNIX and not APPLE
                              ; how to replicate the following?
                              #|find_package(Fontconfig QUIET)
                              if(Fontconfig_FOUND)
                              list(APPEND EXTERNAL_PROJECT_OPTIONAL_CMAKE_CACHE_ARGS
                                          -DModule_vtkRenderingFreeTypeFontConfig=ON
                                          )
                              endif()|#
                              ; "-DOpenGL_GL_PREFERENCE=${OpenGL_GL_PREFERENCE}"

                              "-DVTK_BUILD_TESTING=OFF"
                              ; "-DVTK_MODULE_ENABLE_VTK_AcceleratorsVTKm=NO"
                              "-DCMAKE_INSTALL_LIBDIR=lib" ;Force value to prevent lib64 from being used on Linux
                              "-DVTK_MODULE_ENABLE_VTK_GUISupportQtQuick=NO"

                              ; "-DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}"
                              ; "-DCMAKE_CXX_FLAGS=${ep_common_cxx_flags}"
                              ; "-DCMAKE_C_COMPILER=R}"
                              ; "-DCMAKE_C_FLAGS=${ep_common_c_flags}"
                              ; "-DCMAKE_CXX_STANDARD=${CMAKE_CXX_STANDARD}"
                              ; "-DCMAKE_CXX_STANDARD_REQUIRED=${CMAKE_CXX_STANDARD_REQUIRED}"
                              ; "-DCMAKE_CXX_EXTENSIONS=${CMAKE_CXX_EXTENSIONS}"
                              ; "-DVTK_DEBUG_LEAKS=${VTK_DEBUG_LEAKS}"
                              "-DVTK_LEGACY_REMOVE=ON"
                              ; "-DVTK_USE_RPATH=ON # Unused"
                              ; "-DVTK_WRAP_PYTHON=${VTK_WRAP_PYTHON}"
                              ; "-DVTK_INSTALL_RUNTIME_DIR=${Slicer_INSTALL_BIN_DIR}"
                              ; "-DVTK_INSTALL_LIBRARY_DIR=${Slicer_INSTALL_LIB_DIR}"
                              ; "-DVTK_INSTALL_ARCHIVE_DIR=${Slicer_INSTALL_LIB_DIR}"
                              "-DVTK_Group_Qt=ON"
                              ; "-DVTK_USE_SYSTEM_ZLIB=ON"
                              ; "-DZLIB_ROOT=${ZLIB_ROOT}"
                              ; "-DZLIB_INCLUDE_DIR=${ZLIB_INCLUDE_DIR}"
                              ; "-DZLIB_LIBRARY=${ZLIB_LIBRARY}"
                              "-DVTK_ENABLE_KITS=ON"
                              ; "-DVTK_SMP_IMPLEMENTATION_TYPE=${Slicer_VTK_SMP_IMPLEMENTATION_TYPE}"
                              )

                         #:phases
                         #~(modify-phases %standard-phases
                                          (add-after 'unpack 'clear-reference-to-compiler
                                                     (lambda _
                                                       (define (choose . files)
                                                         (let loop ((files files))
                                                           (if (null? files)
                                                             #f
                                                             (if (file-exists? (car files))
                                                               (car files)
                                                               (loop (cdr files))))))

                                                       ;; Do not retain a reference to GCC.
                                                       (substitute* (choose
                                                                      "Common/Core/vtkBuild.h.in" ;dummy >=v9.3
                                                                      "Common/Core/vtkConfigureDeprecated.h.in" ;v9.x
                                                                      "Common/Core/vtkConfigure.h.in") ;v7.x
                                                                    (("@CMAKE_CXX_COMPILER@") "c++")))))

                         #:tests? #f))                          ;XXX: test data not included
                 (inputs
                   (list double-conversion
                         eigen
                         expat
                         freetype
                         gl2ps
                         glew
                         glu
                         hdf5
                         libharu
                         libjpeg-turbo
                         jsoncpp
                         libtheora
                         libx11
                         libxml2
                         libxt
                         lz4
                         mesa
                         netcdf
                         libpng
                         libtiff
                         openmpi
                         proj
                         python
                         ;("pugixml" ,pugixml)
                         sqlite
                         xorgproto
                         zlib

                         python-pyqt

                         qtbase-5))
                 (propagated-inputs
                   (list libogg
                         tbb))))

; vtk
