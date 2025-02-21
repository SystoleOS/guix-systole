(define-module (systole-vtk)
	       ;#:use-module (guix packages)
	       ;#:use-module (guix git-download)
	       ;#:use-module (guix build-system cmake)
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
	       #:use-module (srfi srfi-1))

(define-public vtk
	       (package
		 (name "vtk")
		 (version "9.2.20230607-1ff325c54-2")
		 (source (origin
			   (method git-fetch)
			   (uri (git-reference
				  (url "https://github.com/slicer/VTK.git")
				  (commit "492821449a5f2a9a1f5c73c3c6dd4389f1059d66")))
			   ;(file-name (git-file-name name version))
			   (sha256
			     (base32
			       "1s8vd34nhrgnw1bf9zhfn062d53fwq3csjfwvm7lxcr5a8lvkizx"))
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
		 (properties `((release-monitoring-url . "https://vtk.org/download/")))
		 (build-system cmake-build-system)
		 (arguments
		   (list #:build-type "Release"           ;Build without '-g' to save space.
			 #:configure-flags
			 #~'( ;;"-DBUILD_TESTING:BOOL=TRUE"  ;not honored
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
			      "-DVTK_PYTHON_VERSION:STRING=3"

			      "-DVTK_SMP_ENABLE_OPENNMP=ON"
			      "-DVTK_SMP_ENABLE_TBB=ON"
			      "-DVTK_USE_MPI=ON"
			      #$@(if (target-riscv64?)
				   '("-DCMAKE_SHARED_LINKER_FLAGS=-latomic"
				     "-DCMAKE_EXE_LINKER_FLAGS=-latomic")
				   '()))

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
			 zlib))
		 (propagated-inputs
		   ;; VTK's 'VTK-vtk-module-find-packages.cmake' calls
		   ;; 'find_package(THEORA)', which in turns looks for libogg.  Likewise for
		   ;; TBB.
		   (list libogg
			 tbb))
		 (home-page "https://vtk.org/")
		 (synopsis "Libraries for 3D computer graphics")
		 (description
		   "The Visualization Toolkit (VTK) is a C++ library for 3D computer graphics,
		   image processing and visualization.  It supports a wide variety of
		   visualization algorithms including: scalar, vector, tensor, texture, and
		   volumetric methods; and advanced modeling techniques such as: implicit
		   modeling, polygon reduction, mesh smoothing, cutting, contouring, and Delaunay
		   triangulation.  VTK has an extensive information visualization framework, has
		   a suite of 3D interaction widgets, supports parallel processing, and
		   integrates with various databases on GUI toolkits such as Qt and Tk.")
		   (license license:bsd-3)))
