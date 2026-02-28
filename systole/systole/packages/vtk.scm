;;
;; Copyright @ 2025 Oslo University Hospital
;;
;; This file is part of SystoleOS.
;;
;; SystoleOS is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later version.
;;
;; SystoleOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with SystoleOS. If not, see <https://www.gnu.org/licenses/>.
;;

(define-module (systole packages vtk)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix utils)
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
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages xiph)
  #:use-module (systole packages maths)
  #:use-module (systole packages))

;; Private non-Python base — used only for (inherit) in vtk-slicer (Python).
(define %vtk-slicer
  (package
    (inherit imgproc:vtk)
    (name "vtk-slicer")
    (version "9.2")
    (source
     (origin
       (method url-fetch)
       ;; URI from VTK version: slicer-v9.2.20230607-1ff325c54-2
       (uri (string-append "https://github.com/Slicer/VTK/archive/"
                           "59ec450206012e86d4855bc669800499254bfc77.tar.gz"))
       (sha256
        (base32 "0dmibmx170aj59qqqfgh28dhlvkakpdfnzgh4zn4hvn24i9j0id9"))))
    (arguments
     `(#:tests? #f
       #:build-type "Release"
       #:configure-flags (list "-DBUILD_TESTING:BOOL=OFF"
                          "-DVTK_USE_EXTERNAL:BOOL=OFF"
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
                          "-DVTK_INSTALL_NO_DEVELOPMENT:BOOL=OFF"

                          ;; Enable VTK modules
                          "-DVTK_MODULE_ENABLE_VTK_ChartsCore:STRING=YES"
                          "-DVTK_MODULE_ENABLE_VTK_ViewsContext2D:STRING=YES"
                          "-DVTK_MODULE_ENABLE_VTK_RenderingContext2D:STRING=YES"
                          "-DVTK_MODULE_ENABLE_VTK_RenderingContextOpenGL2:STRING=YES"
                          "-DVTK_MODULE_ENABLE_VTK_GUISupportQt:STRING=YES"
                          "-DVTK_MODULE_ENABLE_VTK_IOExportGL2PS:STRING=YES"

                          "-DVTK_GROUP_ENABLE_Qt:STRING=YES"
                          "-DVTK_QT_VERSION:STRING=5"
                          "-DVTK_Group_Qt:BOOL=ON"
                          "-DVTK_BUILD_TESTING:BOOL=OFF"
                          ;; Force value to prevent lib64 from being used on Linux
                          "-DCMAKE_INSTALL_LIBDIR:STRING=lib"
                          "-DVTK_MODULE_ENABLE_VTK_GUISupportQtQuick:STRING=NO"
                          "-DVTK_LEGACY_REMOVE:BOOL=ON"
                          "-DVTK_Group_Qt:BOOL=ON"
                          "-DVTK_ENABLE_KITS:BOOL=ON")))
    (inputs (modify-inputs (package-inputs imgproc:vtk)
            (replace "hdf5" hdf5-1.10)
            (replace "netcdf" netcdf-slicer)
              (append python-pyqt qtbase-5 tbb openmpi)))))

;; Python-enabled VTK for use by the Slicer stack.
;; Adds VTK Python wrappers (vtkmodules/*.so) and the Guix python package.
;; CMake's FindPython3 auto-detects the interpreter from PATH when python is in inputs.
(define-public vtk-slicer
  (package
    (inherit %vtk-slicer)
    (name "vtk-slicer")
    (arguments
     (substitute-keyword-arguments (package-arguments %vtk-slicer)
       ((#:configure-flags flags)
        `(cons "-DVTK_WRAP_PYTHON:BOOL=ON"
               (delete "-DVTK_WRAP_PYTHON:BOOL=OFF" ,flags)))))
    (inputs
     (modify-inputs (package-inputs %vtk-slicer)
       (prepend python)))))

(define-public vtk-slicer-source
  ;; Upstream VTK source at the exact commit used by vtk-slicer.
  ;; No build patches — suitable as a read-only reference for code search.
  (package
    (inherit %vtk-slicer)
    (name "vtk-slicer-source")
    (source (origin (inherit (package-source %vtk-slicer))
                    (patches '())))
    (build-system trivial-build-system)
    (native-inputs (list tar gzip))
    (inputs '())
    (propagated-inputs '())
    (native-search-paths '())
    (arguments
     (list #:builder
           (with-imported-modules '((guix build utils))
             #~(begin
                 (use-modules (guix build utils))
                 (setenv "PATH"
                         (string-append #$(file-append tar "/bin") ":"
                                        #$(file-append gzip "/bin")))
                 (mkdir-p #$output)
                 (invoke "tar" "xf" #$source
                         "--strip-components=1" "-C" #$output)))))
    (synopsis "VTK source tree (Slicer variant)")
    (description
     "Upstream VTK source tree at the exact commit used by @code{vtk-slicer},
without any Guix-specific build patches.  Useful as a read-only reference for
code search and API exploration.")))

;; Private non-Python base — used only for (inherit) in vtkaddon (Python).
(define %vtkaddon
  (package
    (name "vtkaddon")
    (version "b5aa061")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/Slicer/vtkAddon/archive/b5aa0615a6486b6bdceeb13bd59c2fb9f89cce42.tar.gz")
       (sha256
        (base32 "0wazsirav972mxkawfaw0lpnkylxfr19xjrd5s03blr2kid50a91"))
       (patches (search-patches
                 "0007-ENH-packages-vtk-use-CMAKE-GNUInstallDirs.patch"
                 "0001-COMP-Fix-Python-detection-in-vtkMacroKitPythonWrap-f.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DBUILD_SHARED_LIBS:BOOL=ON"
                               "-DBUILD_TESTING:BOOL=OFF"
                               "-DvtkAddon_INSTALL_NO_DEVELOPMENT:BOOL=OFF" ;Required to install header files
                               ;; "-DvtkAddon_USE_UTF8:BOOL=ON"
                               "-DvtkAddon_WRAP_PYTHON:BOOL=OFF" ;Enable whenever Python support is enabled
                               ;; "-DvtkAddon_LAUNCH_COMMAND:STRING=" ;; Needs testing
                               "-DCMAKE_BUILD_WITH_INSTALL_RPATH:BOOL=ON")))
    (inputs (list %vtk-slicer
                  eigen
                  expat
                  double-conversion
                  freetype
                  gl2ps
                  glew
                  hdf5-1.10
                  libjpeg-turbo
                  libxml2
                  jsoncpp
                  libharu
                  libtheora
                  libxml++
                  lz4
                  mpich
                  netcdf-slicer
                  proj
                  qtbase-5
                  tbb))
    (home-page "https://github.com/Slicer/vtkAddon/")
    (synopsis
     "General-purpose features that may be integrated into VTK library in the future.")
    (description
     "General-purpose features that may be integrated into VTK library in the future.")
    (license license:bsd-3)))

;; Python-enabled vtkAddon.  Links against vtk-slicer so the generated
;; Python wrappers are consistent with the Python-enabled VTK build.
(define-public vtkaddon
  (package
    (inherit %vtkaddon)
    (name "vtkaddon")
    (arguments
     (substitute-keyword-arguments (package-arguments %vtkaddon)
       ((#:configure-flags flags)
        `(cons "-DvtkAddon_WRAP_PYTHON:BOOL=ON"
               (delete "-DvtkAddon_WRAP_PYTHON:BOOL=OFF" ,flags)))))
    (inputs
     (modify-inputs (package-inputs %vtkaddon)
       (replace "vtk-slicer" vtk-slicer)
       (prepend python)))))
