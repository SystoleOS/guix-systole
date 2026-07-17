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

(define-module (systole packages ctk)
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
  #:use-module (gnu packages)                   ; libxml2, expat
  #:use-module (gnu packages base)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (systole packages itk)
  #:use-module (systole packages maths)
  #:use-module (systole packages pythonqt)
  #:use-module (systole packages vtk)
  #:use-module (systole packages))

;; --------------------------- CTK ---------------------------
;; Private non-Python base — used only for (inherit) in ctk (Python).
(define %ctk
  (package
   (name "ctk")
   (version "0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/commontk/CTK")
           (commit "82cae5781621845486bad2697aed095f04cfbe76")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1hy9hhpsyg8zi8n9hfn4ngjimkw28120f6ra6p756mdvk313imdq"))
     (patches (search-patches
               "0001-ENH-Fix-locating-DCMTK-when-using-CTK.patch"
               "0002-ENH-Add-FindPythonQt.cmake-to-installed-cmake-modules.patch"
               "0003-COMP-Fix-VTK-include-dirs-missing-when-PYTHONQT-USE-VTK.patch"))
     ))
   (build-system cmake-build-system)
   (arguments
    (list #:tests? #f
          #:parallel-build? #t    ; Scheme building using multiple threads.
          #:configure-flags
          #~(list ;; --------------------------- Build Flags ---------------------------
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
             "-DCTK_ENABLE_Python_Wrapping:BOOL=OFF"
             (string-append "-DDCMTK_DIR:PATH="
                            #$(this-package-input "dcmtk")
                            "/lib/cmake/dcmtk")
             )))
   (inputs
    (list qtbase-5
          qttools-5
          qtsvg-5
          dcmtk
          vtk-slicer
          itk-slicer
                                        ; --- Libraries for Visualization VTK widgets and ITK core ---
           hdf5-1.10
           python
           glew
           libtheora
           netcdf-slicer
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
    (home-page "https://github.com/commontk/CTK")
    (synopsis "Common support code for medical imaging and surgical navigation")
   (description
    "The goal of CTK is to support biomedical image computing.  CTK
code is licensed under Apache 2.0.  This means that users of CTK are allowed to
use the code for academic, commercial, or other purposes without paying license
fees or being restricted in their ability to redistribute their code or keep it
private.

CTK works on topics that are not covered by existing toolkits that support the
mutual interest and needs of the CTK community.  The main scope of current CTK
efforts includes the topics DICOM, DICOM Application Hosting, Widgets, and
Plugin Framework.")
   (license license:asl2.0)))

(define-public ctk-source
  ;; Upstream CTK source at the exact commit used by ctk.
  ;; Patches stripped — suitable as a read-only reference for code search.
  (package
    (inherit %ctk)
    (name "ctk-source")
    (source (origin (inherit (package-source %ctk))
                    (patches '())))
    (build-system copy-build-system)
    (inputs '())
    (propagated-inputs '())
    (native-search-paths '())
    (arguments
     ;; The git checkout is already the bare source tree; install it as-is.
     (list #:install-plan #~'(("." "/"))))
    (synopsis "CTK (Common Toolkit) source tree")
    (description
     "Upstream CTK source tree at the exact commit used by @code{ctk},
without any Guix-specific build patches.  Useful as a read-only reference for
code search and API exploration.")))

(define-public ctkapplauncher
  (package
   (name "ctkapplauncher")
   (version "0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/commontk/AppLauncher")
           (commit "8759e03985738b8a8f3eb74ab516ba4e8ef29988")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1d74gkpnl0rn9fbkij111zzwsxir57cgirgr4pj7xlslkiplg26i"))))
   (build-system cmake-build-system)
   (arguments
    (list
     #:tests? #f
     #:configure-flags
     #~(list "-DBUILD_TESTING=OFF"
             "-DCTKAppLauncher_QT_VERSION=5"
             "-DCTKAppLauncher_INSTALL_LauncherLibrary=ON")))
   (inputs (list qtbase-5))
   (home-page "http://www.commontk.org/")
   (synopsis "Launcher that sets up the environment of an executable")
   (description
    "CTKAppLauncher is a simple and small program that sets up the
environment (library paths and environment variables) of an arbitrary
executable before launching it.  It is used by CTK-based applications such
as 3D Slicer.")
   (license license:asl2.0)))

;;;
;;; Factory
;;;

;; make-ctk generates a Python-enabled CTK package linked against a specific
;; VTK, ITK, Python, and PythonQt combination.  All variant deps are captured
;; directly in gexps via #$pkg instead of this-package-input so that spawning
;; new variants requires only changing the keyword arguments.
;;
;; The VTK PythonQt bridge (PYTHONQT_USE_VTK) is always ON:
;; ctkVTKPythonQtWrapperFactory is compiled into libCTKVisualizationVTKCore
;; and Slicer's qSlicerCorePythonManager unconditionally references this symbol.
(define* (make-ctk
          #:key
          (vtk-pkg vtk-slicer)
          (itk-pkg itk-slicer)
          (python-pkg python)
          (python-version "3.11")
          (pythonqt-pkg pythonqt-commontk))
  (package
    (inherit %ctk)
    (name "ctk")
    (arguments
     (list #:tests? #f
           #:parallel-build? #t
           #:configure-flags
           #~(list
              ;; --------------------------- Build Flags ---------------------------
              "-DCTK_USE_GIT_PROTOCOL:BOOL=OFF"
              "-DCTK_SUPERBUILD:BOOL=OFF"
              "-DBUILD_TESTING:BOOL=OFF"
              "-DCTK_INSTALL_LIB_DIR=lib"
              "-DCTK_BUILD_QTDESIGNER_PLUGINS:BOOL=ON"
              ;; -------------------------- CTKdata flags --------------------------
              "-DCTK_ENABLE_CTKDATA:BOOL=OFF"
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
              "-DCTK_LIB_Visualization/VTK/Widgets:BOOL=ON"
              "-DCTK_LIB_Visualization/VTK/Widgets_USE_TRANSFER_FUNCTION_CHARTS:BOOL=ON"
              "-DCTK_LIB_ImageProcessing/ITK/Core:BOOL=ON"
              "-DCTK_LIB_PluginFramework:BOOL=OFF"
              "-DCTK_PLUGIN_org.commontk.eventbus:BOOL=OFF"
              ;; ---------------------- PythonQt wrapping — ON ---------------------
              "-DCTK_LIB_Scripting/Python/Core:BOOL=ON"
              ;; VTK bridge ON: ctkVTKPythonQtWrapperFactory is required by Slicer
              "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_USE_VTK:BOOL=ON"
              "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTCORE:BOOL=ON"
              "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTGUI:BOOL=ON"
              "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTUITOOLS:BOOL=ON"
              "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTNETWORK:BOOL=ON"
              "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTMULTIMEDIA:BOOL=ON"
              ;; QtWebKit absent in Qt 5.6+ → OFF
              "-DCTK_LIB_Scripting/Python/Core_PYTHONQT_WRAP_QTWEBKIT:BOOL=OFF"
              "-DCTK_LIB_Scripting/Python/Widgets:BOOL=ON"
              "-DCTK_ENABLE_Python_Wrapping:BOOL=ON"
              ;; PythonQt location — captured directly from factory arg
              (string-append "-DPYTHONQT_INSTALL_DIR=" #$pythonqt-pkg)
              ;; Python — captured directly from factory arg
              (string-append "-DPython3_EXECUTABLE=" #$python-pkg
                             "/bin/python3")
              (string-append "-DPython3_INCLUDE_DIR=" #$python-pkg
                             "/include/python" #$python-version)
              (string-append "-DPython3_LIBRARY=" #$python-pkg
                             "/lib/libpython" #$python-version ".so")
              ;; DCMTK — still uses this-package-input (always "dcmtk", never varies)
              (string-append "-DDCMTK_DIR:PATH="
                             #$(this-package-input "dcmtk")
                             "/lib/cmake/dcmtk"))))
    (inputs
     (list qtbase-5
           qttools-5
           qtsvg-5
           dcmtk
           vtk-pkg
           itk-pkg
           hdf5-1.10
           python-pkg
           glew
           libtheora
           netcdf-slicer
           proj
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
           tbb
           pythonqt-pkg
           qtmultimedia-5))
    ;; bin/Python/ contains ctk/__init__.py and qt/__init__.py used by Slicer's
    ;; Python environment.  Collected into SLICER_PYTHONPATH (merged into
    ;; PYTHONPATH by Slicer at startup, patch 0046).
    (native-search-paths
     (list (search-path-specification
            (variable "SLICER_PYTHONPATH")
            (files '("bin/Python")))))))

;;;
;;; Public instances
;;;

;; Default CTK — VTK 9.2, ITK 5.4.0, Python 3.11 (Slicer 5.8 stack)
(define-public ctk
  (make-ctk))

;; Slicer 5.10 variant — VTK 9.5, ITK 5.4.4, Python 3.12
(define-public ctk-for-slicer-5.10
  (let ((base (make-ctk #:vtk-pkg vtk-slicer-9.5
                        #:itk-pkg itk-slicer-5.4.4
                        #:python-pkg python-3.12
                        #:python-version "3.12"
                        #:pythonqt-pkg pythonqt-commontk-for-slicer-5.10)))
    (package
      (inherit base)
      (name "ctk-for-slicer-5.10")
      (source
       (origin
         (inherit (package-source base))
         (patches (append (origin-patches (package-source base))
                          (list (search-patch
                                 "0004-COMP-Fix-vtkStdString-to-QString-conversion-for-VTK-9.5.patch")
                                (search-patch
                                 "0005-COMP-Add-currentComponent-API-to-ctkVTKVolumePropert.patch")))))))))

;; Slicer 5.12 variant — CTK 5056664a, VTK 9.6, ITK 5.4.6, Python 3.12.
;;
;; Unlike ctk-for-slicer-5.10 (which reuses the %ctk source pin), this variant
;; bumps the CTK commit to the exact revision referenced by Slicer v5.12.2's
;; SuperBuild/External_CTK.cmake.  The patch list is rebuilt for that commit:
;;   - 0006 replaces 0001 (DCMTK locating; rebased, upstream reordered the
;;     CMakeExternals/DCMTK.cmake blocks).
;;   - 0002 (FindPythonQt.cmake install) still applies unchanged.
;;   - 0007 replaces 0003 (VTK include dirs with PYTHONQT_USE_VTK; rebased,
;;     upstream dropped the VTK 8.90 version conditional).
;;   - 0004 (vtkStdString→QString) is upstreamed at this commit and dropped.
;;   - 0005 (ctkVTKVolumeProperty currentComponent API) still applies unchanged.
(define-public ctk-for-slicer-5.12
  (let ((base (make-ctk #:vtk-pkg vtk-slicer-9.6
                        #:itk-pkg itk-slicer-5.4.6
                        #:python-pkg python-3.12
                        #:python-version "3.12"
                        #:pythonqt-pkg pythonqt-commontk-for-slicer-5.12)))
    (package
      (inherit base)
      (name "ctk-for-slicer-5.12")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/commontk/CTK")
               (commit "5056664a20a3d0a393bb6d91f040525581e0dcdf")))
         (file-name (git-file-name "ctk-for-slicer-5.12" "0.1"))
         (sha256
          (base32 "1izfcc5png7sswprcp8i0ij2mfpsn11h10y9y8a8sbxfnvdpf9a8"))
         (patches (search-patches
                   "0006-ENH-Fix-locating-DCMTK-when-using-CTK-5.12.patch"
                   "0002-ENH-Add-FindPythonQt.cmake-to-installed-cmake-modules.patch"
                   "0007-COMP-Fix-VTK-include-dirs-missing-when-PYTHONQT-USE-VTK-5.12.patch"
                   "0005-COMP-Add-currentComponent-API-to-ctkVTKVolumePropert.patch")))))))
