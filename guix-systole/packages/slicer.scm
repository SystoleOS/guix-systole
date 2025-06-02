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

(define-module (guix-systole packages slicer)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages backup)
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
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
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
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix-systole packages ctk)
  #:use-module (guix-systole packages itk)
  #:use-module (guix-systole packages libarchive)
  #:use-module (guix-systole packages maths)
  #:use-module (guix-systole packages qrestapi)
  #:use-module (guix-systole packages teem)
  #:use-module (guix-systole packages vtk)
  #:use-module (guix-systole packages)
  )

(define-public slicer-5.8
  (package
    (name "slicer-5.8")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/Slicer/Slicer/archive/11eaf62e5a70b828021ff8beebbdd14d10d4f51c.tar.gz")
       (sha256
        (base32 "05rz797ddci3a2m8297zyzv2g2hp6bd6djmwa1n0gbsla8b175bx"))
       (patches (search-patches
                 "0001-COMP-Add-vtk-CommonSystem-component-as-requirement.patch"
                 "0002-COMP-Find-Eigen-required.patch"
                 "0003-COMP-Adapt-to-new-qRestAPI-cmake.patch"
                 "0004-COMP-Hard-code-path-to-teem-library.patch"
                 "0005-COMP-Add-vtk-dependency-to-MRMLWidgets.patch"
                 "0006-COMP-Find-itk-on-non-superbuild.patch"
                 "0007-COMP-Scope-CPack-blocks.patch"
                 "0008-COMP-Remove-LastConfigureStep.patch"
                 "0009-COMP-Fix-path-for-SlicerConfig.cmake-and-SlicerConfi.patch"
                 "0010-ENH-Fix-installation-of-development-files.patch"
                 "0011-ENH-Add-installation-of-Slicer-base-development-file.patch"
                 "0012-ENH-AppLauncher-add-SlicerModules-libdir.patch"
                 ))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:validate-runpath? #f
           #:configure-flags
           #~(list
              ;; Compiler info
              ;; https://stackoverflow.com/a/41361741
              "-DCMAKE_BUILD_TYPE:STRING=Release"
              "-DCMAKE_CXX_COMPILER:STRING=g++"
              "-DCMAKE_C_COMPILER:STRING=gcc"
              "-DCMAKE_CXX_STANDARD:STRING=17"

              ;; Compiler flags
              "-DCMAKE_EXE_LINKER_FLAGS=-pthread"
              "-DSlicer_SUPERBUILD:BOOL=OFF"
              "-DBUILD_TESTING:BOOL=OFF"
              "-DBUILD_SHARED_LIBS:BOOL=ON"
              "-DSlicer_BUILD_EXTENSIONMANAGER_SUPPORT:BOOL=OFF"
              "-DSlicer_DONT_USE_EXTENSION:BOOL=ON"
              "-DSlicer_REQUIRED_QT_VERSION:STRING=5"
              ;; "-DSlicer_BUILD_DICOM_SUPPORT:BOOL=$(usex DICOM ON OFF)"
              "-DSlicer_BUILD_ITKPython:BOOL=OFF"

              ;; CLI
              "-DSlicer_BUILD_CLI:BOOL=OFF"
              "-DSlicer_BUILD_CLI_SUPPORT:BOOL=OFF"

              ;; QT
              "-DSlicer_BUILD_QTLOADABLEMODULES:BOOL=ON"
              "-DSlicer_BUILD_QTSCRIPTEDMODULES:BOOL=OFF"
              "-DSlicer_BUILD_QT_DESIGNER_PLUGINS:BOOL=OFF" ;Turn ON?
              "-DSlicer_USE_QtTesting:BOOL=OFF"
              "-DSlicer_USE_SlicerITK:BOOL=ON"
              "-DSlicer_USE_CTKAPPLAUNCHER:BOOL=ON"
              "-DSlicer_BUILD_WEBENGINE_SUPPORT:BOOL=OFF"
              (string-append "-DQt5_DIR:PATH="
                             #$(this-package-input "qtbase"))
              ;; "-DSlicer_USE_SimpleITK:BOOL=OFF"
              ;; "-DSlicer_VTK_RENDERING_BACKEND:STRING=OpenGL2"
              "-DSlicer_VTK_VERSION_MAJOR:STRING=9"
              "-DSlicer_BUILD_vtkAddon:BOOL=OFF" ;This should be OFF, so Slicer uses the system installed one.

              "-DSlicer_INSTALL_DEVELOPMENT:BOOL=ON"
              "-DSlicer_INSTALL_DEVELOPMENT:BOOL=ON"
              "-DSlicer_USE_TBB:BOOL=ON"

              ;; "-DCTK_INSTALL_QTPLUGIN_DIR:STRING=/usr/lib64/qt5/plugins"
              ;; "-DQT_PLUGINS_DIR:STRING=/usr/lib64/designer"
              ;; "-DSlicer_QtPlugins_DIR:STRING=/usr/lib64/designer"
              ;; "-DjqPlot_DIR:STRING=/usr/share/jqPlot"
              ;; "-DSlicer_VTK_WRAP_HIERARCHY_DIR:STRING=#{$\x7b;BUILD_DIR\x7d;}#"
              ;; "-DSlicer_USE_SimpleITK:BOOL=OFF"
              "-DSlicer_BUILD_DICOM_SUPPORT:BOOL=OFF" ;Disabled as we do not have IODCMTK support yet

              ;; Python
              ;; "-DPython3_INCLUDE_DIR:FILEPATH="
              ;; "-DPython3_LIBRARY:FILEPATH="
              ;; "-DPython3_EXECUTABLE:FILEPATH="
              "-DVTK_WRAP_PYTHON:BOOL=OFF"
              "-DSlicer_USE_PYTHONQT:BOOL=OFF"
              "-DSlicer_USE_SYSTEM_python:BOOL=OFF"

              ;; Other required external modules. These are required, otherwise Slicer tries to download them.
              "-DSlicer_USE_SYSTEM_bzip2:BOOL=ON"
              "-DSlicer_USE_SYSTEM_CTK:BOOL=ON"
              "-DSlicer_USE_SYSTEM_TBB:BOOL=ON"
              "-DSlicer_USE_SYSTEM_teem:BOOL=ON"
              "-DSlicer_USE_SYSTEM_QT:BOOL=ON"
              "-DSlicer_USE_SYSTEM_curl:BOOL=ON"
              "-DSlicer_USE_SYSTEM_DCMTK:BOOL=ON"
              "-DSlicer_USE_SYSTEM_ITK:BOOL=ON"
              "-DSlicer_USE_SYSTEM_LibArchive:BOOL=ON"
              "-DSlicer_USE_SYSTEM_LibFFI:BOOL=ON"
              "-DSlicer_USE_SYSTEM_LZMA:BOOL=ON"
              "-DSlicer_USE_SYSTEM_RapidJSON:BOOL=ON"
              "-DSlicer_USE_SYSTEM_sqlite:BOOL=ON"
              "-DSlicer_USE_SYSTEM_VTK:BOOL=ON"
              "-DSlicer_USE_SYSTEM_zlib:BOOL=ON"

              ;; Hack to fix error "Variable Slicer_WC_LAST_CHANGED_DATE is expected to be defined."
              "-DSlicer_WC_LAST_CHANGED_DATE:STRING=2025-3-2 19:58:36 -0500")
           #:out-of-source? #t
           #:phases
           #~(modify-phases %standard-phases
                            (add-before 'configure 'set-cmake-paths
                                        (lambda* (#:key inputs #:allow-other-keys)
                                          ;; Make 'vtkaddon' discoverable by CMake

                                          (setenv "CMAKE_PREFIX_PATH"
                                                  (string-append (assoc-ref inputs "vtkaddon")
                                                                 "/lib/cmake:"

                                                                 ;; (assoc-ref inputs
                                                                 ;;            "slicerexecutionmodel")
                                                                 ;; "/lib/CMake:"

                                                                 (or (getenv "CMAKE_PREFIX_PATH")
                                                                     ""))) #t))

                            (add-after 'install 'wrap
                                       (lambda* (#:key outputs #:allow-other-keys)
                                                (let* ((out (assoc-ref outputs "out"))
                                                       (slicer-launcher (string-append out "/Slicer"))
                                                       (slicer-wrapper (string-append out "/Slicer-wrapper")))
                                                  ;; Create new wrapper that calls the original launcher with our additions
                                                  (call-with-output-file slicer-wrapper
                                                                         (lambda (port)
                                                                           ; (format port "#!/bin/bash
                                                                           (format port
"export LD_LIBRARY_PATH=\"$HOME/.guix-profile/lib/Slicer-5.8/SlicerModules${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"
exec ~a --additional-module-path \"$HOME/.guix-profile/lib/Slicer-5.8/SlicerModules\" \"$@\"~%"
                                                                                   slicer-launcher)))
                                                  ;; Make the new wrapper executable
                                                  (chmod slicer-wrapper #o755)
                                                  #t)))
                            (add-after 'wrap 'symlink-slicer-applauncher
                                       (lambda* (#:key outputs #:allow-other-keys)
                                                (symlink (string-append (assoc-ref outputs "out")
                                                                        "/Slicer-wrapper")
                                                         (string-append (string-append (assoc-ref
                                                                                         outputs "out")
                                                                                       "/bin/Slicer")))
                                                #t)))))
    (inputs
     (list libxt
           eigen
           expat
           openssl-3.0
           git
           hdf5-1.10
           libffi
           libjpeg-turbo
           libxinerama
           mesa ;libGL equivalent
           rapidjson
           tbb

           ;; QT5
           qtbase-5
           qtmultimedia-5
           qtxmlpatterns
           qtdeclarative-5
           qtsvg-5
           qtx11extras
           ;; qtwebengine-5
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
           libarchive-slicer
           teem-slicer
           vtkaddon
           ;;slicerexecutionmodel
           qrestapi))
    (native-inputs (list pkg-config))
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

;; (define slicerexecutionmodel
;;   (package
;;    (name "slicerexecutionmodel")
;;    (version "2.0.0")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri
;;       "https:///github.com/Slicer/SlicerExecutionModel/archive/91b921bd5977c3384916ba4b03705d87b26067f7.tar.gz")
;;      (sha256
;;       (base32 "10k1m3impplv9hwhxx06wfmvlx9h54avhibv4id1pjlqjn5gjjza"))
;;      (patches (search-patches
;;                "0011-COMP-packages-slicer-Add-GNUInstallDirs-for-execution-model.patch"
;;                "0012-COMP-packages-slicer-Generate-configuration-file-for-execution-model.patch"
;;                "0013-ENH-packages-slicer-Generate-configuration-file-for-install-ModuleDe.patch"
;;                "0014-COMP-packages-slicer-Install-GenerateCLP.cmake-GenerateCLP-and-Gener.patch"))))
;;    (build-system cmake-build-system)
;;    (arguments
;;     `(#:tests? #f
;;       #:parallel-build? #f
;;       #:configure-flags (list "-DBUILD_TESTING:BOOL=OFF"
;;                               "-DSlicerExecutionModel_USE_UTF8:BOOL=ON"
;;                               "-DSlicerExecutionModel_INSTALL_NO_DEVELOPMENT:BOOL=OFF"
;;                               "-DSlicerExecutionModel_DEFAULT_CLI_TARGETS_FOLDER_PREFIX:STRING=Module-")))
;;    (inputs (list
;;             ;; Slicer modules
;;             itk-slicer

;;             ;; Libraries
;;             double-conversion
;;             eigen
;;             expat
;;             freetype
;;             gl2ps
;;             glew
;;             hdf5-1.10
;;             libharu
;;             libjpeg-turbo
;;             libogg
;;             libpng
;;             libtheora
;;             libxml2
;;             lz4
;;             jsoncpp
;;             mpich
;;             netcdf-slicer
;;             proj
;;             qtbase-5
;;             tbb))
;;    (home-page
;;     "https://www.slicer.org/wiki/Documentation/Nightly/Developers/SlicerExecutionModel/")
;;    (synopsis
;;     "The SlicerExecutionModel is a CMake-based project providing
;; macros and associated tools allowing to easily build Slicer CLI (Command line
;; module).")
;;    (description
;;     "It is designed to improve the acceptance and productivity of Slicer
;; application developers. The Execution Model provides a simple mechanism for
;; incorporating command line programs as Slicer modules. These command line
;; modules are self-describing, emitting an XML description of its command line
;; arguments. Slicer uses this XML description to construct a GUI for the module.")
;;    (license license:bsd-2)))
