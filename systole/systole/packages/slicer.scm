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

(define-module (systole packages slicer)
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
  #:use-module (guix utils)
  #:use-module (systole packages ctk)
  #:use-module (systole packages itk)
  #:use-module (systole packages libarchive)
  #:use-module (systole packages maths)
  #:use-module (systole packages qrestapi)
  #:use-module (systole packages teem)
  #:use-module (systole packages vtk)
  #:use-module (systole packages)
  #:use-module (srfi srfi-1)
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
                 "0013-ENH-Add-link-directories.patch"
                 "0014-ENH-Add-link-libraries-to-SlicerMacroBuildModuleLogi.patch"
                 "0015-ENH-add-Qt5-and-loadable-modules-includes-for-non-su.patch"
                 "0016-ENH-improve-CMake-support-for-system-installed-Slice.patch"
                 "0017-ENH-Fix-file-glob-pattern-for-header-installation.patch"
                 "0018-ENH-Install-CMake-template-files-alongside-cmake-mod.patch"
                 "0019-ENH-Add-LINK_DIRECTORIES-support-to-SlicerMacroBuild.patch"
                 "0020-ENH-Add-Slicer_Libs_INCLUDE_DIRS-to-SlicerMacroBuil.patch"
                 "0021-COMP-Add-Qt5-Xml-to-SlicerMacroBuildModuleQtLibrary.patch"
                 "0022-COMP-Add-CTKVisualizationVTKWidgets-to-SlicerMacroB.patch"
                 "0023-COMP-Add-VTK-CommonCore-to-SlicerMacroBuildModuleLog.patch"
                 "0024-COMP-Add-Qt5-Widgets-Xml-CTK-to-SlicerMacroBuildLoad.patch"
                 "0025-COMP-Fix-empty-Slicer_INSTALL_QTLOADABLEMODULES_INCL.patch"
                 "0026-COMP-Add-qSlicerBaseQTCore-to-standalone-module-widg.patch"
                 "0027-COMP-Add-EXTRA_MODULE_LIB_DIRS-to-module-build-macro.patch"
                 "0028-COMP-Add-qMRMLWidgets-to-standalone-module-widgets-l.patch"
                 "0029-COMP-Add-Slicer_GUI_LIBRARY-fallback-to-SlicerMacroB.patch"
                 "0030-COMP-Add-Slicer-root-include-dir-to-SlicerMacroBuild.patch"
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
              "-DSlicer_BUILD_QTLOADABLEMODULES:BOOL=OFF"
              "-DSlicer_BUILD_QTSCRIPTEDMODULES:BOOL=OFF"
              "-DSlicer_BUILD_QT_DESIGNER_PLUGINS:BOOL=OFF" ;Turn ON?
              "-DSlicer_USE_QtTesting:BOOL=OFF"
              "-DSlicer_USE_SlicerITK:BOOL=ON"
              "-DSlicer_USE_CTKAPPLAUNCHER:BOOL=OFF"
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
                                                       (slicer-real (string-append out "/bin/SlicerApp-real"))
                                                       (slicer-wrapper (string-append out "/Slicer-wrapper")))
                                                  ;; Replace the CTK app launcher with a plain shell
                                                  ;; wrapper.  The CTK launcher's only job on Guix is to
                                                  ;; set a handful of environment variables before
                                                  ;; exec-ing bin/SlicerApp-real; we do that here instead.
                                                  (call-with-output-file slicer-wrapper
                                                                         (lambda (port)
                                                                           (format port
"#!/bin/sh
# Guix wrapper for 3D Slicer – replaces the CTK application launcher.

# Slicer uses SLICER_HOME to locate resources, modules, Python, etc.
export SLICER_HOME=~a

# SlicerApp-real's RUNPATH does not include lib/Slicer-5.8/ where the
# core Slicer shared libraries live; add them explicitly.
export LD_LIBRARY_PATH=~a/lib/Slicer-5.8:~a/lib/Slicer-5.8/qt-loadable-modules${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}

# Required for QtWebEngine in environments without user namespaces.
export QTWEBENGINE_DISABLE_SANDBOX=1

# Translate SLICER_ADDITIONAL_MODULE_PATHS (colon-separated list set by
# Guix when module packages share a profile) into --additional-module-path
# arguments recognised by 3D Slicer.
module_path_args=\"\"
if [ -n \"${SLICER_ADDITIONAL_MODULE_PATHS}\" ]; then
  old_IFS=\"$IFS\"
  IFS=':'
  for path in ${SLICER_ADDITIONAL_MODULE_PATHS}; do
    [ -n \"$path\" ] && module_path_args=\"${module_path_args} --additional-module-path ${path}\"
  done
  IFS=\"${old_IFS}\"
fi
exec ~a ${module_path_args} \"$@\"~%"
                                                                                   out out out
                                                                                   slicer-real)))
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
           qtxmlpatterns-5
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
    ;; Each Slicer module package (e.g. slicer-volumes-5.8) installs its
    ;; shared library under lib/Slicer-5.8/qt-loadable-modules/.  Guix
    ;; collects those directories into SLICER_ADDITIONAL_MODULE_PATHS when
    ;; module packages share a profile with slicer-5.8.  The wrapper script
    ;; installed by the 'wrap phase translates this variable into
    ;; --additional-module-path arguments for 3D Slicer.
    (native-search-paths
     (list (search-path-specification
            (variable "SLICER_ADDITIONAL_MODULE_PATHS")
            (files '("lib/Slicer-5.8/qt-loadable-modules")))))
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

;;;
;;; Factory for standalone Slicer loadable-module packages
;;;

;; Do NOT use (inherit slicer-5.8) in the packages produced here: Guix
;; forbids listing a package as an input when it also appears in the
;; inheritance chain.  We reuse only the source origin and build each
;; sub-module as a fully independent package that depends on an installed
;; slicer-5.8.
;;
;; We reference #$slicer-5.8 directly in gexps rather than using the
;; this-package-input macro, which is only available syntactically inside
;; a (package …) form.  Both resolve to the same store path.
(define* (make-slicer-loadable-module
          #:key
          name            ; package name string, e.g. "slicer-volumes-5.8"
          module-subdir   ; source sub-directory, e.g. "Volumes"
          patches         ; list of patch filename strings
          synopsis        ; one-line synopsis string
          description     ; multi-line description string
          ;; Extra packages added to inputs *before* slicer-5.8's own inputs.
          ;; Use this to declare inter-module build-time dependencies
          ;; (e.g. slicer-terminologies-5.8 for SubjectHierarchy).
          (extra-inputs '())
          ;; A gexp that evaluates to a (possibly empty) list of extra
          ;; CMake -D flags.  Defaults to the empty list.
          (extra-configure-flags #~'()))
  (package
   (name name)
   (version (package-version slicer-5.8))
   (source
    (origin
     (inherit (package-source slicer-5.8))
     (patches (map search-patch patches))))
   (build-system cmake-build-system)
   (arguments
    (list #:tests? #f
          #:validate-runpath? #f
          #:out-of-source? #t
          #:configure-flags
          ;; Append any caller-supplied flags after the common base flags.
          #~(append
             (list "-DCMAKE_BUILD_TYPE:STRING=Release"
                   "-DBUILD_TESTING:BOOL=OFF"
                   ;; Install headers so downstream modules can use this one
                   ;; as a build input.
                   "-DSlicer_INSTALL_DEVELOPMENT:BOOL=ON"
                   ;; Point cmake directly at Slicer's config directory.
                   ;; Avoids CMAKE_PREFIX_PATH list-separator ambiguity
                   ;; (CMake -D variables use ";" while env vars use ":").
                   (string-append "-DSlicer_DIR="
                                  #$slicer-5.8
                                  "/lib/Slicer-5.8"))
             #$extra-configure-flags)
          #:phases
          ;; Build only the named sub-directory, not the Slicer root.
          #~(modify-phases %standard-phases
              (replace 'configure
                (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
                  (let* ((source (getcwd))
                         (out (assoc-ref outputs "out")))
                    (apply invoke "cmake"
                           "-S" (string-append source "/Modules/Loadable/"
                                               #$module-subdir)
                           "-B" "build"
                           (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                           configure-flags)
                    (chdir "build")
                    #t))))))
   ;; UseSlicer.cmake transitively requires all of slicer-5.8's build-time
   ;; libraries (Qt5, VTK, ITK, etc.) to be present in the build
   ;; environment, not just slicer-5.8 itself.  We therefore start from
   ;; slicer-5.8's input list and prepend slicer-5.8 so cmake can locate
   ;; SlicerConfig.cmake.  Any extra-inputs (other standalone modules this
   ;; module depends on) are also prepended so cmake can link against them.
   (inputs (fold (lambda (pkg acc)
                   (modify-inputs acc (prepend pkg)))
                 (modify-inputs (package-inputs slicer-5.8)
                   (prepend slicer-5.8))
                 extra-inputs))
   (home-page (package-home-page slicer-5.8))
   (synopsis synopsis)
   (description description)
   (license (package-license slicer-5.8))))

(define-public slicer-terminologies-5.8
  (make-slicer-loadable-module
   #:name "slicer-terminologies-5.8"
   #:module-subdir "Terminologies"
   #:patches (list "terminologies/0001-ENH-Add-missing-dependencies-for-standalone-build-of.patch"
                   "terminologies/0002-COMP-Restore-Slicer_INSTALL_QTLOADABLEMODULES_LIB_DI.patch")
   #:synopsis "3D Slicer Terminologies loadable module"
   #:description
   "The Terminologies loadable module extracted from 3D Slicer.  It provides
DICOM-based anatomical and segmentation terminology support (category, type,
modifier look-ups backed by JSON terminology files) and is built from the
@file{Modules/Loadable/Terminologies} subtree of the Slicer source tree."
   ;; RapidJSON is used by the Terminologies Logic but is not re-exported
   ;; by Slicer, so point cmake at its config directory explicitly.
   ;; rapidjson is already in slicer-5.8's inputs so it is present in the
   ;; build environment; we only need to tell cmake where to find it.
   #:extra-configure-flags
   #~(list (string-append "-DRapidJSON_DIR="
                          #$rapidjson
                          "/lib/cmake/RapidJSON"))))

(define-public slicer-subjecthierarchy-5.8
  (make-slicer-loadable-module
   #:name "slicer-subjecthierarchy-5.8"
   #:module-subdir "SubjectHierarchy"
   #:patches (list "subjecthierarchy/0001-ENH-Add-standalone-build-support-for-SubjectHierarch.patch"
                   "subjecthierarchy/0002-COMP-Add-vtkSlicerTerminologiesModuleLogic-include-d.patch"
                   "subjecthierarchy/0003-COMP-Remove-vtkSlicerVolumesModuleLogic-dep-from-sta.patch"
                   "subjecthierarchy/0004-COMP-Add-vtkSlicerTerminologiesModuleLogic-to-Subjec.patch")
   #:synopsis "3D Slicer SubjectHierarchy loadable module"
   #:description
   "The SubjectHierarchy loadable module extracted from 3D Slicer.  It provides
a hierarchical data model for MRML scene items together with a subject
hierarchy tree view, plugin infrastructure for per-node context menus, and
default plugins for cloning, folding, opacity, visibility, and registration
actions.  Built from the @file{Modules/Loadable/SubjectHierarchy} subtree of
the Slicer source tree."
   ;; SubjectHierarchy Widgets link against Terminologies module.
   #:extra-inputs (list slicer-terminologies-5.8)
   #:extra-configure-flags
   #~(list
      ;; Include dirs for qSlicerTerminologyItemDelegate.h and friends.
      (string-append
       "-DqSlicerTerminologiesModuleWidgets_INCLUDE_DIRS="
       #$slicer-terminologies-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerTerminologiesModuleWidgets")
      ;; Include dirs for vtkSlicerTerminologiesModuleLogic.h and
      ;; vtkSlicerTerminologyEntry.h (used directly by Widgets source files).
      (string-append
       "-DvtkSlicerTerminologiesModuleLogic_INCLUDE_DIRS="
       #$slicer-terminologies-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerTerminologiesModuleLogic")
      ;; Link directory for the Terminologies module libraries.
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-terminologies-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-colors-5.8
  (make-slicer-loadable-module
   #:name "slicer-colors-5.8"
   #:module-subdir "Colors"
   #:patches (list "colors/0001-ENH-Add-standalone-build-support-for-Colors-module.patch"
                   "colors/0002-COMP-Add-VTK-RenderingAnnotation-dependency-to-Color.patch")
   #:synopsis "3D Slicer Colors loadable module"
   #:description
   "The Colors loadable module extracted from 3D Slicer.  It provides color
table management, color legend display nodes and widgets (including a scalar
bar actor), and a subject hierarchy plugin for color legends.  Built from the
@file{Modules/Loadable/Colors} subtree of the Slicer source tree."
   ;; Colors SubjectHierarchyPlugins link against SubjectHierarchy.
   #:extra-inputs (list slicer-subjecthierarchy-5.8)
   #:extra-configure-flags
   #~(list
      ;; Include dirs for qSlicerSubjectHierarchyAbstractPlugin.h and friends.
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      ;; Include dirs for vtkSlicerSubjectHierarchyModuleLogic.h.
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      ;; Link directory for the SubjectHierarchy module libraries.
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-annotations-5.8
  (make-slicer-loadable-module
   #:name "slicer-annotations-5.8"
   #:module-subdir "Annotations"
   #:patches (list "annotations/0001-ENH-Add-standalone-CMake-build-support.patch")
   #:synopsis "3D Slicer Annotations loadable module"
   #:description
   "The Annotations loadable module extracted from 3D Slicer.  It provides
legacy annotation support for fiducials, rulers, and ROIs with backward
compatibility for older Slicer scenes.  This module has been largely superseded
by the Markups module but remains available for loading legacy annotation data.
Built from the @file{Modules/Loadable/Annotations} subtree of the Slicer
source tree."
   ;; Annotations likely depends on SubjectHierarchy for plugins
   #:extra-inputs (list slicer-subjecthierarchy-5.8)
   #:extra-configure-flags
   #~(list
      ;; Include dirs for qSlicerSubjectHierarchyAbstractPlugin.h and friends.
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      ;; Include dirs for vtkSlicerSubjectHierarchyModuleLogic.h.
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      ;; Link directory for the SubjectHierarchy module libraries.
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-markups-5.8
  (make-slicer-loadable-module
   #:name "slicer-markups-5.8"
   #:module-subdir "Markups"
   #:patches (list "markups/0001-ENH-Add-standalone-build-support-for-Markups-module.patch"
                   "markups/0002-COMP-Add-vtkAddon-and-ITKCommon-to-Markups-MRML-link.patch"
                   "markups/0003-COMP-Add-Annotations-module-MRML-include-directory.patch"
                   "markups/0004-ENH-Add-Annotations-module-MRML-include-directory-de.patch"
                   "markups/0005-ENH-Add-LINK_DIRECTORIES-to-module-logic-build.patch"
                   "markups/0006-ENH-Add-vtkSegmentationCore-dependency-to-SubjectHie.patch"
                   "markups/0007-ENH-Add-vtkSlicerTerminologiesModuleLogic-dependency.patch"
                   "markups/0008-ENH-Add-Annotations-module-logic-include-directory.patch"
                   )
   #:synopsis "3D Slicer Markups loadable module"
   #:description
   "The Markups loadable module extracted from 3D Slicer.  It provides
annotation and markup support including fiducials, lines, angles, curves,
planes, and ROIs, along with a subject hierarchy plugin and legacy Annotations
format reader.  Built from the @file{Modules/Loadable/Markups} subtree of
the Slicer source tree."
   ;; SubjectHierarchyPlugins depend on SubjectHierarchy, Terminologies and
   ;; Colors; the top-level module also links against Colors.
   #:extra-inputs (list slicer-subjecthierarchy-5.8
                        slicer-annotations-5.8
                        slicer-terminologies-5.8
                        slicer-colors-5.8)
   #:extra-configure-flags
   #~(list
      ;; RapidJSON is used by Markups/MRML but not re-exported by Slicer.
      (string-append "-DRapidJSON_DIR="
                     #$rapidjson
                     "/lib/cmake/RapidJSON")
      ;; Include dirs for SubjectHierarchy (SubjectHierarchyPlugins + top-level).
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      ;; Include dirs for Terminologies (SubjectHierarchyPlugins).
      (string-append
       "-DqSlicerTerminologiesModuleWidgets_INCLUDE_DIRS="
       #$slicer-terminologies-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerTerminologiesModuleWidgets")
      (string-append
       "-DvtkSlicerTerminologiesModuleLogic_INCLUDE_DIRS="
       #$slicer-terminologies-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerTerminologiesModuleLogic")
      ;; Include dirs for Colors (top-level module).
      (string-append
       "-DvtkSlicerColorsModuleLogic_INCLUDE_DIRS="
       #$slicer-colors-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerColorsModuleLogic")
      (string-append
       "-DvtkSlicerColorsModuleMRML_INCLUDE_DIRS="
       #$slicer-colors-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerColorsModuleMRML")
      (string-append
       "-DqSlicerColorsModuleWidgets_INCLUDE_DIRS="
       #$slicer-colors-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerColorsModuleWidgets")
      ;; Include dirs for Annotations (top-level module).
      (string-append
       "-DvtkSlicerAnnotationsModuleMRML_INCLUDE_DIRS="
       #$slicer-annotations-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerAnnotationsModuleMRML")
      (string-append
       "-DvtkSlicerAnnotationsModuleLogic_INCLUDE_DIRS="
       #$slicer-annotations-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerAnnotationsModuleLogic")
      ;; Library search paths for all three external modules.
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-terminologies-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-colors-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-annotations-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-volumes-5.8
  (make-slicer-loadable-module
   #:name "slicer-volumes-5.8"
   #:module-subdir "Volumes"
   #:patches (list "volumes/0001-ENH-Make-Volumes-a-separate-module.patch")
   #:synopsis "3D Slicer Volumes loadable module"
   #:description
   "The Volumes loadable module extracted from 3D Slicer.  It provides
volume rendering and scalar-volume display capabilities and is built from the
@file{Modules/Loadable/Volumes} subtree of the Slicer source tree."
   ;; Volumes SubjectHierarchyPlugins link against SubjectHierarchy and Colors.
   #:extra-inputs (list slicer-subjecthierarchy-5.8 slicer-colors-5.8)
   #:extra-configure-flags
   #~(list
      ;; Include dirs for qSlicerSubjectHierarchyAbstractPlugin.h and friends.
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      ;; Include dirs for vtkSlicerSubjectHierarchyModuleLogic.h.
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      ;; Include dirs for vtkSlicerColorLogic.h.
      (string-append
       "-DvtkSlicerColorsModuleLogic_INCLUDE_DIRS="
       #$slicer-colors-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerColorsModuleLogic")
      ;; Include dirs for vtkMRMLColorLegendDisplayNode.h.
      (string-append
       "-DvtkSlicerColorsModuleMRML_INCLUDE_DIRS="
       #$slicer-colors-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerColorsModuleMRML")
      ;; Include dirs for qMRMLColorLegendDisplayNodeWidget.h (used by
      ;; qSlicerVolumesModuleWidget.ui → ui_qSlicerVolumesModuleWidget.h).
      (string-append
       "-DqSlicerColorsModuleWidgets_INCLUDE_DIRS="
       #$slicer-colors-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerColorsModuleWidgets")
      ;; Link directories for the SubjectHierarchy and Colors module libraries.
      ;; CMake list separator (;) allows passing multiple paths.
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-colors-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-units-5.8
  (make-slicer-loadable-module
   #:name "slicer-units-5.8"
   #:module-subdir "Units"
   #:patches (list "units/0001-ENH-Add-standalone-build-support-for-Units-module.patch")
   #:synopsis "3D Slicer Units loadable module"
   #:description
   "The Units loadable module extracted from 3D Slicer.  It provides
measurement unit management (length, time, frequency, velocity, etc.) and
a settings panel for configuring display precision.  Built from the
@file{Modules/Loadable/Units} subtree of the Slicer source tree."))

(define-public slicer-tables-5.8
  (make-slicer-loadable-module
   #:name "slicer-tables-5.8"
   #:module-subdir "Tables"
   #:patches (list "tables/0001-ENH-Add-standalone-build-support-for-Tables-module.patch")
   #:synopsis "3D Slicer Tables loadable module"
   #:description
   "The Tables loadable module extracted from 3D Slicer.  It provides
spreadsheet-style display and editing of MRML table nodes, including support
for adding, removing, and renaming columns of various types.  Built from the
@file{Modules/Loadable/Tables} subtree of the Slicer source tree."
   ;; Tables SubjectHierarchyPlugins link against SubjectHierarchy.
   #:extra-inputs (list slicer-subjecthierarchy-5.8)
   #:extra-configure-flags
   #~(list
      ;; Include dirs for qSlicerSubjectHierarchyAbstractPlugin.h and friends.
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      ;; Include dirs for vtkSlicerSubjectHierarchyModuleLogic.h.
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      ;; Link directory for the SubjectHierarchy module libraries.
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-cameras-5.8
  (make-slicer-loadable-module
   #:name "slicer-cameras-5.8"
   #:module-subdir "Cameras"
   #:patches (list "cameras/0001-ENH-Add-standalone-build-support-for-Cameras-module.patch")
   #:synopsis "3D Slicer Cameras loadable module"
   #:description
   "The Cameras loadable module extracted from 3D Slicer.  It manages camera
nodes in the MRML scene, allowing persistent camera positions across sessions
and multiple synchronized views.  Built from the
@file{Modules/Loadable/Cameras} subtree of the Slicer source tree."))
