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
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)  ; for dcmtk
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
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
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (systole packages ctk)
  #:use-module (systole packages itk)
  #:use-module (systole packages libarchive)
  #:use-module (systole packages maths)
  #:use-module (systole packages pythonqt)
  #:use-module (systole packages qrestapi)
  #:use-module (systole packages teem)
  #:use-module (systole packages vtk)
  #:use-module (systole packages)
  #:use-module (srfi srfi-1)
  )

(define %slicer-5.8-commit "11eaf62e5a70b828021ff8beebbdd14d10d4f51c")
(define %slicer-5.8-hash (base32 "05rz797ddci3a2m8297zyzv2g2hp6bd6djmwa1n0gbsla8b175bx"))

(define-public slicer-source-5.8
  ;; Upstream Slicer 5.8 source tree, without Guix-specific build patches.
  ;; Suitable as a code-search reference for development tools and skills.
  (package
    (name "slicer-source-5.8")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Slicer/Slicer/archive/"
             %slicer-5.8-commit ".tar.gz"))
       (sha256 %slicer-5.8-hash)))
    (build-system trivial-build-system)
    (native-inputs (list tar gzip))
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
                         "--strip-components=1"
                         "-C" #$output)))))
    (synopsis "3D Slicer 5.8 upstream source tree")
    (description
     "Upstream source tree of 3D Slicer 5.8 (commit @code{11eaf62e}), without
any Guix-specific build patches.  Useful as a read-only reference for
development tools, code search, and documentation generation.")
    (home-page "https://www.slicer.org")
    (license license:bsd-3)))

(define-public slicer-5.8
  (package
    (name "slicer-5.8")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/Slicer/Slicer/archive/"
         %slicer-5.8-commit ".tar.gz"))
       (sha256 %slicer-5.8-hash)
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
                 "0012-ENH-Add-link-directories.patch"
                 "0013-ENH-Add-link-libraries-to-SlicerMacroBuildModuleLogi.patch"
                 "0014-ENH-add-Qt5-and-loadable-modules-includes-for-non-su.patch"
                 "0015-ENH-improve-CMake-support-for-system-installed-Slice.patch"
                 "0016-ENH-Fix-file-glob-pattern-for-header-installation.patch"
                 "0017-ENH-Install-CMake-template-files-.h.in-.cxx.in-along.patch"
                 "0018-ENH-Add-LINK_DIRECTORIES-support-to-SlicerMacroBuild.patch"
                 "0019-ENH-Add-Slicer_Libs_INCLUDE_DIRS-to-SlicerMacroBuild.patch"
                 "0020-COMP-Add-Qt5-Xml-to-SlicerMacroBuildModuleQtLibrary-.patch"
                 "0021-COMP-Add-CTKVisualizationVTKWidgets-to-SlicerMacroBu.patch"
                 "0022-COMP-Add-VTK-CommonCore-to-SlicerMacroBuildModuleLog.patch"
                 "0023-COMP-Add-Qt5-Widgets-Xml-CTK-to-SlicerMacroBuildLoad.patch"
                 "0024-COMP-Fix-empty-Slicer_INSTALL_QTLOADABLEMODULES_INCL.patch"
                 "0025-COMP-Add-qSlicerBaseQTCore-to-standalone-module-widg.patch"
                 "0026-COMP-Add-EXTRA_MODULE_LIB_DIRS-to-module-build-macro.patch"
                 "0027-COMP-Add-qMRMLWidgets-to-standalone-module-widgets-l.patch"
                 "0028-COMP-Add-Slicer_GUI_LIBRARY-fallback-to-SlicerMacroB.patch"
                 "0029-COMP-Add-Slicer-root-include-dir-to-SlicerMacroBuild.patch"
                 "0030-COMP-Add-qSlicerBaseQTCore-to-standalone-loadable-mo.patch"
                 "0031-ENH-Install-generated-UI-headers-from-qMRMLWidgets-f.patch"
                 "0032-COMP-Add-Slicer_INSTALL_LIB_DIR-to-QtLibrary-link-se.patch"
                 "0033-COMP-Fix-install-path-for-qSlicerModuleGenericTest.p.patch"
                 "0034-COMP-Guard-MRMLCLIPython-import-when-CLI-support-is-.patch"
                 "0035-COMP-Install-qrcc.py-and-fix-Slicer_QRCC_SCRIPT-for-.patch"
                 "0036-COMP-Ensure-PYTHON_EXECUTABLE-is-set-in-slicerFuncti.patch"
                 "0037-COMP-Fix-module-install-dir-variables-for-install-tr.patch"
                 "0038-COMP-Bridge-Python3-target-to-legacy-PYTHON_LIBRARIE.patch"
                 "0039-COMP-Fix-Slicer_BINARY_DIR-usage-and-expose-QTSCRIPT.patch"
                 "0040-COMP-Add-PythonQt-include-dir-to-global-include-path.patch"
                 "0041-COMP-Use-abspath-instead-of-realpath-in-SubjectHiera.patch"
                 "0042-COMP-Skip-launcher-settings-read-when-file-is-absent.patch"
                 "0043-COMP-Bake-SlicerExecutionModel_DIR-into-install-tree.patch"
                 "0044-COMP-Expose-MRMLCLI_INCLUDE_DIRS-in-install-tree-Sli.patch"
                 "0045-ENH-Read-SLICER_ADDITIONAL_MODULE_PATHS-env-var-in-a.patch"
                 "0046-ENH-Extend-LD_LIBRARY_PATH-and-PYTHONPATH-from-SLICE.patch"
                 "0047-ENH-Register-CTK-plugin-path-and-disable-QtWebEngine.patch"
                 "0048-ENH-Prepend-CTK-and-vtkAddon-lib-dirs-to-PYTHONPATH-.patch"
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

              ;; CLI — support library only; CLI module executables are
              ;; built as separate packages via make-slicer-cli-module.
              "-DSlicer_BUILD_CLI:BOOL=OFF"
              "-DSlicer_BUILD_CLI_SUPPORT:BOOL=ON"
              ;; Tell Slicer where to find SlicerExecutionModel.  The install-tree
              ;; SlicerExecutionModelConfig.cmake is installed at <sem>/lib/.
              (string-append "-DSlicerExecutionModel_DIR="
                             #$(this-package-input "slicerexecutionmodel")
                             "/lib")
              ;; Also set GenerateCLP_DIR so Slicer's own CMake can find the
              ;; GenerateCLP sub-package config directly.
              (string-append "-DGenerateCLP_DIR="
                             #$(this-package-input "slicerexecutionmodel")
                             "/lib/GenerateCLP")

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
              "-DSlicer_BUILD_DICOM_SUPPORT:BOOL=ON"

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
                                                                 (or (getenv "CMAKE_PREFIX_PATH")
                                                                     ""))) #t))

                            (add-after 'install 'patch-runpath
                              ;; SlicerApp-real's build RUNPATH does not
                              ;; include lib/Slicer-5.8/ where the core Slicer
                              ;; shared libraries live.  Extend it with
                              ;; $ORIGIN-relative entries so the binary finds
                              ;; them without LD_LIBRARY_PATH.
                              (lambda* (#:key outputs #:allow-other-keys)
                                (let* ((out (assoc-ref outputs "out"))
                                       (bin (string-append out "/bin/SlicerApp-real")))
                                  (invoke "patchelf" "--add-rpath"
                                          (string-append "$ORIGIN/../lib/Slicer-5.8"
                                                         ":"
                                                         "$ORIGIN/../lib/Slicer-5.8/qt-loadable-modules")
                                          bin))))
                            (add-after 'patch-runpath 'install-slicer-symlink
                              ;; Expose the binary as bin/Slicer (on PATH).
                              ;; bin/SlicerApp-real keeps its original name for
                              ;; compatibility (e.g. gdb bin/SlicerApp-real).
                              (lambda* (#:key outputs #:allow-other-keys)
                                (let* ((out (assoc-ref outputs "out"))
                                       (link (string-append out "/bin/Slicer")))
                                  ;; The CTK launcher was installed as bin/Slicer;
                                  ;; replace it with a symlink to the real binary.
                                  (when (file-exists? link)
                                    (delete-file link))
                                  (symlink "SlicerApp-real" link)))))))
    (inputs
     (list libxt
           dcmtk
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
           slicerexecutionmodel
           qrestapi))
    (native-inputs (list patchelf pkg-config))
    ;; Each Slicer module package (e.g. slicer-volumes-5.8) installs its
    ;; shared library under lib/Slicer-5.8/qt-loadable-modules/.  Guix
    ;; collects those directories into SLICER_ADDITIONAL_MODULE_PATHS when
    ;; module packages share a profile with slicer-5.8.  Slicer reads this
    ;; variable directly (patch 0045) and extends LD_LIBRARY_PATH from it
    ;; at startup (patch 0046).
    (native-search-paths
     (list (search-path-specification
            (variable "SLICER_ADDITIONAL_MODULE_PATHS")
            (files '("lib/Slicer-5.8/qt-loadable-modules"
                     "lib/Slicer-5.8/cli-modules")))))
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

(define-public slicerexecutionmodel
  ;; SlicerExecutionModel provides the SEMMacroBuildCLI macro and the
  ;; GenerateCLP code-generator used to build Slicer CLI modules.
  ;; tclap and ModuleDescriptionParser are bundled as subdirectories;
  ;; the only external dependency is ITK (for ModuleDescriptionParser).
  (package
   (name "slicerexecutionmodel")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://github.com/Slicer/SlicerExecutionModel/archive/91b921bd5977c3384916ba4b03705d87b26067f7.tar.gz")
     (sha256
      (base32 "10k1m3impplv9hwhxx06wfmvlx9h54avhibv4id1pjlqjn5gjjza"))
     (patches (search-patches
               "0001-comp-use-generateclp-directly-instead-of-launcher-in.patch"
               "0002-comp-add-install-tree-cmake-config-infrastructure-fo.patch"
               "0003-comp-re-enable-itk-io-factory-registration-for-stand.patch"))))
   (build-system cmake-build-system)
   (arguments
    (list
     #:tests? #f
     #:configure-flags
     #~(list
        "-DBUILD_TESTING:BOOL=OFF"
        "-DSlicerExecutionModel_USE_UTF8:BOOL=ON"
        ;; Install development files (GenerateCLP binary, cmake config,
        ;; headers) so downstream packages can use GenerateCLP.
        "-DSlicerExecutionModel_INSTALL_NO_DEVELOPMENT:BOOL=OFF"
        ;; tclap/CMakeLists.txt checks ${PROJECT_NAME}_INSTALL_NO_DEVELOPMENT
        ;; where PROJECT_NAME=TCLAP (uppercase), but the root CMakeLists only
        ;; propagates tclap_INSTALL_NO_DEVELOPMENT (lowercase).  Set the
        ;; uppercase variant explicitly so tclap headers and TCLAPConfig.cmake
        ;; are actually installed.
        "-DTCLAP_INSTALL_NO_DEVELOPMENT:BOOL=OFF"
        (string-append "-DITK_DIR="
                       #$(this-package-input "itk-slicer")
                       "/lib/cmake/ITK-5.4"))
     #:phases
     #~(modify-phases %standard-phases
         (add-after 'install 'fix-rpath
           ;; libModuleDescriptionParser.so is installed to lib/ModuleDescriptionParser/
           ;; but GenerateCLP's RPATH only includes lib/.  Append the subdirectory to
           ;; the existing RPATH (read via --print-rpath) so we don't lose the GCC
           ;; libstdc++/libgcc_s paths that CMake's linker already recorded.
           (lambda* (#:key outputs #:allow-other-keys)
             (use-modules (ice-9 popen) (ice-9 textual-ports))
             (let* ((out (assoc-ref outputs "out"))
                    (mdp (string-append out "/lib/ModuleDescriptionParser")))
               (for-each
                (lambda (bin)
                  (when (file-exists? bin)
                    (let* ((pipe (open-pipe* OPEN_READ "patchelf" "--print-rpath" bin))
                           (cur  (string-trim-right (get-string-all pipe) #\newline)))
                      (close-pipe pipe)
                      (invoke "patchelf" "--set-rpath"
                              (string-append cur ":" mdp)
                              bin))))
                (list (string-append out "/bin/GenerateCLP")
                      (string-append out "/bin/GenerateCLPLauncher")))
               #t))))))
   (inputs (list itk-slicer
                 expat       ; ITKExpat / ITKIOXML dependency
                 hdf5-1.10)) ; ITKHDF5 pulled in transitively via ITKConfig
   (native-inputs (list pkg-config patchelf))
   (home-page "https://github.com/Slicer/SlicerExecutionModel")
   (synopsis "Slicer Execution Model — CLI module build infrastructure")
   (description
    "SlicerExecutionModel provides the @code{SEMMacroBuildCLI} CMake macro and
the @code{GenerateCLP} code generator used to build 3D Slicer CLI (Command
Line Interface) modules.  It bundles @code{tclap} and
@code{ModuleDescriptionParser}; the only external dependency is ITK.")
   (license license:bsd-3)))

;;;
;;; Factory for standalone Slicer CLI-module packages
;;;

;; CLI (Command-Line Interface) modules are standalone executables that expose
;; image-processing algorithms via the SEM XML descriptor protocol.  They are
;; discovered and launched as subprocesses by the qSlicerCLIModuleFactory
;; inside a running Slicer process.
;;
;; Pure-ITK CLI modules (Tier 1) require only SlicerExecutionModel + ITK; they
;; do NOT call find_package(Slicer) and therefore do not need VTK, CTK, Qt, or
;; any other Slicer dependency.  Their CMakeLists.txt files have no
;; cmake_minimum_required / project() preamble (they assumed being
;; add_subdirectory()-d from the Slicer root), so each module requires a
;; small preamble patch.
;;
;; Executables are installed to lib/Slicer-5.8/cli-modules so that Guix
;; profile-based search via SLICER_ADDITIONAL_MODULE_PATHS picks them up.
(define* (make-slicer-cli-module
          #:key
          name          ; package name string, e.g. "slicer-add-scalar-volumes-5.8"
          module-subdir ; source sub-directory, e.g. "AddScalarVolumes"
          patches       ; list of patch filename strings
          synopsis      ; one-line synopsis string
          description   ; multi-line description string
          ;; Extra packages added to inputs before the common ones.
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
                   ;; Tell SEMMacroBuildCLI where to install the executable.
                   ;; On Linux, UseSlicer.cmake does not set this variable
                   ;; (only APPLE gets it), so we must supply it ourselves.
                   "-DSlicerExecutionModel_DEFAULT_CLI_INSTALL_RUNTIME_DESTINATION=lib/Slicer-5.8/cli-modules"
                   "-DSlicerExecutionModel_DEFAULT_CLI_INSTALL_LIBRARY_DESTINATION=lib/Slicer-5.8/cli-modules"
                   ;; SlicerExecutionModelInstallConfig.cmake does not set the
                   ;; *_OUTPUT_DIRECTORY vars (build-tree paths).  SEMMacroBuildCLI.cmake
                   ;; falls back to SlicerExecutionModel_CLI_*_OUTPUT_DIRECTORY, so set
                   ;; those to simple relative paths (cmake expands them from binary dir).
                   "-DSlicerExecutionModel_CLI_RUNTIME_OUTPUT_DIRECTORY=bin"
                   "-DSlicerExecutionModel_CLI_LIBRARY_OUTPUT_DIRECTORY=lib"
                   "-DSlicerExecutionModel_CLI_ARCHIVE_OUTPUT_DIRECTORY=lib"
                   ;; SEMMacroBuildCLI.cmake requires this to point to the
                   ;; shared-library wrapper source file installed by SEM.
                   ;; The install-tree SlicerExecutionModelConfig.cmake does not
                   ;; set it, so we supply the absolute store path explicitly.
                   (string-append "-DSlicerExecutionModel_DEFAULT_CLI_LIBRARY_WRAPPER_CXX="
                                  #$slicerexecutionmodel "/lib/CMake/SEMCommandLineLibraryWrapper.cxx")
                   ;; Point cmake at the installed SlicerExecutionModel tree.
                   (string-append "-DSlicerExecutionModel_DIR="
                                  #$slicerexecutionmodel "/lib")
                   (string-append "-DGenerateCLP_DIR="
                                  #$slicerexecutionmodel "/lib/GenerateCLP")
                   (string-append "-DModuleDescriptionParser_DIR="
                                  #$slicerexecutionmodel "/lib/ModuleDescriptionParser")
                   (string-append "-DTCLAP_DIR="
                                  #$slicerexecutionmodel "/lib/tclap")
                   (string-append "-DITK_DIR="
                                  #$itk-slicer "/lib/cmake/ITK-5.4"))
             #$extra-configure-flags)
          #:phases
          #~(modify-phases %standard-phases
              ;; Build only the named CLI sub-directory, not the Slicer root.
              ;; Pass Base/CLI from the source tree as an extra include dir so
              ;; that itkPluginUtilities.h is found without requiring an
              ;; installed Slicer package as a build input.
              (replace 'configure
                (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
                  (let* ((source (getcwd))
                         (out (assoc-ref outputs "out")))
                    (apply invoke "cmake"
                           "-S" (string-append source "/Modules/CLI/"
                                               #$module-subdir)
                           "-B" "build"
                           (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                           ;; Base/CLI provides itkPluginUtilities.h;
                           ;; Libs/vtkITK provides itkConstrainedValue*Filter.h
                           ;; (used by e.g. MultiplyScalarVolumes).  Both are
                           ;; read directly from the source tree.  CMake accepts
                           ;; semicolon-separated lists for -D string vars.
                           (string-append "-DSlicerExecutionModel_EXTRA_INCLUDE_DIRECTORIES="
                                          source "/Base/CLI;"
                                          source "/Libs/vtkITK")
                           configure-flags)
                    (chdir "build")
                    #t))))))
   ;; slicerexecutionmodel and itk-slicer are the primary build deps.
   ;; expat and hdf5-1.10 are explicit because ITKConfig.cmake references them
   ;; but they are not propagated-inputs of itk-slicer.
   ;; zlib is a direct NEEDED entry in CLI binaries (via ITKMetaIO) but is not
   ;; in the itk-slicer propagated-inputs, so it must be listed here to get a
   ;; correct RUNPATH entry in the installed binary.
   ;; No installed Slicer package is needed: itkPluginUtilities.h is read
   ;; directly from the Slicer source tree during the configure phase.
   (inputs (fold (lambda (pkg acc)
                   (modify-inputs acc (prepend pkg)))
                 (list slicerexecutionmodel itk-slicer expat hdf5-1.10 zlib)
                 extra-inputs))
   (native-inputs (list pkg-config))
   (home-page (package-home-page slicer-5.8))
   (synopsis synopsis)
   (description description)
   (license (package-license slicer-5.8))))

;;;
;;; Python-enabled Slicer variant
;;;

;; slicer-python-5.8 is an (inherit slicer-5.8) variant that enables the full
;; Python stack: PythonQt (via CTK), VTK Python wrappers, and Qt-scripted
;; modules.  It uses the *-python variants of CTK, VTK, and vtkAddon.
;;
;; All standalone loadable modules (slicer-*-5.8) are built against this
;; variant so their build-time and runtime ABIs are identical: the same
;; ctk-python / vtk-slicer-python / … libraries that Slicer uses at runtime
;; are also present when the module's headers are compiled.
;; Slicer_USE_PYTHONQT is overridden to OFF in standalone module builds to
;; suppress PythonQt wrapper generation (which requires the full Slicer build
;; tree and is not needed for standalone C++ modules).
;;
;; ITK Python wrapping (Slicer_BUILD_ITKPython) remains OFF until
;; python-pygccxml is packaged in Guix.  See itk.scm for the TODO.
(define-public slicer-python-5.8
  (package
    (inherit slicer-5.8)
    (name "slicer-python-5.8")
    (arguments
     (substitute-keyword-arguments (package-arguments slicer-5.8)
       ((#:configure-flags flags)
        #~(append
           (list
            ;; Python — Guix Python 3.11
            (string-append "-DPython3_EXECUTABLE="
                           #$(this-package-input "python") "/bin/python3")
            (string-append "-DPython3_INCLUDE_DIR="
                           #$(this-package-input "python") "/include/python3.11")
            (string-append "-DPython3_LIBRARY="
                           #$(this-package-input "python") "/lib/libpython3.11.so")
            "-DVTK_WRAP_PYTHON:BOOL=ON"
            "-DSlicer_USE_PYTHONQT:BOOL=ON"
            "-DSlicer_USE_SYSTEM_python:BOOL=ON"
            ;; Disable Qt-scripted modules since they can be installed as separate packages
            "-DSlicer_BUILD_QTSCRIPTEDMODULES:BOOL=OFF"
            ;; PythonQt location (CTK's FindPythonQt uses PYTHONQT_INSTALL_DIR)
            (string-append "-DPYTHONQT_INSTALL_DIR="
                           #$(this-package-input "pythonqt-commontk"))
            ;; vtkAddon_CMAKE_DIR is used at the top of Slicer's CMakeLists.txt
            ;; (line ~803) before any find_package(vtkAddon) runs, so it must be
            ;; pre-set as a cache variable.
            (string-append "-DvtkAddon_CMAKE_DIR="
                           #$(this-package-input "vtkaddon") "/lib/cmake"))
           (filter (lambda (f)
                     (not (member f '("-DVTK_WRAP_PYTHON:BOOL=OFF"
                                      "-DSlicer_USE_PYTHONQT:BOOL=OFF"
                                      "-DSlicer_USE_SYSTEM_python:BOOL=OFF"
                                      "-DSlicer_BUILD_QTSCRIPTEDMODULES:BOOL=OFF"))))
                   #$flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; Extend CMAKE_PREFIX_PATH to include pythonqt-commontk.
            (replace 'set-cmake-paths
              (lambda* (#:key inputs #:allow-other-keys)
                (setenv "CMAKE_PREFIX_PATH"
                        (string-append
                         (assoc-ref inputs "pythonqt-commontk") "/lib/cmake:"
                         (assoc-ref inputs "vtkaddon") "/lib/cmake:"
                         (or (getenv "CMAKE_PREFIX_PATH") "")))
                #t))
))))
    (inputs
     (modify-inputs (package-inputs slicer-5.8)
       ;; Replace the entire VTK-dependent chain with Python-enabled variants so
       ;; that all cmake configs (CTKConfig, ITKConfig, VTK-targets) reference the
       ;; same vtk-slicer-python, preventing duplicate cmake target conflicts.
       (replace "ctk" ctk-python)
       (replace "vtk-slicer" vtk-slicer-python)
       (replace "vtkaddon" vtkaddon-python)
       (replace "itk-slicer" itk-slicer-python)
       ;; Add python and pythonqt-commontk explicitly for CMake find modules.
       (prepend python pythonqt-commontk)))
    ;; Python runtime packages (numpy, scipy, pydicom, requests,
    ;; dicomweb-client) are intentionally NOT listed here.  They are propagated
    ;; by slicer-python-all-5.8 instead.  Keeping them in slicer-python-5.8
    ;; would cause --without-tests=<any-of-them> to cascade through
    ;; slicer-python-5.8 into every loadable module (which all use
    ;; slicer-python-5.8 as a build input), producing duplicate derivations
    ;; (original + transformed) for those modules.  Placing the runtime deps
    ;; only in the meta-package breaks the cascade cleanly.
    ;;
    ;; vtk-slicer-python, ctk-python, and vtkaddon-python ARE propagated so
    ;; that their Python directories land in the Guix profile symlink farm:
    ;;   vtk-slicer-python/lib/python3.11/site-packages  → `import vtk`
    ;;   ctk-python/bin/Python                           → `import ctk`, `import qt`
    ;;   vtkaddon-python/lib/python3.11/site-packages    → vtkAddon Python wrappers
    ;; SLICER_PYTHONPATH (native-search-path) then collects all three via its
    ;; `lib/python3.11/site-packages` and `bin/Python` patterns, and Slicer
    ;; prepends SLICER_PYTHONPATH to PYTHONPATH at startup (patch 0046/0048).
    (propagated-inputs (list vtk-slicer-python ctk-python vtkaddon-python))
    ;; Extend the base search-path to include qt-scripted-modules so that
    ;; standalone scripted-module packages (installed to that subdirectory)
    ;; are discovered when sharing a profile with slicer-python-5.8.
    (native-search-paths
     (list (search-path-specification
            (variable "SLICER_ADDITIONAL_MODULE_PATHS")
            (files '("lib/Slicer-5.8/qt-loadable-modules"
                     "lib/Slicer-5.8/qt-scripted-modules"
                     "lib/Slicer-5.8/cli-modules")))
           (search-path-specification
            (variable "SLICER_PYTHONPATH")
            ;; bin/Python: .py wrappers installed by slicer-python-5.8 and
            ;;   ctk-python (ctk/, qt/ packages) — contributed via ctk-python's
            ;;   own SLICER_PYTHONPATH native-search-path.
            ;; lib/Slicer-5.8: C-extension .so (MRMLCorePython, …).
            ;; lib/python3.11/site-packages: numpy, vtk, vtkAddon, user pkgs.
            (files '("bin/Python"
                     "lib/Slicer-5.8"
                     "lib/python3.11/site-packages")))))))

;;;
;;; Factory for standalone Slicer loadable-module packages
;;;

;; Do NOT use (inherit slicer) in the packages produced here: Guix forbids
;; listing a package as an input when it also appears in the inheritance chain.
;; We reuse only the source origin and build each sub-module as a fully
;; independent package that depends on an installed Slicer.
;;
;; The #:slicer keyword (default: slicer-python-5.8) selects which Slicer
;; variant to build against.  Using slicer-python-5.8 ensures that the
;; build-time CTK/VTK/Slicer headers and libraries are the same Python-enabled
;; variants that will be resident in memory at runtime, eliminating any
;; possibility of ABI mismatch caused by conditional compilation guards
;; (e.g. #ifdef CTK_BUILD_PYTHON_SCRIPTING) changing class layouts between
;; the ctk and ctk-python variants.
;;
;; Slicer_USE_PYTHONQT is overridden to OFF to suppress PythonQt wrapper
;; generation inside standalone module CMake builds (the macro
;; slicerMacroBuildLoadableModule tries to wrap module classes when
;; Slicer_USE_PYTHONQT=ON, which requires the full Slicer build tree and
;; is not available in a standalone build).
(define* (make-slicer-loadable-module
          #:key
          name            ; package name string, e.g. "slicer-volumes-5.8"
          module-subdir   ; source sub-directory, e.g. "Volumes"
          patches         ; list of patch filename strings
          synopsis        ; one-line synopsis string
          description     ; multi-line description string
          ;; Slicer variant to build against.  Must be slicer-python-5.8 (the
          ;; default) or slicer-5.8.  All production packages use the Python
          ;; variant so build-time and runtime ABIs are consistent.
          (slicer slicer-python-5.8)
          ;; Extra packages added to inputs *before* slicer's own inputs.
          ;; Use this to declare inter-module build-time dependencies
          ;; (e.g. slicer-terminologies-5.8 for SubjectHierarchy).
          (extra-inputs '())
          ;; A gexp that evaluates to a (possibly empty) list of extra
          ;; CMake -D flags.  Defaults to the empty list.
          (extra-configure-flags #~'())
          ;; Packages that must be present in the profile at runtime.
          ;; Use this to declare inter-module runtime (dlopen) dependencies
          ;; (e.g. slicer-colors-5.8 for modules that load Colors widgets).
          (propagated-inputs '()))
  (package
   (name name)
   (version (package-version slicer))
   (source
    (origin
     (inherit (package-source slicer))
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
                                  #$slicer
                                  "/lib/Slicer-5.8")
                   ;; Prevent slicerMacroBuildLoadableModule from attempting
                   ;; PythonQt wrapper generation, which requires the full
                   ;; Slicer build tree and fails in standalone builds.
                   "-DSlicer_USE_PYTHONQT:BOOL=OFF"
                   ;; UseSlicer.cmake calls find_package(PythonQt) which uses
                   ;; PYTHONQT_INSTALL_DIR as a hint to find PythonQt.h and set
                   ;; PYTHONQT_INCLUDE_DIR.  Without this hint, find_path may
                   ;; not locate PythonQt.h, leaving PYTHONQT_INCLUDE_DIR empty
                   ;; and the include_directories() call in UseSlicer a no-op.
                   (string-append "-DPYTHONQT_INSTALL_DIR="
                                  #$pythonqt-commontk))
             #$extra-configure-flags)
          #:phases
          #~(modify-phases %standard-phases
              ;; Build only the named sub-directory, not the Slicer root.
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
   ;; UseSlicer.cmake transitively requires all of slicer's build-time
   ;; libraries (Qt5, VTK, ITK, etc.) to be present in the build environment,
   ;; not just slicer itself.  We therefore start from slicer's input list and
   ;; prepend slicer so cmake can locate SlicerConfig.cmake.  Any extra-inputs
   ;; (other standalone modules this module depends on) are also prepended so
   ;; cmake can link against them.
   (inputs (fold (lambda (pkg acc)
                   (modify-inputs acc (prepend pkg)))
                 (modify-inputs (package-inputs slicer)
                   (prepend slicer))
                 extra-inputs))
   (propagated-inputs propagated-inputs)
   (home-page (package-home-page slicer))
   (synopsis synopsis)
   (description description)
   (license (package-license slicer))))

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
                   "subjecthierarchy/0004-COMP-Add-vtkSlicerTerminologiesModuleLogic-to-Subjec.patch"
                   ;; Fix ${Slicer_BINARY_DIR} → ${CMAKE_BINARY_DIR} in SubjectHierarchyPlugins
                   ;; so the standalone build uses the current project's binary tree.
                   "subjecthierarchy/0005-COMP-Fix-Slicer_BINARY_DIR-usage-in-SubjectHierarchy.patch"
                   ;; Link Python3 explicitly for qSlicerSubjectHierarchyScriptedPlugin
                   ;; which uses the Python C API directly.
                   "subjecthierarchy/0006-COMP-Link-Python3-for-ScriptedPlugin-in-standalone-b.patch"
                   ;; Link CTKScriptingPythonCore explicitly for
                   ;; ctkAbstractPythonManager::executeString() called by the
                   ;; module's setup() function.
                   "subjecthierarchy/0007-COMP-Link-CTKScriptingPythonCore-for-standalone-Subj.patch"
                   ;; Use os.path.abspath instead of os.path.realpath in
                   ;; SubjectHierarchyPlugins/__init__.py so that the merged
                   ;; profile directory (not the store symlink target) is scanned
                   ;; for plugins contributed by other modules.
                   "0041-COMP-Use-abspath-instead-of-realpath-in-SubjectHiera.patch")
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
   ;; Terminologies must be in the profile so dlopen resolves
   ;; libqSlicerTerminologiesModuleWidgets.so at runtime.
   #:propagated-inputs (list slicer-terminologies-5.8)
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
                   "colors/0002-COMP-Add-VTK-RenderingAnnotation-dependency-to-Color.patch"
                   "colors/0003-COMP-Set-SubjectHierarchy-include-dirs-for-standalon.patch")
   #:synopsis "3D Slicer Colors loadable module"
   #:description
   "The Colors loadable module extracted from 3D Slicer.  It provides color
table management, color legend display nodes and widgets (including a scalar
bar actor), and a subject hierarchy plugin for color legends.  Built from the
@file{Modules/Loadable/Colors} subtree of the Slicer source tree."
   ;; Colors SubjectHierarchyPlugins link against SubjectHierarchy.
   #:extra-inputs (list slicer-subjecthierarchy-5.8)
   ;; SubjectHierarchy must be in the profile so dlopen resolves
   ;; libqSlicerSubjectHierarchyModuleWidgets.so at runtime.
   ;; slicer-subjecthierarchy-5.8 itself propagates slicer-terminologies-5.8.
   #:propagated-inputs (list slicer-subjecthierarchy-5.8)
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
   ;; Annotations depends on SubjectHierarchy for plugins.
   #:extra-inputs (list slicer-subjecthierarchy-5.8)
   ;; SubjectHierarchy must be in the profile so dlopen resolves
   ;; libqSlicerSubjectHierarchyModuleWidgets.so at runtime.
   #:propagated-inputs (list slicer-subjecthierarchy-5.8)
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
   ;; Colors and Annotations must be in the profile so dlopen resolves their
   ;; widgets .so files at runtime.  Colors propagates SubjectHierarchy, which
   ;; propagates Terminologies — so all four are covered transitively.
   #:propagated-inputs (list slicer-colors-5.8 slicer-annotations-5.8)
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
   ;; dcmtk is needed at runtime: libqSlicerVolumesModule.so directly links
   ;; against libi2d.so.19 and other DCMTK libraries (via CTK DICOM widgets).
   ;; Adding it as a direct input ensures its lib path ends up in the RUNPATH.
   #:extra-inputs (list slicer-subjecthierarchy-5.8 slicer-colors-5.8 dcmtk)
   ;; Colors must be in the profile so dlopen resolves
   ;; libqSlicerColorsModuleWidgets.so at runtime.
   ;; slicer-colors-5.8 propagates slicer-subjecthierarchy-5.8 transitively.
   ;; slicer-units-5.8 is needed at runtime: Volumes calls
   ;; GetModuleLogic("Units") for unit-aware scalar display.
   #:propagated-inputs (list slicer-colors-5.8 slicer-units-5.8)
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

;; TODO: slicer-cropvolume-5.8 is blocked by a missing CLI dependency.
;;
;; CropVolume's Logic (vtkSlicerCropVolumeLogic.cxx) includes
;; <vtkSlicerCLIModuleLogic.h> from Base/QTCLI and calls
;;   vtkSlicerCLIModuleLogic::SafeDownCast(this->GetModuleLogic("ResampleScalarVectorDWIVolume"))
;; for its interpolated-crop path.  Our slicer-5.8 build has
;;   -DSlicer_BUILD_CLI:BOOL=OFF
;;   -DSlicer_BUILD_CLI_SUPPORT:BOOL=OFF
;; so qSlicerBaseQTCLI is never built and vtkSlicerCLIModuleLogic.h is not
;; installed.  Resolution options:
;;   a) Enable CLI support in slicer-5.8 (large change, new dependencies).
;;   b) Patch vtkSlicerCropVolumeLogic.cxx to guard the CLI include and
;;      CropInterpolated body with #ifdef Slicer_BUILD_CLI_SUPPORT, making
;;      interpolated crop a no-op when CLI is absent.
;;
;; (define-public slicer-cropvolume-5.8
;;   (make-slicer-loadable-module
;;    #:name "slicer-cropvolume-5.8"
;;    #:module-subdir "CropVolume"
;;    #:patches (list "cropvolume/0001-ENH-Add-standalone-CMake-build-support-for-CropVolum.patch"
;;                    "cropvolume/0002-COMP-Add-missing-include-directories-to-Logic-module.patch")
;;    #:synopsis "3D Slicer CropVolume loadable module"
;;    #:description
;;    "The CropVolume loadable module extracted from 3D Slicer.  It allows
;; cropping a volume to a region of interest (ROI) node, with optional
;; resampling (interpolated or voxel-based).  Built from the
;; @file{Modules/Loadable/CropVolume} subtree of the Slicer source tree."
;;    #:extra-inputs (list slicer-volumes-5.8 slicer-markups-5.8)
;;    #:extra-configure-flags
;;    #~(list
;;       ;; Volumes Logic headers (vtkSlicerVolumesLogic.h).
;;       (string-append
;;        "-DvtkSlicerVolumesModuleLogic_INCLUDE_DIRS="
;;        #$slicer-volumes-5.8
;;        "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerVolumesModuleLogic")
;;       ;; Markups MRML headers (vtkMRMLMarkupsFiducialNode.h, vtkMRMLMarkupsROINode.h).
;;       (string-append
;;        "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
;;        #$slicer-markups-5.8
;;        "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
;;       ;; Link directories for Volumes and Markups libraries.
;;       (string-append
;;        "-DEXTRA_MODULE_LIB_DIRS="
;;        #$slicer-volumes-5.8
;;        "/lib/Slicer-5.8/qt-loadable-modules;"
;;        #$slicer-markups-5.8
;;        "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-tables-5.8
  (make-slicer-loadable-module
   #:name "slicer-tables-5.8"
   #:module-subdir "Tables"
   #:patches (list "tables/0001-ENH-Add-standalone-build-support-for-Tables-module.patch"
                   "tables/0002-COMP-Set-SubjectHierarchy-include-dirs-for-standalon.patch")
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

(define-public slicer-data-5.8
  (make-slicer-loadable-module
   #:name "slicer-data-5.8"
   #:module-subdir "Data"
   #:patches (list "data/0001-ENH-Add-standalone-CMake-build-support-for-Data-modu.patch"
                   "data/0002-COMP-Add-LINK_DIRECTORIES-for-external-module-librar.patch")
   #:synopsis "3D Slicer Data loadable module"
   #:description
   "The Data loadable module extracted from 3D Slicer.  It provides the
main data panel for managing MRML scene nodes including loading and saving
scenes, and integrates with the SubjectHierarchy tree for data organization.
It also provides scene reader/writer support via the Cameras module logic for
camera state persistence.  Built from the @file{Modules/Loadable/Data}
subtree of the Slicer source tree."
   #:extra-inputs (list slicer-cameras-5.8 slicer-subjecthierarchy-5.8)
   #:extra-configure-flags
   #~(list
      ;; Cameras Logic include directory: replaces _SOURCE_DIR + _BINARY_DIR.
      (string-append
       "-DvtkSlicerCamerasModuleLogic_INCLUDE_DIRS="
       #$slicer-cameras-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerCamerasModuleLogic")
      ;; SubjectHierarchy includes.
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      ;; Link directories for Cameras and SubjectHierarchy libraries.
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-cameras-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-models-5.8
  (make-slicer-loadable-module
   #:name "slicer-models-5.8"
   #:module-subdir "Models"
   #:patches (list "models/0001-ENH-Add-standalone-CMake-build-support-for-Models-mo.patch"
                   "models/0002-ENH-Add-MRMLCore-dependency-to-Widgets-target-librar.patch"
                   "models/0003-ENH-Add-missing-vtkSegmentationCore-dependency-and-l.patch"
                   )
   #:synopsis "3D Slicer Models loadable module"
   #:description
   "The Models loadable module extracted from 3D Slicer.  It provides
loading, display, and manipulation of 3D surface mesh models (VTK
polydata), including per-scalar colorization and subject hierarchy
integration.  Built from the @file{Modules/Loadable/Models} subtree
of the Slicer source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.8
                        slicer-terminologies-5.8
                        slicer-colors-5.8)
   #:extra-configure-flags
   #~(list
      ;; SubjectHierarchy includes (top-level module + SubjectHierarchyPlugins)
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      ;; Terminologies includes (SubjectHierarchyPlugins)
      (string-append
       "-DqSlicerTerminologiesModuleWidgets_INCLUDE_DIRS="
       #$slicer-terminologies-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerTerminologiesModuleWidgets")
      (string-append
       "-DvtkSlicerTerminologiesModuleLogic_INCLUDE_DIRS="
       #$slicer-terminologies-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerTerminologiesModuleLogic")
      ;; Colors includes (top-level module)
      (string-append
       "-DqSlicerColorsModuleWidgets_INCLUDE_DIRS="
       #$slicer-colors-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerColorsModuleWidgets")
      (string-append
       "-DvtkSlicerColorsModuleLogic_INCLUDE_DIRS="
       #$slicer-colors-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerColorsModuleLogic")
      (string-append
       "-DvtkSlicerColorsModuleMRML_INCLUDE_DIRS="
       #$slicer-colors-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerColorsModuleMRML")
      ;; Link directories for all three external modules
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-terminologies-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-colors-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-sequences-5.8
  (make-slicer-loadable-module
   #:name "slicer-sequences-5.8"
   #:module-subdir "Sequences"
   #:patches (list "sequences/0001-ENH-Add-standalone-CMake-build-support-for-Sequences.patch"
                   "sequences/0002-ENH-Add-Markups-MRML-include-directories-to-Logic.patch"
                   "sequences/0003-COMP-Guard-DesignerPlugins-subdirectory-with-Slicer_.patch")
   #:synopsis "3D Slicer Sequences loadable module"
   #:description
   "The Sequences loadable module extracted from 3D Slicer.  It provides
support for storing and replaying time-sequences of MRML nodes (e.g.
4D image series, tracked tool trajectories), including a browser widget
for navigating sequence items.  Built from the
@file{Modules/Loadable/Sequences} subtree of the Slicer source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.8
                        slicer-terminologies-5.8
                        slicer-colors-5.8)
   #:extra-configure-flags
   #~(list
      ;; Markups includes (top-level module)
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      ;; Link directories for all three external modules
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-terminologies-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-colors-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))
   ))

(define-public slicer-viewcontrollers-5.8
  (make-slicer-loadable-module
   #:name "slicer-viewcontrollers-5.8"
   #:module-subdir "ViewControllers"
   #:patches (list "viewcontrollers/0001-ENH-Add-standalone-CMake-build-support-for-ViewContr.patch"
                   "viewcontrollers/0002-ENH-Add-qMRMLWidgets-to-target-libraries.patch"
                   )
   #:synopsis "3D Slicer ViewControllers loadable module"
   #:description
   "The ViewControllers loadable module extracted from 3D Slicer.  It
provides the collapsible slice and 3D view controller bars that appear
at the top of each viewer panel, exposing orientation, slice offset,
link/unlink, and camera controls.  Built from the
@file{Modules/Loadable/ViewControllers} subtree of the Slicer source
tree."))

(define-public slicer-reformat-5.8
  (make-slicer-loadable-module
   #:name "slicer-reformat-5.8"
   #:module-subdir "Reformat"
   #:patches (list "reformat/0001-ENH-Add-standalone-CMake-build-support-for-Reformat-.patch"
                   "reformat/0002-ENH-Add-qMRMLWidgets-library-dependency.patch")
   #:synopsis "3D Slicer Reformat loadable module"
   #:description
   "The Reformat loadable module extracted from 3D Slicer.  It provides
interactive repositioning and reorientation of slice planes (axial,
coronal, sagittal, or oblique) with rotation and translation handles
directly in the 3D viewer.  Built from the
@file{Modules/Loadable/Reformat} subtree of the Slicer source tree."))

(define-public slicer-plots-5.8
  (make-slicer-loadable-module
   #:name "slicer-plots-5.8"
   #:module-subdir "Plots"
   #:patches (list "plots/0001-ENH-Add-standalone-CMake-build-support-for-Plots-mod.patch")
   #:synopsis "3D Slicer Plots loadable module"
   #:description
   "The Plots loadable module extracted from 3D Slicer.  It provides
2D chart and plot display backed by VTK charts, supporting line, bar,
and scatter plots tied to MRML table nodes, with subject hierarchy
integration.  Built from the @file{Modules/Loadable/Plots} subtree of
the Slicer source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.8)
   ;; SubjectHierarchy must be in the profile so dlopen resolves
   ;; libqSlicerSubjectHierarchyModuleWidgets.so (and transitively
   ;; libqSlicerTerminologiesModuleWidgets.so) at runtime.
   #:propagated-inputs (list slicer-subjecthierarchy-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-sceneviews-5.8
  (make-slicer-loadable-module
   #:name "slicer-sceneviews-5.8"
   #:module-subdir "SceneViews"
   #:patches (list "sceneviews/0001-ENH-Add-standalone-CMake-build-support-for-SceneView.patch"
                   "sceneviews/0002-COMP-Add-explicit-link-directories-for-SceneViews-Su.patch")
   #:synopsis "3D Slicer SceneViews loadable module"
   #:description
   "The SceneViews loadable module extracted from 3D Slicer.  It provides
named scene snapshots that capture the full MRML scene state (layout,
display properties, camera positions) so users can save and restore
multiple views of the same data set.  Built from the
@file{Modules/Loadable/SceneViews} subtree of the Slicer source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-segmentations-5.8
  (make-slicer-loadable-module
   #:name "slicer-segmentations-5.8"
   #:module-subdir "Segmentations"
   #:patches (list "segmentations/0001-ENH-Add-standalone-CMake-build-support-for-Segmentat.patch"
                   "segmentations/0002-COMP-Add-missing-library-dependencies-to-Segmentatio.patch"
                   "segmentations/0003-COMP-Fix-Slicer_BINARY_DIR-in-SegmentEditorEffects-f.patch"
                   "segmentations/0004-COMP-Link-Python3-for-ScriptedEffect-in-standalone-S.patch")
   #:synopsis "3D Slicer Segmentations loadable module"
   #:description
   "The Segmentations loadable module extracted from 3D Slicer.  It provides
MRML node types, display managers, editor effects, and widgets for working
with segmentation objects—multi-label volumetric masks that represent
anatomical structures.  The module includes the full segmentation editor
(qMRMLSegmentEditorWidget), binary label-map and closed-surface
representations, and subject-hierarchy integration.  Built from the
@file{Modules/Loadable/Segmentations} subtree of the Slicer source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.8
                        slicer-terminologies-5.8
                        slicer-markups-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DqSlicerTerminologiesModuleWidgets_INCLUDE_DIRS="
       #$slicer-terminologies-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerTerminologiesModuleWidgets")
      (string-append
       "-DvtkSlicerTerminologiesModuleLogic_INCLUDE_DIRS="
       #$slicer-terminologies-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerTerminologiesModuleLogic")
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-terminologies-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-markups-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-volumerendering-5.8
  (make-slicer-loadable-module
   #:name "slicer-volumerendering-5.8"
   #:module-subdir "VolumeRendering"
   #:patches (list "volumerendering/0001-ENH-Add-standalone-CMake-build-support-for-VolumeRen.patch"
                   "volumerendering/0002-COMP-Use-VTK-RenderingVolumeOpenGL2-directly-in-MRML.patch"
                   "volumerendering/0003-COMP-Add-Markups-module-dependencies-to-CMakeLists.patch"
                   "volumerendering/0004-COMP-Add-vtkSlicerMarkupsModuleMRML-include-dirs-to-.patch")
   #:synopsis "3D Slicer VolumeRendering loadable module"
   #:description
   "The VolumeRendering loadable module extracted from 3D Slicer.  It
provides CPU and GPU ray-cast volume rendering of scalar and multi-volume
data sets, including transfer-function presets, shader-property MRML nodes,
and a subject-hierarchy plugin.  Built from the
@file{Modules/Loadable/VolumeRendering} subtree of the Slicer source tree."
   ;; dcmtk is needed at runtime: libqSlicerVolumeRenderingModule.so directly
   ;; links against DCMTK libraries (via CTK DICOM widgets).  Adding it as a
   ;; direct input ensures its lib path ends up in the RUNPATH.
   #:extra-inputs (list slicer-subjecthierarchy-5.8
                        slicer-markups-5.8
                        slicer-volumes-5.8
                        dcmtk)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DqSlicerMarkupsModuleWidgets_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerMarkupsModuleWidgets")
      (string-append
       "-DqSlicerVolumesSubjectHierarchyPlugins_INCLUDE_DIRS="
       #$slicer-volumes-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerVolumesSubjectHierarchyPlugins")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-markups-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-volumes-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-transforms-5.8
  (make-slicer-loadable-module
   #:name "slicer-transforms-5.8"
   #:module-subdir "Transforms"
   #:patches (list "transforms/0001-ENH-Add-standalone-CMake-build-support-for-Transform.patch"
                   "transforms/0002-COMP-add-missing-CMake-dependencies.patch"
                   ;; Lower NRRD confidence to 0.4 unless header has "kinds: vector",
                   ;; so scalar volumes are not misidentified as grid transforms when
                   ;; the Transforms module is loaded before Volumes alphabetically.
                   "transforms/0003-COMP-Transforms-Lower-NRRD-confidence-unless-header-.patch")
   #:synopsis "3D Slicer Transforms loadable module"
   #:description
   "The Transforms loadable module extracted from 3D Slicer.  It provides
interactive display and editing of spatial transformation nodes (linear,
B-spline, and grid transforms) in 2D and 3D views, together with a
subject-hierarchy plugin for managing transform hierarchies.  Built from
the @file{Modules/Loadable/Transforms} subtree of the Slicer source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.8
                        slicer-markups-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-markups-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-texts-5.8
  (make-slicer-loadable-module
   #:name "slicer-texts-5.8"
   #:module-subdir "Texts"
   #:patches (list "texts/0001-ENH-Add-standalone-CMake-build-support-for-Texts-mod.patch"
                   "texts/0002-COMP-Add-MRMLCore-to-module-target-libraries.patch")
   #:synopsis "3D Slicer Texts loadable module"
   #:description
   "The Texts loadable module extracted from 3D Slicer.  It provides MRML
support for storing, displaying, and editing plain-text or rich-text
annotations attached to MRML scene nodes.  Includes a subject-hierarchy
plugin for managing text nodes.  Built from the
@file{Modules/Loadable/Texts} subtree of the Slicer source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.8
                        slicer-markups-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-markups-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-slicerwelcome-5.8
  (make-slicer-loadable-module
   #:name "slicer-slicerwelcome-5.8"
   #:module-subdir "SlicerWelcome"
   #:patches (list "slicerwelcome/0001-ENH-Add-standalone-CMake-build-support-for-SlicerWel.patch"
                   "slicerwelcome/0002-COMP-Guard-ExtensionUpdatesStatusButton-connection-w.patch")
   #:synopsis "3D Slicer Welcome loadable module"
   #:description
   "The Welcome loadable module extracted from 3D Slicer.  It provides the
Welcome screen shown to new users, with quick-access buttons for loading
data, accessing recent files, and linking to online resources.  Built from
the @file{Modules/Loadable/SlicerWelcome} subtree of the Slicer source
tree."))

;;;
;;; Meta-package
;;;

(define %slicer-5.8-loadable-modules
  ;; All standalone loadable modules for slicer-5.8, in dependency order.
  (list slicer-terminologies-5.8
        slicer-subjecthierarchy-5.8
        slicer-colors-5.8
        slicer-volumes-5.8
        slicer-volumerendering-5.8
        slicer-units-5.8
        slicer-tables-5.8
        slicer-cameras-5.8
        slicer-data-5.8
        slicer-annotations-5.8
        slicer-markups-5.8
        slicer-models-5.8
        slicer-sequences-5.8
        slicer-viewcontrollers-5.8
        slicer-reformat-5.8
        slicer-plots-5.8
        slicer-sceneviews-5.8
        slicer-transforms-5.8
        slicer-texts-5.8
        slicer-slicerwelcome-5.8
        slicer-segmentations-5.8))



;;;
;;; factory for standalone slicer scripted-module packages
;;;

;; analogous to make-slicer-loadable-module but for python scripted modules.
;; these require slicer-python-5.8 (slicer_use_pythonqt=on) and install
;; python scripts to lib/slicer-5.8/qt-scripted-modules/.
;;
;; each scripted-module source branch is named
;;   guix-systole-<modulename>-scripted-module-5.8.1
;; in ~/src/slicer/slicer-systole, following the same convention as the
;; loadable-module branches.  patches are generated with git format-patch and
;; stored under systole/packages/patches/slicer/<modulename>/.
(define* (make-slicer-scripted-module
          #:key
          name            ; package name string, e.g. "slicer-sampledata-5.8"
          module-subdir   ; source sub-directory, e.g. "sampledata"
          patches         ; list of patch filename strings
          synopsis        ; one-line synopsis string
          description     ; multi-line description string
          ;; extra packages added to inputs *before* slicer-python-5.8's own inputs.
          (extra-inputs '())
          ;; a gexp that evaluates to a (possibly empty) list of extra cmake -d flags.
          (extra-configure-flags #~'()))
  (package
   (name name)
   (version (package-version slicer-python-5.8))
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
          #~(append
             (list "-DCMAKE_BUILD_TYPE:STRING=Release"
                   "-DBUILD_TESTING:BOOL=OFF"
                   ;; point cmake at the python-enabled slicer config directory.
                   (string-append "-DSlicer_DIR="
                                  #$slicer-python-5.8
                                  "/lib/Slicer-5.8"))
             #$extra-configure-flags)
          #:phases
          ;; build only the named scripted-module sub-directory.
          #~(modify-phases %standard-phases
              (replace 'configure
                (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
                  (let* ((source (getcwd))
                         (out (assoc-ref outputs "out")))
                    (apply invoke "cmake"
                           "-S" (string-append source "/Modules/Scripted/"
                                               #$module-subdir)
                           "-B" "build"
                           (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                           configure-flags)
                    (chdir "build")
                    #t))))))
   ;; useslicer.cmake requires the full python-enabled dependency tree.
   ;; we start from slicer-python-5.8's inputs and prepend slicer-python-5.8
   ;; itself so cmake can locate slicerconfig.cmake.
   (inputs (fold (lambda (pkg acc)
                   (modify-inputs acc (prepend pkg)))
                 (modify-inputs (package-inputs slicer-python-5.8)
                   (prepend slicer-python-5.8))
                 extra-inputs))
   (home-page (package-home-page slicer-5.8))
   (synopsis synopsis)
   (description description)
   (license (package-license slicer-5.8))))

(define-public slicer-sampledata-5.8
  (make-slicer-scripted-module
   #:name "slicer-sampledata-5.8"
   #:module-subdir "SampleData"
   #:patches (list "sampledata/0001-ENH-Add-standalone-build-support-for-SampleData-scri.patch")
   #:synopsis "3d slicer sampledata scripted module"
   #:description
   "the sampledata scripted module extracted from 3d slicer.  it provides a
catalog of sample medical data sets (mr brain, ct chest, dti, etc.) that can
be downloaded and loaded directly from within slicer.  built from the
@file{modules/scripted/sampledata} subtree of the slicer source tree."))

(define-public slicer-endoscopy-5.8
  (make-slicer-scripted-module
   #:name "slicer-endoscopy-5.8"
   #:module-subdir "Endoscopy"
   #:patches (list "endoscopy/0001-ENH-Add-standalone-build-support-for-Endoscopy-scrip.patch")
   #:synopsis "3d slicer endoscopy scripted module"
   #:description
   "the endoscopy scripted module extracted from 3d slicer.  it provides
virtual endoscopy visualization by flying a camera through tubular structures
(e.g. the colon) along a path defined by a curve markup node.  built from the
@file{modules/scripted/endoscopy} subtree of the slicer source tree."))

(define-public slicer-importitksnaplabel-5.8
  (make-slicer-scripted-module
   #:name "slicer-importitksnaplabel-5.8"
   #:module-subdir "ImportItkSnapLabel"
   #:patches (list "importitksnaplabel/0001-ENH-Add-standalone-build-support-for-ImportItkSnapLa.patch")
   #:synopsis "3d slicer importitksnaplabel scripted module"
   #:description
   "the importitksnaplabel scripted module extracted from 3d slicer.  it
imports itk-snap label description files (.label) and creates a slicer color
table node from the label definitions.  built from the
@file{modules/scripted/importitksnaplabel} subtree of the slicer source tree."))

(define-public slicer-performancetests-5.8
  (make-slicer-scripted-module
   #:name "slicer-performancetests-5.8"
   #:module-subdir "PerformanceTests"
   #:patches (list "performancetests/0001-ENH-Add-standalone-build-support-for-PerformanceTest.patch")
   #:synopsis "3d slicer performancetests scripted module"
   #:description
   "the performancetests scripted module extracted from 3d slicer.  it runs a
suite of rendering and scene-management benchmarks to measure slicer
performance across different hardware configurations.  built from the
@file{modules/scripted/performancetests} subtree of the slicer source tree."))

(define-public slicer-selftests-5.8
  (make-slicer-scripted-module
   #:name "slicer-selftests-5.8"
   #:module-subdir "SelfTests"
   #:patches (list "selftests/0001-ENH-Add-standalone-build-support-for-SelfTests-scrip.patch")
   #:synopsis "3d slicer selftests scripted module"
   #:description
   "the selftests scripted module extracted from 3d slicer.  it provides a
graphical interface for running slicer's built-in python unit tests directly
from the application.  built from the
@file{modules/scripted/selftests} subtree of the slicer source tree."))

(define-public slicer-screencapture-5.8
  (make-slicer-scripted-module
   #:name "slicer-screencapture-5.8"
   #:module-subdir "ScreenCapture"
   #:patches (list "screencapture/0001-ENH-Add-standalone-build-support-for-ScreenCapture-s.patch")
   #:synopsis "3d slicer screencapture scripted module"
   #:description
   "the screencapture scripted module extracted from 3d slicer.  it captures
still images and animated sequences (video, animated gif) from any combination
of 2d and 3d views, with optional watermarking and rotation sweep.  built from
the @file{modules/scripted/screencapture} subtree of the slicer source tree."))

(define-public slicer-vectortoscalarvolume-5.8
  (make-slicer-scripted-module
   #:name "slicer-vectortoscalarvolume-5.8"
   #:module-subdir "VectorToScalarVolume"
   #:patches (list "vectortoscalarvolume/0001-ENH-Add-standalone-build-support-for-VectorToScalarV.patch")
   #:synopsis "3d slicer vectortoscalarvolume scripted module"
   #:description
   "the vectortoscalarvolume scripted module extracted from 3d slicer.  it
converts a vector-valued volume (e.g. rgb or multi-component images) to a
scalar volume by extracting a single component or computing a luminance or
magnitude measure.  built from the
@file{modules/scripted/vectortoscalarvolume} subtree of the slicer source
tree."))

(define-public slicer-dataprobe-5.8
  (make-slicer-scripted-module
   #:name "slicer-dataprobe-5.8"
   #:module-subdir "DataProbe"
   #:patches (list "dataprobe/0001-ENH-Add-standalone-build-support-for-DataProbe-scrip.patch")
   #:synopsis "3d slicer dataprobe scripted module"
   #:description
   "the dataprobe scripted module extracted from 3d slicer.  it displays
voxel-level annotation overlays in slice views showing the current cursor
position in ras coordinates, ijk voxel indices, and interpolated scalar
values for all visible volumes.  built from the
@file{modules/scripted/dataprobe} subtree of the slicer source tree."))

(define-public slicer-cropvolumesequence-5.8
  (make-slicer-scripted-module
   #:name "slicer-cropvolumesequence-5.8"
   #:module-subdir "CropVolumeSequence"
   #:patches (list "cropvolumesequence/0001-ENH-Add-standalone-build-support-for-CropVolumeSeque.patch")
   #:synopsis "3d slicer cropvolumesequence scripted module"
   #:description
   "the cropvolumesequence scripted module extracted from 3d slicer.  it
applies the same crop-volume roi to every item in a sequence node, enabling
batch cropping of 4d image series to a user-defined region of interest.  built
from the @file{modules/scripted/cropvolumesequence} subtree of the slicer
source tree."))

(define-public slicer-webserver-5.8
  (make-slicer-scripted-module
   #:name "slicer-webserver-5.8"
   #:module-subdir "WebServer"
   #:patches (list "webserver/0001-ENH-Add-standalone-build-support-for-WebServer-scrip.patch")
   #:synopsis "3d slicer webserver scripted module"
   #:description
   "the webserver scripted module extracted from 3d slicer.  it embeds a
lightweight http server inside slicer that exposes rest endpoints for scene
inspection, dicom browsing, and remote rendering, together with a built-in
web client served from the module's docroot.  built from the
@file{modules/scripted/webserver} subtree of the slicer source tree."))

(define-public slicer-dicompatcher-5.8
  (make-slicer-scripted-module
   #:name "slicer-dicompatcher-5.8"
   #:module-subdir "DICOMPatcher"
   #:patches (list "dicompatcher/0001-ENH-Add-standalone-build-support-for-DICOMPatcher-sc.patch")
   #:synopsis "3d slicer dicompatcher scripted module"
   #:description
   "the dicompatcher scripted module extracted from 3d slicer.  it provides a
graphical interface for fixing common dicom conformance issues in series
imported into the slicer dicom database, including missing or malformed tags,
incorrect transfer syntax, and multi-frame conversion.  built from the
@file{modules/scripted/dicompatcher} subtree of the slicer source tree."))

(define-public slicer-dicomplugins-5.8
  (make-slicer-scripted-module
   #:name "slicer-dicomplugins-5.8"
   #:module-subdir "DICOMPlugins"
   #:patches (list "dicomplugins/0001-ENH-Add-standalone-build-support-for-DICOMPlugins-sc.patch")
   #:synopsis "3d slicer dicomplugins scripted module"
   #:description
   "the dicomplugins scripted module extracted from 3d slicer.  it provides the
core set of dicom import plugins used by the slicer dicom browser, including
scalar volume, enhanced ultrasound volume, image sequence, volume sequence,
geabus, and slicer data bundle readers.  built from the
@file{modules/scripted/dicomplugins} subtree of the slicer source tree."))

(define-public slicer-segmenteditor-5.8
  (make-slicer-scripted-module
   #:name "slicer-segmenteditor-5.8"
   #:module-subdir "SegmentEditor"
   #:patches (list "segmenteditor/0001-ENH-Add-standalone-build-support-for-SegmentEditor-s.patch")
   #:synopsis "3d slicer segmenteditor scripted module"
   #:description
   "the segmenteditor scripted module extracted from 3d slicer.  it provides an
interactive segmentation editor supporting multiple effect plugins (paint,
draw, erase, threshold, grow from seeds, fill between slices, etc.) and
operates on segmentation nodes backed by the mrml segmentations framework.
built from the @file{modules/scripted/segmenteditor} subtree of the slicer
source tree."))

(define-public slicer-segmentstatistics-5.8
  (make-slicer-scripted-module
   #:name "slicer-segmentstatistics-5.8"
   #:module-subdir "SegmentStatistics"
   #:patches (list "segmentstatistics/0001-ENH-Add-standalone-build-support-for-SegmentStatisti.patch")
   #:synopsis "3d slicer segmentstatistics scripted module"
   #:description
   "the segmentstatistics scripted module extracted from 3d slicer.  it computes
quantitative statistics (volume, surface area, mean intensity, etc.) for each
segment in a segmentation node using a plugin-based architecture that supports
labelmap, scalar volume, and closed-surface measurement backends.  built from
the @file{modules/scripted/segmentstatistics} subtree of the slicer source
tree."))

(define-public slicer-dicom-5.8
  (make-slicer-scripted-module
   #:name "slicer-dicom-5.8"
   #:module-subdir "DICOM"
   #:patches (list "dicom/0001-ENH-Add-standalone-build-support-for-DICOM-scripted-.patch")
   #:synopsis "3d slicer dicom scripted module"
   #:description
   "the dicom scripted module extracted from 3d slicer.  it provides the main
dicom browser panel for importing dicom studies into the slicer scene,
browsing the local dicom database, and invoking loadable-series plugins
for import and export.  depends on @code{slicer-dicomplugins-5.8} and
@code{slicer-dicomlib-5.8} at runtime.  built from the
@file{modules/scripted/dicom} subtree of the slicer source tree."))

(define-public slicer-extensionwizard-5.8
  (make-slicer-scripted-module
   #:name "slicer-extensionwizard-5.8"
   #:module-subdir "ExtensionWizard"
   #:patches (list "extensionwizard/0001-ENH-Add-standalone-build-support-for-ExtensionWizard.patch")
   #:synopsis "3d slicer extensionwizard scripted module"
   #:description
   "the extensionwizard scripted module extracted from 3d slicer.  it provides
an interactive wizard for creating new slicer extensions and module templates,
managing template search paths, and editing extension metadata.  built from
the @file{modules/scripted/extensionwizard} subtree of the slicer source
tree."))

(define-public slicer-dicomlib-5.8
  (make-slicer-scripted-module
   #:name "slicer-dicomlib-5.8"
   #:module-subdir "DICOMLib"
   #:patches (list "dicomlib/0001-ENH-Add-standalone-build-support-for-DICOMLib-script.patch")
   #:synopsis "3d slicer dicomlib scripted module"
   #:description
   "the dicomlib scripted module extracted from 3d slicer.  it provides the
core dicom infrastructure: c++ logic for loadable and exportable series
descriptors, qt widgets for dicom export dialogs and tag editing, a
subject-hierarchy dicom plugin, and python utilities for dicom database
management, series import, and export.  built from the
@file{modules/scripted/dicomlib} subtree of the slicer source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-python-5.8
       "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-python-5.8
       "/lib/Slicer-5.8"))))

;;;
;;; scripted module list and python meta-package
;;;

(define %slicer-5.8-scripted-modules
  ;; all standalone scripted modules for slicer-python-5.8, in load order.
  (list slicer-sampledata-5.8
        slicer-endoscopy-5.8
        slicer-importitksnaplabel-5.8
        slicer-performancetests-5.8
        slicer-selftests-5.8
        slicer-screencapture-5.8
        slicer-vectortoscalarvolume-5.8
        slicer-dataprobe-5.8
        slicer-cropvolumesequence-5.8
        slicer-webserver-5.8
        slicer-dicompatcher-5.8
        slicer-dicomplugins-5.8
        slicer-segmenteditor-5.8
        slicer-segmentstatistics-5.8
        slicer-dicom-5.8
        slicer-extensionwizard-5.8
        slicer-dicomlib-5.8))


;;;
;;; cli module packages (tier 1: pure itk + slicerexecutionmodel)
;;;

(define-public slicer-add-scalar-volumes-5.8
  (make-slicer-cli-module
   #:name "slicer-add-scalar-volumes-5.8"
   #:module-subdir "AddScalarVolumes"
   #:patches (list "cli/addscalarvolumes/0001-ENH-Add-standalone-build-preamble-for-AddScalarVolum.patch")
   #:synopsis "3d slicer addscalarvolumes cli module"
   #:description
   "the addscalarvolumes cli module extracted from 3d slicer.  it adds two
scalar volumes voxel-by-voxel, with optional weighting factors.  built from
the @file{modules/cli/addscalarvolumes} subtree of the slicer source tree."))

(define-public slicer-cast-scalar-volume-5.8
  (make-slicer-cli-module
   #:name "slicer-cast-scalar-volume-5.8"
   #:module-subdir "CastScalarVolume"
   #:patches (list "cli/castscalarvolume/0001-ENH-Add-standalone-build-preamble-for-CastScalarVolu.patch")
   #:synopsis "3d slicer castscalarvolume cli module"
   #:description
   "the castscalarvolume cli module extracted from 3d slicer.  it casts a
scalar volume to a user-specified scalar type.  built from the
@file{modules/cli/castscalarvolume} subtree of the slicer source tree."))

(define-public slicer-checker-board-filter-5.8
  (make-slicer-cli-module
   #:name "slicer-checker-board-filter-5.8"
   #:module-subdir "CheckerBoardFilter"
   #:patches (list "cli/checkerboardfilter/0001-ENH-Add-standalone-build-preamble-for-CheckerBoardFi.patch")
   #:synopsis "3d slicer checkerboardfilter cli module"
   #:description
   "the checkerboardfilter cli module extracted from 3d slicer.  it creates a
checkerboard pattern image by compositing two scalar volumes — useful for
visually comparing registration results.  built from the
@file{modules/cli/checkerboardfilter} subtree of the slicer source tree."))

(define-public slicer-curvature-anisotropic-diffusion-5.8
  (make-slicer-cli-module
   #:name "slicer-curvature-anisotropic-diffusion-5.8"
   #:module-subdir "CurvatureAnisotropicDiffusion"
   #:patches (list "cli/curvatureanisotropicdiffusion/0001-ENH-Add-standalone-build-preamble-for-CurvatureAniso.patch")
   #:synopsis "3d slicer curvatureanisotropicdiffusion cli module"
   #:description
   "the curvatureanisotropicdiffusion cli module extracted from 3d slicer.  it
performs edge-preserving smoothing using curvature-driven anisotropic
diffusion.  built from the @file{modules/cli/curvatureanisotropicdiffusion}
subtree of the slicer source tree."))

(define-public slicer-gaussian-blur-image-filter-5.8
  (make-slicer-cli-module
   #:name "slicer-gaussian-blur-image-filter-5.8"
   #:module-subdir "GaussianBlurImageFilter"
   #:patches (list "cli/gaussianblurimagefilter/0001-ENH-Add-standalone-build-preamble-for-GaussianBlurIm.patch")
   #:synopsis "3d slicer gaussianblurimagefilter cli module"
   #:description
   "the gaussianblurimagefilter cli module extracted from 3d slicer.  it
applies a gaussian smoothing filter to a scalar volume.  built from the
@file{modules/cli/gaussianblurimagefilter} subtree of the slicer source tree."))

(define-public slicer-gradient-anisotropic-diffusion-5.8
  (make-slicer-cli-module
   #:name "slicer-gradient-anisotropic-diffusion-5.8"
   #:module-subdir "GradientAnisotropicDiffusion"
   #:patches (list "cli/gradientanisotropicdiffusion/0001-ENH-Add-standalone-build-preamble-for-GradientAnisot.patch")
   #:synopsis "3d slicer gradientanisotropicdiffusion cli module"
   #:description
   "the gradientanisotropicdiffusion cli module extracted from 3d slicer.  it
performs edge-preserving smoothing using gradient-driven anisotropic
diffusion.  built from the @file{modules/cli/gradientanisotropicdiffusion}
subtree of the slicer source tree."))

(define-public slicer-grayscale-fill-hole-image-filter-5.8
  (make-slicer-cli-module
   #:name "slicer-grayscale-fill-hole-image-filter-5.8"
   #:module-subdir "GrayscaleFillHoleImageFilter"
   #:patches (list "cli/grayscalefillholeimagefilter/0001-ENH-Add-standalone-build-preamble-for-GrayscaleFillH.patch")
   #:synopsis "3d slicer grayscalefillholeimagefilter cli module"
   #:description
   "the grayscalefillholeimagefilter cli module extracted from 3d slicer.  it
fills holes in a grayscale image using morphological reconstruction.  built
from the @file{modules/cli/grayscalefillholeimagefilter} subtree of the slicer
source tree."))

(define-public slicer-grayscale-grind-peak-image-filter-5.8
  (make-slicer-cli-module
   #:name "slicer-grayscale-grind-peak-image-filter-5.8"
   #:module-subdir "GrayscaleGrindPeakImageFilter"
   #:patches (list "cli/grayscalegrindpeakimagefilter/0001-ENH-Add-standalone-build-preamble-for-GrayscaleGrind.patch")
   #:synopsis "3d slicer grayscalegrindpeakimagefilter cli module"
   #:description
   "the grayscalegrindpeakimagefilter cli module extracted from 3d slicer.  it
removes peaks from a grayscale image using morphological reconstruction.
built from the @file{modules/cli/grayscalegrindpeakimagefilter} subtree of the
slicer source tree."))

(define-public slicer-histogram-matching-5.8
  (make-slicer-cli-module
   #:name "slicer-histogram-matching-5.8"
   #:module-subdir "HistogramMatching"
   #:patches (list "cli/histogrammatching/0001-ENH-Add-standalone-build-preamble-for-HistogramMatch.patch")
   #:synopsis "3d slicer histogrammatching cli module"
   #:description
   "the histogrammatching cli module extracted from 3d slicer.  it normalises
one image's intensity distribution to match a reference image's histogram.
built from the @file{modules/cli/histogrammatching} subtree of the slicer
source tree."))

(define-public slicer-image-label-combine-5.8
  (make-slicer-cli-module
   #:name "slicer-image-label-combine-5.8"
   #:module-subdir "ImageLabelCombine"
   #:patches (list "cli/imagelabelcombine/0001-ENH-Add-standalone-build-preamble-for-ImageLabelComb.patch")
   #:synopsis "3d slicer imagelabelcombine cli module"
   #:description
   "the imagelabelcombine cli module extracted from 3d slicer.  it combines
two label-map volumes into one, optionally resolving label conflicts.  built
from the @file{modules/cli/imagelabelcombine} subtree of the slicer source
tree."))

(define-public slicer-label-map-smoothing-5.8
  (make-slicer-cli-module
   #:name "slicer-label-map-smoothing-5.8"
   #:module-subdir "LabelMapSmoothing"
   #:patches (list "cli/labelmapsmoothing/0001-ENH-Add-standalone-build-preamble-for-LabelMapSmooth.patch")
   #:synopsis "3d slicer labelmapsmoothing cli module"
   #:description
   "the labelmapsmoothing cli module extracted from 3d slicer.  it smooths
the borders of label-map segmentations using an anti-aliased, gaussian-blurred
binary threshold pipeline.  built from the @file{modules/cli/labelmapsmoothing}
subtree of the slicer source tree."))

(define-public slicer-mask-scalar-volume-5.8
  (make-slicer-cli-module
   #:name "slicer-mask-scalar-volume-5.8"
   #:module-subdir "MaskScalarVolume"
   #:patches (list "cli/maskscalarvolume/0001-ENH-Add-standalone-build-preamble-for-MaskScalarVolu.patch")
   #:synopsis "3d slicer maskscalarvolume cli module"
   #:description
   "the maskscalarvolume cli module extracted from 3d slicer.  it applies a
binary mask image to a scalar volume, zeroing out voxels outside the mask.
built from the @file{modules/cli/maskscalarvolume} subtree of the slicer
source tree."))

(define-public slicer-median-image-filter-5.8
  (make-slicer-cli-module
   #:name "slicer-median-image-filter-5.8"
   #:module-subdir "MedianImageFilter"
   #:patches (list "cli/medianimagefilter/0001-ENH-Add-standalone-build-preamble-for-MedianImageFil.patch")
   #:synopsis "3d slicer medianimagefilter cli module"
   #:description
   "the medianimagefilter cli module extracted from 3d slicer.  it applies a
median filter of configurable radius to denoise a scalar volume.  built from
the @file{modules/cli/medianimagefilter} subtree of the slicer source tree."))

(define-public slicer-multiply-scalar-volumes-5.8
  (make-slicer-cli-module
   #:name "slicer-multiply-scalar-volumes-5.8"
   #:module-subdir "MultiplyScalarVolumes"
   #:patches (list "cli/multiplyscalarvolumes/0001-ENH-Add-standalone-build-preamble-for-MultiplyScalar.patch")
   #:synopsis "3d slicer multiplyscalarvolumes cli module"
   #:description
   "the multiplyscalarvolumes cli module extracted from 3d slicer.  it
multiplies two scalar volumes voxel-by-voxel.  built from the
@file{modules/cli/multiplyscalarvolumes} subtree of the slicer source tree."))

(define-public slicer-orient-scalar-volume-5.8
  (make-slicer-cli-module
   #:name "slicer-orient-scalar-volume-5.8"
   #:module-subdir "OrientScalarVolume"
   #:patches (list "cli/orientscalarvolume/0001-ENH-Add-standalone-build-preamble-for-OrientScalarVo.patch")
   #:synopsis "3d slicer orientscalarvolume cli module"
   #:description
   "the orientscalarvolume cli module extracted from 3d slicer.  it reorients
a scalar volume to a standard anatomical orientation (e.g. ras, lps, rai).
built from the @file{modules/cli/orientscalarvolume} subtree of the slicer
source tree."))

(define-public slicer-resample-scalar-volume-5.8
  (make-slicer-cli-module
   #:name "slicer-resample-scalar-volume-5.8"
   #:module-subdir "ResampleScalarVolume"
   #:patches (list "cli/resamplescalarvolume/0001-ENH-Add-standalone-build-preamble-for-ResampleScalar.patch")
   #:synopsis "3d slicer resamplescalarvolume cli module"
   #:description
   "the resamplescalarvolume cli module extracted from 3d slicer.  it
resamples a scalar volume to a new voxel spacing using linear, nearest-
neighbour, or b-spline interpolation.  built from the
@file{modules/cli/resamplescalarvolume} subtree of the slicer source tree."))

(define-public slicer-subtract-scalar-volumes-5.8
  (make-slicer-cli-module
   #:name "slicer-subtract-scalar-volumes-5.8"
   #:module-subdir "SubtractScalarVolumes"
   #:patches (list "cli/subtractscalarvolumes/0001-ENH-Add-standalone-build-preamble-for-SubtractScalar.patch")
   #:synopsis "3d slicer subtractscalarvolumes cli module"
   #:description
   "the subtractscalarvolumes cli module extracted from 3d slicer.  it
subtracts one scalar volume from another voxel-by-voxel, with optional
weighting factors.  built from the @file{modules/cli/subtractscalarvolumes}
subtree of the slicer source tree."))

(define-public slicer-threshold-scalar-volume-5.8
  (make-slicer-cli-module
   #:name "slicer-threshold-scalar-volume-5.8"
   #:module-subdir "ThresholdScalarVolume"
   #:patches (list "cli/thresholdscalarvolume/0001-ENH-Add-standalone-build-preamble-for-ThresholdScala.patch")
   #:synopsis "3d slicer thresholdscalarvolume cli module"
   #:description
   "the thresholdscalarvolume cli module extracted from 3d slicer.  it
thresholds a scalar volume by clamping, replacing, or zeroing voxels outside
a user-defined intensity range.  built from the
@file{modules/cli/thresholdscalarvolume} subtree of the slicer source tree."))

(define-public slicer-voting-binary-hole-filling-image-filter-5.8
  (make-slicer-cli-module
   #:name "slicer-voting-binary-hole-filling-image-filter-5.8"
   #:module-subdir "VotingBinaryHoleFillingImageFilter"
   #:patches (list "cli/votingbinaryholefillingimagefilter/0001-ENH-Add-standalone-build-preamble-for-VotingBinaryHo.patch")
   #:synopsis "3d slicer votingbinaryholefillingimagefilter cli module"
   #:description
   "the votingbinaryholefillingimagefilter cli module extracted from 3d slicer.
it fills binary holes in a label-map volume using majority-vote neighbourhood
inspection.  built from the
@file{modules/cli/votingbinaryholefillingimagefilter} subtree of the slicer
source tree."))

(define-public slicer-fiducial-registration-5.8
  (make-slicer-cli-module
   #:name "slicer-fiducial-registration-5.8"
   #:module-subdir "FiducialRegistration"
   #:patches (list "cli/fiducialregistration/0001-ENH-Add-standalone-build-preamble-for-FiducialRegist.patch")
   #:synopsis "3d slicer fiducialregistration cli module"
   #:description
   "the fiducialregistration cli module extracted from 3d slicer.  it computes
a rigid or similarity transform between two sets of corresponding fiducial
points using itk's landmark-based registration.  built from the
@file{modules/cli/fiducialregistration} subtree of the slicer source tree."))

(define-public slicer-create-dicom-series-5.8
  (make-slicer-cli-module
   #:name "slicer-create-dicom-series-5.8"
   #:module-subdir "CreateDICOMSeries"
   #:patches (list "cli/createdicomseries/0001-ENH-Add-standalone-build-preamble-for-CreateDICOMSer.patch")
   #:synopsis "3d slicer createdicomseries cli module"
   #:description
   "the createdicomseries cli module extracted from 3d slicer.  it converts a
scalar volume to a dicom image series, setting required dicom header fields.
built from the @file{modules/cli/createdicomseries} subtree of the slicer
source tree."))

(define-public slicer-n4-itk-bias-field-correction-5.8
  (make-slicer-cli-module
   #:name "slicer-n4-itk-bias-field-correction-5.8"
   #:module-subdir "N4ITKBiasFieldCorrection"
   #:patches (list "cli/n4itkbiasfieldcorrection/0001-ENH-Add-standalone-build-preamble-for-N4ITKBiasField.patch")
   #:synopsis "3d slicer n4itkbiasfieldcorrection cli module"
   #:description
   "the n4itkbiasfieldcorrection cli module extracted from 3d slicer.  it
corrects mr intensity non-uniformity (bias field) using the itk n4 algorithm.
built from the @file{modules/cli/n4itkbiasfieldcorrection} subtree of the
slicer source tree."))

(define-public slicer-simple-region-growing-segmentation-5.8
  (make-slicer-cli-module
   #:name "slicer-simple-region-growing-segmentation-5.8"
   #:module-subdir "SimpleRegionGrowingSegmentation"
   #:patches (list "cli/simpleregiongrowingsegmentation/0001-ENH-Add-standalone-build-preamble-for-SimpleRegionGr.patch")
   #:synopsis "3d slicer simpleregiongrowingsegmentation cli module"
   #:description
   "the simpleregiongrowingsegmentation cli module extracted from 3d slicer.
it performs connected-threshold region-growing segmentation starting from a
seed point, with optional smoothing.  built from the
@file{modules/cli/simpleregiongrowingsegmentation} subtree of the slicer
source tree."))

;;;
;;; cli module list and meta-package
;;;

(define %slicer-5.8-cli-modules
  (list slicer-add-scalar-volumes-5.8
        slicer-cast-scalar-volume-5.8
        slicer-checker-board-filter-5.8
        slicer-curvature-anisotropic-diffusion-5.8
        slicer-gaussian-blur-image-filter-5.8
        slicer-gradient-anisotropic-diffusion-5.8
        slicer-grayscale-fill-hole-image-filter-5.8
        slicer-grayscale-grind-peak-image-filter-5.8
        slicer-histogram-matching-5.8
        slicer-image-label-combine-5.8
        slicer-label-map-smoothing-5.8
        slicer-mask-scalar-volume-5.8
        slicer-median-image-filter-5.8
        slicer-multiply-scalar-volumes-5.8
        slicer-orient-scalar-volume-5.8
        slicer-resample-scalar-volume-5.8
        slicer-subtract-scalar-volumes-5.8
        slicer-threshold-scalar-volume-5.8
        slicer-voting-binary-hole-filling-image-filter-5.8
        slicer-fiducial-registration-5.8
        slicer-create-dicom-series-5.8
        slicer-n4-itk-bias-field-correction-5.8
        slicer-simple-region-growing-segmentation-5.8))

(define-public slicer-all-5.8
  (package
    (name "slicer-all-5.8")
    (version (package-version slicer-python-5.8))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     (append (list slicer-5.8)
             %slicer-5.8-loadable-modules
             %slicer-5.8-cli-modules))
    (synopsis "3d slicer 5.8 with all loadable modules")
    (description
     "meta-package that installs python-enabled 3d slicer 5.8 together with
all its standalone loadable modules (terminologies, subjecthierarchy, colors,
volumes, volumerendering, units, tables, cameras, data, annotations, markups,
models, sequences, viewcontrollers, reformat, plots, sceneviews, transforms,
texts, and welcome).  all loadable modules are built against
@code{slicer-python-5.8} so their build-time and runtime abis are
consistent.")
    (home-page (package-home-page slicer-python-5.8))
    (license (package-license slicer-python-5.8))))

(define-public slicer-python-all-5.8
  (package
    (name "slicer-python-all-5.8")
    (version (package-version slicer-python-5.8))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     ;; Python runtime packages live here (not in slicer-python-5.8) so that
     ;; --without-tests transforms on any of them do not cascade into every
     ;; loadable module derivation and cause duplicate builds.
     (append (list slicer-python-5.8
                   python-numpy python-requests python-pydicom python-scipy
                   python-dicomweb-client)
             %slicer-5.8-loadable-modules
             %slicer-5.8-scripted-modules
             %slicer-5.8-cli-modules))
    (synopsis "3d slicer 5.8 (python) with all loadable and scripted modules")
    (description
     "meta-package that installs python-enabled 3d slicer 5.8 together with
all its standalone loadable modules (terminologies, subjecthierarchy, colors,
volumes, volumerendering, units, tables, cameras, data, annotations, markups,
models, sequences, viewcontrollers, reformat, plots, sceneviews, transforms,
texts, slicerwelcome, segmentations) and all python scripted modules
(sampledata, endoscopy, importitksnaplabel, performancetests, selftests,
screencapture, vectortoscalarvolume, dataprobe, cropvolumesequence,
webserver, dicompatcher, dicomplugins, segmenteditor, segmentstatistics,
dicom, extensionwizard, dicomlib).")
    (home-page (package-home-page slicer-python-5.8))
    (license (package-license slicer-python-5.8))))
