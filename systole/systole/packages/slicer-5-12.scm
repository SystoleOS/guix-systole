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
(define-module (systole packages slicer-5-12)
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
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (systole packages python-xyz)
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
  #:use-module (guix git-download)
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
  #:use-module ((systole packages slicer-factory)
                #:select (slicerexecutionmodel
                          (make-slicer-loadable-module . factory:make-slicer-loadable-module)
                          (make-slicer-scripted-module . factory:make-slicer-scripted-module)))
  #:use-module (srfi srfi-1)
  )

;; The 87 base patches shared by slicer-5.12 and slicer-next (the latter
;; substitutes a rebased 0007 from the next/ subdirectory, see below).
(define %slicer-5.12-patch-names
  (list
   "0001-COMP-Add-vtk-CommonSystem-component-as-requirement.patch"
   "0002-COMP-Find-Eigen-required.patch"
   "0003-COMP-Adapt-to-new-qRestAPI-cmake.patch"
   "0004-COMP-Document-teem-imported-target-linkage.patch"
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
   "0049-COMP-Guard-Windows-11-numpy-scipy-preload-workaround.patch"
   "0050-COMP-Fall-back-to-no-module-when-saved-home-module-i.patch"
   "0051-ENH-Load-custom-splash-screen-and-QSS-stylesheet-fro.patch"
   "0052-ENH-Execute-SLICER_INIT_DIR-init.py-after-slicerqt.p.patch"
   "0053-COMP-Expose-extension-build-vars-in-install-tree-Sli.patch"
   "0054-ENH-Auto-discover-Guix-installed-modules-via-GUIX_EN.patch"
   "0055-ENH-Bake-GLEW-store-paths-into-install-tree-SlicerIn.patch"
   "0056-COMP-Install-CXX-test-templates-and-expose-Slicer_CX.patch"
   "0057-ENH-Add-slicer-launch-wrapper-and-set-Slicer_LAUNCH_.patch"
   "0058-COMP-Expose-Slicer_PYTHON_MODULE_TEST_TEMPLATES_DIR-.patch"
   "0059-COMP-Install-CMake-.cmake.in-templates-to-developmen.patch"
   "0060-COMP-Add-Qt5-component-include-dirs-globally-in-UseS.patch"
   "0061-COMP-Fix-vtkAddon_LIB_DIR-in-install-tree-Slicer_Lib.patch"
   "0062-COMP-Set-CMP0177-policy-in-SlicerMacroBuildModuleVTK.patch"
   "0063-COMP-Fix-Slicer_Base_INCLUDE_DIRS-in-install-tree-Sl.patch"
   "0064-COMP-Install-.txx-template-files-alongside-.h-in-Bas.patch"
   "0065-COMP-Bake-PYTHONQT_INSTALL_DIR-into-install-tree-Sli.patch"
   "0066-COMP-Link-qSlicerBaseQTApp-and-CTKScriptingPythonCor.patch"
   "0067-COMP-Guard-vtkWin32OutputWindow.h-include-behind-WIN.patch"
   "0068-COMP-Link-qSlicerBaseQTApp-and-CTKScriptingPythonCor.patch"
   "0069-COMP-Install-.txx-template-files-in-MRMLCore-develop.patch"
   "0070-COMP-Expose-Slicer_QTLOADABLEMODULES_-SUBDIR-BIN-LIB.patch"
   "0071-COMP-Register-Slicer-qMRML-designer-plugins-dir-so-Q.patch"
   "0072-COMP-Export-Slicer_BUILD_QT_DESIGNER_PLUGINS-in-inst.patch"
   "0073-COMP-Fix-designer-plugin-build-dir-and-install-path-.patch"
   "0074-COMP-Install-Slicer-VTK-hierarchy-files-and-expose-p.patch"
   "0075-COMP-Use-lowercase-rapidjson-target-in-MRMLCore.patch"
   "0076-COMP-Move-Q_OBJECT-headers-to-MOC_SRCS-in-qMRMLWidge.patch"
   "0077-COMP-Move-qSlicerStylePlugin-Q_OBJECT-header-to-MOC_.patch"
   "0078-COMP-Move-Q_OBJECT-headers-to-MOC_SRCS-in-QTGUI-QTCL.patch"
   "0079-COMP-Install-AUTOUIC-generated-ui_-.h-from-qMRMLWidg.patch"
   "0080-COMP-Install-AUTOUIC-generated-ui_-.h-from-qSlicerBa.patch"
   "0081-COMP-Guard-ui_qSlicerWebWidget.h-install-on-WEBENGIN.patch"
   "0082-COMP-Install-AUTOUIC-generated-ui_-.h-from-qSlicerBa.patch"
   "0083-COMP-Add-qMRMLWidgets-to-standalone-loadable-module-.patch"
   "0084-COMP-Fix-missing-endif-for-Slicer_BUILD_CLI_SUPPORT-.patch"
   "0085-COMP-Fix-MOC-processing-for-qSlicerIconEnginePlugin.patch"
   "0086-ENH-Install-SlicerWizard-regardless-of-extension-man.patch"
   "0087-ENH-Also-descend-into-SlicerWizard-subdir-without-ex.patch"
   ;; 0088: upstream 5.12 switched seven libraries to GenerateExportHeader
   ;; but left ${configure_header_file} (never set) in their development
   ;; install lines; install the generated *Export.h explicitly instead.
   "0088-COMP-Install-the-generated-export-headers-in-develop.patch"))

;; Slicer stable 5.12.2 (tag v5.12.2).  The 88 guix-systole patches are the
;; 5.10 patch series rebased onto v5.12.2 (branch guix-systole-slicer-5.12 in
;; the Slicer-Systole repository); see patches/slicer-5.12/.
(define %slicer-5.12-commit "f7879b5651239865f336e5731e7f9e9a65d63871")
(define %slicer-5.12-hash (base32 "0imnj4h187f213xzbihrkxl03m9cikvrfs421273m90pcllvg9cy"))

;;;
;;; Slicer 5.12
;;;

(define %slicer-5.12
  (package
    (name "slicer-5.12")
    (version "5.12.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Slicer/Slicer")
             (commit %slicer-5.12-commit)))
       (file-name (git-file-name name version))
       (sha256 %slicer-5.12-hash)
       (patches (map (lambda (p) (slicer-patch "5.12" p))
              %slicer-5.12-patch-names))))

    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:validate-runpath? #f
           #:configure-flags
           #~(list
              ;; Compiler info
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
              "-DSlicer_BUILD_ITKPython:BOOL=OFF"

              ;; CLI — support library only
              "-DSlicer_BUILD_CLI:BOOL=OFF"
              "-DSlicer_BUILD_CLI_SUPPORT:BOOL=ON"
              (string-append "-DSlicerExecutionModel_DIR="
                             #$(this-package-input "slicerexecutionmodel")
                             "/lib")
              (string-append "-DGenerateCLP_DIR="
                             #$(this-package-input "slicerexecutionmodel")
                             "/lib/GenerateCLP")

              ;; QT
              "-DSlicer_BUILD_QTLOADABLEMODULES:BOOL=OFF"
              "-DSlicer_BUILD_QTSCRIPTEDMODULES:BOOL=OFF"
              "-DSlicer_BUILD_QT_DESIGNER_PLUGINS:BOOL=ON"
              "-DSlicer_USE_QtTesting:BOOL=OFF"
              "-DSlicer_USE_SlicerITK:BOOL=ON"
              "-DSlicer_USE_CTKAPPLAUNCHER:BOOL=OFF"
              "-DSlicer_BUILD_WEBENGINE_SUPPORT:BOOL=OFF"
              (string-append "-DQt5_DIR:PATH="
                             #$(this-package-input "qtbase"))
              "-DSlicer_VTK_VERSION_MAJOR:STRING=9"
              "-DSlicer_VTK_VERSION_MINOR:STRING=6"
              "-DSlicer_BUILD_vtkAddon:BOOL=OFF"

              "-DSlicer_INSTALL_DEVELOPMENT:BOOL=ON"
              "-DSlicer_USE_TBB:BOOL=ON"
              "-DSlicer_BUILD_DICOM_SUPPORT:BOOL=ON"

              ;; Python (disabled in base variant; enabled in slicer-5.12)
              "-DVTK_WRAP_PYTHON:BOOL=OFF"
              "-DSlicer_USE_PYTHONQT:BOOL=OFF"
              "-DSlicer_USE_SYSTEM_python:BOOL=OFF"

              ;; External dependencies
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

              ;; Hack to fix "Variable Slicer_WC_LAST_CHANGED_DATE is expected to be defined."
              "-DSlicer_WC_LAST_CHANGED_DATE:STRING=2026-6-1 00:00:00 -0500")
           #:out-of-source? #t
           #:phases
           #~(modify-phases %standard-phases
                            (add-before 'configure 'set-cmake-paths
                                        (lambda _
                                          (setenv "CMAKE_PREFIX_PATH"
                                                  (string-append #$(this-package-input "vtkaddon-9.6")
                                                                 "/lib/cmake:"
                                                                 (or (getenv "CMAKE_PREFIX_PATH")
                                                                     "")))))

                            (add-after 'install 'patch-runpath
                              (lambda _
                                (invoke "patchelf" "--add-rpath"
                                        (string-append "$ORIGIN/../lib/Slicer-5.12"
                                                       ":"
                                                       "$ORIGIN/../lib/Slicer-5.12/qt-loadable-modules")
                                        (string-append #$output "/bin/SlicerApp-real"))))
                            (add-after 'patch-runpath 'install-slicer-symlink
                              (lambda _
                                (let ((wrapper (string-append #$output "/bin/Slicer")))
                                  (when (file-exists? wrapper)
                                    (delete-file wrapper))
                                  (call-with-output-file wrapper
                                    (lambda (port)
                                      (display "#!/bin/sh\n" port)
                                      (display "# Slicer launcher: set LD_LIBRARY_PATH from\n" port)
                                      (display "# SLICER_ADDITIONAL_MODULE_PATHS then exec SlicerApp-real.\n" port)
                                      (display "IFS=:\n" port)
                                      (display "for _d in $SLICER_ADDITIONAL_MODULE_PATHS; do\n" port)
                                      (display "  LD_LIBRARY_PATH=\"$_d${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"\n" port)
                                      (display "done\n" port)
                                      (display "unset IFS\n" port)
                                      (display "if [ -n \"$GUIX_ENVIRONMENT\" ]; then\n" port)
                                      (display "  _guix_mods=\"$GUIX_ENVIRONMENT/lib/Slicer-5.12/qt-loadable-modules\"\n" port)
                                      (display "  if [ -d \"$_guix_mods\" ]; then\n" port)
                                      (display "    LD_LIBRARY_PATH=\"$_guix_mods${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"\n" port)
                                      (display "  fi\n" port)
                                      (display "fi\n" port)
                                      (display "export LD_LIBRARY_PATH\n" port)
                                      (display "export PIP_USER=1\n" port)
                                      (display "_dir=\"$(dirname \"$(readlink -f \"$0\")\")\"\n" port)
                                      (display "exec \"$_dir/SlicerApp-real\" \"$@\"\n" port)))
                                  (chmod wrapper #o755))))
                            (add-after 'install-slicer-symlink 'install-slicer-launch
                              (lambda _
                                (let ((script (string-append #$output "/bin/slicer-launch")))
                                  (call-with-output-file script
                                    (lambda (port)
                                      (display "#!/bin/sh\n" port)
                                      (display "# slicer-launch: extend LD_LIBRARY_PATH from\n" port)
                                      (display "# SLICER_ADDITIONAL_MODULE_PATHS then exec args.\n" port)
                                      (display "IFS=:\n" port)
                                      (display "for _d in $SLICER_ADDITIONAL_MODULE_PATHS; do\n" port)
                                      (display "  LD_LIBRARY_PATH=\"$_d${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"\n" port)
                                      (display "done\n" port)
                                      (display "unset IFS\n" port)
                                      (display "export LD_LIBRARY_PATH\n" port)
                                      (display "export PIP_USER=1\n" port)
                                      (display "exec \"$@\"\n" port)))
                                  (chmod script #o755))))
                            )))
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
           mesa
           rapidjson
           tbb

           ;; QT5
           qtbase-5
           qtmultimedia-5
           qtxmlpatterns-5
           qtdeclarative-5
           qtsvg-5
           qtx11extras
           qtwebchannel-5
           qttools-5

           ;; VTK 9.6
           vtk-slicer-9.6
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
           ctk-for-slicer-5.12
           ctkapplauncher
           itk-slicer-5.4.6
           libarchive-slicer
           teem-slicer
           vtkaddon-9.6
           slicerexecutionmodel
           qrestapi))
    (native-inputs (list patchelf pkg-config))
    (native-search-paths
     (list (search-path-specification
            (variable "SLICER_ADDITIONAL_MODULE_PATHS")
            (files '("lib/Slicer-5.12/qt-loadable-modules"
                     "lib/Slicer-5.12/cli-modules")))
           (search-path-specification
            (variable "SLICER_INIT_DIR")
            (files '("share/slicer-init"))
            (separator #f))))
    (synopsis "3D Slicer - Medical visualization and computing environment")
    (description
     "3D Slicer is a multi-platform, free and open source software package for
visualization and medical image computing.")
    (home-page "https://www.slicer.org/")
    (license license:bsd-3)))

;; slicer-5.12 is the canonical public Slicer 5.12 package.  Python support is
;; enabled via python-3.12.  VTK 9.6, ITK 5.4.6, and vtkAddon (2ed3e222) are used.
(define-public slicer-5.12
  (package
    (inherit %slicer-5.12)
    (name "slicer-5.12")
    (arguments
     (substitute-keyword-arguments (package-arguments %slicer-5.12)
       ((#:configure-flags flags)
        #~(append
           (list
            ;; Python 3.12.  Capture the package binding directly -- input
            ;; labels follow upstream package NAMES, and python-3.12's name
            ;; changed from "python-next" to "python" when it became the
            ;; default, silently turning label lookups into #f.
            (string-append "-DPython3_EXECUTABLE="
                           #$python-3.12 "/bin/python3")
            (string-append "-DPython3_INCLUDE_DIR="
                           #$python-3.12 "/include/python"
                           #$(version-major+minor (package-version python-3.12)))
            (string-append "-DPython3_LIBRARY="
                           #$python-3.12 "/lib/libpython"
                           #$(version-major+minor (package-version python-3.12)) ".so")
            "-DVTK_WRAP_PYTHON:BOOL=ON"
            "-DSlicer_USE_PYTHONQT:BOOL=ON"
            "-DSlicer_USE_SYSTEM_python:BOOL=ON"
            "-DSlicer_BUILD_QTSCRIPTEDMODULES:BOOL=OFF"
            (string-append "-DPYTHONQT_INSTALL_DIR="
                           #$(this-package-input "pythonqt-commontk-for-slicer-5.12"))
            (string-append "-DvtkAddon_CMAKE_DIR="
                           #$(this-package-input "vtkaddon-9.6") "/lib/cmake"))
           (filter (lambda (f)
                     (not (member f '("-DVTK_WRAP_PYTHON:BOOL=OFF"
                                      "-DSlicer_USE_PYTHONQT:BOOL=OFF"
                                      "-DSlicer_USE_SYSTEM_python:BOOL=OFF"
                                      "-DSlicer_BUILD_QTSCRIPTEDMODULES:BOOL=OFF"))))
                   #$flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'set-cmake-paths
              (lambda _
                (setenv "CMAKE_PREFIX_PATH"
                        (string-append
                         #$(this-package-input "pythonqt-commontk-for-slicer-5.12") "/lib/cmake:"
                         #$(this-package-input "vtkaddon-9.6") "/lib/cmake:"
                         (or (getenv "CMAKE_PREFIX_PATH") "")))))
            (add-after 'install-slicer-symlink 'patch-python-extension-runpath
              (lambda _
                (let ((dir (string-append #$output "/lib/Slicer-5.12")))
                  (for-each
                   (lambda (lib) (invoke "patchelf" "--add-rpath" "$ORIGIN" lib))
                   (find-files dir
                     (lambda (f stat)
                       (let ((rel (string-drop f (1+ (string-length dir)))))
                         (and (string-suffix? ".so" rel)
                              (not (string-contains rel "/"))))))))))
            (add-after 'patch-python-extension-runpath 'link-vtkaddon-python
              (lambda _
                (symlink
                 (string-append #$(this-package-input "vtkaddon-9.6")
                                "/lib/vtkAddonPython.so")
                 (string-append #$output
                                "/lib/Slicer-5.12/vtkAddonPython.so"))))
            (add-after 'link-vtkaddon-python 'create-logic-shim
              (lambda _
                (call-with-output-file
                    (string-append #$output "/bin/Python/logic.py")
                  (lambda (port)
                    (display "from slicer.logic import *\n" port)))))))))
    (inputs
     (modify-inputs (package-inputs %slicer-5.12)
       (prepend python-3.12 pythonqt-commontk-for-slicer-5.12)))
    (propagated-inputs (list vtk-slicer-9.6 ctk-for-slicer-5.12 vtkaddon-9.6 hdf5-1.10 libtheora
                             netcdf-slicer proj jsoncpp libharu gl2ps eigen
                             openmpi double-conversion lz4 libxml2
                             qtbase-5 qttools-5 qtxmlpatterns-5 qtsvg-5
                             qtmultimedia-5 qtx11extras qtdeclarative-5))
    (native-search-paths
     (list (search-path-specification
            (variable "CMAKE_PREFIX_PATH")
            (files '("")))
           (search-path-specification
            (variable "SLICER_ADDITIONAL_MODULE_PATHS")
            (files '("lib/Slicer-5.12/qt-loadable-modules"
                     "lib/Slicer-5.12/qt-scripted-modules"
                     "lib/Slicer-5.12/cli-modules")))
           (search-path-specification
            (variable "SLICER_PYTHONPATH")
            (files '("bin/Python"
                     "lib/Slicer-5.12"
                     "lib/python3.12/site-packages"
                     ;; Pure-Python packages (requests, pydicom, dicomweb-client)
                     ;; install to lib/python3.11/site-packages in the Guix default
                     ;; Python builds.  Their .py files are version-independent and
                     ;; importable from Python 3.12.  Compiled extensions (numpy,
                     ;; scipy) will still fail to import because the .so files have
                     ;; cpython-311 in their names, but pure Python packages work.
                     "lib/python3.11/site-packages")))))))

;;;
;;; Slicer 5.12 — standalone loadable/scripted module factories
;;;

;; Thin wrappers around the version-neutral factories from (systole packages
;; slicer-factory), with the 5.12 base package, version string, and PythonQt
;; pre-applied.  Keyword arguments are passed through unchanged.

(define (make-slicer-loadable-module-5.12 . args)
  (apply factory:make-slicer-loadable-module
         #:slicer slicer-5.12
         #:slicer-version "5.12"
         #:pythonqt pythonqt-commontk-for-slicer-5.12
         ;; VTK 9.6 wrappers no longer inherit libpython transitively;
         ;; explicit hints make the module's *Python.so link it (see the
         ;; factory's #:python docstring).
         #:python python-3.12
         args))

(define (make-slicer-scripted-module-5.12 . args)
  (apply factory:make-slicer-scripted-module
         #:slicer slicer-5.12
         #:slicer-version "5.12"
         #:python python-3.12
         args))

;;;
;;; Slicer 5.12 — standalone loadable/scripted module packages
;;;

(define-public slicer-terminologies-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-terminologies-5.12"
   #:module-subdir "Terminologies"
   #:patches (list "terminologies/0001-ENH-Add-standalone-build-support-for-Terminologies-m.patch")
   #:synopsis "3D Slicer Terminologies loadable module (5.12)"
   #:description
   "The Terminologies loadable module extracted from 3D Slicer 5.12.  It provides
DICOM-based anatomical and segmentation terminology support (category, type,
modifier look-ups backed by JSON terminology files) and is built from the
@file{Modules/Loadable/Terminologies} subtree of the Slicer 5.12 source tree."
   #:extra-configure-flags
   #~(list (string-append "-DRapidJSON_DIR="
                          #$rapidjson
                          "/lib/cmake/RapidJSON"))))

(define-public slicer-subjecthierarchy-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-subjecthierarchy-5.12"
   #:module-subdir "SubjectHierarchy"
   #:patches (list "subjecthierarchy/0001-ENH-Add-standalone-build-support-for-SubjectHierarch.patch"
                  "subjecthierarchy/0002-COMP-Move-Q_OBJECT-headers-to-MOC_SRCS-in-SubjectHie.patch"
                  "subjecthierarchy/0003-BUG-Use-abspath-not-realpath-so-merged-profile-dirs-.patch")
   #:synopsis "3D Slicer SubjectHierarchy loadable module (5.12)"
   #:description
   "The SubjectHierarchy loadable module extracted from 3D Slicer 5.12.  It
provides a hierarchical data model for MRML scene items together with a subject
hierarchy tree view, plugin infrastructure for per-node context menus, and
default plugins for cloning, folding, opacity, visibility, and registration
actions.  Built from the @file{Modules/Loadable/SubjectHierarchy} subtree of
the Slicer 5.12 source tree."
   #:extra-inputs (list slicer-terminologies-5.12)
   #:propagated-inputs (list slicer-terminologies-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerTerminologiesModuleWidgets_INCLUDE_DIRS="
       #$slicer-terminologies-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerTerminologiesModuleWidgets")
      (string-append
       "-DvtkSlicerTerminologiesModuleLogic_INCLUDE_DIRS="
       #$slicer-terminologies-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerTerminologiesModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-terminologies-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-tables-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-tables-5.12"
   #:module-subdir "Tables"
   #:patches (list "tables/0001-ENH-Add-standalone-build-support-for-Tables-module-5.patch")
   #:synopsis "3D Slicer Tables loadable module (5.12)"
   #:description
   "The Tables loadable module extracted from 3D Slicer 5.12.  It provides
tabular data display and editing with MRML table nodes and subject hierarchy
integration.  Built from the @file{Modules/Loadable/Tables} subtree of
the Slicer 5.12 source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12)
   #:propagated-inputs (list slicer-subjecthierarchy-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DvtkSlicerTablesModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-cameras-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-cameras-5.12"
   #:module-subdir "Cameras"
   #:patches (list "cameras/0001-ENH-Add-standalone-build-support-for-Cameras-module-.patch")
   #:synopsis "3D Slicer Cameras loadable module (5.12)"
   #:description
   "The Cameras loadable module extracted from 3D Slicer 5.12.  It provides
management of camera nodes in the MRML scene with per-layout camera
persistence.  Built from the @file{Modules/Loadable/Cameras} subtree of
the Slicer 5.12 source tree."))

(define-public slicer-data-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-data-5.12"
   #:module-subdir "Data"
   #:patches (list "data/0001-ENH-Add-standalone-build-support-for-Data-module-5.1.patch"
                  "data/0002-COMP-Pass-LINK_DIRECTORIES-to-slicerMacroBuildLoadab.patch")
   #:synopsis "3D Slicer Data loadable module (5.12)"
   #:description
   "The Data loadable module extracted from 3D Slicer 5.12.  It provides
MRML scene management, scene reader/writer, and subject hierarchy tree view
integration.  Built from the @file{Modules/Loadable/Data} subtree of
the Slicer 5.12 source tree."
   #:extra-inputs (list slicer-cameras-5.12
                        slicer-subjecthierarchy-5.12)
   #:propagated-inputs (list slicer-cameras-5.12
                             slicer-subjecthierarchy-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DvtkSlicerCamerasModuleLogic_INCLUDE_DIRS="
       #$slicer-cameras-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerCamerasModuleLogic")
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-cameras-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-units-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-units-5.12"
   #:module-subdir "Units"
   #:patches (list "units/0001-ENH-Add-standalone-build-support-for-Units-module-5..patch")
   #:synopsis "3D Slicer Units loadable module (5.12)"
   #:description
   "The Units loadable module extracted from 3D Slicer 5.12.  It provides
unit management (length, time, frequency, velocity, temperature, angle)
with per-node unit definitions and a settings panel.  Built from the
@file{Modules/Loadable/Units} subtree of the Slicer 5.12 source tree."))

(define-public slicer-colors-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-colors-5.12"
   #:module-subdir "Colors"
   #:patches (list "colors/0001-ENH-Add-standalone-build-support-for-Colors-module-5.patch"
                  "colors/0002-COMP-Link-VTK-RenderingAnnotation-and-RenderingCore-.patch"
                  "colors/0003-COMP-Add-vtkSlicerTerminologiesModuleLogic-include-d.patch"
                  "colors/0004-COMP-Link-vtkSlicerTerminologiesModuleLogic-in-Color.patch")
   #:synopsis "3D Slicer Colors loadable module (5.12)"
   #:description
   "The Colors loadable module extracted from 3D Slicer 5.12.  It provides
color table management, color legend display nodes, and subject hierarchy
integration for color overlays.  Built from the
@file{Modules/Loadable/Colors} subtree of the Slicer 5.12 source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12
                        slicer-terminologies-5.12)
   #:propagated-inputs (list slicer-subjecthierarchy-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DqSlicerTerminologiesModuleWidgets_INCLUDE_DIRS="
       #$slicer-terminologies-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerTerminologiesModuleWidgets")
      (string-append
       "-DvtkSlicerTerminologiesModuleLogic_INCLUDE_DIRS="
       #$slicer-terminologies-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerTerminologiesModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-terminologies-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-annotations-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-annotations-5.12"
   #:module-subdir "Annotations"
   #:patches (list "annotations/0001-ENH-Add-standalone-build-support-for-Annotations-mod.patch"
                  "annotations/0002-COMP-Add-ITK-Common-to-Annotations-MRML-link-librari.patch")
   #:synopsis "3D Slicer Annotations loadable module (5.12)"
   #:description
   "The Annotations loadable module extracted from 3D Slicer 5.12.  It provides
legacy annotation support for fiducials, rulers, and ROIs with backward
compatibility for older Slicer scenes.  Built from the
@file{Modules/Loadable/Annotations} subtree of the Slicer 5.12 source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12)
   #:propagated-inputs (list slicer-subjecthierarchy-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-markups-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-markups-5.12"
   #:module-subdir "Markups"
   #:patches (list "markups/0001-ENH-Add-standalone-build-support-for-Markups-module-.patch"
                  "markups/0002-COMP-Fix-Markups-MRML-for-standalone-build-5.12.patch"
                  "markups/0003-COMP-Add-Annotations-include-dir-and-LINK_DIRECTORIE.patch"
                  "markups/0004-COMP-Add-vtkSlicerTerminologiesModuleLogic-and-LINK_.patch"
                  "markups/0005-COMP-Fix-MOC_SRCS-for-DesignerPlugins-and-add-CTKScr.patch")
   #:synopsis "3D Slicer Markups loadable module (5.12)"
   #:description
   "The Markups loadable module extracted from 3D Slicer 5.12.  It provides
fiducials, lines, angles, curves, planes, and ROIs, along with a subject
hierarchy plugin and legacy Annotations reader.  Built from the
@file{Modules/Loadable/Markups} subtree of the Slicer 5.12 source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12
                        slicer-annotations-5.12
                        slicer-colors-5.12
                        slicer-terminologies-5.12)
   #:propagated-inputs (list slicer-colors-5.12
                             slicer-annotations-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DqSlicerColorsModuleWidgets_INCLUDE_DIRS="
       #$slicer-colors-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerColorsModuleWidgets")
      (string-append
       "-DvtkSlicerColorsModuleMRML_INCLUDE_DIRS="
       #$slicer-colors-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerColorsModuleMRML")
      (string-append
       "-DvtkSlicerColorsModuleLogic_INCLUDE_DIRS="
       #$slicer-colors-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerColorsModuleLogic")
      (string-append
       "-DqSlicerTerminologiesModuleWidgets_INCLUDE_DIRS="
       #$slicer-terminologies-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerTerminologiesModuleWidgets")
      (string-append
       "-DvtkSlicerTerminologiesModuleLogic_INCLUDE_DIRS="
       #$slicer-terminologies-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerTerminologiesModuleLogic")
      (string-append
       "-DvtkSlicerAnnotationsModuleMRML_INCLUDE_DIRS="
       #$slicer-annotations-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerAnnotationsModuleMRML")
      (string-append
       "-DvtkSlicerAnnotationsModuleLogic_INCLUDE_DIRS="
       #$slicer-annotations-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerAnnotationsModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-annotations-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-colors-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-terminologies-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-reformat-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-reformat-5.12"
   #:module-subdir "Reformat"
   #:patches (list "reformat/0001-ENH-Add-standalone-build-support-for-Reformat-module.patch")
   #:synopsis "3D Slicer Reformat loadable module (5.12)"
   #:description
   "The Reformat loadable module extracted from 3D Slicer 5.12.  It provides
oblique slice reformat controls.  Built from the
@file{Modules/Loadable/Reformat} subtree of the Slicer 5.12 source tree."))

(define-public slicer-plots-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-plots-5.12"
   #:module-subdir "Plots"
   #:patches (list "plots/0001-ENH-Add-standalone-build-support-for-Plots-module-5..patch")
   #:synopsis "3D Slicer Plots loadable module (5.12)"
   #:description
   "The Plots loadable module extracted from 3D Slicer 5.12.  It provides
chart and plot visualization.  Built from the
@file{Modules/Loadable/Plots} subtree of the Slicer 5.12 source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12)
   #:propagated-inputs (list slicer-subjecthierarchy-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-viewcontrollers-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-viewcontrollers-5.12"
   #:module-subdir "ViewControllers"
   #:patches (list "viewcontrollers/0001-ENH-Add-standalone-build-support-for-ViewControllers.patch")
   #:synopsis "3D Slicer ViewControllers loadable module (5.12)"
   #:description
   "The ViewControllers loadable module extracted from 3D Slicer 5.12.  It provides
slice and 3D view controller widgets.  Built from the
@file{Modules/Loadable/ViewControllers} subtree of the Slicer 5.12 source tree."))

(define-public slicer-sequences-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-sequences-5.12"
   #:module-subdir "Sequences"
   #:patches (list "sequences/0001-ENH-Add-standalone-build-support-for-Sequences-modul.patch")
   #:synopsis "3D Slicer Sequences loadable module (5.12)"
   #:description
   "The Sequences loadable module extracted from 3D Slicer 5.12.  It provides
sequence browser functionality for time-varying data.  Built from the
@file{Modules/Loadable/Sequences} subtree of the Slicer 5.12 source tree."))

(define-public slicer-sceneviews-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-sceneviews-5.12"
   #:module-subdir "SceneViews"
   #:patches (list "sceneviews/0001-ENH-Add-standalone-build-support-for-SceneViews-modu.patch"
                   "sceneviews/0002-COMP-Add-LINK_DIRECTORIES-to-SceneViews-Logic-for-st.patch"
                   "sceneviews/0003-COMP-Add-vtkSlicerSequencesModuleMRML-to-SceneViews-.patch"
                   "sceneviews/0004-COMP-Add-qSlicerBaseQTApp-to-SceneViews-MODULE_TARGE.patch")
   #:synopsis "3D Slicer SceneViews loadable module (5.12)"
   #:description
   "The SceneViews loadable module extracted from 3D Slicer 5.12.  It provides
scene capture and restoration functionality.  Built from the
@file{Modules/Loadable/SceneViews} subtree of the Slicer 5.12 source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12
                        slicer-sequences-5.12)
   #:propagated-inputs (list slicer-sequences-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSequencesModuleMRML_INCLUDE_DIRS="
       #$slicer-sequences-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSequencesModuleMRML")
      (string-append
       "-DvtkSlicerSequencesModuleLogic_INCLUDE_DIRS="
       #$slicer-sequences-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSequencesModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-sequences-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-texts-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-texts-5.12"
   #:module-subdir "Texts"
   #:patches (list "texts/0001-ENH-Add-standalone-build-support-for-Texts-module-5..patch")
   #:synopsis "3D Slicer Texts loadable module (5.12)"
   #:description
   "The Texts loadable module extracted from 3D Slicer 5.12.  It provides
text annotation support linked to Markups.  Built from the
@file{Modules/Loadable/Texts} subtree of the Slicer 5.12 source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12
                        slicer-markups-5.12)
   #:propagated-inputs (list slicer-markups-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-markups-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-transforms-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-transforms-5.12"
   #:module-subdir "Transforms"
   #:patches (list "transforms/0001-ENH-Add-standalone-build-support-for-Transforms-modu.patch"
                   "transforms/0002-COMP-Fix-Transforms-missing-CMake-dependencies-for-s.patch")
   #:synopsis "3D Slicer Transforms loadable module (5.12)"
   #:description
   "The Transforms loadable module extracted from 3D Slicer 5.12.  It provides
linear and non-linear transform support with display and subject hierarchy.
Built from the @file{Modules/Loadable/Transforms} subtree of the Slicer
5.12 source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12
                        slicer-markups-5.12)
   #:propagated-inputs (list slicer-markups-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-markups-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-models-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-models-5.12"
   #:module-subdir "Models"
   #:patches (list "models/0001-ENH-Add-standalone-build-support-for-Models-module-5.patch"
                  "models/0002-COMP-Add-vtkSlicerSubjectHierarchyModuleLogic-and-MR.patch")
   #:synopsis "3D Slicer Models loadable module (5.12)"
   #:description
   "The Models loadable module extracted from 3D Slicer 5.12.  It provides
model loading, display, and subject hierarchy integration.  Built from the
@file{Modules/Loadable/Models} subtree of the Slicer 5.12 source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12
                        slicer-colors-5.12
                        slicer-terminologies-5.12)
   #:propagated-inputs (list slicer-colors-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DqSlicerColorsModuleWidgets_INCLUDE_DIRS="
       #$slicer-colors-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerColorsModuleWidgets")
      (string-append
       "-DvtkSlicerColorsModuleMRML_INCLUDE_DIRS="
       #$slicer-colors-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerColorsModuleMRML")
      (string-append
       "-DvtkSlicerColorsModuleLogic_INCLUDE_DIRS="
       #$slicer-colors-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerColorsModuleLogic")
      (string-append
       "-DqSlicerTerminologiesModuleWidgets_INCLUDE_DIRS="
       #$slicer-terminologies-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerTerminologiesModuleWidgets")
      (string-append
       "-DvtkSlicerTerminologiesModuleLogic_INCLUDE_DIRS="
       #$slicer-terminologies-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerTerminologiesModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-colors-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-terminologies-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-volumes-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-volumes-5.12"
   #:module-subdir "Volumes"
   #:patches (list "volumes/0001-ENH-Add-standalone-build-support-for-Volumes-module-.patch"
                   "volumes/0002-COMP-Fix-Volumes-Logic-use-rapidjson-target-and-add-.patch"
                   "volumes/0003-COMP-Add-vtkSlicerVolumesModuleLogic-to-Volumes-SHP-.patch")
   #:synopsis "3D Slicer Volumes loadable module (5.12)"
   #:description
   "The Volumes loadable module extracted from 3D Slicer 5.12.  It provides
volume loading, display, and scalar/diffusion tensor volume capabilities.
Built from the @file{Modules/Loadable/Volumes} subtree of the Slicer 5.12
source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12
                        slicer-colors-5.12
                        slicer-units-5.12)
   #:propagated-inputs (list slicer-colors-5.12 slicer-units-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DvtkSlicerColorsModuleLogic_INCLUDE_DIRS="
       #$slicer-colors-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerColorsModuleLogic")
      (string-append
       "-DvtkSlicerColorsModuleMRML_INCLUDE_DIRS="
       #$slicer-colors-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerColorsModuleMRML")
      (string-append
       "-DqSlicerColorsModuleWidgets_INCLUDE_DIRS="
       #$slicer-colors-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerColorsModuleWidgets")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-colors-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-volumerendering-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-volumerendering-5.12"
   #:module-subdir "VolumeRendering"
   #:patches (list "volumerendering/0001-ENH-Add-standalone-build-support-for-VolumeRendering.patch")
   #:synopsis "3D Slicer VolumeRendering loadable module (5.12)"
   #:description
   "The VolumeRendering loadable module extracted from 3D Slicer 5.12.  It
provides CPU and GPU ray-cast volume rendering, transfer-function presets,
shader-property MRML nodes, and a subject-hierarchy plugin.  Built from the
@file{Modules/Loadable/VolumeRendering} subtree of the Slicer 5.12 source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12
                        slicer-markups-5.12
                        slicer-volumes-5.12)
   #:propagated-inputs (list slicer-markups-5.12 slicer-volumes-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DqSlicerMarkupsModuleWidgets_INCLUDE_DIRS="
       #$slicer-markups-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerMarkupsModuleWidgets")
      (string-append
       "-DqSlicerVolumesSubjectHierarchyPlugins_INCLUDE_DIRS="
       #$slicer-volumes-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerVolumesSubjectHierarchyPlugins")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-markups-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-volumes-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-segmentations-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-segmentations-5.12"
   #:module-subdir "Segmentations"
   #:patches (list "segmentations/0001-ENH-Add-standalone-build-support-for-Segmentations-m.patch"
                   "segmentations/0002-COMP-Add-missing-library-dependencies-to-Segmentatio.patch"
                   "segmentations/0003-COMP-Add-LINK_DIRECTORIES-to-Segmentations-MRML-and-.patch"
                   "segmentations/0004-COMP-Fix-Slicer_BINARY_DIR-in-SegmentEditorEffects-f.patch"
                   "segmentations/0005-COMP-Link-Python3-for-ScriptedEffect-in-standalone-S.patch"
                   "segmentations/0006-COMP-Add-MOC_SRCS-for-DesignerPlugins-in-standalone-.patch")
   #:synopsis "3D Slicer Segmentations loadable module (5.12)"
   #:description
   "The Segmentations loadable module extracted from 3D Slicer 5.12.  It
provides MRML node types, display managers, editor effects, and widgets for
working with segmentation objects.  Built from the
@file{Modules/Loadable/Segmentations} subtree of the Slicer 5.12 source tree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12
                        slicer-terminologies-5.12
                        slicer-markups-5.12)
   ;; slicer-markups-5.12 propagates colors+annotations+SH+terminologies.
   #:propagated-inputs (list slicer-markups-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DqSlicerTerminologiesModuleWidgets_INCLUDE_DIRS="
       #$slicer-terminologies-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerTerminologiesModuleWidgets")
      (string-append
       "-DvtkSlicerTerminologiesModuleLogic_INCLUDE_DIRS="
       #$slicer-terminologies-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerTerminologiesModuleLogic")
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-terminologies-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-markups-5.12
       "/lib/Slicer-5.12/qt-loadable-modules"))))

(define-public slicer-slicerwelcome-5.12
  (make-slicer-loadable-module-5.12
   #:name "slicer-slicerwelcome-5.12"
   #:module-subdir "SlicerWelcome"
   #:patches (list "slicerwelcome/0001-ENH-Add-standalone-CMake-build-support-for-SlicerWel.patch"
                   "slicerwelcome/0002-COMP-Guard-ExtensionUpdatesStatusButton-connection-w.patch")
   #:synopsis "3D Slicer Welcome loadable module (5.12)"
   #:description
   "The Welcome loadable module extracted from 3D Slicer 5.12.  It provides
the Welcome screen shown to new users, with quick-access buttons for loading
data, accessing recent files, and linking to online resources.  Built from
the @file{Modules/Loadable/SlicerWelcome} subtree of the Slicer 5.12 source
tree."))

;;;
;;; Slicer 5.12 — scripted module packages
;;;

(define-public slicer-sampledata-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-sampledata-5.12"
   #:module-subdir "SampleData"
   #:patches (list "sampledata/0001-ENH-Add-standalone-build-support-for-SampleData-scri.patch")
   #:synopsis "3D Slicer SampleData scripted module (5.12)"
   #:description
   "The SampleData scripted module extracted from 3D Slicer 5.12.  It provides
a catalog of sample medical data sets that can be downloaded and loaded from
within Slicer.  Built from the @file{Modules/Scripted/SampleData} subtree."))

(define-public slicer-endoscopy-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-endoscopy-5.12"
   #:module-subdir "Endoscopy"
   #:patches (list "endoscopy/0001-ENH-Add-standalone-build-support-for-Endoscopy-scrip.patch")
   #:synopsis "3D Slicer Endoscopy scripted module (5.12)"
   #:description
   "The Endoscopy scripted module extracted from 3D Slicer 5.12.  It provides
virtual endoscopy visualization along curve markup paths.  Built from
the @file{Modules/Scripted/Endoscopy} subtree."))

(define-public slicer-importitksnaplabel-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-importitksnaplabel-5.12"
   #:module-subdir "ImportItkSnapLabel"
   #:patches (list "importitksnaplabel/0001-ENH-Add-standalone-build-support-for-ImportItkSnapLa.patch")
   #:synopsis "3D Slicer ImportItkSnapLabel scripted module (5.12)"
   #:description
   "The ImportItkSnapLabel scripted module extracted from 3D Slicer 5.12.
Imports ITK-SNAP label description files.  Built from
the @file{Modules/Scripted/ImportItkSnapLabel} subtree."))

(define-public slicer-performancetests-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-performancetests-5.12"
   #:module-subdir "PerformanceTests"
   #:patches (list "performancetests/0001-ENH-Add-standalone-build-support-for-PerformanceTest.patch")
   #:synopsis "3D Slicer PerformanceTests scripted module (5.12)"
   #:description
   "The PerformanceTests scripted module extracted from 3D Slicer 5.12.
Provides rendering and pipeline performance benchmarks.  Built from
the @file{Modules/Scripted/PerformanceTests} subtree."))

(define-public slicer-selftests-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-selftests-5.12"
   #:module-subdir "SelfTests"
   #:patches (list "selftests/0001-ENH-Add-standalone-build-support-for-SelfTests-scrip.patch")
   #:synopsis "3D Slicer SelfTests scripted module (5.12)"
   #:description
   "The SelfTests scripted module extracted from 3D Slicer 5.12.
Provides self-test infrastructure for Slicer modules.  Built from
the @file{Modules/Scripted/SelfTests} subtree."))

(define-public slicer-screencapture-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-screencapture-5.12"
   #:module-subdir "ScreenCapture"
   #:patches (list "screencapture/0001-ENH-Add-standalone-build-support-for-ScreenCapture-s.patch")
   #:synopsis "3D Slicer ScreenCapture scripted module (5.12)"
   #:description
   "The ScreenCapture scripted module extracted from 3D Slicer 5.12.
Captures screenshots and animations from Slicer views.  Built from
the @file{Modules/Scripted/ScreenCapture} subtree."))

(define-public slicer-vectortoscalarvolume-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-vectortoscalarvolume-5.12"
   #:module-subdir "VectorToScalarVolume"
   #:patches (list "vectortoscalarvolume/0001-ENH-Add-standalone-build-support-for-VectorToScalarV.patch")
   #:synopsis "3D Slicer VectorToScalarVolume scripted module (5.12)"
   #:description
   "The VectorToScalarVolume scripted module extracted from 3D Slicer 5.12.
Converts vector volumes to scalar volumes.  Built from
the @file{Modules/Scripted/VectorToScalarVolume} subtree."))

(define-public slicer-dataprobe-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-dataprobe-5.12"
   #:module-subdir "DataProbe"
   #:patches (list "dataprobe/0001-ENH-Add-standalone-build-support-for-DataProbe-scrip.patch")
   #:synopsis "3D Slicer DataProbe scripted module (5.12)"
   #:description
   "The DataProbe scripted module extracted from 3D Slicer 5.12.
Displays voxel values under the cursor in slice views.  Built from
the @file{Modules/Scripted/DataProbe} subtree."))

(define-public slicer-cropvolumesequence-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-cropvolumesequence-5.12"
   #:module-subdir "CropVolumeSequence"
   #:patches (list "cropvolumesequence/0001-ENH-Add-standalone-build-support-for-CropVolumeSeque.patch")
   #:synopsis "3D Slicer CropVolumeSequence scripted module (5.12)"
   #:description
   "The CropVolumeSequence scripted module extracted from 3D Slicer 5.12.
Crops all volumes in a sequence node using a region of interest.  Built from
the @file{Modules/Scripted/CropVolumeSequence} subtree."))

(define-public slicer-webserver-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-webserver-5.12"
   #:module-subdir "WebServer"
   #:patches (list "webserver/0001-ENH-Add-standalone-build-support-for-WebServer-scrip.patch")
   #:synopsis "3D Slicer WebServer scripted module (5.12)"
   #:description
   "The WebServer scripted module extracted from 3D Slicer 5.12.
Embeds a lightweight HTTP server for remote Slicer control.  Built from
the @file{Modules/Scripted/WebServer} subtree."))

(define-public slicer-dicompatcher-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-dicompatcher-5.12"
   #:module-subdir "DICOMPatcher"
   #:patches (list "dicompatcher/0001-ENH-Add-standalone-build-support-for-DICOMPatcher-sc.patch")
   #:synopsis "3D Slicer DICOMPatcher scripted module (5.12)"
   #:description
   "The DICOMPatcher scripted module extracted from 3D Slicer 5.12.
Fixes common DICOM compliance issues in datasets.  Built from
the @file{Modules/Scripted/DICOMPatcher} subtree."))

(define-public slicer-dicomplugins-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-dicomplugins-5.12"
   #:module-subdir "DICOMPlugins"
   #:patches (list "dicomplugins/0001-ENH-Add-standalone-build-support-for-DICOMPlugins-sc.patch")
   #:synopsis "3D Slicer DICOMPlugins scripted module (5.12)"
   #:description
   "The DICOMPlugins scripted module extracted from 3D Slicer 5.12.
Provides base DICOM loading plugins (scalar volumes, segmentations,
fiducials, etc.).  Built from the @file{Modules/Scripted/DICOMPlugins} subtree."))

(define-public slicer-segmenteditor-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-segmenteditor-5.12"
   #:module-subdir "SegmentEditor"
   #:patches (list "segmenteditor/0001-ENH-Add-standalone-build-support-for-SegmentEditor-s.patch")
   #:synopsis "3D Slicer SegmentEditor scripted module (5.12)"
   #:description
   "The SegmentEditor scripted module extracted from 3D Slicer 5.12.
Provides the main scripted-module wrapper that exposes the
qMRMLSegmentEditorWidget as a Slicer module panel.  Built from
the @file{Modules/Scripted/SegmentEditor} subtree."))

(define-public slicer-segmentstatistics-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-segmentstatistics-5.12"
   #:module-subdir "SegmentStatistics"
   #:patches (list "segmentstatistics/0001-ENH-Add-standalone-build-support-for-SegmentStatisti.patch")
   #:synopsis "3D Slicer SegmentStatistics scripted module (5.12)"
   #:description
   "The SegmentStatistics scripted module extracted from 3D Slicer 5.12.
Computes per-segment statistics (volume, surface area, etc.).  Built from
the @file{Modules/Scripted/SegmentStatistics} subtree."))

(define-public slicer-dicom-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-dicom-5.12"
   #:module-subdir "DICOM"
   #:patches (list "dicom/0001-ENH-Add-standalone-build-support-for-DICOM-scripted-.patch")
   #:synopsis "3D Slicer DICOM scripted module (5.12)"
   #:description
   "The DICOM scripted module extracted from 3D Slicer 5.12.
Provides the DICOM browser UI for importing and loading DICOM data.  Built
from the @file{Modules/Scripted/DICOM} subtree."))

(define-public slicer-extensionwizard-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-extensionwizard-5.12"
   #:module-subdir "ExtensionWizard"
   #:patches (list "extensionwizard/0001-ENH-Add-standalone-build-support-for-ExtensionWizard.patch")
   #:synopsis "3D Slicer ExtensionWizard scripted module (5.12)"
   #:description
   "The ExtensionWizard scripted module extracted from 3D Slicer 5.12.
Provides a wizard for creating new Slicer extensions and modules.  Built
from the @file{Modules/Scripted/ExtensionWizard} subtree."))

(define-public slicer-dicomlib-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-dicomlib-5.12"
   #:module-subdir "DICOMLib"
   #:patches (list "dicomlib/0001-ENH-Add-standalone-build-support-for-DICOMLib-script.patch"
                   "dicomlib/0002-ENH-Make-dicomweb_client-import-optional-in-DICOMPro.patch")
   #:synopsis "3D Slicer DICOMLib scripted module (5.12)"
   #:description
   "The DICOMLib scripted module extracted from 3D Slicer 5.12.
Provides the core DICOM infrastructure: C++ logic for loadable and
exportable series, the DICOM database, and Python bindings.  Built from
the @file{Modules/Scripted/DICOMLib} subtree."
   #:extra-inputs (list slicer-subjecthierarchy-5.12)
   #:propagated-inputs (list slicer-subjecthierarchy-5.12)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/qSlicerSubjectHierarchyModuleWidgets")
      (string-append
       "-DvtkSlicerSubjectHierarchyModuleLogic_INCLUDE_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/include/Slicer-5.12/qt-loadable-modules/vtkSlicerSubjectHierarchyModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-subjecthierarchy-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-5.12
       "/lib/Slicer-5.12/qt-loadable-modules;"
       #$slicer-5.12
       "/lib/Slicer-5.12"))))

(define-public slicer-lineprofile-5.12
  (make-slicer-scripted-module-5.12
   #:name "slicer-lineprofile-5.12"
   #:module-subdir "LineProfile"
   #:patches (list "lineprofile/0001-ENH-Add-standalone-build-support-for-LineProfile-scr.patch")
   #:synopsis "3D Slicer LineProfile scripted module (5.12)"
   #:description
   "The LineProfile scripted module extracted from 3D Slicer 5.12.
Plots intensity profiles along a line markup.  Built from
the @file{Modules/Scripted/LineProfile} subtree."))

(define %slicer-5.12-loadable-modules
  (list slicer-terminologies-5.12
        slicer-subjecthierarchy-5.12
        slicer-colors-5.12
        slicer-volumes-5.12
        slicer-volumerendering-5.12
        slicer-units-5.12
        slicer-tables-5.12
        slicer-cameras-5.12
        slicer-data-5.12
        slicer-annotations-5.12
        slicer-markups-5.12
        slicer-models-5.12
        slicer-sequences-5.12
        slicer-viewcontrollers-5.12
        slicer-reformat-5.12
        slicer-plots-5.12
        slicer-sceneviews-5.12
        slicer-transforms-5.12
        slicer-texts-5.12
        slicer-slicerwelcome-5.12
        slicer-segmentations-5.12))

(define %slicer-5.12-scripted-modules
  (list slicer-sampledata-5.12
        slicer-endoscopy-5.12
        slicer-importitksnaplabel-5.12
        slicer-performancetests-5.12
        slicer-selftests-5.12
        slicer-screencapture-5.12
        slicer-vectortoscalarvolume-5.12
        slicer-dataprobe-5.12
        slicer-cropvolumesequence-5.12
        slicer-webserver-5.12
        slicer-dicompatcher-5.12
        slicer-dicomplugins-5.12
        slicer-segmenteditor-5.12
        slicer-segmentstatistics-5.12
        slicer-dicom-5.12
        slicer-extensionwizard-5.12
        slicer-dicomlib-5.12
        slicer-lineprofile-5.12))

;; Runtime Python packages for Slicer 5.12.
;;
;; Slicer 5.12 embeds Python 3.12.  Pure-Python packages (requests, pydicom, pip)
;; are imported from lib/python3.11/site-packages via SLICER_PYTHONPATH.  numpy has
;; compiled C extensions, so it is built against Python 3.12 explicitly via
;; python-numpy-3.12 (see systole/packages/python-xyz.scm).
(define-public slicer-all-5.12
  (package
    (name "slicer-all-5.12")
    (version (package-version slicer-5.12))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     (append (list slicer-5.12
                   python-numpy-3.12
                   python-requests python-pydicom python-pip)
             %slicer-5.12-loadable-modules
             %slicer-5.12-scripted-modules))
    (synopsis "3D Slicer 5.12 with all loadable and scripted modules")
    (description
     "Meta-package that installs 3D Slicer 5.12 (Python-enabled) together with
all its standalone loadable modules (terminologies, subjecthierarchy, colors,
volumes, volumerendering, units, tables, cameras, data, annotations, markups,
models, sequences, viewcontrollers, reformat, plots, sceneviews, transforms,
texts, slicerwelcome, segmentations) and all Python scripted modules
(sampledata, endoscopy, importitksnaplabel, performancetests, selftests,
screencapture, vectortoscalarvolume, dataprobe, cropvolumesequence,
webserver, dicompatcher, dicomplugins, segmenteditor, segmentstatistics,
dicom, extensionwizard, dicomlib, lineprofile).")
    (home-page (package-home-page slicer-5.12))
    (license (package-license slicer-5.12))))

;;;
;;; Slicer preview (slicer-next)
;;;

;; slicer-next tracks the Slicer main branch (5.13.0 development).  Upstream
;; main currently uses the SAME dependency pins as v5.12.2 (VTK 9.6.2, ITK
;; 5.4.6, CTK 5056664a, PythonQt 74dcd675, vtkAddon 2ed3e222, teem, libarchive,
;; Python 3.12, Qt5), so slicer-next shares the whole 5.12 dependency stack and
;; differs only in the Slicer source pin.
;;
;; The 87 base patches are shared with slicer-5.12, except 0007 (Scope CPack
;; blocks), which needed a rebase against main (upstream main reworked the
;; macOS qt_root_dir detection in CMake/SlicerCPack.cmake); the rebased
;; variant lives at patches/slicer-5.12/next/ and is generated from the
;; guix-systole-slicer-next branch in the Slicer-Systole repository.
;;
;; slicer-next is the base application package ONLY: no per-module
;; (slicer-<mod>-next) packages and no slicer-all-next meta-package are
;; provided.  Preview users build standalone modules on demand against
;; slicer-next using the factories in (systole packages slicer-factory).
(define %slicer-next-commit "394cc5296300bf205c10d00937b2ee2b0126a226")
(define %slicer-next-hash (base32 "0sysvn25857lrhp6hzg7wmsv3rc8wvrvsvvxxbyyh0q8bvlfmq2r"))

(define-public slicer-next
  (package
    (inherit slicer-5.12)
    (name "slicer-next")
    (version (git-version "5.13.0" "0" %slicer-next-commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Slicer/Slicer")
             (commit %slicer-next-commit)))
       (file-name (git-file-name "slicer-next" (git-version "5.13.0" "0" %slicer-next-commit)))
       (sha256 %slicer-next-hash)
       (patches (map (lambda (p)
                       (slicer-patch "5.12"
                                     (if (string=? p "0007-COMP-Scope-CPack-blocks.patch")
                                         "next/0007-COMP-Scope-CPack-blocks.patch"
                                         p)))
                     %slicer-5.12-patch-names))))
    ;; A main-branch build installs into lib/Slicer-5.13 (Slicer_VERSION is
    ;; 5.13 on main), so every inherited phase and search path that hard-codes
    ;; the lib/Slicer-5.12 directory is overridden with its 5.13 equivalent.
    (arguments
     (substitute-keyword-arguments (package-arguments slicer-5.12)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'patch-runpath
              (lambda _
                (invoke "patchelf" "--add-rpath"
                        (string-append "$ORIGIN/../lib/Slicer-5.13"
                                       ":"
                                       "$ORIGIN/../lib/Slicer-5.13/qt-loadable-modules")
                        (string-append #$output "/bin/SlicerApp-real"))))
            (replace 'install-slicer-symlink
              (lambda _
                (let ((wrapper (string-append #$output "/bin/Slicer")))
                  (when (file-exists? wrapper)
                    (delete-file wrapper))
                  (call-with-output-file wrapper
                    (lambda (port)
                      (display "#!/bin/sh\n" port)
                      (display "# Slicer launcher: set LD_LIBRARY_PATH from\n" port)
                      (display "# SLICER_ADDITIONAL_MODULE_PATHS then exec SlicerApp-real.\n" port)
                      (display "IFS=:\n" port)
                      (display "for _d in $SLICER_ADDITIONAL_MODULE_PATHS; do\n" port)
                      (display "  LD_LIBRARY_PATH=\"$_d${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"\n" port)
                      (display "done\n" port)
                      (display "unset IFS\n" port)
                      (display "if [ -n \"$GUIX_ENVIRONMENT\" ]; then\n" port)
                      (display "  _guix_mods=\"$GUIX_ENVIRONMENT/lib/Slicer-5.13/qt-loadable-modules\"\n" port)
                      (display "  if [ -d \"$_guix_mods\" ]; then\n" port)
                      (display "    LD_LIBRARY_PATH=\"$_guix_mods${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"\n" port)
                      (display "  fi\n" port)
                      (display "fi\n" port)
                      (display "export LD_LIBRARY_PATH\n" port)
                      (display "export PIP_USER=1\n" port)
                      (display "_dir=\"$(dirname \"$(readlink -f \"$0\")\")\"\n" port)
                      (display "exec \"$_dir/SlicerApp-real\" \"$@\"\n" port)))
                  (chmod wrapper #o755))))
            (replace 'patch-python-extension-runpath
              (lambda _
                (let ((dir (string-append #$output "/lib/Slicer-5.13")))
                  (for-each
                   (lambda (lib) (invoke "patchelf" "--add-rpath" "$ORIGIN" lib))
                   (find-files dir
                     (lambda (f stat)
                       (let ((rel (string-drop f (1+ (string-length dir)))))
                         (and (string-suffix? ".so" rel)
                              (not (string-contains rel "/"))))))))))
            (replace 'link-vtkaddon-python
              (lambda _
                (symlink
                 (string-append #$(this-package-input "vtkaddon-9.6")
                                "/lib/vtkAddonPython.so")
                 (string-append #$output
                                "/lib/Slicer-5.13/vtkAddonPython.so"))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "CMAKE_PREFIX_PATH")
            (files '("")))
           (search-path-specification
            (variable "SLICER_ADDITIONAL_MODULE_PATHS")
            (files '("lib/Slicer-5.13/qt-loadable-modules"
                     "lib/Slicer-5.13/qt-scripted-modules"
                     "lib/Slicer-5.13/cli-modules")))
           (search-path-specification
            (variable "SLICER_PYTHONPATH")
            (files '("bin/Python"
                     "lib/Slicer-5.13"
                     "lib/python3.12/site-packages"
                     "lib/python3.11/site-packages")))))
    (synopsis "3D Slicer preview (main branch) - Medical visualization and computing environment")
    (description
     "3D Slicer preview build tracking the upstream main branch (5.13.0
development).  Base application only; standalone loadable/scripted module
packages are not provided for the preview - build modules on demand against
this package instead.")))
