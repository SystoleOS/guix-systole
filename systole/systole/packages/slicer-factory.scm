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

(define-module (systole packages slicer-factory)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (systole packages itk)
  #:use-module (systole packages)
  #:use-module (srfi srfi-1)
  #:export (make-slicer-loadable-module
            make-slicer-scripted-module
            make-slicer-cli-module))

;;; Commentary:
;;;
;;; Version-neutral build infrastructure shared by the per-version Slicer
;;; package modules (systole packages slicer-5-8) and (systole packages
;;; slicer-5-10):
;;;
;;;   - slicerexecutionmodel: the SEM/GenerateCLP package used by every
;;;     Slicer base package and by the CLI-module factory.
;;;   - make-slicer-loadable-module / make-slicer-scripted-module /
;;;     make-slicer-cli-module: factories for standalone Slicer module
;;;     packages.  Each takes the base Slicer package (#:slicer) and its
;;;     version string (#:slicer-version, used for patch lookup and the
;;;     lib/Slicer-<version> install layout) explicitly; the per-version
;;;     modules wrap them with those arguments pre-applied.
;;;
;;; Code:

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
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/Slicer/SlicerExecutionModel")
           (commit "91b921bd5977c3384916ba4b03705d87b26067f7")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1ppff74lsncf7wgz15k4r735mbgsl3r2c6yw9jskihs3b9m460qr"))
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
           (lambda _
             (use-modules (ice-9 popen) (ice-9 textual-ports))
             (let ((mdp (string-append #$output "/lib/ModuleDescriptionParser")))
               (for-each
                (lambda (bin)
                  (when (file-exists? bin)
                    (let* ((pipe (open-pipe* OPEN_READ "patchelf" "--print-rpath" bin))
                           (cur  (string-trim-right (get-string-all pipe) #\newline)))
                      (close-pipe pipe)
                      (invoke "patchelf" "--set-rpath"
                              (string-append cur ":" mdp)
                              bin))))
                (list (string-append #$output "/bin/GenerateCLP")
                      (string-append #$output "/bin/GenerateCLPLauncher")))))))))
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
;; Executables are installed to lib/Slicer-<version>/cli-modules so that Guix
;; profile-based search via SLICER_ADDITIONAL_MODULE_PATHS picks them up.
(define* (make-slicer-cli-module
          #:key
          slicer         ; base Slicer package (source/version/home-page/license)
          slicer-version ; version string for patch lookup, e.g. "5.8"
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
   (version (package-version slicer))
   (source
    (origin
     (inherit (package-source slicer))
     (patches (map (lambda (p) (slicer-patch slicer-version p)) patches))))
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
                   #$(string-append
                      "-DSlicerExecutionModel_DEFAULT_CLI_INSTALL_RUNTIME_DESTINATION="
                      "lib/Slicer-" slicer-version "/cli-modules")
                   #$(string-append
                      "-DSlicerExecutionModel_DEFAULT_CLI_INSTALL_LIBRARY_DESTINATION="
                      "lib/Slicer-" slicer-version "/cli-modules")
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
                (lambda* (#:key configure-flags #:allow-other-keys)
                  (let ((source (getcwd)))
                    (apply invoke "cmake"
                           "-S" (string-append source "/Modules/CLI/"
                                               #$module-subdir)
                           "-B" "build"
                           (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
                           ;; Base/CLI provides itkPluginUtilities.h;
                           ;; Libs/vtkITK provides itkConstrainedValue*Filter.h
                           ;; (used by e.g. MultiplyScalarVolumes).  Both are
                           ;; read directly from the source tree.  CMake accepts
                           ;; semicolon-separated lists for -D string vars.
                           (string-append "-DSlicerExecutionModel_EXTRA_INCLUDE_DIRECTORIES="
                                          source "/Base/CLI;"
                                          source "/Libs/vtkITK")
                           configure-flags)
                    (chdir "build")))))))
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
   (home-page (package-home-page slicer))
   (synopsis synopsis)
   (description description)
   (license (package-license slicer))))

;;;
;;; Factory for standalone Slicer loadable-module packages
;;;

;; Do NOT use (inherit slicer) in the packages produced here: Guix forbids
;; listing a package as an input when it also appears in the inheritance chain.
;; We reuse only the source origin and build each sub-module as a fully
;; independent package that depends on an installed Slicer.
;;
;; All modules are built against the canonical Slicer base (Python-enabled).
;; Python wrapping (VTK + PythonQt) is enabled by SlicerConfig.cmake, which
;; unconditionally sets Slicer_USE_PYTHONQT=ON; each module therefore produces
;; *Python.so and *PythonQt.so wrappers as part of the normal build.
(define* (make-slicer-loadable-module
          #:key
          slicer          ; base Slicer package to build against
          slicer-version  ; version string, e.g. "5.8" (patch lookup + layout)
          pythonqt        ; PythonQt package matching the base Slicer
          name            ; package name string, e.g. "slicer-volumes-5.8"
          module-subdir   ; source sub-directory, e.g. "Volumes"
          patches         ; list of patch filename strings
          synopsis        ; one-line synopsis string
          description     ; multi-line description string
          ;; Extra packages added to inputs *before* the base Slicer's own
          ;; inputs.  Use this to declare inter-module build-time dependencies
          ;; (e.g. slicer-terminologies-5.8 for SubjectHierarchy).
          (extra-inputs '())
          ;; A gexp that evaluates to a (possibly empty) list of extra
          ;; CMake -D flags.  Defaults to the empty list.
          (extra-configure-flags #~'())
          ;; Optional python package.  When set, pass explicit Python3
          ;; hints so the FindPython3 call inside VTK's wrap machinery
          ;; resolves the full Development component and the generated
          ;; *Python.so wrapper links libpython -- required under
          ;; Slicer's -Wl,--no-undefined (VTK 9.6 wrappers no longer
          ;; get libpython transitively).
          (python #f)
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
     (patches (map (lambda (p) (slicer-patch slicer-version p)) patches))))
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
                                  #$(string-append "/lib/Slicer-"
                                                   slicer-version))
                   ;; UseSlicer.cmake calls find_package(PythonQt) which uses
                   ;; PYTHONQT_INSTALL_DIR as a hint to find PythonQt.h and set
                   ;; PYTHONQT_INCLUDE_DIR.  Without this hint, find_path may
                   ;; not locate PythonQt.h, leaving PYTHONQT_INCLUDE_DIR empty
                   ;; and the include_directories() call in UseSlicer a no-op.
                   (string-append "-DPYTHONQT_INSTALL_DIR="
                                  #$pythonqt)
                   #$@(if python
                          (let ((pyver (version-major+minor
                                        (package-version python))))
                            (list
                             #~(string-append "-DPython3_EXECUTABLE="
                                              #$python "/bin/python3")
                             #~(string-append "-DPython3_INCLUDE_DIR="
                                              #$python "/include/python"
                                              #$pyver)
                             #~(string-append "-DPython3_LIBRARY="
                                              #$python "/lib/libpython"
                                              #$pyver ".so")
                             ;; vtkAddon 9.6's vtkWrapPython.cmake calls
                             ;; find_package(Python3) but still reads the
                             ;; FindPythonLibs-era PYTHON_LIBRARY when
                             ;; setting VTK_Python3_LIBRARIES (upstream
                             ;; half-migration); without it the wrapper
                             ;; links an empty list and fails under
                             ;; -Wl,--no-undefined.  The Slicer base build
                             ;; is unaffected because Slicer's top-level
                             ;; CMake sets the compat variable itself.
                             #~(string-append "-DPYTHON_LIBRARY="
                                              #$python "/lib/libpython"
                                              #$pyver ".so")))
                          '()))
             #$extra-configure-flags)
          #:phases
          #~(modify-phases %standard-phases
              ;; Build only the named sub-directory, not the Slicer root.
              (replace 'configure
                (lambda* (#:key configure-flags #:allow-other-keys)
                  (apply invoke "cmake"
                         "-S" (string-append (getcwd) "/Modules/Loadable/"
                                             #$module-subdir)
                         "-B" "build"
                         (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
                         configure-flags)
                  (chdir "build"))))))
   ;; UseSlicer.cmake transitively requires all of the base Slicer's build-time
   ;; libraries (Qt5, VTK, ITK, etc.) to be present in the build environment,
   ;; not just the base Slicer itself.  We therefore start from its input
   ;; list and prepend the base Slicer so cmake can locate SlicerConfig.cmake.
   ;; Any extra-inputs (other standalone modules this module depends on) are
   ;; also prepended so cmake can link against them.
   (inputs (fold (lambda (pkg acc)
                   (modify-inputs acc (prepend pkg)))
                 (modify-inputs (package-inputs slicer)
                   (prepend slicer))
                 extra-inputs))
   ;; Propagate the base Slicer so "guix shell slicer-<name>-<version>" gives
   ;; a usable Slicer in the profile.
   (propagated-inputs (cons slicer propagated-inputs))
   (home-page (package-home-page slicer))
   (synopsis synopsis)
   (description description)
   (license (package-license slicer))))

;;;
;;; factory for standalone slicer scripted-module packages
;;;

;; analogous to make-slicer-loadable-module but for python scripted modules.
;; these install python scripts to lib/slicer-<version>/qt-scripted-modules/.
;;
;; each scripted-module source branch is named
;;   guix-systole-<modulename>-scripted-module-<version>
;; in ~/src/slicer/slicer-systole, following the same convention as the
;; loadable-module branches.  patches are generated with git format-patch and
;; stored under systole/packages/patches/slicer-<version>/<modulename>/.
(define* (make-slicer-scripted-module
          #:key
          slicer          ; base Slicer package to build against
          slicer-version  ; version string, e.g. "5.8" (patch lookup + layout)
          name            ; package name string, e.g. "slicer-sampledata-5.8"
          module-subdir   ; source sub-directory, e.g. "sampledata"
          patches         ; list of patch filename strings
          synopsis        ; one-line synopsis string
          description     ; multi-line description string
          ;; extra packages added to inputs *before* the base Slicer's own inputs.
          (extra-inputs '())
          ;; a gexp that evaluates to a (possibly empty) list of extra cmake -d flags.
          (extra-configure-flags #~'())
          ;; Optional python package.  When set, pass explicit Python3
          ;; hints so the FindPython3 call inside VTK's wrap machinery
          ;; resolves the full Development component and the generated
          ;; *Python.so wrapper links libpython -- required under
          ;; Slicer's -Wl,--no-undefined (VTK 9.6 wrappers no longer
          ;; get libpython transitively).
          (python #f)
          ;; Packages that must be present in the profile at runtime.
          ;; Use this to declare inter-module runtime (dlopen) dependencies.
          (propagated-inputs '()))
  (package
   (name name)
   (version (package-version slicer))
   (source
    (origin
     (inherit (package-source slicer))
     (patches (map (lambda (p) (slicer-patch slicer-version p)) patches))))
   (build-system cmake-build-system)
   (arguments
    (list #:tests? #f
          #:validate-runpath? #f
          #:out-of-source? #t
          #:configure-flags
          #~(append
             (list "-DCMAKE_BUILD_TYPE:STRING=Release"
                   "-DBUILD_TESTING:BOOL=OFF"
                   ;; point cmake at the slicer config directory.
                   (string-append "-DSlicer_DIR="
                                  #$slicer
                                  #$(string-append "/lib/Slicer-"
                                                   slicer-version))
                   #$@(if python
                          (let ((pyver (version-major+minor
                                        (package-version python))))
                            (list
                             #~(string-append "-DPython3_EXECUTABLE="
                                              #$python "/bin/python3")
                             #~(string-append "-DPython3_INCLUDE_DIR="
                                              #$python "/include/python"
                                              #$pyver)
                             #~(string-append "-DPython3_LIBRARY="
                                              #$python "/lib/libpython"
                                              #$pyver ".so")
                             ;; vtkAddon 9.6's vtkWrapPython.cmake calls
                             ;; find_package(Python3) but still reads the
                             ;; FindPythonLibs-era PYTHON_LIBRARY when
                             ;; setting VTK_Python3_LIBRARIES (upstream
                             ;; half-migration); without it the wrapper
                             ;; links an empty list and fails under
                             ;; -Wl,--no-undefined.  The Slicer base build
                             ;; is unaffected because Slicer's top-level
                             ;; CMake sets the compat variable itself.
                             #~(string-append "-DPYTHON_LIBRARY="
                                              #$python "/lib/libpython"
                                              #$pyver ".so")))
                          '()))
             #$extra-configure-flags)
          #:phases
          ;; build only the named scripted-module sub-directory.
          #~(modify-phases %standard-phases
              (replace 'configure
                (lambda* (#:key configure-flags #:allow-other-keys)
                  (apply invoke "cmake"
                         "-S" (string-append (getcwd) "/Modules/Scripted/"
                                             #$module-subdir)
                         "-B" "build"
                         (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
                         configure-flags)
                  (chdir "build"))))))
   ;; useslicer.cmake requires the full dependency tree.
   ;; we start from the base slicer's inputs and prepend the base slicer
   ;; itself so cmake can locate slicerconfig.cmake.
   (inputs (fold (lambda (pkg acc)
                   (modify-inputs acc (prepend pkg)))
                 (modify-inputs (package-inputs slicer)
                   (prepend slicer))
                 extra-inputs))
   ;; Propagate the base Slicer so "guix shell slicer-<name>-<version>"
   ;; provides a usable Slicer in the profile, plus any declared runtime
   ;; module deps.
   (propagated-inputs (cons slicer propagated-inputs))
   (home-page (package-home-page slicer))
   (synopsis synopsis)
   (description description)
   (license (package-license slicer))))
