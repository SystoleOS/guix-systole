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

(define-module (systole packages slicer-igt)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (systole packages igsio)
  #:use-module (systole packages openigtlink)
  #:use-module (systole packages pythonqt)
  #:use-module (systole packages slicer)
  #:use-module (systole packages))

;;;
;;; Shared source binding for all SlicerIGT packages
;;;

(define %slicer-igt-commit "42091cddf620d760cdb0e22564edb8c55338edd9")
(define %slicer-igt-version "0.0.0-42091cd")

(define %slicer-igt-source
  (origin
   (method url-fetch)
   (uri (string-append
         "https://github.com/SlicerIGT/SlicerIGT/archive/"
         %slicer-igt-commit ".tar.gz"))
   (sha256
    (base32 "127ni5kdg7qbs4ibg6wfklm3rxc17mhh427gi3k8j0ymmc66j0c5"))))

;;;
;;; Factory for standalone SlicerIGT loadable-module packages.
;;;
;; Analogous to make-slicer-loadable-module (slicer.scm) but:
;;  - Source is the SlicerIGT tarball, not the Slicer source tree.
;;  - cmake -S points to <source>/<module-subdir>/ (top-level in repo).
;;  - All modules are built against slicer-5.8 (Python-enabled, canonical).
;;  - No Slicer_INSTALL_DEVELOPMENT=ON (no downstream C++ inter-IGT deps).
(define* (make-slicer-igt-loadable-module
          #:key
          name          ; package name string, e.g. "slicer-igt-breachwarning"
          module-subdir ; source sub-directory, e.g. "BreachWarning"
          patches       ; list of patch filename strings
          synopsis      ; one-line synopsis string
          description   ; multi-line description string
          ;; Extra packages added to inputs before slicer-5.8's own inputs.
          (extra-inputs '())
          ;; Packages propagated to consumers (runtime Slicer module deps).
          (propagated-inputs '())
          ;; A gexp evaluating to a list of extra CMake -D flags.
          (extra-configure-flags #~'()))
  (package
   (name name)
   (version %slicer-igt-version)
   (source
    (origin
     (inherit %slicer-igt-source)
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
                   (string-append "-DSlicer_DIR="
                                  #$slicer-5.8 "/lib/Slicer-5.8")
                   (string-append "-DPYTHONQT_INSTALL_DIR="
                                  #$pythonqt-commontk))
             #$extra-configure-flags)
          #:phases
          #~(modify-phases %standard-phases
              (replace 'configure
                (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
                  (let ((out (assoc-ref outputs "out")))
                    (apply invoke "cmake"
                           "-S" (string-append (getcwd) "/" #$module-subdir)
                           "-B" "build"
                           (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                           configure-flags)
                    (chdir "build")
                    #t))))))
   (inputs (fold (lambda (pkg acc)
                   (modify-inputs acc (prepend pkg)))
                 (modify-inputs (package-inputs slicer-5.8)
                   (prepend slicer-5.8))
                 extra-inputs))
   ;; Propagate the Slicer variant so "guix shell slicer-igt-<name>" gives
   ;; a usable Slicer in the profile.
   (propagated-inputs (cons slicer-5.8 propagated-inputs))
   (home-page "https://github.com/SlicerIGT/SlicerIGT")
   (synopsis synopsis)
   (description description)
   (license license:bsd-3)))

;;;
;;; Factory for standalone SlicerIGT scripted-module packages.
;;;
;; Always uses slicer-5.8; cmake -S points to <source>/<module-subdir>/.
(define* (make-slicer-igt-scripted-module
          #:key
          name          ; package name string
          module-subdir ; source sub-directory
          patches       ; list of patch filename strings
          synopsis      ; one-line synopsis string
          description   ; multi-line description string
          (extra-inputs '())
          (propagated-inputs '())
          (extra-configure-flags #~'()))
  (package
   (name name)
   (version %slicer-igt-version)
   (source
    (origin
     (inherit %slicer-igt-source)
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
                   (string-append "-DSlicer_DIR="
                                  #$slicer-5.8 "/lib/Slicer-5.8"))
             #$extra-configure-flags)
          #:phases
          #~(modify-phases %standard-phases
              (replace 'configure
                (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
                  (let ((out (assoc-ref outputs "out")))
                    (apply invoke "cmake"
                           "-S" (string-append (getcwd) "/" #$module-subdir)
                           "-B" "build"
                           (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                           configure-flags)
                    (chdir "build")
                    #t))))))
   (inputs (fold (lambda (pkg acc)
                   (modify-inputs acc (prepend pkg)))
                 (modify-inputs (package-inputs slicer-5.8)
                   (prepend slicer-5.8))
                 extra-inputs))
   ;; Propagate Slicer so "guix shell slicer-igt-<name>" gives a usable
   ;; Slicer in the profile.
   (propagated-inputs (cons slicer-5.8 propagated-inputs))
   (home-page "https://github.com/SlicerIGT/SlicerIGT")
   (synopsis synopsis)
   (description description)
   (license license:bsd-3)))

;;;
;;; Loadable modules
;;;

(define-public slicer-igt-breachwarning
  (make-slicer-igt-loadable-module
   #:name "slicer-igt-breachwarning"
   #:module-subdir "BreachWarning"
   #:patches (list "breachwarning/0001-ENH-Add-standalone-CMake-preamble-for-BreachWarning.patch"
                   "breachwarning/0002-COMP-Add-markups-include-and-link-dirs-for-BreachWar.patch"
                   "breachwarning/0003-COMP-Add-Qt5-Multimedia-to-BreachWarning-target-libr.patch")
   #:synopsis "SlicerIGT BreachWarning loadable module"
   #:description
   "The BreachWarning module from the SlicerIGT extension.  It monitors the
distance between a tracked tool and a surface model and provides visual and
audio alerts when the tool approaches or breaches the surface.  Built from
the @file{BreachWarning} sub-directory of the SlicerIGT source tree."
   #:extra-inputs (list slicer-markups-5.8)
   #:propagated-inputs (list slicer-markups-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-markups-5.8 "/lib/Slicer-5.8/qt-loadable-modules"))))


(define-public slicer-igt-collectpoints
  (make-slicer-igt-loadable-module
   #:name "slicer-igt-collectpoints"
   #:module-subdir "CollectPoints"
   #:patches (list "collectpoints/0001-ENH-Add-standalone-CMake-preamble-for-CollectPoints.patch"
                   "collectpoints/0002-COMP-Add-markups-includes-LINK_DIRECTORIES-and-qMRMLWidgets.patch")
   #:synopsis "SlicerIGT CollectPoints loadable module"
   #:extra-inputs (list slicer-markups-5.8)
   #:propagated-inputs (list slicer-markups-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-markups-5.8 "/lib/Slicer-5.8/qt-loadable-modules"))
   #:description
   "The CollectPoints module from the SlicerIGT extension.  It records the
positions of a tracked tool as a fiducial markup list, suitable for
calibration or registration workflows.  Built from the @file{CollectPoints}
sub-directory of the SlicerIGT source tree."))


(define-public slicer-igt-createmodels
  (make-slicer-igt-loadable-module
   #:name "slicer-igt-createmodels"
   #:module-subdir "CreateModels"
   #:patches (list "createmodels/0001-ENH-Add-standalone-CMake-preamble-for-CreateModels.patch"
                   "createmodels/0002-COMP-Add-qMRMLWidgets-to-MODULE_TARGET_LIBRARIES.patch")
   #:synopsis "SlicerIGT CreateModels loadable module"
   #:description
   "The CreateModels module from the SlicerIGT extension.  It provides
interactive creation of simple geometric models (sphere, cylinder, cube,
needle, coordinate axes) directly from within 3D Slicer, useful for
IGT scene setup and testing.  Built from the @file{CreateModels}
sub-directory of the SlicerIGT source tree."))


(define-public slicer-igt-fiducialregistrationwizard
  (make-slicer-igt-loadable-module
   #:name "slicer-igt-fiducialregistrationwizard"
   #:module-subdir "FiducialRegistrationWizard"
   #:patches (list "fiducialregistrationwizard/0001-ENH-Add-standalone-CMake-preamble-for-FiducialRegistrationWizard.patch"
                   "fiducialregistrationwizard/0002-COMP-Add-markups-includes-and-LINK_DIRECTORIES-for-FiducialRegistrationWizard.patch")
   #:synopsis "SlicerIGT FiducialRegistrationWizard loadable module"
   #:description
   "The FiducialRegistrationWizard module from SlicerIGT.  It provides a
guided workflow for point-based rigid registration between image and
physical space using fiducial landmarks.  Depends on the Markups module
for landmark placement.  Built from the @file{FiducialRegistrationWizard}
sub-directory of the SlicerIGT source tree."
   #:extra-inputs (list slicer-markups-5.8)
   #:propagated-inputs (list slicer-markups-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerMarkupsModuleWidgets_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerMarkupsModuleWidgets")
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DvtkSlicerMarkupsModuleLogic_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-markups-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))


(define-public slicer-igt-landmarkdetection
  (make-slicer-igt-loadable-module
   #:name "slicer-igt-landmarkdetection"
   #:module-subdir "LandmarkDetection"
   #:patches (list "landmarkdetection/0001-ENH-Add-standalone-CMake-preamble-for-LandmarkDetection.patch"
                   "landmarkdetection/0002-COMP-Add-markups-includes-LINK_DIRECTORIES-and-qMRMLWidgets.patch")
   #:synopsis "SlicerIGT LandmarkDetection loadable module"
   #:description
   "The LandmarkDetection module from SlicerIGT.  It automatically detects
anatomical landmarks from tracked tool trajectories using the IGSIO
calibration library.  Built from the @file{LandmarkDetection} sub-directory
of the SlicerIGT source tree."
   #:extra-inputs (list igsio slicer-markups-5.8)
   #:propagated-inputs (list igsio slicer-markups-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DVTKIGSIOCALIBRATION_INCLUDE_DIRS="
       #$igsio "/include/IGSIO-1.0")
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$igsio "/lib;"
       #$slicer-markups-5.8
       "/lib/Slicer-5.8/qt-loadable-modules"))))


(define-public slicer-igt-pathexplorer
  (make-slicer-igt-loadable-module
   #:name "slicer-igt-pathexplorer"
   #:module-subdir "PathExplorer"
   #:patches (list "pathexplorer/0001-ENH-Add-standalone-CMake-preamble-for-PathExplorer.patch"
                   "pathexplorer/0002-COMP-Add-markups-includes-and-LINK_DIRECTORIES-for-PathExplorer.patch")
   #:synopsis "SlicerIGT PathExplorer loadable module"
   #:description
   "The PathExplorer module from SlicerIGT.  It provides tools for exploring
cross-sectional images along a user-defined curved path (e.g. for needle
trajectory planning), with reslicing views and a subject-hierarchy plugin.
Built from the @file{PathExplorer} sub-directory of the SlicerIGT source tree."
   #:extra-inputs (list slicer-markups-5.8 slicer-subjecthierarchy-5.8 slicer-plots-5.8)
   #:propagated-inputs (list slicer-markups-5.8 slicer-subjecthierarchy-5.8 slicer-plots-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DqSlicerMarkupsModuleWidgets_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/qSlicerMarkupsModuleWidgets")
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
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
       #$slicer-markups-5.8 "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-subjecthierarchy-5.8 "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-plots-5.8 "/lib/Slicer-5.8/qt-loadable-modules"))))


(define-public slicer-igt-pivotcalibration
  (make-slicer-igt-loadable-module
   #:name "slicer-igt-pivotcalibration"
   #:module-subdir "PivotCalibration"
   #:patches (list "pivotcalibration/0001-ENH-Add-standalone-CMake-preamble-for-PivotCalibration.patch"
                   "pivotcalibration/0002-COMP-Add-qMRMLWidgets-to-MODULE_TARGET_LIBRARIES.patch"
                   "pivotcalibration/0003-COMP-Add-IGSIO-include-dirs-and-LINK_DIRECTORIES-to-PivotCalibration-Logic.patch")
   #:synopsis "SlicerIGT PivotCalibration loadable module"
   #:description
   "The PivotCalibration module from SlicerIGT.  It computes the pivot point
of a tracked tool (e.g. a pointer or needle) by recording positions while
pivoting around its tip, using the IGSIO pivot calibration algorithm.
Built from the @file{PivotCalibration} sub-directory of the SlicerIGT
source tree."
   #:extra-inputs (list igsio)
   #:propagated-inputs (list igsio)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DVTKIGSIOCALIBRATION_INCLUDE_DIRS="
       #$igsio "/include/IGSIO-1.0")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$igsio "/lib"))))


(define-public slicer-igt-transformprocessor
  (make-slicer-igt-loadable-module
   #:name "slicer-igt-transformprocessor"
   #:module-subdir "TransformProcessor"
   #:patches (list "transformprocessor/0001-ENH-Add-standalone-CMake-preamble-for-TransformProcessor.patch"
                   "transformprocessor/0002-COMP-Add-qMRMLWidgets-to-MODULE_TARGET_LIBRARIES.patch")
   #:synopsis "SlicerIGT TransformProcessor loadable module"
   #:description
   "The TransformProcessor module from SlicerIGT.  It computes composite
transforms from multiple input transforms in real time, enabling matrix
operations (inversion, stabilization, terrifying, etc.) on transform nodes
in the MRML scene.  Built from the @file{TransformProcessor} sub-directory
of the SlicerIGT source tree."))


(define-public slicer-igt-ultrasoundsnapshots
  (make-slicer-igt-loadable-module
   #:name "slicer-igt-ultrasoundsnapshots"
   #:module-subdir "UltrasoundSnapshots"
   #:patches (list "ultrasoundsnapshots/0001-ENH-Add-standalone-CMake-preamble-for-UltrasoundSnapshots.patch"
                   "ultrasoundsnapshots/0002-COMP-Add-qMRMLWidgets-to-MODULE_TARGET_LIBRARIES.patch")
   #:synopsis "SlicerIGT UltrasoundSnapshots loadable module"
   #:description
   "The UltrasoundSnapshots module from SlicerIGT.  It captures snapshots of
the current ultrasound image plane (tracked or untracked) and stores them as
2-D model nodes in the MRML scene for later review or overlay.  Built from
the @file{UltrasoundSnapshots} sub-directory of the SlicerIGT source tree."))


(define-public slicer-igt-volumereslicedriver
  (make-slicer-igt-loadable-module
   #:name "slicer-igt-volumereslicedriver"
   #:module-subdir "VolumeResliceDriver"
   #:patches (list "volumereslicedriver/0001-ENH-Add-standalone-CMake-preamble-for-VolumeResliceDriver.patch"
                   "volumereslicedriver/0002-COMP-Add-markups-annotations-includes-and-LINK_DIRECTORIES.patch")
   #:synopsis "SlicerIGT VolumeResliceDriver loadable module"
   #:description
   "The VolumeResliceDriver module from SlicerIGT.  It drives the reslice
planes of 2D slice views to follow a tracked tool or a user-defined
transform node in real time, enabling live image guidance workflows.
Built from the @file{VolumeResliceDriver} sub-directory of the SlicerIGT
source tree."
   #:extra-inputs (list slicer-markups-5.8 slicer-annotations-5.8)
   #:propagated-inputs (list slicer-markups-5.8 slicer-annotations-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DvtkSlicerAnnotationsModuleMRML_INCLUDE_DIRS="
       #$slicer-annotations-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerAnnotationsModuleMRML")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-markups-5.8 "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-annotations-5.8 "/lib/Slicer-5.8/qt-loadable-modules"))))


(define-public slicer-igt-watchdog
  (make-slicer-igt-loadable-module
   #:name "slicer-igt-watchdog"
   #:module-subdir "Watchdog"
   #:patches (list "watchdog/0001-ENH-Add-standalone-CMake-preamble-for-Watchdog.patch"
                   "watchdog/0002-COMP-Add-Qt5-Multimedia-to-Watchdog-MODULE_TARGET_LIBRARIES.patch")
   #:synopsis "SlicerIGT Watchdog loadable module"
   #:description
   "The Watchdog module from SlicerIGT.  It monitors transform nodes for
staleness (lack of updates within a configurable timeout) and provides a
visual indicator in the 3D view, alerting the user when tracked hardware
stops sending data.  Built from the @file{Watchdog} sub-directory of the
SlicerIGT source tree."))


(define-public slicer-igt-volumereconstruction
  (make-slicer-igt-loadable-module
   #:name "slicer-igt-volumereconstruction"
   #:module-subdir "VolumeReconstruction"
   #:patches (list "volumereconstruction/0001-ENH-Add-standalone-CMake-preamble-for-VolumeReconstruction.patch"
                   "volumereconstruction/0002-COMP-Add-LINK_DIRECTORIES-to-VolumeReconstruction-MRML-and-Logic.patch")
   #:synopsis "SlicerIGT VolumeReconstruction loadable module"
   #:description
   "The VolumeReconstruction module from SlicerIGT.  It reconstructs a
3-D volume from a sequence of tracked 2-D ultrasound images using the IGSIO
volume-reconstruction library.  Requires SlicerIGSIOCommon and the IGSIO
library.  Built from the @file{VolumeReconstruction} sub-directory of the
SlicerIGT source tree."
   #:extra-inputs (list igsio
                        slicer-igsio-common
                        slicer-annotations-5.8
                        slicer-markups-5.8
                        slicer-sequences-5.8
                        slicer-volumes-5.8)
   #:propagated-inputs (list igsio
                             slicer-igsio-common
                             slicer-annotations-5.8
                             slicer-markups-5.8
                             slicer-sequences-5.8
                             slicer-volumes-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DVTKIGSIOCOMMON_INCLUDE_DIRS="
       #$igsio "/include/IGSIO-1.0")
      (string-append
       "-DVTKVOLUMERECONSTRUCTION_INCLUDE_DIRS="
       #$igsio "/include/IGSIO-1.0")
      (string-append
       "-DSlicerIGSIOCommon_INCLUDE_DIRS="
       #$slicer-igsio-common
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerIGSIOCommon")
      (string-append
       "-DvtkSlicerSequencesModuleMRML_INCLUDE_DIRS="
       #$slicer-sequences-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSequencesModuleMRML")
      (string-append
       "-DvtkSlicerAnnotationsModuleMRML_INCLUDE_DIRS="
       #$slicer-annotations-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerAnnotationsModuleMRML")
      (string-append
       "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
       #$slicer-markups-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
      (string-append
       "-DvtkSlicerVolumesModuleLogic_INCLUDE_DIRS="
       #$slicer-volumes-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerVolumesModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$igsio "/lib;"
       #$slicer-igsio-common "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-annotations-5.8 "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-markups-5.8 "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-sequences-5.8 "/lib/Slicer-5.8/qt-loadable-modules;"
       #$slicer-volumes-5.8 "/lib/Slicer-5.8/qt-loadable-modules"))))


;;;
;;; Scripted modules (Python-only)
;;;

(define-public slicer-igt-fiducialstomodelregistration
  (make-slicer-igt-scripted-module
   #:name "slicer-igt-fiducialstomodelregistration"
   #:module-subdir "FiducialsToModelRegistration"
   #:patches (list "fiducialstomodelregistration/0001-ENH-Add-standalone-CMake-preamble-for-FiducialsToModelRegistration.patch")
   #:synopsis "SlicerIGT FiducialsToModelRegistration scripted module"
   #:description
   "The FiducialsToModelRegistration scripted module from SlicerIGT.  It
registers a fiducial list to a surface model using iterative closest-point
(ICP) or point-based methods, providing a fast patient-to-image registration
workflow.  Built from the @file{FiducialsToModelRegistration}
sub-directory of the SlicerIGT source tree."))

(define-public slicer-igt-guidelet
  (make-slicer-igt-scripted-module
   #:name "slicer-igt-guidelet"
   #:module-subdir "Guidelet"
   #:patches (list "guidelet/0001-ENH-Add-standalone-CMake-preamble-for-Guidelet.patch")
   #:synopsis "SlicerIGT Guidelet scripted module"
   #:description
   "The Guidelet scripted module from SlicerIGT.  It provides a base class
and framework for building simplified, touch-friendly image-guided therapy
applications on top of 3D Slicer, with built-in ultrasound support.
Built from the @file{Guidelet} sub-directory of the SlicerIGT source tree."))

(define-public slicer-igt-modelregistration
  (make-slicer-igt-scripted-module
   #:name "slicer-igt-modelregistration"
   #:module-subdir "ModelRegistration"
   #:patches (list "modelregistration/0001-ENH-Add-standalone-CMake-preamble-for-ModelRegistration.patch")
   #:synopsis "SlicerIGT ModelRegistration scripted module"
   #:description
   "The ModelRegistration scripted module from SlicerIGT.  It computes a
rigid registration transform between two surface models using ICP.
Built from the @file{ModelRegistration} sub-directory of the SlicerIGT
source tree."))

(define-public slicer-igt-sequencereplay
  (make-slicer-igt-scripted-module
   #:name "slicer-igt-sequencereplay"
   #:module-subdir "SequenceReplay"
   #:patches (list "sequencereplay/0001-ENH-Add-standalone-CMake-preamble-for-SequenceReplay.patch")
   #:synopsis "SlicerIGT SequenceReplay scripted module"
   #:description
   "The SequenceReplay scripted module from SlicerIGT.  It provides playback
controls for MRML Sequence nodes, with play, pause, step, and loop functions,
for reviewing recorded tracking or imaging sessions.  Built from the
@file{SequenceReplay} sub-directory of the SlicerIGT source tree."))

(define-public slicer-igt-texturemodel
  (make-slicer-igt-scripted-module
   #:name "slicer-igt-texturemodel"
   #:module-subdir "TextureModel"
   #:patches (list "texturemodelscripted/0001-ENH-Add-standalone-CMake-preamble-for-TextureModel.patch")
   #:synopsis "SlicerIGT TextureModel scripted module"
   #:description
   "The TextureModel scripted module from SlicerIGT.  It applies a 2-D image
as a texture onto a 3-D surface model, useful for visualizing ultrasound
panoramas draped onto anatomy models.  Built from the @file{TextureModel}
sub-directory of the SlicerIGT source tree."))

(define-public slicer-igt-viewpoint
  (make-slicer-igt-scripted-module
   #:name "slicer-igt-viewpoint"
   #:module-subdir "Viewpoint"
   #:patches (list "viewpoint/0001-ENH-Add-standalone-CMake-preamble-for-Viewpoint.patch")
   #:synopsis "SlicerIGT Viewpoint scripted module"
   #:description
   "The Viewpoint scripted module from SlicerIGT.  It provides camera controls
for the 3D view, including locking the camera to a tracked tool, flying to a
target, and saving/restoring camera positions.  Built from the
@file{Viewpoint} sub-directory of the SlicerIGT source tree."))

;;;
;;; Module lists and meta-packages
;;;

(define-public %slicer-igt-loadable-modules
  ;; Loadable modules (built against slicer-5.8, Python-enabled).
  (list slicer-igt-breachwarning
        slicer-igt-collectpoints
        slicer-igt-createmodels
        slicer-igt-fiducialregistrationwizard
        slicer-igt-landmarkdetection
        slicer-igt-pathexplorer
        slicer-igt-pivotcalibration
        slicer-igt-transformprocessor
        slicer-igt-ultrasoundsnapshots
        slicer-igt-volumereslicedriver
        slicer-igt-watchdog
        slicer-igt-volumereconstruction))


(define-public %slicer-igt-scripted-modules
  ;; Python scripted modules (Python-only, always Python-enabled).
  (list slicer-igt-fiducialstomodelregistration
        slicer-igt-guidelet
        slicer-igt-modelregistration
        slicer-igt-sequencereplay
        slicer-igt-texturemodel
        slicer-igt-viewpoint))

(define-public slicer-igt
  ;; Meta-package: non-Python loadable modules + slicer-openigtlink.
  ;; Pulls in only the Slicer modules each IGT module actually needs at
  ;; runtime (via their propagated-inputs), not all of slicer-all-5.8.
  (package
   (name "slicer-igt")
   (version %slicer-igt-version)
   (source #f)
   (build-system trivial-build-system)
   (arguments (list #:builder #~(mkdir #$output)))
   (propagated-inputs
    (append (list slicer-5.8
                  slicer-openigtlink)
            %slicer-igt-loadable-modules
            %slicer-igt-scripted-modules))
   (synopsis "SlicerIGT extension — all loadable and scripted modules")
   (description
    "Meta-package that installs all SlicerIGT loadable modules (BreachWarning,
CollectPoints, CreateModels, FiducialRegistrationWizard, LandmarkDetection,
PathExplorer, PivotCalibration, TransformProcessor, UltrasoundSnapshots,
VolumeResliceDriver, VolumeReconstruction, and Watchdog) together with all
scripted modules (FiducialsToModelRegistration, Guidelet, ModelRegistration,
SequenceReplay, TextureModel, Viewpoint), and their dependencies
(SlicerOpenIGTLink, IGSIO, SlicerIGSIOCommon, and the specific Slicer
modules each IGT module requires at runtime).")
   (home-page "https://github.com/SlicerIGT/SlicerIGT")
   (license license:bsd-3)))

