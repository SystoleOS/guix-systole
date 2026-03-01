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

(define-module (systole packages plustoolkit)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xiph)
  #:use-module (systole packages itk)
  #:use-module (systole packages igsio)
  #:use-module (systole packages maths)
  #:use-module (systole packages openigtlink)
  #:use-module (systole packages vtk)
  #:use-module (systole packages))

;;;
;;; PlusLibData — data files for PlusLib (config files, test images, CAD models)
;;;

(define %pluslibdata-commit "51dcbb76d9f29fad94cf80788a7a0b7c704fb5dc")
(define %pluslibdata-version (string-append "2.9.0-" (string-take %pluslibdata-commit 7)))

(define-public pluslibdata
  (package
   (name "pluslibdata")
   (version %pluslibdata-version)
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/PlusToolkit/PlusLibData/archive/"
           %pluslibdata-commit ".tar.gz"))
     (sha256
      (base32 "1zsagn5k74dqsi2l4vayf0qbwqk93i0r66gv2sd1fapi04blj21i"))))
   (build-system trivial-build-system)
   (native-inputs (list gzip tar))
   (arguments
    (list
     #:modules '((guix build utils))
     #:builder
     #~(begin
         (use-modules (guix build utils))
         ;; tar -z invokes gzip as a subprocess; put it on PATH.
         (setenv "PATH" (string-append #$(this-package-native-input "gzip") "/bin:"
                                       #$(this-package-native-input "tar") "/bin"))
         (let ((share (string-append #$output "/share/PlusLib-2.9")))
           (mkdir-p share)
           ;; Extract tarball, stripping the top-level archive directory so
           ;; ConfigFiles/, TestImages/, and CADModels/ land directly under
           ;; share/PlusLib-2.9/ — the path PlusLib's PlusConfig.xml expects.
           (invoke "tar" "-xzf" #$source "--strip-components=1" "-C" share)))))
   (home-page "https://plustoolkit.github.io/")
   (synopsis "Data files for the PlusLib medical imaging toolkit")
   (description
    "PlusLibData provides configuration files, test images, and CAD models
used by PlusLib (Platform for Ultrasound-guided Intervention Software).
The data is organized as follows under @file{share/PlusLib-2.9/}:

@table @file
@item ConfigFiles/
Device configuration XML files and scene files for a wide range of supported
ultrasound imagers and tracking systems.
@item TestImages/
Medical ultrasound image sequences (@file{.mha} format) used as reference
data by the PlusLib test suite.
@item CADModels/
3-D model files for visualization of phantoms and calibration tools.
@end table")
   (license license:bsd-3)))

;;;
;;; PlusLib — Platform for Ultrasound-guided Intervention Software core library
;;;

(define %pluslib-commit "0c75fc06b030540ff304cefe584e58cc4c0e5c2d")
(define %pluslib-version "2.9.0-0c75fc0")
(define %pluslib-patches
  (list (search-patch
         "plustoolkit/0001-COMP-Find-vtkAddon-for-transitively-required-headers.patch")))

(define-public pluslib
  (package
   (name "pluslib")
   (version %pluslib-version)
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/PlusToolkit/PlusLib/archive/"
           %pluslib-commit ".tar.gz"))
     (sha256
      (base32 "0k33jhmw3lsqabr11w3nnpsxkn0yafc4p6i0vjdmfpqzih0zkcz3"))
     (patches %pluslib-patches)))
   (build-system cmake-build-system)
   (arguments
    (list #:tests? #f
          #:configure-flags
          #~(list
             "-DBUILD_TESTING:BOOL=OFF"
             "-DBUILD_SHARED_LIBS:BOOL=ON"
             ;; Enable OpenIGTLink protocol support (we have it packaged)
             "-DPLUS_USE_OpenIGTLink:BOOL=ON"
             ;; Enable Qt widget library (required by fCal and PlusServerLauncher
             ;; in PlusApp, which include QPlusConfigFileSaverDialog.h etc.)
             "-DPLUS_BUILD_WIDGETS:BOOL=ON"
             ;; Python3 hints — VTK's cmake config calls find_package(Python3)
             ;; unconditionally; provide explicit paths so it finds Guix Python.
             (string-append "-DPython3_EXECUTABLE="
                            #$(this-package-input "python") "/bin/python3")
             (string-append "-DPython3_INCLUDE_DIR="
                            #$(this-package-input "python")
                            "/include/python3.11")
             (string-append "-DPython3_LIBRARY="
                            #$(this-package-input "python")
                            "/lib/libpython3.11.so")
             ;; Use Guix's system zlib instead of VTK's bundled zlib target
             "-DPLUS_USE_SYSTEM_ZLIB:BOOL=ON"
             (string-append "-DZLIB_LIBRARY="
                            #$(this-package-input "zlib") "/lib/libz.so")
             (string-append "-DZLIB_INCLUDE_DIR="
                            #$(this-package-input "zlib") "/include")
             ;; Dependency cmake config paths
             (string-append "-DVTK_DIR="
                            #$(this-package-input "vtk-slicer")
                            "/lib/cmake/vtk-9.2")
             (string-append "-DITK_DIR="
                            #$(this-package-input "itk-slicer")
                            "/lib/cmake/ITK-5.4")
             (string-append "-DIGSIO_DIR="
                            #$(this-package-input "igsio")
                            "/lib/cmake/IGSIO")
             (string-append "-DvtkAddon_DIR="
                            #$(this-package-input "vtkaddon")
                            "/lib/cmake")
             (string-append "-DOpenIGTLink_DIR="
                            #$(this-package-input "openigtlink")
                            "/lib/igtl/cmake/igtl-3.1")
             (string-append "-DOpenIGTLinkIO_DIR="
                            #$(this-package-input "openigtlinkio")
                            "/lib/cmake/igtlio")
             ;; Set the data directory to the pluslibdata store path so that
             ;; the installed PlusConfig.xml has correct absolute paths for
             ;; DeviceSetConfigurationDirectory, ImageDirectory, and
             ;; ModelDirectory.  PlusLib's CMake re-sets PLUSLIB_DATA_DIR to
             ;; an install-relative ${PLUSLIB_INSTALL_PATH}/share/PlusLib-2.9
             ;; after configuring PlusConfig.xml, so this flag only affects the
             ;; XML stamping step and not the installed cmake config.
             (string-append "-DPLUSLIB_DATA_DIR="
                            #$(this-package-input "pluslibdata")
                            "/share/PlusLib-2.9"))))
   (inputs
    (list vtk-slicer
          itk-slicer
          vtkaddon
          igsio
          openigtlink
          openigtlinkio
          python
          ;; VTK external deps — VTK's cmake config requires all packages it
          ;; was built with to be findable at configure time (same rationale
          ;; as in igsio.scm).
          double-conversion
          eigen
          expat
          freetype
          gl2ps
          glew
          hdf5-1.10
          jsoncpp
          libharu
          libjpeg-turbo
          libogg
          libtheora
          libxml2
          lz4
          netcdf-slicer
          openmpi
          proj
          qtbase-5
          tbb
          zlib))
   (propagated-inputs
    ;; PlusLibConfig.cmake unconditionally calls find_package(VTK), find_package(ITK),
    ;; find_package(IGSIO), and find_package(OpenIGTLinkIO), so all four must be
    ;; in the closure of any downstream consumer.
    ;; pluslibdata is propagated so that the store path baked into PlusConfig.xml
    ;; (DeviceSetConfigurationDirectory, ImageDirectory, ModelDirectory) remains
    ;; valid as long as pluslib is in the profile — GC will not collect pluslibdata
    ;; independently.
    (list igsio itk-slicer openigtlink openigtlinkio pluslibdata vtk-slicer))
   (home-page "https://plustoolkit.github.io/")
   (synopsis "Platform for Ultrasound-guided Intervention Software — core library")
   (description
    "PlusLib is the core C++ library of the PlusToolkit (PLUS — Platform for
Ultrasound-guided Intervention Software).  It provides:
@itemize
@item A device abstraction layer for ultrasound imagers and tracking systems,
including virtual sources (saved data replay, synthetic ultrasound simulator).
@item 3-D volume reconstruction from tracked 2-D ultrasound images.
@item Pivot and landmark calibration algorithms.
@item Image processing filters (temporal and spatial calibration, compounding).
@item A network server and client using the OpenIGTLink protocol.
@end itemize

This build enables OpenIGTLink support and disables all device drivers that
require proprietary vendor SDKs.  The virtual and network data sources
(SavedDataSource, UltraSoundSimulator, OpenIGTLinkTracker) are always included.")
   (license license:bsd-3)))

;;;
;;; PlusApp — Platform for Ultrasound-guided Intervention Software applications
;;;

(define %plusapp-commit "17011e5df15ca2cfdd6ba3f285ab7a8568d942a1")
(define %plusapp-version "2.9.0-17011e5")
(define %plusapp-patches
  (list (search-patch
         "plusapp/0001-COMP-Guard-CPack-inclusion-behind-PLUSAPP_BUILD_PACK.patch")))

(define-public plusapp
  (package
   (name "plusapp")
   (version %plusapp-version)
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/PlusToolkit/PlusApp/archive/"
           %plusapp-commit ".tar.gz"))
     (sha256
      (base32 "05zr1rdpi2kp5j1jrqa4x2j4z2fgypp01pvzay3c6ki3chrl37a2"))
     (patches %plusapp-patches)))
   (build-system cmake-build-system)
   (arguments
    (list #:tests? #f
          #:configure-flags
          #~(list
             "-DBUILD_TESTING:BOOL=OFF"
             "-DBUILD_SHARED_LIBS:BOOL=ON"
             ;; Skip git revision lookup (no .git in the extracted source)
             "-DPLUSAPP_OFFLINE_BUILD:BOOL=ON"
             "-DPLUSAPP_BUILD_DiagnosticTools:BOOL=ON"
             "-DPLUSAPP_BUILD_fCal:BOOL=ON"
             (string-append "-DPlusLib_DIR="
                            #$(this-package-input "pluslib")
                            "/lib/cmake/PlusLib-2.9")
             ;; PlusLibConfig.cmake tries to locate ITK via PLUSLIB_INSTALL_PREFIX
             ;; which is a build-time variable and is not defined in the install tree.
             ;; Provide the path explicitly so find_package(ITK) succeeds.
             (string-append "-DITK_DIR="
                            #$(this-package-input "itk-slicer")
                            "/lib/cmake/ITK-5.4")
             ;; Our patch calls find_package(vtkAddon) to add vtkAddon include
             ;; dirs (needed transitively via igsioVideoFrame.h). vtkAddon uses
             ;; a flat cmake directory, so we must provide the path explicitly.
             (string-append "-DvtkAddon_DIR="
                            #$(this-package-input "vtkaddon")
                            "/lib/cmake")
             ;; PlusLibConfig.cmake calls find_package(VTK) which in turn
             ;; calls find_package(Python3); provide explicit paths.
             (string-append "-DPython3_EXECUTABLE="
                            #$(this-package-input "python") "/bin/python3")
             (string-append "-DPython3_INCLUDE_DIR="
                            #$(this-package-input "python")
                            "/include/python3.11")
             (string-append "-DPython3_LIBRARY="
                            #$(this-package-input "python")
                            "/lib/libpython3.11.so"))))
   (inputs
    (list pluslib
          itk-slicer
          vtkaddon
          python
          ;; Qt5 components used by fCal and PlusServerLauncher
          qtbase-5
          qtxmlpatterns-5
          ;; VTK external deps — PlusLibConfig.cmake calls find_package(VTK)
          ;; which requires all packages VTK was built with to be findable.
          double-conversion
          eigen
          expat
          freetype
          gl2ps
          glew
          hdf5-1.10
          jsoncpp
          libharu
          libjpeg-turbo
          libogg
          libtheora
          libxml2
          lz4
          netcdf-slicer
          openmpi
          proj
          tbb
          zlib))
   (home-page "https://plustoolkit.github.io/")
   (synopsis "Platform for Ultrasound-guided Intervention Software — applications")
   (description
    "PlusApp provides a collection of end-user applications built on top of
PlusLib (the core library of the PlusToolkit):
@itemize
@item @command{fCal} — a Qt/VTK GUI for spatial and temporal calibration,
including pivot calibration, phantom registration, and volume reconstruction.
@item @command{PlusServerLauncher} — a Qt GUI for starting and monitoring a
PlusServer instance (available when OpenIGTLink support is enabled).
@item @command{DiagDataCollection} — a command-line tool for collecting
diagnostic data from connected devices.
@item @command{TrackingDataServer} — a command-line OpenIGTLink tracking
data server.
@item @command{PointSetExtractor} and @command{SpatialSensorFusion} —
command-line utilities for post-processing.
@end itemize

This build enables OpenIGTLink support and the fCal calibration GUI.
All device drivers that require proprietary vendor SDKs are disabled.")
   (license license:bsd-3)))
