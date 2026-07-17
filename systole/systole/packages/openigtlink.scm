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

(define-module (systole packages openigtlink)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages elf)
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (systole packages pythonqt)
  #:use-module (systole packages slicer)
  #:use-module (systole packages ctk)
  #:use-module (systole packages itk)
  #:use-module (systole packages libarchive)
  #:use-module (systole packages qrestapi)
  #:use-module (systole packages teem)
  #:use-module (systole packages vtk)
  #:use-module (systole packages)
  #:use-module (srfi srfi-1))

(define-public openigtlink
  (let ((commit "c512727425c2b7a594fabb9cd1fbfac512bf376e") ;commit used by PlusBuild (Plus 2.8)
        (revision "0"))
  (package
   (name "openigtlink")
   (version (git-version "0.0.0" revision commit))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/openigtlink/OpenIGTLink")
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "17gpkq4c4rh1pglm327958b5mi94bxqgwim8qkwsqia02hq1xg4x"))))
   (build-system cmake-build-system)
   (arguments
    (list
     #:tests? #f
     #:configure-flags
     #~(list "-DBUILD_EXAMPLES:BOOL=OFF"
             "-DBUILD_TESTING:BOOL=OFF"
             "-DOpenIGTLink_SUPERBUILD:BOOL=OFF"
             "-DOpenIGTLink_PROTOCOL_VERSION_2:BOOL=OFF"
             "-DOpenIGTLink_PROTOCOL_VERSION_3:BOOL=ON"
             "-DOpenIGTLink_ENABLE_VIDEOSTREAMING:BOOL=ON"
             "-DOpenIGTLink_USE_VP9:BOOL=OFF"
             ;;"-DOpenIGTLink_INSTALL_PACKAGE_DIR:PATH=lib/cmake/OpenIGTLink"
             "-DBUILD_SHARED_LIBS:BOOL=ON")))
   (inputs (list glew))
   (home-page "https://openigtlink.org/")
   (synopsis
    "Free, open-source network communication library for image-guided therapy")
   (description
    "The OpenIGTLink Library is a C/C++ implementation of The OpenIGTLink
Protocol.  OpenIGTLink is an open-source network communication interface
specifically designed for image-guided interventions.  It aims to provide a
plug-and-play unified real-time communications (URTC) in operating rooms
(ORs) for image-guided interventions, where imagers, sensors, surgical
robots, and computers from different vendors work cooperatively.  This URTC
will ensure the seamless data flow among those components and enable a
closed-loop process of planning, control, delivery, and feedback.  The
specification of OpenIGTLink is open, and can be used without any license
fee; hence OpenIGTLink is suitable for both industrial and academic
developers.")
   (license license:bsd-3))))

(define-public slicer-openigtlink
  (let ((commit "6fbdadf16d6ccee8e840d9d408422bec4c95e867")
        (revision "0"))
  (package
   (name "slicer-openigtlink")
   (version (git-version "0.0.0" revision commit))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/openigtlink/SlicerOpenIGTLink")
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "03vipywwds2hmnrw1rk27x51mlph66rqirlhj55dl6g05bbw9356"))
     (patches (search-patches
               "0001-COMP-Add-conditional-build-of-UltrasoundRemoteContro.patch"
               "0002-COMP-Fix-include-directories-and-use-CMake-variables.patch"))))
   (build-system cmake-build-system)
   (arguments
    (list #:tests? #f
          #:validate-runpath? #f
          #:configure-flags
          #~(list
             "-DSlicerOpenIGTLink_SUPERBUILD:BOOL=OFF"
             "-DBUILD_TESTING:BOOL=OFF"
             ;; vtk-slicer's cmake config calls find_package(Python3)
             (string-append "-DPython3_EXECUTABLE="
                            #$(this-package-input "python")
                            "/bin/python3")
             (string-append "-DPython3_INCLUDE_DIR="
                            #$(this-package-input "python")
                            "/include/python3.11")
             (string-append "-DPython3_LIBRARY="
                            #$(this-package-input "python")
                            "/lib/libpython3.11.so")
             ;; SlicerConfig.cmake forces Slicer_USE_PYTHONQT=ON; WRAP_PYTHONQT in
             ;; Widgets/CMakeLists.txt then requires PythonQt to be findable.
             (string-append "-DPYTHONQT_INSTALL_DIR="
                            #$(this-package-input "pythonqt-commontk"))
             (string-append "-DSlicer_DIR:PATH="
                            #$(this-package-input "slicer-5.8")
                            "/lib/Slicer-5.8")
             (string-append "-DOpenIGTLink_DIR:PATH="
                            #$(this-package-input "openigtlink")
                            "/lib/igtl/cmake/igtl-3.1")
             (string-append "-DOpenIGTLinkIO_DIR:PATH="
                            #$(this-package-input "openigtlinkio")
                            "/lib/cmake/igtlio")
             (string-append "-DvtkSlicerMarkupsModuleMRML_INCLUDE_DIRS="
                            #$(this-package-input "slicer-markups-5.8")
                            "/include/Slicer-5.8/qt-loadable-modules/"
                            "vtkSlicerMarkupsModuleMRML")
             (string-append "-DvtkSlicerAnnotationsModuleMRML_INCLUDE_DIRS="
                            #$(this-package-input "slicer-annotations-5.8")
                            "/include/Slicer-5.8/qt-loadable-modules/"
                            "vtkSlicerAnnotationsModuleMRML")
             (string-append "-DvtkSlicerColorsModuleLogic_INCLUDE_DIRS="
                            #$(this-package-input "slicer-colors-5.8")
                            "/include/Slicer-5.8/qt-loadable-modules/"
                            "vtkSlicerColorsModuleLogic")
             (string-append "-DEXTRA_MODULE_LIB_DIRS="
                            #$(this-package-input "slicer-markups-5.8")
                            "/lib/Slicer-5.8/qt-loadable-modules;"
                            #$(this-package-input "slicer-annotations-5.8")
                            "/lib/Slicer-5.8/qt-loadable-modules;"
                            #$(this-package-input "slicer-colors-5.8")
                            "/lib/Slicer-5.8/qt-loadable-modules"))

          #:phases
          #~(modify-phases %standard-phases
              (add-after 'install 'symlink-so-files
                (lambda _
                  (let* ((lib-dir (string-append #$output "/lib"))
                         (modules-dir (string-append
                                       lib-dir "/Slicer-5.8/SlicerModules")))
                    (mkdir-p modules-dir)
                    (for-each
                     (lambda (file)
                       (symlink file
                                (string-append modules-dir "/"
                                               (basename file))))
                     (find-files lib-dir "\\.so$")))))
              (add-after 'symlink-so-files 'patch-runpath
                ;; Each library in qt-loadable-modules must be able to find its
                ;; sibling libraries (e.g. libqSlicerOpenIGTLinkIFModule.so needs
                ;; libqSlicerOpenIGTLinkIFModuleWidgets.so).  Adding $ORIGIN ensures
                ;; the dynamic linker searches the library's own directory first.
                (lambda _
                  (let ((modules-dir
                         (string-append
                          #$output "/lib/Slicer-5.8/qt-loadable-modules")))
                    (for-each
                     (lambda (lib)
                       (invoke "patchelf" "--add-rpath" "$ORIGIN" lib))
                     (find-files modules-dir "\\.so$"))))))))
   (inputs
    (list slicer-5.8
          python
          mesa
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
          ;;VTK
          vtk-slicer
          itk-slicer
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
          libxt
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
          pythonqt-commontk
          ctk
          ctkapplauncher
          libarchive-slicer
          teem-slicer
          vtkaddon
          qrestapi
          openigtlink
          openigtlinkio
          ;; Extra Slicer loadable modules needed for headers/libs
          slicer-markups-5.8
          slicer-annotations-5.8
          slicer-colors-5.8))
   (native-inputs (list patchelf))
   (synopsis "Slicer extension for communication of IGT data")
   (description
    "SlicerOpenIGTLink is a 3D Slicer extension designed to facilitate the
communication between 3D Slicer and other platforms via the OpenIGTLink
protocol.")
   (license license:bsd-2)
   (home-page "https://github.com/openigtlink/SlicerOpenIGTLink"))))

(define-public openigtlinkio
  (let ((commit "a262c1f5e63c00831cbf67d5284f4734f8a7b143")
        (revision "0"))
  (package
   (name "openigtlinkio")
   (version (git-version "0.0.0" revision commit))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/IGSIO/OpenIGTLinkIO")
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0rrcwm4sh4xrdidfci17srqqh9yfsl85nc0nfksd0d8x3mw2lywp"))
     (patches
      (search-patches
       "openigtlinkio/0001-COMP-Install-igtlioLogic.h-as-a-public-header.patch"))))
   (build-system cmake-build-system)
   (arguments
    (list #:tests? #f
          #:validate-runpath? #f
          #:configure-flags
          #~(list
             ;; vtk-slicer VTK cmake config calls find_package(Python3)
             ;; unconditionally; provide paths so it finds Guix Python.
             (string-append "-DPython3_EXECUTABLE="
                            #$(this-package-input "python")
                            "/bin/python3")
             (string-append "-DPython3_INCLUDE_DIR="
                            #$(this-package-input "python")
                            "/include/python3.11")
             (string-append "-DPython3_LIBRARY="
                            #$(this-package-input "python")
                            "/lib/libpython3.11.so")
             (string-append "-DSlicer_DIR:PATH="
                            #$(this-package-input "slicer-5.8")
                            "/lib/Slicer-5.8")
             (string-append "-DOpenIGTLink_DIR:PATH="
                            #$(this-package-input "openigtlink")
                            "/lib/igtl/cmake/igtl-3.1"))))
  (inputs
   (list slicer-5.8
         python
         mesa
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
         ;;VTK
         vtk-slicer
         itk-slicer
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
         libxt
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
         ctk
         ctkapplauncher
         libarchive-slicer
         teem-slicer
         vtkaddon
         qrestapi
         openigtlink))
  (synopsis "VTK- and Qt-based wrapper library around OpenIGTLink")
  (description
    "OpenIGTLinkIO contains several wrapper layers on top of the OpenIGTLink
library.  The code originates from OpenIGTLink/OpenIGTLinkIF.  The main intent
of the library is to share igtl code between Slicer, CustusX, IBIS, MITK and
other systems.")
  (license license:bsd-2)
  (home-page "https://github.com/IGSIO/OpenIGTLinkIO"))))

