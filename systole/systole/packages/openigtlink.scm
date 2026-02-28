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
  #:use-module (guix download)
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
  #:use-module (guix download)
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
  (package
   (name "openigtlink")
   (version "0.0.0-c512727") ;version used by PlusBuild (Plus 2.8)
   (source
    (origin
     (method url-fetch)
     (uri
      "https://github.com/openigtlink/OpenIGTLink/archive/c512727425c2b7a594fabb9cd1fbfac512bf376e.tar.gz")
     (sha256
      (base32 "0s2rxa4igs2d354205vnp57bf81yj5fpqh91hy5v3zz34gri46j5"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags (list "-DBUILD_EXAMPLES:BOOL=OFF"
                              "-DBUILD_TESTING:BOOL=OFF"
                              "-DOpenIGTLink_SUPERBUILD:BOOL=OFF"
                              "-DOpenIGTLink_PROTOCOL_VERSION_2:BOOL=OFF"
                              "-DOpenIGTLink_PROTOCOL_VERSION_3:BOOL=ON"
                              "-DOpenIGTLink_ENABLE_VIDEOSTREAMING:BOOL=ON"
                              "-DOpenIGTLink_USE_VP9:BOOL=OFF"
                              ;;"-DOpenIGTLink_INSTALL_PACKAGE_DIR:PATH=lib/cmake/OpenIGTLink"
                              "-DBUILD_SHARED_LIBS:BOOL=ON")
      #:tests? #f))
   (inputs (list glew))
   (home-page "openigtlink.org")
   (synopsis
    "Free, open-source network communication library for image-guided therapy")
   (description
    "The OpenIGTLink Library is a C/C++ implementation of The OpenIGTLink
Protocol. OpenIGTLink is an open-source network communication interface
specifically designed for image-guided interventions. It aims to provide a
plug-and-play unified real-time communications (URTC) in operating rooms (ORs)
for image-guided interventions, where imagers, sensors, surgical robots,and
computers from different vendors work cooperatively. This URTC will ensure the
seamless data flow among those components and enable a closed-loop process of
planning, control, delivery, and feedback. The specification of OpenIGTLink is
open, and can be used without any license fee; hence OpenIGTLink is suitable for
both industrial and academic developers.")
   (license license:bsd-3)))

(define-public slicer-openigtlink
  (package
   (name "slicer-openigtlink")
   (version "0.0.0-6fbdadf1")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/openigtlink/SlicerOpenIGTLink/archive/6fbdadf16d6ccee8e840d9d408422bec4c95e867.tar.gz")
     (sha256
      (base32 "0m56zqjdv87iv0p6g269kj9bjknjsaq18ijpsjawh9wi2w8ybsaj"))
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
                            "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerMarkupsModuleMRML")
             (string-append "-DvtkSlicerAnnotationsModuleMRML_INCLUDE_DIRS="
                            #$(this-package-input "slicer-annotations-5.8")
                            "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerAnnotationsModuleMRML")
             (string-append "-DvtkSlicerColorsModuleLogic_INCLUDE_DIRS="
                            #$(this-package-input "slicer-colors-5.8")
                            "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerColorsModuleLogic")
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
               (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                        (lib-dir (string-append out "/lib"))
                        (modules-dir (string-append lib-dir "/Slicer-5.8/SlicerModules")))
                     (mkdir-p modules-dir)
                     (for-each
                     (lambda (file)
                        (let ((target (string-append modules-dir "/" (basename file))))
                           (symlink file target)))
                     (find-files lib-dir "\\.so$")))))
            (add-after 'symlink-so-files 'patch-runpath
               ;; Each library in qt-loadable-modules must be able to find its
               ;; sibling libraries (e.g. libqSlicerOpenIGTLinkIFModule.so needs
               ;; libqSlicerOpenIGTLinkIFModuleWidgets.so).  Adding $ORIGIN ensures
               ;; the dynamic linker searches the library's own directory first.
               (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (modules-dir (string-append out "/lib/Slicer-5.8/qt-loadable-modules")))
                     (for-each
                     (lambda (lib)
                        (invoke "patchelf" "--add-rpath" "$ORIGIN" lib))
                     (find-files modules-dir "\\.so$"))))))
                            ))
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
          slicer-colors-5.8
          ))
   (native-inputs (list patchelf))
   (synopsis "Slicer Extension for communication of IGT data")
   (description "SlicerOpenIGTLink is a 3D Slicer extension designed to facilitate the communication between 3D Slicer and other platforms via OpenIGTLink.")
   (license license:bsd-2)
   (home-page "https://github.com/openigtlink/SlicerOpenIGTLink")))

(define-public openigtlinkio
  (package
   (name "openigtlinkio")
   (version "0.0.0-a262c1f")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/IGSIO/OpenIGTLinkIO/archive/a262c1f5e63c00831cbf67d5284f4734f8a7b143.tar.gz")
     (sha256
      (base32 "01y6nhv7c5m57clpql8vg1g43k4k37mvb0bvasl28r90mqm4dvsm"))))
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
  (synopsis "Library for interfacing to openigtlink/OpenIGTLink, dependent on VTK and Qt. Based on openigtlink/OpenIGTLinkIF")
  (description "OpenIGTLinkIO contains several wrapper layers on top of OpenIGTLink. The code originates from OpenIGTLink/OpenIGTLinkIF. The main intent of the library is to share igtl code between Slicer, CustusX, IBIS, MITK and other systems.")
  (license license:bsd-2)
  (home-page "https://github.com/IGSIO/OpenIGTLinkIO")))

