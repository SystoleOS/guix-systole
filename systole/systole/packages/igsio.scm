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

(define-module (systole packages igsio)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages)
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
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (systole packages itk)
  #:use-module (systole packages maths)
  #:use-module (systole packages pythonqt)
  #:use-module (systole packages slicer)
  #:use-module (systole packages vtk)
  #:use-module (systole packages ctk)
  #:use-module (systole packages))

;;;
;;; IGSIO — Image Guided Surgery InterOperability library
;;;

(define %igsio-commit "4f3605eb1928c9926be4e476c8608c0754f35873")
(define %igsio-version "0.0.0-4f3605e")

(define-public igsio
  (package
   (name "igsio")
   (version %igsio-version)
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/IGSIO/IGSIO/archive/"
           %igsio-commit ".tar.gz"))
     (sha256
      (base32 "1wqzyi28yi5s5jhip996b594lxcjmbf2l6l0a7a3rvy6qbx68lxs"))
     (patches (search-patches
               "0001-ENH-Add-install-tree-CMake-configuration-to-IGSIO.patch"
               "0002-ENH-Restrict-find_package-VTK-to-required-components.patch"
               "0003-COMP-Fall-back-to-find_library-when-vtkAddonTargets..patch"
               "0004-COMP-Install-vtkigsiocalibration_export.h-generated-header.patch"
               "0005-COMP-Always-install-VolumeReconstruction-headers-on-all-platforms.patch"))))
   (build-system cmake-build-system)
   (arguments
    (list #:tests? #f
          #:configure-flags
          #~(list "-DIGSIO_SUPERBUILD:BOOL=OFF"
                  "-DBUILD_TESTING:BOOL=OFF"
                  "-DIGSIO_BUILD_SEQUENCEIO:BOOL=ON"
                  "-DIGSIO_BUILD_VOLUMERECONSTRUCTION:BOOL=ON"
                  "-DIGSIO_BUILD_CODECS:BOOL=OFF"
                  "-DIGSIO_SEQUENCEIO_ENABLE_MKV:BOOL=OFF"
                  "-DIGSIO_USE_SYSTEM_ZLIB:BOOL=ON"
                  "-DIGSIO_USE_3DSlicer:BOOL=OFF"
                  (string-append "-DVTK_DIR="
                                 #$(this-package-input "vtk-slicer")
                                 "/lib/cmake/vtk-9.2")
                  (string-append "-DITK_DIR="
                                 #$(this-package-input "itk-slicer")
                                 "/lib/cmake/ITK-5.4")
                  (string-append "-DvtkAddon_DIR="
                                 #$(this-package-input "vtkaddon-python")
                                 "/lib/cmake")
                  (string-append "-DQt5_DIR="
                                 #$(this-package-input "qtbase")
                                 "/lib/cmake/Qt5")
                  ;; TODO check whether these could not be picked up from Slicer
                  ;; cmake configuration files instead
                  (string-append "-DPython3_EXECUTABLE="
                                 #$(this-package-input "python") "/bin/python3")
                  (string-append "-DPython3_INCLUDE_DIR="
                                 #$(this-package-input "python") "/include/python3.11")
                  (string-append "-DPython3_LIBRARY="
                                 #$(this-package-input "python") "/lib/libpython3.11.so")
                  (string-append "-DGLEW_INCLUDE_DIR="
                                 #$(this-package-input "glew") "/include/GL")
                  (string-append "-DGLEW_LIBRARY="
                                 #$(this-package-input "glew") "/lib/libGLEW.so")

                  )))
   (inputs (list
            vtk-slicer
            itk-slicer
            vtkaddon-python
            zlib
            ;; VTK external deps — VTK's cmake config (and ITK's UseITK.cmake, which
            ;; calls find_package(VTK) internally) requires all packages that vtk-slicer
            ;; was built with to be findable at configure time.
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
            libtheora
            libogg
            libxml2
            lz4
            netcdf-slicer
            openmpi
            proj
            qtbase-5
            tbb
            python))

   (propagated-inputs (list vtk-slicer-python ctk-python vtkaddon-python))

   (home-page "https://github.com/IGSIO/IGSIO")
   (synopsis "Image Guided Surgery InterOperability library")
   (description
    "IGSIO (Image Guided Surgery InterOperability) is a C++ library providing
algorithms and tools for image-guided interventions, including volume
reconstruction from tracked 2-D ultrasound images, sequence I/O (reading and
writing tracked image sequences), and pivot and landmark calibration
algorithms.  It is used as the algorithmic back-end for the SlicerIGT and
SlicerIGSIO 3D Slicer extensions.")
   (license license:bsd-3)))

(define-public igsio-python
  (package
   (inherit igsio)
   (name "igsio-python")
   (inputs (modify-inputs (package-inputs igsio)
             (replace "vtk-slicer" vtk-slicer-python)))
   (arguments
    (substitute-keyword-arguments (package-arguments igsio)
      ((#:configure-flags flags)
       #~(map (lambda (f)
                (cond
                  ((string-prefix? "-DVTK_DIR=" f)
                   (string-append "-DVTK_DIR="
                                  #$vtk-slicer-python "/lib/cmake/vtk-9.2"))
                  ((string-prefix? "-DvtkAddon_DIR=" f)
                   (string-append "-DvtkAddon_DIR="
                                  #$vtkaddon-python "/lib/cmake"))
                  (else f)))
              #$flags))))))

;;;
;;; SlicerIGSIOCommon — Slicer loadable module providing IGSIO-Slicer bridge
;;;

(define %slicer-igsio-commit "1a89776c9f1c8bbbad62000561aa892afe1e7077")
(define %slicer-igsio-version "0.0.0-1a89776")

(define %slicer-igsio-source
  (origin
   (method url-fetch)
   (uri (string-append
         "https://github.com/IGSIO/SlicerIGSIO/archive/"
         %slicer-igsio-commit ".tar.gz"))
   (sha256
    (base32 "1dxwbjwdazzf0k29hyzg0zh8mm07q0lqhzcj1xrsgaf9r2zq8h0z"))
   (patches (search-patches
             "0001-ENH-Add-standalone-CMake-preamble-for-SlicerIGSIOCom.patch"
             "0002-COMP-Guard-vtkIGSIOMkvSequenceIO-include-behind-IGSI.patch"))))

;;; Factory for SlicerIGSIOCommon (non-Python and Python variants).
(define* (make-slicer-igsio-common
          #:key
          (name "slicer-igsio-common")
          (slicer slicer-python-5.8)
          (igsio igsio)
          (extra-inputs '())
          (extra-configure-flags #~'()))
  (package
   (name name)
   (version %slicer-igsio-version)
   (source
    (origin (inherit %slicer-igsio-source)))
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
                                  #$slicer "/lib/Slicer-5.8")
                   "-DSlicer_USE_PYTHONQT:BOOL=OFF"
                   (string-append "-DPYTHONQT_INSTALL_DIR="
                                  #$pythonqt-commontk)
                   (string-append "-DIGSIO_DIR="
                                  #$igsio "/lib/cmake/IGSIO"))
             #$extra-configure-flags)
          #:phases
          #~(modify-phases %standard-phases
              (replace 'configure
                (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
                  (let ((out (assoc-ref outputs "out")))
                    (apply invoke "cmake"
                           "-S" (string-append (getcwd) "/SlicerIGSIOCommon")
                           "-B" "build"
                           (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                           configure-flags)
                    (chdir "build")
                    #t))))))
   (inputs (fold (lambda (pkg acc)
                   (modify-inputs acc (prepend pkg)))
                 (modify-inputs (package-inputs slicer)
                   (prepend slicer)
                   (prepend igsio))
                 extra-inputs))
   (home-page "https://github.com/IGSIO/SlicerIGSIO")
   (synopsis "SlicerIGSIO common library — IGSIO-to-Slicer bridge")
   (description
    "The SlicerIGSIOCommon loadable module provides @code{vtkSlicerIGSIOCommon}
and @code{vtkSlicerIGSIOLogger}, which bridge IGSIO algorithms (volume
reconstruction, sequence I/O) to the 3D Slicer MRML scene and logging
infrastructure.  It is a dependency of the VolumeReconstruction module in
SlicerIGT.")
   (license license:bsd-3)))

(define-public slicer-igsio-common
  (make-slicer-igsio-common
   #:name "slicer-igsio-common"
   #:slicer slicer-python-5.8
   #:igsio igsio
   #:extra-inputs (list slicer-sequences-5.8 slicer-volumes-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DvtkSlicerSequencesModuleMRML_INCLUDE_DIRS="
       #$slicer-sequences-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSequencesModuleMRML")
      (string-append
       "-DvtkSlicerVolumesModuleLogic_INCLUDE_DIRS="
       #$slicer-volumes-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerVolumesModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-sequences-5.8 "/lib/Slicer-5.8/qt-loadable-modules"
       ";" #$slicer-volumes-5.8 "/lib/Slicer-5.8/qt-loadable-modules"))))

(define-public slicer-igsio-common-python
  (make-slicer-igsio-common
   #:name "slicer-igsio-common-python"
   #:slicer slicer-python-5.8
   #:igsio igsio-python
   #:extra-inputs (list slicer-sequences-5.8 slicer-volumes-5.8)
   #:extra-configure-flags
   #~(list
      (string-append
       "-DvtkSlicerSequencesModuleMRML_INCLUDE_DIRS="
       #$slicer-sequences-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerSequencesModuleMRML")
      (string-append
       "-DvtkSlicerVolumesModuleLogic_INCLUDE_DIRS="
       #$slicer-volumes-5.8
       "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerVolumesModuleLogic")
      (string-append
       "-DEXTRA_MODULE_LIB_DIRS="
       #$slicer-sequences-5.8 "/lib/Slicer-5.8/qt-loadable-modules"
       ";" #$slicer-volumes-5.8 "/lib/Slicer-5.8/qt-loadable-modules"))))
