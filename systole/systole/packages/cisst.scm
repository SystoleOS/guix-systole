;;
;; Copyright @ 2026 Oslo University Hospital
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

;;; Commentary:
;;;
;;; JHU cisst (Computer Integrated Surgical Systems and Technology) —
;;; the robotics / surgical-systems C++ framework that underlies the
;;; entire JHU saw- stack, including sawSensablePhantom and, through
;;; it, Laura Connolly's SlicerROS2 Touch demo.
;;;
;;; cisst ships a small Custom BSD-like license ("cisst license") that
;;; is documented at http://www.cisst.org/cisst/license.txt; we map it
;;; to @code{license:bsd-3} which closely matches its terms.
;;;
;;; Code:

(define-module (systole packages cisst)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages commencement) ; gfortran-toolchain
  #:use-module (gnu packages gcc)           ; gfortran
  #:use-module (gnu packages maths)         ; lapack (unused, reference)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)    ; pkg-config
  #:use-module (gnu packages serialization) ; jsoncpp
  #:use-module (gnu packages xml)           ; libxml2
  #:use-module (gnu packages compression))  ; zlib

;;;
;;; clapack source — a separate origin so cisstNetlib's ExternalProject
;;; pipeline does not need network access at build time.  Pinned to the
;;; commit on jhu-cisst-external/clapack#main from which cisstNetlib
;;; 3.2.2 expects its F2C-translated LAPACK.
;;;

(define clapack-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/jhu-cisst-external/clapack")
          (commit "93ef9f8fc85e8267a56a08a597f796c8624b9e57")))
    (file-name "clapack-93ef9f8.tar.gz")
    (sha256
     (base32 "16yijw248pb545553975imhv5z7blvhisgzyg9vbyxxsdk30idra"))))

;;;
;;; cisstNetlib — JHU's F2C-translated LAPACK/BLAS wrapper library.
;;;

(define-public cisst-netlib
  (package
    (name "cisst-netlib")
    (version "3.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jhu-cisst/cisstNetlib")
             (commit "9112629f712677ef547dfca081a781c7febf911b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08gmjnaxw77bbk3mxv95flj65iz1idfz5djzm1x0w0cyyjpk3igc"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DcisstNetlib_LANGUAGE=C"
              "-DCMAKE_BUILD_TYPE=Release"
              "-DCMAKE_POSITION_INDEPENDENT_CODE=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-clapack-download
            (lambda _
              ;; Strip the network GIT_REPOSITORY/GIT_TAG pair from
              ;; ExternalProject_Add; we will populate the expected
              ;; SOURCE_DIR path manually after 'configure (see
              ;; 'populate-clapack below) so ExternalProject_Add's
              ;; build step finds clapack already in place.
              (substitute* "CMakeLists.txt"
                (("GIT_REPOSITORY \"https://github\\.com/jhu-cisst-external/clapack\"")
                 "DOWNLOAD_COMMAND \"\"")
                (("GIT_TAG \"main\"")
                 ""))))
          (add-after 'configure 'populate-clapack
            (lambda _
              ;; cisstNetlib's ExternalProject_Add computes
              ;;   ${CMAKE_BINARY_DIR}/cisstNetlibLapack/src/cisstNetlib_C
              ;; and cisstNetlib's top-level include_directories() hard
              ;; codes that same path.  Copy the pre-fetched clapack
              ;; tree into it before the ExternalProject build target
              ;; runs.
              (let ((dst "../build/cisstNetlibLapack/src/cisstNetlib_C"))
                (mkdir-p (dirname dst))
                (copy-recursively #$clapack-source dst))))
          (add-after 'unpack 'set-install-rpath
            (lambda _
              (setenv "CMAKE_INSTALL_RPATH"
                      "$ORIGIN:$ORIGIN/../lib"))))))
    (home-page "https://github.com/jhu-cisst/cisstNetlib")
    (synopsis "JHU cisst F2C-translated LAPACK/BLAS wrapper")
    (description
     "@code{cisstNetlib} packages an F2C-translated LAPACK/BLAS plus
cisst-specific numerical routines (Hanson-Haskell, Lawson-Hanson) under
the @code{cisstNetlib_} symbol prefix.  It is the dense-linear-algebra
backend used by @code{cisstNumerical} and, transitively, any cisst
component that performs matrix factorisations — including the trajectory
generation and inverse kinematics used by the sawControllers stack that
drives the 3D Systems Touch.")
    (license license:bsd-3)))

;;;
;;; cisst — JHU's Computer Integrated Surgical Systems and Technology
;;; C++ framework.  Minimal build profile: the 7 default libraries
;;; (cisstCommon, cisstVector, cisstOSAbstraction, cisstNumerical,
;;; cisstMultiTask, cisstParameterTypes, cisstRobot) as shared libs,
;;; with JSON support on and everything else off.  Enough to build
;;; the saw- stack and, transitively, sawSensablePhantom / ROS 2.
;;;

(define-public cisst
  (package
    (name "cisst")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jhu-cisst/cisst")
             (commit "fe84bf08817dc67e0fbf782190c656fe9cc5c2df")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "095icl70bh8kdsq30bds2jmn70gqykmhskjshij89q8gq5d9gqm8"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DCMAKE_BUILD_TYPE=Release"
              "-DCISST_BUILD_SHARED_LIBS=ON"
              "-DCISST_BUILD_TESTS=OFF"
              "-DCISST_BUILD_EXAMPLES=OFF"
              "-DCISST_BUILD_APPLICATIONS=OFF"
              "-DCISST_HAS_JSON=ON"
              ;; Use pkg-config to pick up Guix's jsoncpp instead of
              ;; the cisstJSONExternal ExternalProject_Add that would
              ;; clone jsoncpp from GitHub.
              "-DCISST_USE_PKG_CONFIG=ON"
              "-DCISST_HAS_SWIG_PYTHON=OFF"
              "-DCISST_HAS_QT5=OFF"
              "-DCISST_HAS_IOS=OFF"
              "-DCISST_HAS_LINUX_RTAI=OFF"
              "-DCISST_HAS_LINUX_XENOMAI=OFF"
              "-DCISST_USE_SI_UNITS=ON"
              ;; Optional libs off — only the 7 default libs.
              "-DCISST_cisstMesh=OFF"
              "-DCISST_cisstInteractive=OFF"
              "-DCISST_cisstStereoVision=OFF"
              "-DCISST_cisst3DUserInterface=OFF"
              ;; cisstNetlib location for cisstNumerical.
              (string-append "-DCisstNetlib_DIR="
                             #$(this-package-input "cisst-netlib")
                             "/share/cisstNetlib"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'set-install-rpath
            (lambda _
              ;; cisst installs libcisst*.so into lib/; internal
              ;; libraries dlopen each other and must resolve via
              ;; $ORIGIN, and cisst-netlib's config dir is used at
              ;; configure time only (its libs are static).
              (setenv "CMAKE_INSTALL_RPATH"
                      "$ORIGIN:$ORIGIN/../lib"))))))
    (native-inputs (list pkg-config))
    (inputs (list cisst-netlib jsoncpp libxml2 zlib))
    (home-page "https://github.com/jhu-cisst/cisst")
    (synopsis "JHU cisst robotics / surgical-systems framework")
    (description
     "@code{cisst} is the Johns Hopkins Computer Integrated Surgical
Systems and Technology framework — a C++ collection of libraries used
as the foundation of the JHU saw- robot-integration components and,
through them, Laura Connolly's SlicerROS2 Touch haptic-device demo.

This Guix package builds the seven default shared libraries
(@code{cisstCommon}, @code{cisstVector}, @code{cisstOSAbstraction},
@code{cisstNumerical}, @code{cisstMultiTask}, @code{cisstParameterTypes},
@code{cisstRobot}) with JSON support enabled; SWIG/Python bindings,
Qt5 widgets, FLTK widgets, real-time extensions (RTAI, Xenomai), and
the optional @code{cisstMesh}/@code{cisstStereoVision}/
@code{cisst3DUserInterface} libraries are all disabled — they are not
needed for the haptic-device control path and would pull in large
unused dependencies.")
    (license license:bsd-3)))
