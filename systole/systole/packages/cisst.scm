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
  #:use-module (gnu packages compression)   ; zlib
  #:use-module (systole packages openhaptics))  ; openhaptics-sdk, touch-driver

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
              ;; cisstNumerical defaults CISST_HAS_CISSTNETLIB to OFF
              ;; outside catkin builds; without it cisstRobot is
              ;; silently skipped and downstream saw* packages fail.
              "-DCISST_HAS_CISSTNETLIB=ON"
              ;; cisstRobot defaults to building robReflexxes, which
              ;; ExternalProject-clones ReflexxesTypeII from GitHub.
              ;; Disable — not used by the sawControllers/
              ;; sawSensablePhantom code paths we need.
              "-DCISST_ROB_HAS_REFLEXXES_TYPEII=OFF"
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

;;;
;;; sawKeyboard — cisst/SAW keyboard-input component.  Pure cisst,
;;; no ROS dependency.  Required by sawControllers and beyond.
;;;

(define-public saw-keyboard
  (package
    (name "saw-keyboard")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jhu-saw/sawKeyboard")
             (commit "2c254c5cfd9acc86188a76e7b615e526a21e56ac")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "116j5nhgxdxyazlfsyxn710vr3s9fy95raqg1y1yb04qqzri7lq5"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DCMAKE_BUILD_TYPE=Release"
              (string-append "-Dcisst_DIR="
                             #$(this-package-input "cisst")
                             "/share/cisst-1.4/cmake"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'set-install-rpath
            (lambda _
              (setenv "CMAKE_INSTALL_RPATH"
                      "$ORIGIN:$ORIGIN/../lib"))))))
    (inputs (list cisst cisst-netlib))
    (home-page "https://github.com/jhu-saw/sawKeyboard")
    (synopsis "cisst/SAW keyboard-input component")
    (description
     "@code{sawKeyboard} is a cisst @code{mtsComponent} that turns
terminal key presses into cisst events.  It is a build dependency of
@code{sawControllers} and, transitively, @code{sawSensablePhantom}.")
    (license license:bsd-3)))

;;;
;;; sawControllers (core) — cisst/SAW PID, gravity-compensation, and
;;; teleop controllers.  Required by sawSensablePhantom.
;;;

(define-public saw-controllers
  (package
    (name "saw-controllers")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jhu-saw/sawControllers")
             (commit "cc8870e20ef0954745a83cbc4eda7d167e3bda9a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ih28k1b09hnqn4crk0ffpk6rrjxcnnld0y8xcsxyfk0vijnly3n"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      ;; Build only core/components — core/ also adds_subdirectory
      ;; (examples) which needs Qt; we don't need those.
      #:configure-flags
      #~(list "-DCMAKE_BUILD_TYPE=Release"
              (string-append "-Dcisst_DIR="
                             #$(this-package-input "cisst")
                             "/share/cisst-1.4/cmake")
              (string-append "-DsawKeyboard_DIR="
                             #$(this-package-input "saw-keyboard")
                             "/share/sawKeyboard"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key outputs configure-flags #:allow-other-keys)
              (let ((source (getcwd))
                    (out (assoc-ref outputs "out")))
                (apply invoke "cmake"
                       "-S" (string-append source "/core/components")
                       "-B" "build"
                       (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                       configure-flags))))
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (invoke "cmake" "--build" "build"
                      "-j" (if parallel-build?
                               (number->string (parallel-job-count))
                               "1"))))
          (replace 'install
            (lambda _ (invoke "cmake" "--install" "build")))
          (add-before 'configure 'set-install-rpath
            (lambda _
              (setenv "CMAKE_INSTALL_RPATH"
                      "$ORIGIN:$ORIGIN/../lib"))))))
    (inputs (list cisst cisst-netlib saw-keyboard
                  ;; cisst's mtsCommonXML/JSON leaks libjsoncpp.so.26
                  ;; into downstream link; RUNPATH validation needs it.
                  jsoncpp))
    (home-page "https://github.com/jhu-saw/sawControllers")
    (synopsis "cisst/SAW PID and teleoperation controllers")
    (description
     "@code{sawControllers} bundles the cisst implementations of PID
joint controllers, gravity compensation, and teleoperation components
used by JHU's haptic-device stack.  This Guix package builds only the
@code{core/components} subtree (the shared library); the example
programs in @code{core/examples} are skipped because they pull in Qt
and are not needed for the Touch demo.")
    (license license:bsd-3)))

;;;
;;; sawSensablePhantom (core) — cisst/SAW component that talks to
;;; the 3D Systems Touch via OpenHaptics HD.  First consumer of
;;; openhaptics-sdk and touch-driver from (systole packages openhaptics).
;;;

(define-public saw-sensable-phantom
  (package
    (name "saw-sensable-phantom")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jhu-saw/sawSensablePhantom")
             (commit "64c8b05ec2263a93a1ebd1a2bd508c2c46d024a9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a32y6qdsrpyvq9i2v0w5kw1xbngbi4nkm4rxvn83kaddwy4jm6b"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DCMAKE_BUILD_TYPE=Release"
              (string-append "-Dcisst_DIR="
                             #$(this-package-input "cisst")
                             "/share/cisst-1.4/cmake")
              (string-append "-DsawControllers_DIR="
                             #$(this-package-input "saw-controllers")
                             "/share/sawControllers"))
      #:phases
      #~(modify-phases %standard-phases
          ;; Build only core/components; core/examples requires Qt.
          (replace 'configure
            (lambda* (#:key outputs configure-flags #:allow-other-keys)
              (let ((source (getcwd))
                    (out (assoc-ref outputs "out")))
                (apply invoke "cmake"
                       "-S" (string-append source "/core/components")
                       "-B" "build"
                       (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                       configure-flags))))
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (invoke "cmake" "--build" "build"
                      "-j" (if parallel-build?
                               (number->string (parallel-job-count))
                               "1"))))
          (replace 'install
            (lambda _ (invoke "cmake" "--install" "build")))
          (add-before 'configure 'set-install-rpath
            (lambda _
              (setenv "CMAKE_INSTALL_RPATH"
                      "$ORIGIN:$ORIGIN/../lib"))))))
    (inputs (list cisst cisst-netlib saw-keyboard saw-controllers
                  jsoncpp
                  ;; libHD + libPhantomIOLib42 from the proprietary
                  ;; OpenHaptics stack.  openhaptics-sdk propagates
                  ;; touch-driver automatically.
                  openhaptics-sdk))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/jhu-saw/sawSensablePhantom")
    (synopsis "cisst/SAW component for the 3D Systems Touch haptic device")
    (description
     "@code{sawSensablePhantom} is the cisst @code{mtsComponent} that
wraps the OpenHaptics HD SDK to talk to the 3D Systems Touch.  Links
against the proprietary @code{openhaptics-sdk} and pulls in
@code{touch-driver} at runtime.")
    (license license:bsd-3)))
