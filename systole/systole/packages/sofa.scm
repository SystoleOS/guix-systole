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
;;; SOFA Framework (Simulation Open Framework Architecture) — an open-source
;;; C++ library for real-time physics simulation, primarily used for
;;; soft-tissue mechanics and surgical simulation.  This module packages
;;; the Slicer fork of SOFA v26.06 with bundled plugins (SofaPython3,
;;; SofaSTLIB, BeamAdapter, Registration, Cosserat) built inline via
;;; SOFA_EXTERNAL_DIRECTORIES, matching SlicerSOFA's preview branch (main
;;; after Slicer/SlicerSOFA#83).
;;;
;;; Code:

(define-module (systole packages sofa)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages algebra)       ; eigen
  #:use-module (gnu packages boost)         ; boost
  #:use-module (gnu packages check)          ; googletest
  #:use-module (gnu packages compression)   ; zlib
  #:use-module (gnu packages cpp)           ; nlohmann-json, cxxopts
  #:use-module (gnu packages image-processing) ; cimg
  #:use-module (gnu packages maths)         ; metis
  #:use-module (gnu packages python-xyz)    ; pybind11
  #:use-module (gnu packages gl)            ; glew
  #:use-module (gnu packages python)        ; python
  #:use-module (gnu packages qt)            ; qtbase-5
  #:use-module (gnu packages xml)            ; tinyxml2
  #:use-module (systole packages))          ; search-patches

;;;
;;; tight-inclusion — Continuous Collision Detection library required
;;; by Sofa.Component.Collision.Detection.Intersection.
;;;

(define-public tight-inclusion
  (package
    (name "tight-inclusion")
    (version "1.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sofa-framework/Tight-Inclusion")
             ;; Head of upstream's v1.0.6-export-target branch, the ref
             ;; SOFA 26.06 fetches.  Pinned by commit: the branch was
             ;; force-moved on 2026-07-29 (a273db8, spdlog bump), which
             ;; broke the recorded hash.
             (commit "a08a7c68abe51ae292ef370f74dde9022b987a29")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10dpph0naaj12b4szzx3lpxk8xfw5aq071hg0lhyfwn61rzfwjr2"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags
           #~(list "-DCMAKE_BUILD_TYPE=Release"
                   "-DBUILD_SHARED_LIBS=ON"
                   ;; Disable CPM network fetching — use system packages.
                   "-DCPM_LOCAL_PACKAGES_ONLY=ON"
                   "-DFETCHCONTENT_FULLY_DISCONNECTED=ON"
                   ;; GNUInstallDirs not set when built standalone.
                   "-DCMAKE_INSTALL_INCLUDEDIR=include"
                   "-DCMAKE_INSTALL_LIBDIR=lib"
                   (string-append "-DEIGEN3_INCLUDE_DIR="
                                  #$(this-package-input "eigen")
                                  "/include/eigen3")
                   (string-append "-Dspdlog_DIR="
                                  #$(this-package-input "spdlog")
                                  "/lib/cmake/spdlog"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-cmake
                 (lambda _
                   ;; Add cmake_minimum_required for CMP0048.
                   (substitute* "CMakeLists.txt"
                     (("get_directory_property")
                      (string-append
                       "cmake_minimum_required(VERSION 3.18)\n"
                       "get_directory_property")))
                   ;; Bypass CPM — stub out recipes to use find_package.
                   (call-with-output-file "cmake/recipes/CPM.cmake"
                     (lambda (port)
                       (display "macro(CPMAddPackage)\nendmacro()\n" port)))
                   (call-with-output-file "cmake/recipes/spdlog.cmake"
                     (lambda (port)
                       (display "find_package(spdlog REQUIRED)\n" port)))
                   (call-with-output-file "cmake/recipes/Eigen3.cmake"
                     (lambda (port)
                       (display "find_package(Eigen3 REQUIRED)\n" port)))
                   (call-with-output-file "cmake/recipes/pbar.cmake"
                     (lambda (port)
                       (display "# pbar disabled\n" port)))))
               (add-after 'install 'install-config-header
                 (lambda _
                   (let ((src "../source/src/tight_inclusion/config.hpp"))
                     (when (file-exists? src)
                       (install-file src
                                     (string-append
                                      #$output "/include/tight_inclusion")))))))))
    (inputs (list eigen (@ (gnu packages logging) spdlog)))
    (home-page "https://github.com/sofa-framework/Tight-Inclusion")
    (synopsis "Tight-Inclusion continuous collision detection")
    (description
     "A C++ library for conservative Continuous Collision Detection (CCD)
using tight inclusion.  Required by SOFA's collision detection
intersection component.")
    (license license:expat)))

;;;
;;; Plugin sources — pre-fetched so SOFA's build doesn't need network.
;;; These are passed via -DSOFA_EXTERNAL_DIRECTORIES at configure time.
;;;

(define sofa-plugin-sofapython3
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/Slicer/SofaPython3")
          (commit "64e02c2b0d610f6752442cb177227598b98dd110")))
    (file-name (git-file-name "sofapython3" "64e02c2"))
    (sha256
     (base32 "1244dph1discjjdnnffi97yhch8w7mq0wr44m0shp2f8zn6lj5li"))))

(define sofa-plugin-stlib
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/SofaDefrost/STLIB")
          (commit "d33de40005b6a002ad1b869b972b214a6389fe67")))
    (file-name (git-file-name "sofa-stlib" "d33de40"))
    (sha256
     (base32 "1rn6n58vy620yyzcrp0rr44ad9vhi8lyg3jakpdxlfmyc78pbcgk"))))

(define sofa-plugin-beamadapter
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/sofa-framework/beamadapter")
          (commit "cac4005bd7c9f266c5c775ab88cebbd7c7fa83c7")))
    (file-name (git-file-name "sofa-beamadapter" "cac4005"))
    (sha256
     (base32 "1fsfirihfvgjmr7a1aqchpkgnhpcax3mdplp8ybgwprgy8f72k57"))))

(define sofa-plugin-registration
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/sofa-framework/registration")
          (commit "06ab29453a758b675cced5f3121f49016f84f836")))
    (file-name (git-file-name "sofa-registration" "06ab294"))
    (sha256
     (base32 "148m2p1saj11p10ggp7q0mp6ms9kcrh6wn6g02zninhgirlql1gm"))))

(define sofa-plugin-cosserat
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/SofaDefrost/Cosserat")
          (commit "31aaf6d9af1b1d4feb7412c4b074aff50c92cec8")))
    (file-name (git-file-name "sofa-cosserat" "31aaf6d"))
    (sha256
     (base32 "0vzwf7g1w1na3w209i3pgidl4vr4617fiz0mxzzlrhpybygki6y7"))))

;;;
;;; SOFA Framework v26.06 (Slicer fork) with bundled plugins.
;;;

(define-public sofa-framework
  (package
    (name "sofa-framework")
    (version "26.06.00")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Slicer/sofa")
             ;; Branch slicer-26.06.00-2026-08-24-389df6376.
             (commit "389df6376a893351a9786b9ae25fe7635a1ce61c")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w48ap404dg7bqblnlja946y5p2qakhjgzlfnj7a57b31fvj39h0"))
       (patches
        (search-patches
         "sofa/0001-COMP-Fix-GCC-14-build-with-OpenMP-SIMD-enabled.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:validate-runpath? #f
      #:configure-flags
      #~(list "-DCMAKE_BUILD_TYPE=Release"
              "-DSOFA_BUILD_TESTS=OFF"
              "-DSOFA_ALLOW_FETCH_DEPENDENCIES=OFF"
              "-DBUILD_SHARED_LIBS=ON"
              "-DSOFA_INSTALL_RESOURCES_FILES=OFF"
              ;; Options matching SlicerSOFA's SuperBuild/External_Sofa.cmake
              ;; at 7f086b1 (SOFA 26.06).  26.06 removed the COLLECTION_*
              ;; options and the in-tree Qt GUI (Sofa/GUI/Qt), so those
              ;; flags are gone.
              "-DAPPLICATION_RUNSOFA=OFF"
              "-DAPPLICATION_SCENECHECKING=ON"
              "-DLIBRARY_SOFA_GUI=ON"
              "-DLIBRARY_SOFA_GUI_COMMON=ON"
              "-DMODULE_SOFA_GUI_COMPONENT=ON"
              "-DPLUGIN_SOFA_GUI_BATCH=ON"
              "-DSOFA_WITH_OPENGL=ON"
              "-DSofaSTLIB_ENABLED=ON"
              "-DSofaBeamAdapter_ENABLED=ON"
              "-DRegistration_ENABLED=ON"
              "-DCosserat_ENABLED=ON"
              ;; Performance: enable OpenMP + SIMD.
              "-DSOFA_OPENMP=ON"
              "-DSOFA_ENABLE_SIMD=ON"
              ;; System dependencies:
              (string-append "-DBOOST_ROOT=" #$(this-package-input "boost"))
              "-DBoost_NO_BOOST_CMAKE=FALSE"
              (string-append "-DEIGEN3_INCLUDE_DIR="
                             #$(this-package-input "eigen") "/include/eigen3")
              (string-append "-DGLEW_DIR="
                             #$(this-package-input "glew"))
              (string-append "-DQt5_DIR="
                             #$(this-package-input "qtbase") "/lib/cmake/Qt5")
              (string-append "-DTinyXML2_INCLUDE_DIR="
                             #$(this-package-input "tinyxml2") "/include")
              (string-append "-DTinyXML2_LIBRARY="
                             #$(this-package-input "tinyxml2")
                             "/lib/libtinyxml2.so")
              (string-append "-DZLIB_INCLUDE_DIR="
                             #$(this-package-input "zlib") "/include")
              (string-append "-DZLIB_LIBRARY="
                             #$(this-package-input "zlib") "/lib/libz.so")
              ;; SofaPython3:
              (string-append "-DPYTHON_EXECUTABLE="
                             #$python "/bin/python3")
              (string-append "-DPython3_EXECUTABLE="
                             #$python "/bin/python3")
              (string-append "-DPython_EXECUTABLE="
                             #$python "/bin/python3")
              (string-append "-DPYTHON_LIBRARIES="
                             #$python
                             "/lib/libpython"
                             #$(version-major+minor (package-version python)) ".so")
              (string-append "-DPYTHON_INCLUDE_DIRS="
                             #$python
                             "/include/python"
                             #$(version-major+minor (package-version python)))
              (string-append "-Dpybind11_DIR="
                             #$(this-package-input "pybind11")
                             "/share/cmake/pybind11")
              ;; Plugin sources — SOFA builds them inline via
              ;; add_subdirectory.  Store paths are read-only but
              ;; SOFA only reads from them at configure+build time.
              (string-append
               "-DSOFA_EXTERNAL_DIRECTORIES="
               #$sofa-plugin-sofapython3 ";"
               #$sofa-plugin-stlib ";"
               #$sofa-plugin-beamadapter ";"
               #$sofa-plugin-registration ";"
               #$sofa-plugin-cosserat))
      #:phases
      #~(modify-phases %standard-phases
          ;; SofaPython3 creates ~/.local/lib/pythonX.Y/site-packages
          ;; at configure time.  The build sandbox has no writable HOME.
          (add-before 'configure 'set-home
            (lambda _
              (setenv "HOME" (getcwd))))
          ;; SOFA installs Python modules into non-standard paths
          ;; under plugins/<name>/lib/python3/site-packages/.  Create
          ;; symlinks into lib/pythonX.Y/site-packages/ so Guix's
          ;; profile hooks and Slicer's PYTHONPATH pick them up.
          (add-after 'install 'symlink-python-packages
            (lambda _
              (use-modules (ice-9 ftw))
              (let ((out #$output))
                (let ((pydir (string-append out "/lib/python"
                                    #$(version-major+minor (package-version python))
                                    "/site-packages"))
                      (pdir  (string-append out "/plugins")))
                  (mkdir-p pydir)
                  (when (file-exists? pdir)
                    (for-each
                     (lambda (d)
                       (let ((sp (string-append pdir "/" d "/lib/python3/site-packages")))
                         (when (file-exists? sp)
                           (for-each
                            (lambda (e)
                              (let ((s (string-append sp "/" e))
                                    (t (string-append pydir "/" e)))
                                (unless (file-exists? t) (symlink s t))))
                            (filter (lambda (f) (not (string-prefix? "." f)))
                                    (scandir sp))))))
                     (filter (lambda (f) (not (string-prefix? "." f)))
                             (scandir pdir)))))))))))
    (inputs
     (list eigen boost glew tinyxml2 zlib
           nlohmann-json
           googletest
           tight-inclusion
           (@ (gnu packages logging) spdlog)  ; transitive dep of tight-inclusion
           cxxopts                             ; Sofa.GUI.Common
           cimg                                ; CImgPlugin
           metis                               ; SofaMatrix
           qtbase-5
           pybind11
           python))
    (native-search-paths
     (list (search-path-specification
            (variable "SOFA_ROOT")
            (files '(""))
            (separator #f))
           (search-path-specification
            (variable "PYTHONPATH")
            (files (list (string-append
                          "lib/python"
                          (version-major+minor (package-version python))
                          "/site-packages"))))))
    (home-page "https://www.sofa-framework.org/")
    (synopsis "SOFA real-time physics simulation framework (Slicer fork, v26.06)")
    (description
     "SOFA (Simulation Open Framework Architecture) is an open-source C++
library for real-time simulation of soft-tissue deformation, contact
mechanics, and multiphysics coupling.  This Guix package builds the
Slicer fork at v26.06 with the following plugins compiled inline:
SofaPython3, SofaSTLIB, BeamAdapter, Registration, and Cosserat.
It is the simulation backend for the SlicerSOFA 3D Slicer extension.")
    ;; SOFA itself is LGPL-2.1+; the components built inline add:
    ;;   GPL-2+    Sofa/GUI/Batch
    ;;   LGPL-3    STLIB, Cosserat
    ;;   CeCILL-C  bundled CImg
    ;;   MIT       bundled third-party headers
    (license (list license:lgpl2.1+ license:gpl2+ license:lgpl3
                   license:cecill-c license:expat))))


