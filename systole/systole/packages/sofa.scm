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
;;; the Slicer fork of SOFA v25.12 with bundled plugins (SofaPython3,
;;; SofaSTLIB, BeamAdapter, Registration, Cosserat) built inline via
;;; SOFA_EXTERNAL_DIRECTORIES, matching the SlicerSOFA preview branch.
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
  #:use-module (gnu packages xml))           ; tinyxml2

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
             (commit "v1.0.6-export-target")))
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
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out"))
                         (src "../source/src/tight_inclusion/config.hpp"))
                     (when (file-exists? src)
                       (install-file src
                                     (string-append out "/include/tight_inclusion")))))))))
    (inputs (list eigen (@ (gnu packages logging) spdlog)))
    (home-page "https://github.com/sofa-framework/Tight-Inclusion")
    (synopsis "Tight-Inclusion continuous collision detection")
    (description
     "A C++ library for conservative Continuous Collision Detection (CCD)
using tight inclusion.  Required by SOFA's collision detection
intersection component.")
    (license license:lgpl2.1+)))

;;;
;;; Plugin sources — pre-fetched so SOFA's build doesn't need network.
;;; These are passed via -DSOFA_EXTERNAL_DIRECTORIES at configure time.
;;;

(define sofa-plugin-sofapython3
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/Slicer/SofaPython3")
          (commit "d531197caff3a93080c8b66704f3185c2164043c")))
    (file-name "SofaPython3-d531197.tar.gz")
    (sha256
     (base32 "038mx7q2ywpwgpmbhgy1ydw34c32bc49hydbi9b2xr1zilv6bpgj"))))

(define sofa-plugin-stlib
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/SofaDefrost/STLIB")
          (commit "da062381847b26458390937c75c23968ea0a9c6a")))
    (file-name "STLIB-da06238.tar.gz")
    (sha256
     (base32 "0r0y3lmgdqsbrld7l8l280x56906n3qapga735ia4pdlqn7wwjzk"))))

(define sofa-plugin-beamadapter
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/sofa-framework/beamadapter")
          (commit "5d02b8322cc82fc8a04085862affb99b008ef175")))
    (file-name "BeamAdapter-5d02b83.tar.gz")
    (sha256
     (base32 "1rzgk9845hkgv2p5jfqmwcb8q1n95w7kgdk32rc8s3dz9ls93l9w"))))

(define sofa-plugin-registration
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/sofa-framework/registration")
          (commit "091ebecafafc3fc3ba1e493e855b09ec4918c857")))
    (file-name "Registration-091ebec.tar.gz")
    (sha256
     (base32 "0vfiiy48b0w2v967nhhys2x2vhq13fnk2vri9flbmi4vqhsmjkzr"))))

(define sofa-plugin-cosserat
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/SofaDefrost/Cosserat")
          (commit "f6a4f35075c5fc0364c4d96dd130e22f2c5bb1e3")))
    (file-name "Cosserat-f6a4f35.tar.gz")
    (sha256
     (base32 "01arfhq78hidnyizw576r4vxh3l8b492wc9wc81p6v0wpwfa7j5d"))))

;;;
;;; SOFA Framework v25.12 (Slicer fork) with bundled plugins.
;;;

(define-public sofa-framework
  (package
    (name "sofa-framework")
    (version "25.12.00")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Slicer/sofa")
             (commit "9f48bc12fa974fb3d8d4d5b2ec8ed154181d8c11")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qwllykc72nyjz5f8rrccd2vyxxpcpkaz17kbi2bknhd4689r939"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DCMAKE_BUILD_TYPE=Release"
              "-DSOFA_BUILD_TESTS=OFF"
              "-DSOFA_ALLOW_FETCH_DEPENDENCIES=OFF"
              "-DBUILD_SHARED_LIBS=ON"
              "-DSOFA_INSTALL_RESOURCES_FILES=OFF"
              ;; Collections/plugins matching SlicerSOFA preview SuperBuild:
              "-DAPPLICATION_RUNSOFA=OFF"
              "-DAPPLICATION_SCENECHECKING=ON"
              "-DCOLLECTION_SOFACONSTRAINT=ON"
              "-DCOLLECTION_SOFAGENERAL=ON"
              "-DCOLLECTION_SOFAGRAPHCOMPONENT=ON"
              "-DCOLLECTION_SOFAGUI=ON"
              "-DCOLLECTION_SOFAGUICOMMON=ON"
              "-DCOLLECTION_SOFAGUIQT=ON"
              "-DCOLLECTION_SOFAMISCCOLLISION=ON"
              "-DCOLLECTION_SOFAUSERINTERACTION=ON"
              "-DSOFA_GUI_QT_ENABLE_QDOCBROWSER=OFF"
              "-DLIBRARY_SOFA_GUI=ON"
              "-DLIBRARY_SOFA_GUI_COMMON=ON"
              "-DMODULE_SOFA_GUI_COMPONENT=ON"
              "-DPLUGIN_SOFA_GUI_BATCH=ON"
              "-DPLUGIN_SOFA_GUI_QT=ON"
              "-DSOFA_WITH_OPENGL=ON"
              "-DSofaSTLIB_ENABLED=ON"
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
                             #$(this-package-input "python") "/bin/python3")
              (string-append "-DPython3_EXECUTABLE="
                             #$(this-package-input "python") "/bin/python3")
              (string-append "-DPython_EXECUTABLE="
                             #$(this-package-input "python") "/bin/python3")
              (string-append "-DPYTHON_LIBRARIES="
                             #$(this-package-input "python")
                             "/lib/libpython3.11.so")
              (string-append "-DPYTHON_INCLUDE_DIRS="
                             #$(this-package-input "python")
                             "/include/python3.11")
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
          ;; SofaPython3 creates ~/.local/lib/python3.11/site-packages
          ;; at configure time.  The build sandbox has no writable HOME.
          (add-before 'configure 'set-home
            (lambda _
              (setenv "HOME" (getcwd)))))))
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
    (home-page "https://www.sofa-framework.org/")
    (synopsis "SOFA real-time physics simulation framework (Slicer fork, v25.12)")
    (description
     "SOFA (Simulation Open Framework Architecture) is an open-source C++
library for real-time simulation of soft-tissue deformation, contact
mechanics, and multiphysics coupling.  This Guix package builds the
Slicer fork at v25.12 with the following plugins compiled inline:
SofaPython3, SofaSTLIB, BeamAdapter, Registration, and Cosserat.
It is the simulation backend for the SlicerSOFA 3D Slicer extension.")
    (license license:lgpl2.1+)))


