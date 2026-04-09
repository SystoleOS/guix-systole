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
;;; ROS 2 Jazzy Jalisco distribution data and packages for guix-systole.
;;;
;;; This module instantiates (systole packages ros2) factories with
;;; jazzy-specific version pins.  It is currently a skeleton — upcoming
;;; commits will populate it tier by tier (ament, rosidl, rmw, messages,
;;; rcl, rclcpp/rclpy) culminating in a usable `ros-jazzy' meta-package.
;;;
;;; Code:

(define-module (systole packages ros2 jazzy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages algebra)      ; eigen
  #:use-module (gnu packages check)        ; python-pytest
  #:use-module (gnu packages cpp)          ; pybind11
  #:use-module (gnu packages engineering)  ; orocos-kinematics-dynamics
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science) ; python-numpy
  #:use-module (gnu packages python-xyz)   ; python-empy, python-pyyaml, python-lark
  #:use-module (gnu packages serialization) ; libyaml
  #:use-module (gnu packages xml)          ; tinyxml2
  #:use-module (systole packages ros2)
  #:use-module (systole packages ros2-helpers) ; urdfdom, urdfdom-headers, console-bridge
  #:use-module (srfi srfi-1)               ; delete-duplicates, append-map
  #:use-module (ice-9 match)               ; match-lambda
  #:export (jazzy-distro))

;;;
;;; Distribution record.
;;;
;;; REPOS-SNAPSHOT is the commit SHA of ros2/ros2 on branch `jazzy' from
;;; which per-package commits were harvested.  Bump it (and the per-package
;;; pins below) as a single lockfile update.

(define %ros2-jazzy-repos-snapshot
  ;; TODO: pin to a concrete SHA from https://github.com/ros2/ros2 branch
  ;; `jazzy' once the first real package is added.
  "unpinned")

(define jazzy-distro
  (make-ros-distro
   #:name "jazzy"
   #:suffix "jazzy"
   #:python python
   #:repos-snapshot %ros2-jazzy-repos-snapshot
   #:patch-subdir "ros2/jazzy"))

;;;
;;; Tier 1 — ament build tooling (first two packages).
;;;

(define-public ros-ament-package-jazzy
  (make-ros2-ament-python-package
   #:distro jazzy-distro
   #:ros-name "ament_package"
   #:version "0.16.4"
   #:repo "https://github.com/ament/ament_package"
   #:commit "ee3c5eda4fbc4df2c7a135d0ac1385e4a53fa0ee"
   #:hash (base32 "0nay6winngy4nrv2d708scyimk1jkxcql02awdvnxs9bqifbmx8c")
   #:propagated-inputs (list python-catkin-pkg)
   #:home-page "https://github.com/ament/ament_package"
   #:synopsis "ROS 2 ament package manifest parser"
   #:description
   "@code{ament_package} provides the Python parser for ROS 2 package
manifest files (@file{package.xml}, format 2 and 3) and the helpers used
by the ament build system to discover sibling packages via the AMENT
resource index.  It is the first dependency of every other ament-built
ROS 2 package."))

;;;
;;; Shared upstream coordinates for all packages built from ament/ament_cmake.
;;;

(define %ament-cmake-repo "https://github.com/ament/ament_cmake")
(define %ament-cmake-commit "5741cff5b9f83253bf3521bd8f44108fde3504ad")
(define %ament-cmake-hash
  (base32 "0sysj073m00i2ji18kr3lg0jvzcmqljyrs640gqg36insql8zvk4"))
(define %ament-cmake-version "2.5.6")

(define* (ament-cmake-subpkg ros-name #:key
                             (propagated-inputs '())
                             synopsis description)
  "Helper that builds a sub-package of the ament/ament_cmake monorepo for
the jazzy distribution.  ROS-NAME doubles as the upstream package name and
the source sub-directory."
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name ros-name
   #:version %ament-cmake-version
   #:repo %ament-cmake-repo
   #:commit %ament-cmake-commit
   #:hash %ament-cmake-hash
   #:module-subdir ros-name
   #:propagated-inputs propagated-inputs
   #:home-page "https://github.com/ament/ament_cmake"
   #:synopsis synopsis
   #:description description))

(define-public ros-ament-cmake-core-jazzy
  (ament-cmake-subpkg
   "ament_cmake_core"
   #:propagated-inputs (list ros-ament-package-jazzy)
   #:synopsis "Core CMake helpers for ROS 2 ament builds"
   #:description
   "@code{ament_cmake_core} provides the foundational CMake macros used
by every other ament_cmake-based ROS 2 package: package registration in
the AMENT resource index, environment-hook installation, and helpers for
declaring exported targets, dependencies, and include directories."))

;;;
;;; Tier 1A — leaves of ament_cmake (depend only on ament_cmake_core).
;;;

(define-public ros-ament-cmake-export-definitions-jazzy
  (ament-cmake-subpkg
   "ament_cmake_export_definitions"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy)
   #:synopsis "ament_cmake helper for exporting compile definitions"
   #:description
   "Provides @code{ament_export_definitions} for ament_cmake packages."))

(define-public ros-ament-cmake-export-dependencies-jazzy
  (ament-cmake-subpkg
   "ament_cmake_export_dependencies"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy)
   #:synopsis "ament_cmake helper for exporting CMake dependencies"
   #:description
   "Provides @code{ament_export_dependencies} so downstream packages
re-find a package's transitive @code{find_package} dependencies."))

(define-public ros-ament-cmake-export-include-directories-jazzy
  (ament-cmake-subpkg
   "ament_cmake_export_include_directories"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy)
   #:synopsis "ament_cmake helper for exporting include directories"
   #:description
   "Provides @code{ament_export_include_directories}."))

(define-public ros-ament-cmake-export-interfaces-jazzy
  (ament-cmake-subpkg
   "ament_cmake_export_interfaces"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy)
   #:synopsis "ament_cmake helper for exporting interface targets"
   #:description
   "Provides @code{ament_export_interfaces}."))

(define-public ros-ament-cmake-export-libraries-jazzy
  (ament-cmake-subpkg
   "ament_cmake_export_libraries"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy)
   #:synopsis "ament_cmake helper for exporting libraries"
   #:description
   "Provides @code{ament_export_libraries}."))

(define-public ros-ament-cmake-export-link-flags-jazzy
  (ament-cmake-subpkg
   "ament_cmake_export_link_flags"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy)
   #:synopsis "ament_cmake helper for exporting linker flags"
   #:description
   "Provides @code{ament_export_link_flags}."))

(define-public ros-ament-cmake-export-targets-jazzy
  (ament-cmake-subpkg
   "ament_cmake_export_targets"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy)
   #:synopsis "ament_cmake helper for exporting CMake targets"
   #:description
   "Provides @code{ament_export_targets} for installable target sets."))

(define-public ros-ament-cmake-include-directories-jazzy
  (ament-cmake-subpkg
   "ament_cmake_include_directories"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy)
   #:synopsis "ament_cmake helper for include-directory ordering"
   #:description
   "Provides @code{ament_include_directories_order}."))

(define-public ros-ament-cmake-libraries-jazzy
  (ament-cmake-subpkg
   "ament_cmake_libraries"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy)
   #:synopsis "ament_cmake helper for library list manipulation"
   #:description
   "Provides @code{ament_libraries_deduplicate} and friends."))

(define-public ros-ament-cmake-target-dependencies-jazzy
  (ament-cmake-subpkg
   "ament_cmake_target_dependencies"
   ;; -extras.cmake transitively find_package()s these.
   #:propagated-inputs (list ros-ament-cmake-core-jazzy
                             ros-ament-cmake-include-directories-jazzy
                             ros-ament-cmake-libraries-jazzy)
   #:synopsis "ament_cmake helper for declaring target dependencies"
   #:description
   "Provides @code{ament_target_dependencies}."))

(define-public ros-ament-cmake-version-jazzy
  (ament-cmake-subpkg
   "ament_cmake_version"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy)
   #:synopsis "ament_cmake helper for accessing the package version"
   #:description
   "Provides CMake variables exposing the current package's version."))

(define-public ros-ament-cmake-python-jazzy
  (ament-cmake-subpkg
   "ament_cmake_python"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy)
   #:synopsis "ament_cmake helpers for installing Python code"
   #:description
   "Provides @code{ament_python_install_package} and related macros for
installing Python modules from ament_cmake-based packages."))

(define-public ros-ament-cmake-gtest-jazzy
  (ament-cmake-subpkg
   "ament_cmake_gtest"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy)
   #:synopsis "ament_cmake helper for declaring GoogleTest tests"
   #:description
   "Provides @code{ament_add_gtest} and related macros."))

(define-public ros-ament-cmake-gmock-jazzy
  (ament-cmake-subpkg
   "ament_cmake_gmock"
   #:propagated-inputs (list ros-ament-cmake-gtest-jazzy)
   #:synopsis "ament_cmake helper for declaring GoogleMock tests"
   #:description
   "Provides @code{ament_add_gmock} and related macros."))

;;;
;;; Tier 1B — small set that builds on Tier 1A.
;;;

(define-public ros-ament-cmake-test-jazzy
  (ament-cmake-subpkg
   "ament_cmake_test"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy
                             ros-ament-cmake-python-jazzy)
   #:synopsis "ament_cmake helper for declaring tests"
   #:description
   "Provides @code{ament_add_test} and the underlying test-runner glue."))

(define-public ros-ament-cmake-gen-version-h-jazzy
  (ament-cmake-subpkg
   "ament_cmake_gen_version_h"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy
                             ros-ament-package-jazzy)
   #:synopsis "ament_cmake helper to generate version.h headers"
   #:description
   "Generates @code{<package>/version.h} headers from package.xml at
configure time."))

(define-public ros-ament-cmake-pytest-jazzy
  (ament-cmake-subpkg
   "ament_cmake_pytest"
   ;; pytest is propagated because consumers (e.g. rcutils tests) call
   ;; find_package(ament_cmake_pytest) which transitively requires it.
   #:propagated-inputs (list ros-ament-cmake-core-jazzy
                             ros-ament-cmake-test-jazzy
                             python-pytest)
   #:synopsis "ament_cmake helper for declaring pytest tests"
   #:description
   "Provides @code{ament_add_pytest_test} for ament_cmake-based ROS 2
packages."))

(define-public ros-ament-cmake-vendor-package-jazzy
  (ament-cmake-subpkg
   "ament_cmake_vendor_package"
   #:propagated-inputs (list ros-ament-cmake-core-jazzy
                             ros-ament-cmake-export-dependencies-jazzy)
   #:synopsis "ament_cmake helper for vendoring upstream CMake packages"
   #:description
   "Provides @code{ament_vendor} for ROS 2 vendor packages that wrap
upstream CMake projects."))

;;;
;;; Tier 1C — ament_cmake meta-package.
;;;

(define-public ros-ament-cmake-jazzy
  (ament-cmake-subpkg
   "ament_cmake"
   #:propagated-inputs
   (list ros-ament-cmake-core-jazzy
         ros-ament-cmake-export-definitions-jazzy
         ros-ament-cmake-export-dependencies-jazzy
         ros-ament-cmake-export-include-directories-jazzy
         ros-ament-cmake-export-interfaces-jazzy
         ros-ament-cmake-export-libraries-jazzy
         ros-ament-cmake-export-link-flags-jazzy
         ros-ament-cmake-export-targets-jazzy
         ros-ament-cmake-gen-version-h-jazzy
         ros-ament-cmake-libraries-jazzy
         ros-ament-cmake-python-jazzy
         ros-ament-cmake-target-dependencies-jazzy
         ros-ament-cmake-test-jazzy
         ros-ament-cmake-version-jazzy)
   #:synopsis "Entry-point package for the ament_cmake build system"
   #:description
   "@code{ament_cmake} is the meta-package every ament_cmake-based ROS 2
package depends on.  It pulls in the full set of ament_cmake_* helpers."))

;;;
;;; ament_index — package discovery at runtime.
;;;

(define %ament-index-repo "https://github.com/ament/ament_index")
(define %ament-index-commit "e4cedf60ad57404e597fefcc0209169c11976fb1")
(define %ament-index-hash
  (base32 "08ngwcw9s2zdwsyzhyn68wkhjax1jlwhcllfxisr48nl8qwmyk4v"))
(define %ament-index-version "1.8.3")

(define-public ros-ament-index-python-jazzy
  (make-ros2-ament-python-package
   #:distro jazzy-distro
   #:ros-name "ament_index_python"
   #:version %ament-index-version
   #:repo %ament-index-repo
   #:commit %ament-index-commit
   #:hash %ament-index-hash
   #:module-subdir "ament_index_python"
   #:home-page "https://github.com/ament/ament_index"
   #:synopsis "Python API for the ROS 2 ament resource index"
   #:description
   "@code{ament_index_python} reads the AMENT resource index installed by
ament_cmake-based ROS 2 packages, exposing it through a Python API used
by tools like @code{ros2cli} and @code{rclpy}."))

(define-public ros-ament-index-cpp-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "ament_index_cpp"
   #:version %ament-index-version
   #:repo %ament-index-repo
   #:commit %ament-index-commit
   #:hash %ament-index-hash
   #:module-subdir "ament_index_cpp"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-gen-version-h-jazzy)
   #:home-page "https://github.com/ament/ament_index"
   #:synopsis "C++ API for the ROS 2 ament resource index"
   #:description
   "@code{ament_index_cpp} reads the AMENT resource index installed by
ament_cmake-based ROS 2 packages, exposing it through a C++ API used by
@code{rcl} and other client libraries."))

;;;
;;; ament_cmake_ros — adds ROS-specific test fixtures on top of ament_cmake.
;;;

(define %ament-cmake-ros-repo "https://github.com/ros2/ament_cmake_ros")
(define %ament-cmake-ros-commit "4656ed1c514fa12503a0c01d33fd13d406a5fb74")
(define %ament-cmake-ros-hash
  (base32 "1q64b972skw70xbsn398jjjrd27hazfs4gzhs9whzal65ph12b8i"))
(define %ament-cmake-ros-version "0.12.0")

(define-public ros-domain-coordinator-jazzy
  (make-ros2-ament-python-package
   #:distro jazzy-distro
   #:ros-name "domain_coordinator"
   #:version %ament-cmake-ros-version
   #:repo %ament-cmake-ros-repo
   #:commit %ament-cmake-ros-commit
   #:hash %ament-cmake-ros-hash
   #:module-subdir "domain_coordinator"
   #:home-page "https://github.com/ros2/ament_cmake_ros"
   #:synopsis "ROS 2 helper for coordinating DDS domain IDs in tests"
   #:description
   "@code{domain_coordinator} is a small Python helper used by ROS 2 test
suites to allocate non-overlapping DDS domain IDs across parallel test
runs."))

(define-public ros-ament-cmake-ros-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "ament_cmake_ros"
   #:version %ament-cmake-ros-version
   #:repo %ament-cmake-ros-repo
   #:commit %ament-cmake-ros-commit
   #:hash %ament-cmake-ros-hash
   #:module-subdir "ament_cmake_ros"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-gtest-jazzy
                             ros-ament-cmake-gmock-jazzy
                             ros-ament-cmake-pytest-jazzy
                             ros-domain-coordinator-jazzy)
   #:home-page "https://github.com/ros2/ament_cmake_ros"
   #:synopsis "ament_cmake helpers for ROS 2-specific test fixtures"
   #:description
   "@code{ament_cmake_ros} layers ROS 2-specific helpers (notably DDS
domain coordination via @code{domain_coordinator}) on top of the generic
ament_cmake test helpers."))

;;;
;;; Tier 3a — core C/C++ libraries.
;;;

(define-public ros-rcutils-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rcutils"
   #:version "6.7.5"
   #:repo "https://github.com/ros2/rcutils"
   #:commit "2b6bdcc007d4647b57731cbe7a1ff9477569a679"
   #:hash (base32 "0rk6633kinp7xj10ajr74486mw06skaf28ssq1cvjmlqhj6my7c6")
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy)
   #:extra-native-inputs (list python-empy)
   #:home-page "https://github.com/ros2/rcutils"
   #:synopsis "ROS 2 C-language utility library"
   #:description
   "@code{rcutils} provides logging, allocator abstractions, string
manipulation, time helpers, and other low-level C utilities used by the
ROS 2 client support library (@code{rcl}) and the rosidl runtime."))

(define-public ros-rcpputils-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rcpputils"
   #:version "2.11.3"
   #:repo "https://github.com/ros2/rcpputils"
   #:commit "d01fd8196881bd35e651fe1b0ea6d5c2585e40c7"
   #:hash (base32 "1mycffgacx76y98q2wi3ikm60f58r9g31krcxz7l5wpa0iy9x3i3")
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-ament-cmake-gen-version-h-jazzy
                             ros-rcutils-jazzy)
   #:home-page "https://github.com/ros2/rcpputils"
   #:synopsis "ROS 2 C++ utility library"
   #:description
   "@code{rcpputils} is the C++ companion to @code{rcutils}, providing
filesystem helpers, thread-safety primitives, version macros, and other
C++ utilities used throughout the ROS 2 stack."))

;;;
;;; rosidl — interface generation framework.  All sub-packages share a
;;; common source archive (ros2/rosidl).
;;;

(define %rosidl-repo "https://github.com/ros2/rosidl")
(define %rosidl-commit "59c1cefa98edfb764fbeb09704d7f5e48a7af0c1")
(define %rosidl-hash
  (base32 "1jma93py5yj66kc3s9a9x7x8g4jgzqjqpnn9a25q00f2lnx9d4kd"))
(define %rosidl-version "4.6.7")

(define* (rosidl-cmake-subpkg ros-name #:key
                              (propagated-inputs '())
                              (extra-inputs '())
                              (extra-native-inputs '())
                              synopsis description)
  "Helper for cmake-built sub-packages of ros2/rosidl (jazzy)."
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name ros-name
   #:version %rosidl-version
   #:repo %rosidl-repo
   #:commit %rosidl-commit
   #:hash %rosidl-hash
   #:module-subdir ros-name
   #:propagated-inputs propagated-inputs
   #:extra-inputs extra-inputs
   #:extra-native-inputs extra-native-inputs
   #:home-page "https://github.com/ros2/rosidl"
   #:synopsis synopsis
   #:description description))

(define* (rosidl-python-subpkg ros-name #:key
                               (propagated-inputs '())
                               (extra-native-inputs '())
                               synopsis description)
  "Helper for pure-python sub-packages of ros2/rosidl (jazzy)."
  (make-ros2-ament-python-package
   #:distro jazzy-distro
   #:ros-name ros-name
   #:version %rosidl-version
   #:repo %rosidl-repo
   #:commit %rosidl-commit
   #:hash %rosidl-hash
   #:module-subdir ros-name
   #:propagated-inputs propagated-inputs
   #:extra-native-inputs extra-native-inputs
   #:home-page "https://github.com/ros2/rosidl"
   #:synopsis synopsis
   #:description description))

;;; rosidl Tier A: small C/C++ runtime leaves.

(define-public ros-rosidl-typesupport-interface-jazzy
  (rosidl-cmake-subpkg
   "rosidl_typesupport_interface"
   #:propagated-inputs (list ros-ament-cmake-jazzy)
   #:synopsis "Interface header for the rosidl typesupport ABI"
   #:description
   "Header-only interface library defining the C ABI shared between
generated rosidl typesupport implementations and their consumers."))

(define-public ros-rosidl-runtime-c-jazzy
  (rosidl-cmake-subpkg
   "rosidl_runtime_c"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-rcutils-jazzy
                             ros-rosidl-typesupport-interface-jazzy)
   #:synopsis "ROS 2 rosidl C runtime support library"
   #:description
   "Runtime support library for ROS 2 message types in C: type
descriptions, primitives sequence functions, message and service type
support helpers."))

(define-public ros-rosidl-runtime-cpp-jazzy
  (rosidl-cmake-subpkg
   "rosidl_runtime_cpp"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-rosidl-runtime-c-jazzy)
   #:synopsis "ROS 2 rosidl C++ runtime support library"
   #:description
   "Header-only C++ runtime support for ROS 2 generated message types:
traits, bounded sequences and strings, message initializers."))

;;; rosidl Tier B: Python tooling.

(define-public ros-rosidl-cli-jazzy
  (rosidl-python-subpkg
   "rosidl_cli"
   #:propagated-inputs (list python-argcomplete
                             python-importlib-metadata
                             python-pyyaml)
   #:synopsis "Command-line entry points for rosidl tooling"
   #:description
   "@code{rosidl_cli} is the @code{rosidl} command-line dispatcher used
to invoke language-specific generators (C, C++, Python, ...) and the
@code{translate} sub-command for converting between IDL formats."))

;; rosidl_adapter and rosidl_parser ship a CMakeLists.txt and install
;; their Python code via ament_cmake_python — they are not pure-Python
;; setuptools packages.
(define-public ros-rosidl-adapter-jazzy
  (rosidl-cmake-subpkg
   "rosidl_adapter"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-python-jazzy
                             python-empy
                             ros-rosidl-cli-jazzy)
   #:synopsis "Translate ROS-style .msg/.srv/.action files into IDL"
   #:description
   "@code{rosidl_adapter} converts the ROS @file{.msg}, @file{.srv}, and
@file{.action} interface definition formats into the OMG IDL files
consumed by the rest of the rosidl pipeline."))

(define-public ros-rosidl-parser-jazzy
  (rosidl-cmake-subpkg
   "rosidl_parser"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-python-jazzy
                             python-lark
                             ros-rosidl-adapter-jazzy)
   #:synopsis "Parser for OMG IDL used by ROS 2 rosidl"
   #:description
   "@code{rosidl_parser} parses OMG IDL files into a structured
representation that the rosidl generators consume to emit
language-specific bindings."))

(define-public ros-rosidl-pycommon-jazzy
  (rosidl-python-subpkg
   "rosidl_pycommon"
   #:propagated-inputs (list ros-rosidl-parser-jazzy)
   #:synopsis "Common Python utilities shared by rosidl generators"
   #:description
   "Helper module providing path resolution, file generation, and CMake
import-file writing utilities used by the rosidl C/C++/Python generators."))

(define-public ros-rosidl-cmake-jazzy
  (rosidl-cmake-subpkg
   "rosidl_cmake"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-python-jazzy
                             ros-rosidl-pycommon-jazzy)
   #:synopsis "CMake macros and templates for the rosidl pipeline"
   #:description
   "@code{rosidl_cmake} provides the CMake macros that drive the rosidl
generation pipeline from interface (.msg/.srv/.action) source files,
including @code{rosidl_generate_interfaces} and the templates used by
language-specific generator packages."))

;;; rosidl Tier C: code generators and introspection typesupport.

(define-public ros-rosidl-generator-type-description-jazzy
  (rosidl-cmake-subpkg
   "rosidl_generator_type_description"
   #:propagated-inputs (list ros-ament-cmake-python-jazzy
                             ros-ament-cmake-ros-jazzy
                             ros-rosidl-cli-jazzy
                             ros-rosidl-parser-jazzy
                             ros-rosidl-pycommon-jazzy)
   #:synopsis "Generate type description hashes for ROS 2 interfaces"
   #:description
   "Computes type-hash and type-description metadata for ROS 2 interface
packages.  Used by other generators (C, C++) to embed stable type IDs."))

(define-public ros-rosidl-generator-c-jazzy
  (rosidl-cmake-subpkg
   "rosidl_generator_c"
   #:propagated-inputs (list ros-ament-cmake-python-jazzy
                             ros-ament-cmake-ros-jazzy
                             ros-rcutils-jazzy
                             ros-rosidl-cmake-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-rosidl-typesupport-interface-jazzy
                             ros-rosidl-generator-type-description-jazzy)
   #:synopsis "C code generator for ROS 2 interface packages"
   #:description
   "Generates C structs, message-init/destroy functions, and the C
typesupport glue for ROS 2 messages, services, and actions."))

(define-public ros-rosidl-generator-cpp-jazzy
  (rosidl-cmake-subpkg
   "rosidl_generator_cpp"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-python-jazzy
                             ros-rosidl-cmake-jazzy
                             ros-rosidl-runtime-cpp-jazzy
                             ros-rosidl-generator-type-description-jazzy)
   #:synopsis "C++ code generator for ROS 2 interface packages"
   #:description
   "Generates C++ message classes for ROS 2 messages, services, and
actions, on top of the C generator's output."))

(define-public ros-rosidl-typesupport-introspection-c-jazzy
  (rosidl-cmake-subpkg
   "rosidl_typesupport_introspection_c"
   #:propagated-inputs (list ros-ament-cmake-python-jazzy
                             ros-ament-cmake-ros-jazzy
                             ros-rosidl-cmake-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-rosidl-typesupport-interface-jazzy
                             ros-rosidl-generator-c-jazzy)
   #:synopsis "Runtime introspection typesupport (C) for ROS 2 messages"
   #:description
   "Generates C type-introspection metadata used by middlewares such as
@code{rmw_cyclonedds_cpp} to (de)serialize ROS 2 messages without
language-binding-specific typesupport."))

(define-public ros-rosidl-typesupport-introspection-cpp-jazzy
  (rosidl-cmake-subpkg
   "rosidl_typesupport_introspection_cpp"
   #:propagated-inputs (list ros-ament-cmake-python-jazzy
                             ros-ament-cmake-ros-jazzy
                             ros-rosidl-cmake-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-rosidl-runtime-cpp-jazzy
                             ros-rosidl-typesupport-interface-jazzy
                             ros-rosidl-generator-cpp-jazzy
                             ros-rosidl-typesupport-introspection-c-jazzy)
   #:synopsis "Runtime introspection typesupport (C++) for ROS 2 messages"
   #:description
   "C++ counterpart to @code{rosidl_typesupport_introspection_c}."))

(define-public ros-rosidl-dynamic-typesupport-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rosidl_dynamic_typesupport"
   #:version "0.1.2"
   #:repo "https://github.com/ros2/rosidl_dynamic_typesupport"
   #:commit "cb8c54d12c678daa70410bb0626ed8626f561e45"
   #:hash (base32 "0m8bsr9vbjkxiik4ylmyknmph4vx0yxda5wnzj24kxiy1yyy7a50")
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-rcutils-jazzy
                             ros-rosidl-runtime-c-jazzy)
   #:home-page "https://github.com/ros2/rosidl_dynamic_typesupport"
   #:synopsis "Dynamic (runtime-built) typesupport for ROS 2 messages"
   #:description
   "Provides the C ABI used by ROS 2 middlewares to handle messages whose
type description is only known at runtime (deferred / dynamic types)."))

;;;
;;; rmw — middleware abstraction layer.
;;;

(define-public ros-rmw-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rmw"
   #:version "7.3.3"
   #:repo "https://github.com/ros2/rmw"
   #:commit "9f42ff0f18c8c4fd2a84a7c27d11ec8371e968ea"
   #:hash (base32 "0pgv9zzgckaffpf72wbpq2mnbv1lap7cla10sswn40f1kcl6a9l0")
   #:module-subdir "rmw"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-ament-cmake-version-jazzy
                             ros-rcutils-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-rosidl-dynamic-typesupport-jazzy)
   #:home-page "https://github.com/ros2/rmw"
   #:synopsis "ROS 2 middleware abstraction interface"
   #:description
   "@code{rmw} defines the C ABI shared between the ROS 2 client support
library (@code{rcl}) and the underlying DDS-based middleware
implementations.  Concrete middleware bindings such as
@code{rmw_cyclonedds_cpp} implement this interface."))

;;;
;;; rosidl_typesupport — generic typesupport implementations.
;;;

(define %rosidl-typesupport-repo "https://github.com/ros2/rosidl_typesupport")
(define %rosidl-typesupport-commit
  "b43c9455e6120c71c70c8c774471628728919a7b")
(define %rosidl-typesupport-hash
  (base32 "1p8lczzri6zqy1limk42vl1b921b03yy3kgdxg3vfpn7xasx7xsp"))
(define %rosidl-typesupport-version "3.2.2")

(define-public ros-rosidl-typesupport-c-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rosidl_typesupport_c"
   #:version %rosidl-typesupport-version
   #:repo %rosidl-typesupport-repo
   #:commit %rosidl-typesupport-commit
   #:hash %rosidl-typesupport-hash
   #:module-subdir "rosidl_typesupport_c"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-rcutils-jazzy
                             ros-rcpputils-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-rosidl-typesupport-interface-jazzy
                             ros-rosidl-typesupport-introspection-c-jazzy)
   #:home-page "https://github.com/ros2/rosidl_typesupport"
   #:synopsis "Generic C typesupport for ROS 2 messages"
   #:description
   "Multiplexes between concrete C typesupport implementations
(introspection, fastrtps, ...) at runtime via the AMENT resource index."))

(define-public ros-rosidl-typesupport-cpp-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rosidl_typesupport_cpp"
   #:version %rosidl-typesupport-version
   #:repo %rosidl-typesupport-repo
   #:commit %rosidl-typesupport-commit
   #:hash %rosidl-typesupport-hash
   #:module-subdir "rosidl_typesupport_cpp"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-rcutils-jazzy
                             ros-rcpputils-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-rosidl-typesupport-c-jazzy
                             ros-rosidl-typesupport-introspection-cpp-jazzy)
   #:home-page "https://github.com/ros2/rosidl_typesupport"
   #:synopsis "Generic C++ typesupport for ROS 2 messages"
   #:description
   "C++ counterpart to @code{rosidl_typesupport_c}."))

;;;
;;; rosidl_core / rosidl_defaults — meta-packages declaring which
;;; generators and typesupports a ROS 2 distribution provides.  Their
;;; -extras.cmake files use ament_index_get_resources to discover
;;; installed implementations dynamically, so missing optional pieces
;;; (e.g. fastrtps, generator_py) are silently skipped.
;;;

(define %rosidl-core-repo "https://github.com/ros2/rosidl_core")
(define %rosidl-core-commit "c6eb22b0e77cf57b19e9c6ffd114a00749404215")
(define %rosidl-core-hash
  (base32 "0xv2xv13y30q4cai72fs946zh0al8902i5mmjr7jalf37ifd3lw3"))
(define %rosidl-core-version "0.2.1")

;;;
;;; Python-side rosidl generator (must come before rosidl_core_generators
;;; because the latter's propagated-inputs reference it).
;;;

(define-public ros-python-cmake-module-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "python_cmake_module"
   #:version "0.11.1"
   #:repo "https://github.com/ros2/python_cmake_module"
   #:commit "25a28a647061476c78dd6d46eeac87a9bc5db888"
   #:hash (base32 "0ylywl19rzl5ln7q6lwwakk0c3506r4sk50zwnlrdyvssga9wp1a")
   #:propagated-inputs (list ros-ament-cmake-jazzy)
   #:home-page "https://github.com/ros2/python_cmake_module"
   #:synopsis "CMake helpers for finding Python in ROS 2 packages"
   #:description
   "Provides @code{find_package(python_cmake_module)}, a small wrapper
that locates a usable Python 3 development environment for cmake-based
ROS 2 packages that ship Python extensions."))

(define-public ros-rpyutils-jazzy
  (make-ros2-ament-python-package
   #:distro jazzy-distro
   #:ros-name "rpyutils"
   #:version "0.4.2"
   #:repo "https://github.com/ros2/rpyutils"
   #:commit "973dbcc0a0a18299d840d089e47372d569264c31"
   #:hash (base32 "0h3i9pv3p06ys7rrb4pjcxsx21x1ci35vkxd11kqnhmazvhrhv6m")
   #:home-page "https://github.com/ros2/rpyutils"
   #:synopsis "Common Python utilities for ROS 2 Python packages"
   #:description
   "Small collection of Python helpers used by @code{rclpy} and other
Python ROS 2 packages: dynamic library loading, attribute import."))

(define-public ros-rosidl-generator-py-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rosidl_generator_py"
   #:version "0.22.2"
   #:repo "https://github.com/ros2/rosidl_python"
   #:commit "fe4e01f2007d3451bf73c0cb334b96a7de759ef2"
   #:hash (base32 "1vlkd0yqsx3p1q82m467j6gm57whk7dql0svn27brhsi4g4ysv8g")
   #:module-subdir "rosidl_generator_py"
   ;; rmw is build_export_depend upstream: rosidl_generator_py's
   ;; ament_cmake_export_dependencies-extras.cmake calls find_package(rmw)
   ;; at consumer build time.
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-python-jazzy
                             ros-ament-index-python-jazzy
                             ros-python-cmake-module-jazzy
                             ros-rmw-jazzy
                             ros-rosidl-cli-jazzy
                             ros-rosidl-cmake-jazzy
                             ros-rosidl-generator-c-jazzy
                             ros-rosidl-parser-jazzy
                             ros-rosidl-pycommon-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-rosidl-typesupport-c-jazzy
                             ros-rosidl-typesupport-interface-jazzy
                             ros-rpyutils-jazzy
                             python-numpy)
   #:home-page "https://github.com/ros2/rosidl_python"
   #:synopsis "Python code generator for ROS 2 interface packages"
   #:description
   "Generates Python message classes and (de)serialisation glue from
ROS 2 @file{.msg}, @file{.srv}, and @file{.action} files.  Used by
interface packages whose buildtool_depend list includes
@code{rosidl_default_generators}."))

(define-public ros-rosidl-core-generators-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rosidl_core_generators"
   #:version %rosidl-core-version
   #:repo %rosidl-core-repo
   #:commit %rosidl-core-commit
   #:hash %rosidl-core-hash
   #:module-subdir "rosidl_core_generators"
   ;; rosidl_generator_py is a buildtool_export_depend of
   ;; rosidl_core_generators upstream; propagate it so interface packages
   ;; get Python bindings generated in addition to C/C++.
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-rosidl-cmake-jazzy
                             ros-rosidl-generator-c-jazzy
                             ros-rosidl-generator-cpp-jazzy
                             ros-rosidl-generator-py-jazzy
                             ros-rosidl-typesupport-c-jazzy
                             ros-rosidl-typesupport-cpp-jazzy
                             ros-rosidl-typesupport-introspection-c-jazzy
                             ros-rosidl-typesupport-introspection-cpp-jazzy)
   #:home-page "https://github.com/ros2/rosidl_core"
   #:synopsis "Meta-package declaring core ROS 2 rosidl generators"
   #:description
   "Pulls in the C and C++ rosidl generators and typesupports that any
ROS 2 distribution must provide.  Optional generators (Python, Rust,
Fast DDS) are discovered at consumer build time via the AMENT resource
index and are silently skipped if absent."))

(define-public ros-rosidl-core-runtime-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rosidl_core_runtime"
   #:version %rosidl-core-version
   #:repo %rosidl-core-repo
   #:commit %rosidl-core-commit
   #:hash %rosidl-core-hash
   #:module-subdir "rosidl_core_runtime"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-rosidl-runtime-cpp-jazzy
                             ros-rosidl-typesupport-c-jazzy
                             ros-rosidl-typesupport-cpp-jazzy
                             ros-rosidl-typesupport-introspection-c-jazzy
                             ros-rosidl-typesupport-introspection-cpp-jazzy)
   #:home-page "https://github.com/ros2/rosidl_core"
   #:synopsis "Meta-package declaring core ROS 2 rosidl runtime support"
   #:description
   "Runtime counterpart to @code{rosidl_core_generators}: pulls in the
C/C++ rosidl runtime and typesupport libraries used at message
serialization time."))

(define %rosidl-defaults-repo "https://github.com/ros2/rosidl_defaults")
(define %rosidl-defaults-commit
  "95834cca83ee0d3d783bb68e4932cffd6673274c")
(define %rosidl-defaults-hash
  (base32 "1z0hy3mkam6hh82fikpmzd0rykrqq8j01sj5lg7nkc3ssa29h6sd"))
(define %rosidl-defaults-version "1.6.0")

(define-public ros-rosidl-default-generators-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rosidl_default_generators"
   #:version %rosidl-defaults-version
   #:repo %rosidl-defaults-repo
   #:commit %rosidl-defaults-commit
   #:hash %rosidl-defaults-hash
   #:module-subdir "rosidl_default_generators"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-rosidl-core-generators-jazzy)
   #:home-page "https://github.com/ros2/rosidl_defaults"
   #:synopsis "Meta-package: default rosidl generators for ROS 2"
   #:description
   "Adds @code{action_msgs} and @code{service_msgs} on top of
@code{rosidl_core_generators}, allowing interface packages to declare a
single buildtool dependency that brings in everything needed to generate
messages, services, and actions."))

(define-public ros-rosidl-default-runtime-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rosidl_default_runtime"
   #:version %rosidl-defaults-version
   #:repo %rosidl-defaults-repo
   #:commit %rosidl-defaults-commit
   #:hash %rosidl-defaults-hash
   #:module-subdir "rosidl_default_runtime"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-rosidl-core-runtime-jazzy)
   #:home-page "https://github.com/ros2/rosidl_defaults"
   #:synopsis "Meta-package: default rosidl runtime support for ROS 2"
   #:description
   "Runtime counterpart to @code{rosidl_default_generators}."))

;;;
;;; Interface packages — first real users of the rosidl pipeline.
;;;
;;; The rcl_interfaces monorepo hosts most of the base messages; std_msgs
;;; lives in common_interfaces; unique_identifier_msgs has its own repo.

(define %rcl-interfaces-repo "https://github.com/ros2/rcl_interfaces")
(define %rcl-interfaces-commit "6ec02e7341db2258e1c1a36fb79701c5af5b138e")
(define %rcl-interfaces-hash
  (base32 "13md36vilpahr2zp0g695zgszp983pxw4g4p7sxah5wzv7h59x8g"))
(define %rcl-interfaces-version "2.0.3")

(define* (rcl-interfaces-msg ros-name #:key
                             (message-deps '())
                             synopsis description)
  "Helper for sub-packages of ros2/rcl_interfaces (jazzy)."
  (make-ros2-rosidl-interface-package
   #:distro jazzy-distro
   #:ros-name ros-name
   #:version %rcl-interfaces-version
   #:repo %rcl-interfaces-repo
   #:commit %rcl-interfaces-commit
   #:hash %rcl-interfaces-hash
   #:module-subdir ros-name
   #:message-deps (cons ros-rosidl-default-runtime-jazzy
                        (cons ros-rosidl-default-generators-jazzy
                              message-deps))
   #:home-page "https://github.com/ros2/rcl_interfaces"
   #:synopsis synopsis
   #:description description))

(define-public ros-builtin-interfaces-jazzy
  (rcl-interfaces-msg
   "builtin_interfaces"
   #:synopsis "Built-in primitive interfaces (Time, Duration) for ROS 2"
   #:description
   "@code{builtin_interfaces} provides the @code{Time} and
@code{Duration} message types used throughout ROS 2."))

(define-public ros-service-msgs-jazzy
  (rcl-interfaces-msg
   "service_msgs"
   #:message-deps (list ros-builtin-interfaces-jazzy)
   #:synopsis "Service-related metadata message types for ROS 2"
   #:description
   "@code{service_msgs} provides @code{ServiceEventInfo} and related
metadata types used by ROS 2 service introspection."))

(define-public ros-type-description-interfaces-jazzy
  (rcl-interfaces-msg
   "type_description_interfaces"
   #:message-deps (list ros-builtin-interfaces-jazzy
                        ros-service-msgs-jazzy)
   #:synopsis "Type description interfaces for ROS 2"
   #:description
   "Provides @code{TypeDescription}, @code{TypeSource}, and the
@code{GetTypeDescription} service used by ROS 2 type introspection."))

(define-public ros-rosgraph-msgs-jazzy
  (rcl-interfaces-msg
   "rosgraph_msgs"
   #:message-deps (list ros-builtin-interfaces-jazzy)
   #:synopsis "ROS computation graph message types"
   #:description
   "Messages such as @code{Clock} used by tools that interact with the
ROS computation graph."))

(define-public ros-rcl-interfaces-jazzy
  (rcl-interfaces-msg
   "rcl_interfaces"
   #:message-deps (list ros-builtin-interfaces-jazzy
                        ros-service-msgs-jazzy)
   #:synopsis "rcl-level message and service definitions"
   #:description
   "Interfaces consumed directly by @code{rcl}: parameter
descriptors, log messages, list-parameters service definitions, etc."))

(define-public ros-statistics-msgs-jazzy
  (rcl-interfaces-msg
   "statistics_msgs"
   #:message-deps (list ros-builtin-interfaces-jazzy)
   #:synopsis "Statistics message types for ROS 2 monitoring"
   #:description
   "Message types used by @code{libstatistics_collector} to publish
runtime metrics from ROS 2 nodes."))

(define-public ros-lifecycle-msgs-jazzy
  (rcl-interfaces-msg
   "lifecycle_msgs"
   ;; lifecycle_msgs declares services (e.g. ChangeState), so service_msgs
   ;; must be available at code-generation time.  Upstream's package.xml
   ;; under-declares this — it relies on rosidl_default_generators
   ;; transitively providing it.
   #:message-deps (list ros-service-msgs-jazzy)
   #:synopsis "Lifecycle node state and transition messages"
   #:description
   "Message and service definitions used by ROS 2 managed (lifecycle)
nodes."))

(define-public ros-unique-identifier-msgs-jazzy
  (make-ros2-rosidl-interface-package
   #:distro jazzy-distro
   #:ros-name "unique_identifier_msgs"
   #:version "2.5.0"
   #:repo "https://github.com/ros2/unique_identifier_msgs"
   #:commit "51ec3931400feba6cc0ef0ebd045b5866f63fdd1"
   #:hash (base32 "1jx7z6101lg9q51rrizv3apjrsl2vvbf55nqakfm25zifms9fd5g")
   #:message-deps (list ros-rosidl-default-generators-jazzy
                        ros-rosidl-default-runtime-jazzy)
   #:home-page "https://github.com/ros2/unique_identifier_msgs"
   #:synopsis "Unique identifier (UUID) message types for ROS 2"
   #:description
   "Provides the @code{UUID} message used by ROS 2 actions and other
APIs that need stable globally unique identifiers."))

(define-public ros-action-msgs-jazzy
  (rcl-interfaces-msg
   "action_msgs"
   #:message-deps (list ros-builtin-interfaces-jazzy
                        ros-service-msgs-jazzy
                        ros-unique-identifier-msgs-jazzy)
   #:synopsis "Generic action server / client message types"
   #:description
   "Generic messages and services used by ROS 2 action server and
client implementations: goal info, goal status, cancel goal."))

;;;
;;; ros2/common_interfaces — std_msgs (Phase 1) plus the larger
;;; common-interface message families that ros_base needs (Phase 2).
;;;

(define %common-interfaces-repo "https://github.com/ros2/common_interfaces")
(define %common-interfaces-commit "81e2f6baa6eb9ac734d4c174dfd231b54d5fa1ef")
(define %common-interfaces-hash
  (base32 "0ni0x7fm0fxcyvvmj479s656fnrynhn4a6whfy3d272fni2kvzd4"))
(define %common-interfaces-version "5.3.7")

(define* (common-interfaces-msg ros-name #:key
                                (message-deps '())
                                synopsis description)
  "Helper for sub-packages of ros2/common_interfaces (jazzy)."
  (make-ros2-rosidl-interface-package
   #:distro jazzy-distro
   #:ros-name ros-name
   #:version %common-interfaces-version
   #:repo %common-interfaces-repo
   #:commit %common-interfaces-commit
   #:hash %common-interfaces-hash
   #:module-subdir ros-name
   #:message-deps (cons ros-rosidl-default-runtime-jazzy
                        (cons ros-rosidl-default-generators-jazzy
                              message-deps))
   #:home-page %common-interfaces-repo
   #:synopsis synopsis
   #:description description))

(define-public ros-std-msgs-jazzy
  (common-interfaces-msg
   "std_msgs"
   #:message-deps (list ros-builtin-interfaces-jazzy)
   #:synopsis "Standard primitive message types for ROS 2"
   #:description
   "Standard messages: @code{String}, @code{Bool}, @code{Int32},
@code{Float64}, @code{Header}, etc.  These are the most commonly used
message types in ROS 2 demonstration code."))

(define-public ros-std-srvs-jazzy
  (common-interfaces-msg
   "std_srvs"
   #:message-deps (list ros-service-msgs-jazzy)
   #:synopsis "Standard primitive service types for ROS 2"
   #:description
   "Standard services: @code{Empty}, @code{SetBool}, @code{Trigger}.
The minimal service set ROS 2 nodes use for simple control APIs."))

(define-public ros-geometry-msgs-jazzy
  (common-interfaces-msg
   "geometry_msgs"
   #:message-deps (list ros-builtin-interfaces-jazzy
                        ros-std-msgs-jazzy)
   #:synopsis "Geometric primitive messages for ROS 2"
   #:description
   "Vector, Point, Quaternion, Pose, Twist, Wrench, Transform and the
stamped/covariance variants used by tf2 and most navigation stacks."))

(define-public ros-actionlib-msgs-jazzy
  (common-interfaces-msg
   "actionlib_msgs"
   #:message-deps (list ros-builtin-interfaces-jazzy
                        ros-std-msgs-jazzy)
   #:synopsis "Legacy actionlib message types"
   #:description
   "Legacy message types from ROS 1 actionlib, kept for compatibility
with ports that haven't migrated to the new ROS 2 actions API."))

(define-public ros-shape-msgs-jazzy
  (common-interfaces-msg
   "shape_msgs"
   #:message-deps (list ros-geometry-msgs-jazzy
                        ros-std-msgs-jazzy)
   #:synopsis "Geometric shape messages for ROS 2"
   #:description
   "Plane, SolidPrimitive, Mesh, MeshTriangle — used by motion planners
and collision-checking libraries."))

(define-public ros-trajectory-msgs-jazzy
  (common-interfaces-msg
   "trajectory_msgs"
   #:message-deps (list ros-builtin-interfaces-jazzy
                        ros-geometry-msgs-jazzy
                        ros-std-msgs-jazzy)
   #:synopsis "Trajectory messages (joint and multi-DOF) for ROS 2"
   #:description
   "JointTrajectory, MultiDOFJointTrajectory and their @code{*Point}
companions, used by motion controllers and trajectory planners."))

(define-public ros-sensor-msgs-jazzy
  (common-interfaces-msg
   "sensor_msgs"
   #:message-deps (list ros-builtin-interfaces-jazzy
                        ros-geometry-msgs-jazzy
                        ros-service-msgs-jazzy
                        ros-std-msgs-jazzy)
   #:synopsis "Sensor message types (Image, IMU, LaserScan, ...) for ROS 2"
   #:description
   "The standard ROS 2 sensor messages: @code{Image}, @code{Imu},
@code{LaserScan}, @code{PointCloud2}, @code{CameraInfo}, @code{Joy},
plus a small set of services like @code{SetCameraInfo}."))

(define-public ros-stereo-msgs-jazzy
  (common-interfaces-msg
   "stereo_msgs"
   #:message-deps (list ros-sensor-msgs-jazzy
                        ros-std-msgs-jazzy)
   #:synopsis "Stereo-vision messages for ROS 2"
   #:description
   "@code{DisparityImage} and the small set of stereo-vision-specific
messages used by stereo cameras."))

(define-public ros-nav-msgs-jazzy
  (common-interfaces-msg
   "nav_msgs"
   #:message-deps (list ros-builtin-interfaces-jazzy
                        ros-geometry-msgs-jazzy
                        ros-service-msgs-jazzy
                        ros-std-msgs-jazzy)
   #:synopsis "Navigation message types for ROS 2"
   #:description
   "@code{OccupancyGrid}, @code{Odometry}, @code{Path}, @code{MapMetaData},
plus the @code{GetMap} / @code{GetPlan} services used by navigation
stacks."))

(define-public ros-diagnostic-msgs-jazzy
  (common-interfaces-msg
   "diagnostic_msgs"
   #:message-deps (list ros-builtin-interfaces-jazzy
                        ros-service-msgs-jazzy
                        ros-std-msgs-jazzy)
   #:synopsis "Diagnostic message and service types for ROS 2"
   #:description
   "@code{DiagnosticStatus}, @code{DiagnosticArray}, @code{KeyValue},
plus the @code{AddDiagnostics} / @code{SelfTest} services used by
the @code{diagnostics} stack."))

(define-public ros-visualization-msgs-jazzy
  (common-interfaces-msg
   "visualization_msgs"
   #:message-deps (list ros-builtin-interfaces-jazzy
                        ros-geometry-msgs-jazzy
                        ros-sensor-msgs-jazzy
                        ros-service-msgs-jazzy
                        ros-std-msgs-jazzy)
   #:synopsis "Visualization message types (Marker, MarkerArray, ...) for ROS 2"
   #:description
   "@code{Marker}, @code{MarkerArray}, @code{InteractiveMarker} and
related types used by RViz and other visualization tools."))

;;;
;;; rmw_dds_common — shared helpers and common message types for DDS
;;; based rmw implementations.
;;;

(define-public ros-rmw-dds-common-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rmw_dds_common"
   #:version "3.1.1"
   #:repo "https://github.com/ros2/rmw_dds_common"
   #:commit "2e3ebf31850a8b05430fb44230ead2d566b8ff1d"
   #:hash (base32 "1fgjim663jhpp59xjkvynwfdalxv7cpk4d2bp7c33686789qck55")
   #:module-subdir "rmw_dds_common"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-rcutils-jazzy
                             ros-rcpputils-jazzy
                             ros-rmw-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-rosidl-runtime-cpp-jazzy
                             ros-rosidl-default-generators-jazzy
                             ros-rosidl-default-runtime-jazzy)
   #:home-page "https://github.com/ros2/rmw_dds_common"
   #:synopsis "Common helpers and message types for DDS-based rmw"
   #:description
   "Provides @code{Gid}, @code{ParticipantEntitiesInfo}, and other shared
message types and C++ helpers used by every DDS-based @code{rmw}
implementation (Cyclone DDS, Fast DDS, ...)."))

;;;
;;; tracetools — instrumentation API used by rclcpp/rcl/rmw_*.
;;; Built with LTTng support disabled in Phase 1 (TRACETOOLS_DISABLED=ON).
;;;

(define-public ros-tracetools-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "tracetools"
   #:version "8.2.5"
   #:repo "https://github.com/ros2/ros2_tracing"
   #:commit "00f7331e33886372957e88466e224c9c9f514a8c"
   #:hash (base32 "0syxr3kmjd1c4z9qiznvqjvbhqj5cpgqd0ahyv3jaj92z2a0fzw7")
   #:module-subdir "tracetools"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-ament-cmake-gen-version-h-jazzy)
   #:extra-configure-flags
   #~(list "-DTRACETOOLS_DISABLED=ON")
   #:home-page "https://github.com/ros2/ros2_tracing"
   #:synopsis "ROS 2 instrumentation API (LTTng support disabled)"
   #:description
   "@code{tracetools} provides the C/C++ instrumentation API used by
@code{rclcpp}, @code{rcl}, and the rmw implementations to emit trace
events.  Built here with @code{TRACETOOLS_DISABLED=ON}, so the
tracepoints become no-ops and we avoid pulling in @code{lttng-ust}."))

;;;
;;; rmw_implementation_cmake — small CMake helper from the rmw repo.
;;;

(define-public ros-rmw-implementation-cmake-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rmw_implementation_cmake"
   #:version "7.3.3"
   #:repo "https://github.com/ros2/rmw"
   #:commit "9f42ff0f18c8c4fd2a84a7c27d11ec8371e968ea"
   #:hash (base32 "0pgv9zzgckaffpf72wbpq2mnbv1lap7cla10sswn40f1kcl6a9l0")
   #:module-subdir "rmw_implementation_cmake"
   #:propagated-inputs (list ros-ament-cmake-jazzy ros-rmw-jazzy)
   #:home-page "https://github.com/ros2/rmw"
   #:synopsis "CMake helpers for selecting an rmw implementation"
   #:description
   "Provides @code{find_package(rmw_implementation_cmake)} hooks used by
@code{rmw_implementation} and downstream consumers to discover the
installed @code{rmw} bindings via the AMENT resource index."))

;;;
;;; rmw_cyclonedds_cpp — Cyclone DDS rmw binding.
;;;

(define-public ros-rmw-cyclonedds-cpp-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rmw_cyclonedds_cpp"
   #:version "2.2.3"
   #:repo "https://github.com/ros2/rmw_cyclonedds"
   #:commit "3022e057cde9fc164377fca05536ecdfcc0c5ace"
   #:hash (base32 "1zrhsx3c1xy41794sw0vrbsszsg9zwaajr2gzyy7qy0mrmqg3kkx")
   #:module-subdir "rmw_cyclonedds_cpp"
   ;; cyclonedds must be propagated, not just an input: rmw_cyclonedds_cpp's
   ;; exported -extras.cmake calls find_package(CycloneDDS) at consumer
   ;; build time (e.g. from rmw_implementation).
   #:propagated-inputs (list eclipse-cyclonedds
                             ros-ament-cmake-ros-jazzy
                             ros-rcutils-jazzy
                             ros-rcpputils-jazzy
                             ros-rmw-jazzy
                             ros-rmw-dds-common-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-rosidl-typesupport-introspection-c-jazzy
                             ros-rosidl-typesupport-introspection-cpp-jazzy
                             ros-tracetools-jazzy)
   #:home-page "https://github.com/ros2/rmw_cyclonedds"
   #:synopsis "Eclipse Cyclone DDS implementation of the ROS 2 rmw API"
   #:description
   "@code{rmw_cyclonedds_cpp} implements the @code{rmw} C interface on
top of Eclipse Cyclone DDS.  This is the default DDS middleware in the
guix-systole ROS 2 Jazzy distribution; users select it by setting
@env{RMW_IMPLEMENTATION=rmw_cyclonedds_cpp}."))

;;;
;;; rmw_implementation — runtime dispatcher between installed rmw bindings.
;;;

(define-public ros-rmw-implementation-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rmw_implementation"
   #:version "2.15.6"
   #:repo "https://github.com/ros2/rmw_implementation"
   #:commit "23fe893a860545da06ec98d0a22fdc8554d5c0ac"
   #:hash (base32 "07s1fnzv64634c3dgm6wr98az78ldrvbq5dk4c5s0j7i7nj0v5a3")
   #:module-subdir "rmw_implementation"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-index-cpp-jazzy
                             ros-rcpputils-jazzy
                             ros-rcutils-jazzy
                             ros-rmw-jazzy
                             ros-rmw-implementation-cmake-jazzy
                             ros-rmw-cyclonedds-cpp-jazzy)
   #:home-page "https://github.com/ros2/rmw_implementation"
   #:synopsis "ROS 2 runtime dispatcher between rmw implementations"
   #:description
   "Resolves the @env{RMW_IMPLEMENTATION} environment variable to one of
the installed @code{rmw_*} libraries (Cyclone DDS, Fast DDS, ...) and
forwards every rmw call to it."))

;;;
;;; rcl tier — ROS 2 client support library and its prerequisites.
;;;

(define-public ros-libyaml-vendor-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "libyaml_vendor"
   #:version "1.6.3"
   #:repo "https://github.com/ros2/libyaml_vendor"
   #:commit "a54a952b730921146dc422d5e5beef5563df0fac"
   #:hash (base32 "14zjp6gp4g59378l17nb2jqq341hygr7dkvryy7ch1jh81w2d5br")
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-vendor-package-jazzy
                             pkg-config
                             libyaml)
   #:home-page "https://github.com/ros2/libyaml_vendor"
   #:synopsis "Vendor wrapper around libyaml for ROS 2"
   #:description
   "Provides @code{find_package(libyaml_vendor)} that downstream ROS 2
packages use to depend on libyaml.  In guix-systole the system libyaml
is detected via pkg-config and re-used (no external project build)."))

;;; rcl is a sub-package of the ros2/rcl monorepo.

(define %rcl-repo "https://github.com/ros2/rcl")
(define %rcl-commit "128547b52218f0856c84bb1640b9339a7fddc5ff")
(define %rcl-hash
  (base32 "0zn2g8x8m61fq9xjbp21r1vy0qlfp2md7498sqiv79dfzbrlyz30"))
(define %rcl-version "9.2.9")

(define-public ros-rcl-yaml-param-parser-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rcl_yaml_param_parser"
   #:version %rcl-version
   #:repo %rcl-repo
   #:commit %rcl-commit
   #:hash %rcl-hash
   #:module-subdir "rcl_yaml_param_parser"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-ament-cmake-gen-version-h-jazzy
                             ros-libyaml-vendor-jazzy
                             ros-rcutils-jazzy
                             ros-rmw-jazzy)
   #:home-page "https://github.com/ros2/rcl"
   #:synopsis "Parse ROS 2 parameter YAML files"
   #:description
   "C library used by @code{rcl} to parse parameter files
(@file{*.yaml}) at node initialisation time."))

;;; rcl_logging from ros2/rcl_logging.

(define %rcl-logging-repo "https://github.com/ros2/rcl_logging")
(define %rcl-logging-commit "727920c2592be6deb6983f0fe1b57cbc929cbb70")
(define %rcl-logging-hash
  (base32 "1s9j3641sw2mknnx41r8fv2d4bd8m8vsng945g3ghm5cdsqx2f4n"))
(define %rcl-logging-version "3.1.1")

(define-public ros-rcl-logging-interface-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rcl_logging_interface"
   #:version %rcl-logging-version
   #:repo %rcl-logging-repo
   #:commit %rcl-logging-commit
   #:hash %rcl-logging-hash
   #:module-subdir "rcl_logging_interface"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-rcutils-jazzy)
   #:home-page "https://github.com/ros2/rcl_logging"
   #:synopsis "Interface header for ROS 2 logging back-ends"
   #:description
   "Defines the C ABI shared between @code{rcl} and concrete logging
back-end implementations (@code{rcl_logging_noop},
@code{rcl_logging_spdlog}, ...)."))

(define-public ros-rcl-logging-noop-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rcl_logging_noop"
   #:version %rcl-logging-version
   #:repo %rcl-logging-repo
   #:commit %rcl-logging-commit
   #:hash %rcl-logging-hash
   #:module-subdir "rcl_logging_noop"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-rcl-logging-interface-jazzy
                             ros-rcutils-jazzy)
   #:extra-native-inputs (list python-empy)
   #:home-page "https://github.com/ros2/rcl_logging"
   #:synopsis "No-op logging back-end for ROS 2"
   #:description
   "@code{rcl_logging_noop} discards every log message it receives.
Used here as the default rcl logging back-end so we can avoid pulling
in spdlog for the Phase 1 ros_core build."))

(define-public ros-rcl-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rcl"
   #:version %rcl-version
   #:repo %rcl-repo
   #:commit %rcl-commit
   #:hash %rcl-hash
   #:module-subdir "rcl"
   #:extra-configure-flags
   #~(list "-DRCL_LOGGING_IMPLEMENTATION=rcl_logging_noop")
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-ament-cmake-gen-version-h-jazzy
                             ros-libyaml-vendor-jazzy
                             ros-rcl-interfaces-jazzy
                             ros-rcl-logging-interface-jazzy
                             ros-rcl-logging-noop-jazzy
                             ros-rcl-yaml-param-parser-jazzy
                             ros-rcutils-jazzy
                             ros-rmw-jazzy
                             ros-rmw-implementation-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-service-msgs-jazzy
                             ros-tracetools-jazzy
                             ros-type-description-interfaces-jazzy)
   #:home-page "https://github.com/ros2/rcl"
   #:synopsis "ROS 2 client support library"
   #:description
   "@code{rcl} is the C client support library for ROS 2: it sits
between the higher-level client libraries (@code{rclcpp}, @code{rclpy})
and the @code{rmw} middleware abstraction.  Provides nodes, publishers,
subscribers, services, clients, timers, parameter handling and the main
event loop primitives.  Built with @code{rcl_logging_noop} as the
default logging back-end."))

(define-public ros-rcl-lifecycle-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rcl_lifecycle"
   #:version %rcl-version
   #:repo %rcl-repo
   #:commit %rcl-commit
   #:hash %rcl-hash
   #:module-subdir "rcl_lifecycle"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-ament-cmake-gen-version-h-jazzy
                             ros-lifecycle-msgs-jazzy
                             ros-rcl-jazzy
                             ros-rcutils-jazzy
                             ros-rmw-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-tracetools-jazzy)
   #:home-page "https://github.com/ros2/rcl"
   #:synopsis "Lifecycle (managed-node) state machine for rcl"
   #:description
   "Implements the ROS 2 managed-node state machine on top of @code{rcl},
exposing it via @file{lifecycle_msgs} services."))

(define-public ros-rcl-action-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rcl_action"
   #:version %rcl-version
   #:repo %rcl-repo
   #:commit %rcl-commit
   #:hash %rcl-hash
   #:module-subdir "rcl_action"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-ament-cmake-gen-version-h-jazzy
                             ros-action-msgs-jazzy
                             ros-rcl-jazzy
                             ros-rcutils-jazzy
                             ros-rmw-jazzy
                             ros-rosidl-runtime-c-jazzy)
   #:home-page "https://github.com/ros2/rcl"
   #:synopsis "Action (long-running RPC) support for rcl"
   #:description
   "Implements the ROS 2 action server and client primitives on top of
@code{rcl}, using @file{action_msgs} for goal/feedback/result delivery."))

;;;
;;; libstatistics_collector and rclcpp.
;;;

(define-public ros-libstatistics-collector-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "libstatistics_collector"
   #:version "1.7.4"
   #:repo "https://github.com/ros-tooling/libstatistics_collector"
   #:commit "150e1b0e1fbf920a97e4319f98eaf02be27fa892"
   #:hash (base32 "1q5n6fy857g1gm2pkdpkpgrsmgjz3v9kcd2qmb4ljiqxy2a03w2a")
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-builtin-interfaces-jazzy
                             ros-rcl-jazzy
                             ros-rcpputils-jazzy
                             ros-rmw-jazzy
                             ros-statistics-msgs-jazzy)
   #:home-page "https://github.com/ros-tooling/libstatistics_collector"
   #:synopsis "Collect runtime statistics for ROS 2 nodes"
   #:description
   "Library used by @code{rclcpp} to collect and publish runtime
metrics (message age, period, ...) using the @file{statistics_msgs}
message types."))

(define %rclcpp-repo "https://github.com/ros2/rclcpp")
(define %rclcpp-commit "4321b44ef1a3cc7b46e844c6f2bf148ae5ad2869")
(define %rclcpp-hash
  (base32 "07yxm9n7w4p7r42524gbdhw9m6dn0cgkqfr81bnchjh969jb818w"))
(define %rclcpp-version "28.1.18")

(define-public ros-rclcpp-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rclcpp"
   #:version %rclcpp-version
   #:repo %rclcpp-repo
   #:commit %rclcpp-commit
   #:hash %rclcpp-hash
   #:module-subdir "rclcpp"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-ament-cmake-gen-version-h-jazzy
                             ros-ament-index-cpp-jazzy
                             ros-builtin-interfaces-jazzy
                             ros-libstatistics-collector-jazzy
                             ros-rcl-jazzy
                             ros-rcl-interfaces-jazzy
                             ros-rcl-logging-interface-jazzy
                             ros-rcl-yaml-param-parser-jazzy
                             ros-rcpputils-jazzy
                             ros-rcutils-jazzy
                             ros-rmw-jazzy
                             ros-rosgraph-msgs-jazzy
                             ros-rosidl-dynamic-typesupport-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-rosidl-runtime-cpp-jazzy
                             ros-rosidl-typesupport-c-jazzy
                             ros-rosidl-typesupport-cpp-jazzy
                             ros-statistics-msgs-jazzy
                             ros-tracetools-jazzy)
   #:home-page "https://github.com/ros2/rclcpp"
   #:synopsis "ROS 2 C++ client library"
   #:description
   "@code{rclcpp} is the C++ ROS 2 client library: provides
@code{rclcpp::Node}, executors, publishers/subscribers, services,
clients, parameters, and the high-level RAII wrappers used by virtually
every C++ ROS 2 program."))

(define-public ros-rclcpp-lifecycle-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rclcpp_lifecycle"
   #:version %rclcpp-version
   #:repo %rclcpp-repo
   #:commit %rclcpp-commit
   #:hash %rclcpp-hash
   #:module-subdir "rclcpp_lifecycle"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-lifecycle-msgs-jazzy
                             ros-rcl-jazzy
                             ros-rcl-interfaces-jazzy
                             ros-rcl-lifecycle-jazzy
                             ros-rclcpp-jazzy
                             ros-rcutils-jazzy
                             ros-rmw-jazzy
                             ros-rosidl-typesupport-cpp-jazzy)
   #:home-page "https://github.com/ros2/rclcpp"
   #:synopsis "Lifecycle (managed) node helpers for rclcpp"
   #:description
   "C++ wrappers around @code{rcl_lifecycle} that expose the ROS 2
managed-node state machine through the @code{rclcpp_lifecycle::Node}
class."))

(define-public ros-rclcpp-action-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rclcpp_action"
   #:version %rclcpp-version
   #:repo %rclcpp-repo
   #:commit %rclcpp-commit
   #:hash %rclcpp-hash
   #:module-subdir "rclcpp_action"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-action-msgs-jazzy
                             ros-rcl-jazzy
                             ros-rcl-action-jazzy
                             ros-rclcpp-jazzy
                             ros-rcpputils-jazzy
                             ros-rosidl-runtime-c-jazzy)
   #:home-page "https://github.com/ros2/rclcpp"
   #:synopsis "C++ action server and client helpers for rclcpp"
   #:description
   "Higher-level wrappers around @code{rcl_action} for writing C++ ROS 2
action servers and clients."))

;;;
;;; Python helpers needed downstream (rclpy, interface Python generation).
;;; pybind11_vendor lives with rclpy since only rclpy uses it.
;;;

(define-public ros-pybind11-vendor-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "pybind11_vendor"
   #:version "3.1.3"
   #:repo "https://github.com/ros2/pybind11_vendor"
   #:commit "f67c58969e6033c737f1bbd0edd1fe018fd1d9ad"
   #:hash (base32 "0zg8svvhcbjv7y8zh61pcglaghwq9lvmn258rm7xlh5yb17alr1m")
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-vendor-package-jazzy
                             pybind11)
   #:home-page "https://github.com/ros2/pybind11_vendor"
   #:synopsis "ROS 2 vendor wrapper around pybind11"
   #:description
   "Thin wrapper that lets ROS 2 packages depend on a known-good
@code{pybind11} version.  In guix-systole the upstream pybind11 is used
directly via @code{find_package(pybind11)}; no external project is
built."))

;;;
;;; rclpy — Python client library.
;;;

(define-public ros-rclpy-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rclpy"
   #:version "7.1.11"
   #:repo "https://github.com/ros2/rclpy"
   #:commit "baf9d72cfa127e391a89b4ab51ba9e55c37041fd"
   #:hash (base32 "0ggbnv0rcc9g760gwd1r5zmbvwbd8ml7f7ip2y8q8p0211r9lwgw")
   #:module-subdir "rclpy"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-index-python-jazzy
                             ros-action-msgs-jazzy
                             ros-builtin-interfaces-jazzy
                             ros-lifecycle-msgs-jazzy
                             ros-pybind11-vendor-jazzy
                             ros-python-cmake-module-jazzy
                             ros-rcl-jazzy
                             ros-rcl-action-jazzy
                             ros-rcl-interfaces-jazzy
                             ros-rcl-lifecycle-jazzy
                             ros-rcl-logging-interface-jazzy
                             ros-rcl-yaml-param-parser-jazzy
                             ros-rcpputils-jazzy
                             ros-rcutils-jazzy
                             ros-rmw-jazzy
                             ros-rmw-implementation-jazzy
                             ros-rmw-implementation-cmake-jazzy
                             ros-rosgraph-msgs-jazzy
                             ros-rosidl-runtime-c-jazzy
                             ros-rpyutils-jazzy
                             ros-unique-identifier-msgs-jazzy
                             python-pyyaml)
   #:home-page "https://github.com/ros2/rclpy"
   #:synopsis "ROS 2 Python client library"
   #:description
   "@code{rclpy} is the Python client library for ROS 2.  It exposes
@code{rclpy.Node}, executors, publishers, subscribers, services,
clients, parameters, and timers via a pybind11 C++ extension built on
top of @code{rcl}."))

;;;
;;; ============================================================
;;; Phase 2 — ros_base components.
;;; ============================================================
;;; Everything below is layered on top of the Phase 1 ros_core stack.
;;; ============================================================

;;;
;;; tinyxml2_vendor and console_bridge_vendor — vendor wrappers around
;;; system libraries used by class_loader / urdf / kdl_parser.
;;;

(define-public ros-tinyxml2-vendor-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "tinyxml2_vendor"
   #:version "0.9.2"
   #:repo "https://github.com/ros2/tinyxml2_vendor"
   #:commit "576ca46707b6e183911daac625910ee6d6be2f45"
   #:hash (base32 "1wzsaf0nsf5d4fpazi5dspg5rwz8yqpmxgmly3gbyb6sa56i274l")
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             tinyxml2)
   #:home-page "https://github.com/ros2/tinyxml2_vendor"
   #:synopsis "ROS 2 vendor wrapper around system tinyxml2"
   #:description
   "Thin shim that lets ROS 2 packages depend on @code{tinyxml2_vendor}
and have @code{find_package(TinyXML2)} succeed via the upstream
@code{tinyxml2} library shipped by Guix."))

(define-public ros-console-bridge-vendor-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "console_bridge_vendor"
   #:version "1.7.0"
   #:repo "https://github.com/ros2/console_bridge_vendor"
   #:commit "611ceb78391dd89ed41061b3d72e391b763b78c3"
   #:hash (base32 "0n8rmg8j1d5b8b38s50g0c3nizywdmyx1wlqfvjz2kyaqzprd20y")
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-vendor-package-jazzy
                             console-bridge)
   #:home-page "https://github.com/ros2/console_bridge_vendor"
   #:synopsis "ROS 2 vendor wrapper around console_bridge"
   #:description
   "Vendor wrapper around the upstream @code{console_bridge} logging
shim.  Resolved via @code{find_package(console_bridge)} against the
package shipped by guix-systole."))

;;;
;;; class_loader / pluginlib — runtime plugin loading framework.
;;;

(define-public ros-class-loader-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "class_loader"
   #:version "2.7.0"
   #:repo "https://github.com/ros/class_loader"
   #:commit "e06532188f4d79629866a08ee89e9203686a46ee"
   #:hash (base32 "1vi6fbmn4034xf7m3kalflss4hbvr3pgzbak70r77p5fzmw55jfc")
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-console-bridge-vendor-jazzy
                             console-bridge
                             ros-rcpputils-jazzy)
   #:home-page "https://github.com/ros/class_loader"
   #:synopsis "Runtime C++ class loader (foundation of pluginlib)"
   #:description
   "ROS-independent C++ library for loading plugins via dlopen at
runtime.  Used as the foundation of @code{pluginlib} and the wider
@code{rclcpp_components} infrastructure."))

(define-public ros-pluginlib-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "pluginlib"
   #:version "5.4.5"
   #:repo "https://github.com/ros/pluginlib"
   #:commit "ddb1163d79932739ddf184328352e2a8e973ee29"
   #:hash (base32 "1ndsh17n82aqqhhmwc0fyyi6mv4rl1n6kgxhws0x6pmxjd3gm3nl")
   #:module-subdir "pluginlib"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-index-cpp-jazzy
                             ros-class-loader-jazzy
                             ros-rcutils-jazzy
                             ros-rcpputils-jazzy
                             ros-tinyxml2-vendor-jazzy)
   #:home-page "https://github.com/ros/pluginlib"
   #:synopsis "ROS 2 plugin loading library"
   #:description
   "@code{pluginlib} is the higher-level ROS 2 plugin loading library
built on top of @code{class_loader}, used for runtime discovery and
loading of plugins declared via @file{plugin_description.xml} files."))

;;;
;;; urdf — ROS 2 wrapper around upstream urdfdom.
;;;

(define %urdf-repo "https://github.com/ros2/urdf")
(define %urdf-commit "4f1fded3c9576cd58901959fb9c220fba3065eb9")
(define %urdf-hash
  (base32 "1lnzcw3n6p48rv5mbnccwz2kqy2kkill8a3n0656mwq1zn3c46ar"))
(define %urdf-version "2.10.0")

(define-public ros-urdf-parser-plugin-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "urdf_parser_plugin"
   #:version %urdf-version
   #:repo %urdf-repo
   #:commit %urdf-commit
   #:hash %urdf-hash
   #:module-subdir "urdf_parser_plugin"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             urdfdom-headers)
   #:home-page "https://github.com/ros2/urdf"
   #:synopsis "Plugin interface for ROS 2 URDF parsers"
   #:description
   "Defines the C++ plugin interface that ROS 2 URDF parsers implement,
allowing @code{urdf} to dispatch to multiple parser back-ends."))

(define-public ros-urdf-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "urdf"
   #:version %urdf-version
   #:repo %urdf-repo
   #:commit %urdf-commit
   #:hash %urdf-hash
   #:module-subdir "urdf"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-pluginlib-jazzy
                             ros-tinyxml2-vendor-jazzy
                             ros-urdf-parser-plugin-jazzy
                             urdfdom
                             urdfdom-headers
                             tinyxml2)
   #:home-page "https://github.com/ros2/urdf"
   #:synopsis "ROS 2 URDF parser"
   #:description
   "@code{urdf} is the ROS 2 entry point for parsing Unified Robot
Description Format (URDF) files.  It dispatches to upstream
@code{urdfdom} via @code{urdf_parser_plugin}."))

;;;
;;; eigen3_cmake_module / orocos_kdl_vendor / kdl_parser.
;;;

(define-public ros-eigen3-cmake-module-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "eigen3_cmake_module"
   #:version "1.0.1"
   #:repo "https://github.com/ros2/eigen3_cmake_module"
   #:commit "68eacd252be453560472b326cddebfe09beea0a0"
   #:hash (base32 "038vv7l96z15csafpb3pzvj0dqxpzvhb9dkqcw7ax4y8llm31bci")
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             eigen)
   #:home-page "https://github.com/ros2/eigen3_cmake_module"
   #:synopsis "CMake module for finding Eigen3 in ROS 2 packages"
   #:description
   "Thin ament_cmake helper that makes @code{find_package(Eigen3)}
available to downstream ROS 2 packages via @code{find_package
(eigen3_cmake_module)} + @code{ament_export_dependencies(Eigen3)}."))

(define-public ros-orocos-kdl-vendor-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "orocos_kdl_vendor"
   #:version "0.4.2"
   #:repo "https://github.com/ros2/orocos_kdl_vendor"
   #:commit "76ee1ce6c49a28a42c433a9b6fd20053605ccaf7"
   #:hash (base32 "150arbp2hbb8sm9klgi7gd2y57lvdhpwgh9i8fqidx2pakbmrp46")
   #:module-subdir "orocos_kdl_vendor"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-vendor-package-jazzy
                             ros-eigen3-cmake-module-jazzy
                             orocos-kinematics-dynamics
                             eigen)
   #:home-page "https://github.com/ros2/orocos_kdl_vendor"
   #:synopsis "ROS 2 vendor wrapper around Orocos KDL"
   #:description
   "Vendor wrapper around the upstream Orocos Kinematics and Dynamics
Library (KDL).  In guix-systole this resolves to the
@code{orocos-kinematics-dynamics} package shipped by upstream Guix; no
external project is built."))

(define-public ros-kdl-parser-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "kdl_parser"
   #:version "2.11.0"
   #:repo "https://github.com/ros/kdl_parser"
   #:commit "c6f0299344afd1afbeb442fe37e073be69dd5e47"
   #:hash (base32 "0wrczffl4418pp7dklhwnimlh1w1f9mz20f7ym8yr13qakjzkqhq")
   #:module-subdir "kdl_parser"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-orocos-kdl-vendor-jazzy
                             ros-rcutils-jazzy
                             ros-urdf-jazzy
                             urdfdom-headers)
   #:home-page "https://github.com/ros/kdl_parser"
   #:synopsis "Build KDL trees from URDF robot models"
   #:description
   "@code{kdl_parser} converts URDF robot descriptions into KDL
@code{Tree} objects used by motion planners and kinematic solvers."))

;;;
;;; composition_interfaces / rclcpp_components / message_filters.
;;; composition_interfaces is a sibling of rcl_interfaces in the
;;; ros2/rcl_interfaces monorepo, deferred from Phase 1.  rclcpp_components
;;; was deferred from the Phase 1 rclcpp tier because it needs class_loader
;;; and composition_interfaces.
;;;

(define-public ros-composition-interfaces-jazzy
  (rcl-interfaces-msg
   "composition_interfaces"
   #:message-deps (list ros-rcl-interfaces-jazzy
                        ros-service-msgs-jazzy)
   #:synopsis "Node composition (load/unload component) service definitions"
   #:description
   "Service definitions used by @code{rclcpp_components} to manage
loading, listing, and unloading composable nodes at runtime."))

(define-public ros-rclcpp-components-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rclcpp_components"
   #:version %rclcpp-version
   #:repo %rclcpp-repo
   #:commit %rclcpp-commit
   #:hash %rclcpp-hash
   #:module-subdir "rclcpp_components"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-ament-index-cpp-jazzy
                             ros-class-loader-jazzy
                             ros-composition-interfaces-jazzy
                             ros-rclcpp-jazzy
                             ros-rcpputils-jazzy)
   #:home-page "https://github.com/ros2/rclcpp"
   #:synopsis "Composable-node loader for rclcpp"
   #:description
   "@code{rclcpp_components} provides the runtime machinery for
loading and unloading ROS 2 nodes as plugins at runtime via
@code{class_loader}, exposed through the
@file{composition_interfaces} service API."))

(define-public ros-message-filters-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "message_filters"
   #:version "4.11.12"
   #:repo "https://github.com/ros2/message_filters"
   #:commit "b9bca323eaf110a3e2f5b674f7be593de444159d"
   #:hash (base32 "1jzm8xqvqk09hgy6agkhks41s3m3kfxiy4w6c66wimxxw5vlyxsl")
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-ament-cmake-python-jazzy
                             ros-python-cmake-module-jazzy
                             ros-rclcpp-jazzy
                             ros-rcutils-jazzy
                             ros-std-msgs-jazzy
                             ros-builtin-interfaces-jazzy
                             ros-rclpy-jazzy)
   #:home-page "https://github.com/ros2/message_filters"
   #:synopsis "Incoming-message queue filters (synchronizer, cache, ...)"
   #:description
   "@code{message_filters} provides filters that operate on ROS 2
subscription message streams: exact/approximate time synchronisers,
caches, chainable filters.  Used by @code{tf2_ros} and many
perception/fusion pipelines."))

;;;
;;; tf2 stack (ros2/geometry2@dc13549).
;;;

(define %geometry2-repo "https://github.com/ros2/geometry2")
(define %geometry2-commit "dc13549ede532461d82cc1f59565c1582e98c830")
(define %geometry2-hash
  (base32 "10g82w0nwfyjdyfa5yl6qkhrz3g7rcvmchbrp1m5r1iixqwfjzhw"))
(define %geometry2-version "0.36.20")

(define* (geometry2-subpkg ros-name #:key
                           (propagated-inputs '())
                           synopsis description)
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name ros-name
   #:version %geometry2-version
   #:repo %geometry2-repo
   #:commit %geometry2-commit
   #:hash %geometry2-hash
   #:module-subdir ros-name
   #:propagated-inputs propagated-inputs
   #:home-page %geometry2-repo
   #:synopsis synopsis
   #:description description))

(define-public ros-tf2-msgs-jazzy
  (make-ros2-rosidl-interface-package
   #:distro jazzy-distro
   #:ros-name "tf2_msgs"
   #:version %geometry2-version
   #:repo %geometry2-repo
   #:commit %geometry2-commit
   #:hash %geometry2-hash
   #:module-subdir "tf2_msgs"
   #:message-deps (list ros-rosidl-default-generators-jazzy
                        ros-rosidl-default-runtime-jazzy
                        ros-builtin-interfaces-jazzy
                        ros-geometry-msgs-jazzy
                        ros-action-msgs-jazzy
                        ros-service-msgs-jazzy)
   #:home-page %geometry2-repo
   #:synopsis "Transform tree message types"
   #:description
   "Message and service definitions used by @code{tf2_ros} to
publish, lookup, and transform coordinate-frame data on the ROS 2
transform graph."))

(define-public ros-tf2-jazzy
  (geometry2-subpkg
   "tf2"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-builtin-interfaces-jazzy
                             ros-console-bridge-vendor-jazzy
                             ros-geometry-msgs-jazzy
                             ros-rcutils-jazzy
                             ros-rosidl-runtime-cpp-jazzy)
   #:synopsis "Core C++ transform library"
   #:description
   "Core C++ transform library: maintains a buffer of coordinate-frame
transforms and answers queries for the transform between two frames at
a given time."))

(define-public ros-tf2-eigen-kdl-jazzy
  (geometry2-subpkg
   "tf2_eigen_kdl"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-eigen3-cmake-module-jazzy
                             ros-orocos-kdl-vendor-jazzy
                             ros-tf2-jazzy
                             eigen)
   #:synopsis "Conversions between Eigen and KDL geometric types"
   #:description
   "Conversion utilities between Eigen and Orocos KDL geometric
types."))

(define-public ros-tf2-ros-jazzy
  (geometry2-subpkg
   "tf2_ros"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-builtin-interfaces-jazzy
                             ros-geometry-msgs-jazzy
                             ros-message-filters-jazzy
                             ros-rcl-interfaces-jazzy
                             ros-rclcpp-jazzy
                             ros-rclcpp-action-jazzy
                             ros-rclcpp-components-jazzy
                             ros-tf2-jazzy
                             ros-tf2-msgs-jazzy)
   #:synopsis "ROS 2 client interface for the tf2 transform library"
   #:description
   "C++ publisher/listener API for the ROS 2 transform graph on top of
@code{tf2}, plus the @code{static_transform_publisher} executable."))

(define-public ros-tf2-eigen-jazzy
  (geometry2-subpkg
   "tf2_eigen"
   ;; tf2_eigen's CMakeLists find_package()s tf2_ros REQUIRED even
   ;; though its package.xml only declares tf2; propagate tf2_ros.
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-eigen3-cmake-module-jazzy
                             ros-geometry-msgs-jazzy
                             ros-tf2-jazzy
                             ros-tf2-ros-jazzy
                             eigen)
   #:synopsis "Eigen <-> tf2 conversions"
   #:description
   "Conversion utilities between @code{tf2} transforms and Eigen
geometric types."))

(define-public ros-tf2-geometry-msgs-jazzy
  (geometry2-subpkg
   "tf2_geometry_msgs"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-python-jazzy
                             ros-python-cmake-module-jazzy
                             ros-geometry-msgs-jazzy
                             ros-orocos-kdl-vendor-jazzy
                             ros-tf2-jazzy
                             ros-tf2-ros-jazzy)
   #:synopsis "Conversions between tf2 transforms and geometry_msgs types"
   #:description
   "Glue code that converts between @code{tf2}'s internal transform
types and @file{geometry_msgs/PointStamped}, @file{PoseStamped},
@file{Vector3Stamped}, etc."))

(define-public ros-tf2-kdl-jazzy
  (geometry2-subpkg
   "tf2_kdl"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-builtin-interfaces-jazzy
                             ros-geometry-msgs-jazzy
                             ros-orocos-kdl-vendor-jazzy
                             ros-tf2-jazzy
                             ros-tf2-ros-jazzy)
   #:synopsis "Conversions between tf2 transforms and KDL types"
   #:description
   "Glue code that converts between @code{tf2} internal transforms and
Orocos KDL geometric types."))

(define-public ros-tf2-py-jazzy
  (geometry2-subpkg
   "tf2_py"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-python-jazzy
                             ros-python-cmake-module-jazzy
                             ros-pybind11-vendor-jazzy
                             ros-tf2-jazzy
                             ros-builtin-interfaces-jazzy
                             ros-geometry-msgs-jazzy
                             ros-rclpy-jazzy
                             ros-rpyutils-jazzy)
   #:synopsis "Python bindings for tf2"
   #:description
   "pybind11-based Python bindings for @code{tf2}."))

(define-public ros-tf2-sensor-msgs-jazzy
  (geometry2-subpkg
   "tf2_sensor_msgs"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-python-jazzy
                             ros-python-cmake-module-jazzy
                             ros-eigen3-cmake-module-jazzy
                             ros-geometry-msgs-jazzy
                             ros-sensor-msgs-jazzy
                             ros-std-msgs-jazzy
                             ros-tf2-jazzy
                             ros-tf2-ros-jazzy
                             eigen)
   #:synopsis "Conversions between tf2 transforms and sensor_msgs types"
   #:description
   "Glue that transforms @file{sensor_msgs/PointCloud2} and related
sensor messages between coordinate frames using @code{tf2}."))

(define-public ros-tf2-ros-py-jazzy
  (make-ros2-ament-python-package
   #:distro jazzy-distro
   #:ros-name "tf2_ros_py"
   #:version %geometry2-version
   #:repo %geometry2-repo
   #:commit %geometry2-commit
   #:hash %geometry2-hash
   #:module-subdir "tf2_ros_py"
   #:propagated-inputs (list ros-builtin-interfaces-jazzy
                             ros-geometry-msgs-jazzy
                             ros-rclpy-jazzy
                             ros-sensor-msgs-jazzy
                             ros-std-msgs-jazzy
                             ros-tf2-msgs-jazzy
                             ros-tf2-py-jazzy)
   #:home-page %geometry2-repo
   #:synopsis "Python tf2 ROS 2 client library"
   #:description
   "Python client library for publishing and listening to the ROS 2
transform graph, built on top of @code{tf2_py}."))

;;;
;;; diagnostic_updater + robot_state_publisher.
;;;

(define-public ros-diagnostic-updater-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "diagnostic_updater"
   #:version "4.2.6"
   #:repo "https://github.com/ros/diagnostics"
   #:commit "2c0e15b9d0441750ea8e51ddeef8f28e4c1ee090"
   #:hash (base32 "0c8a31fk244l1dpmgx2nc1l5815i6syw6w92nl1djm25nwj4bs3i")
   #:module-subdir "diagnostic_updater"
   #:propagated-inputs (list ros-ament-cmake-ros-jazzy
                             ros-ament-cmake-python-jazzy
                             ros-diagnostic-msgs-jazzy
                             ros-rclcpp-jazzy
                             ros-rclpy-jazzy
                             ros-std-msgs-jazzy)
   #:home-page "https://github.com/ros/diagnostics"
   #:synopsis "Helper for publishing ROS 2 diagnostic_msgs/DiagnosticStatus"
   #:description
   "@code{diagnostic_updater} is a C++/Python helper for publishing
@file{diagnostic_msgs/DiagnosticStatus} at a regular rate, used by
nodes that report driver/sensor health."))

(define-public ros-robot-state-publisher-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "robot_state_publisher"
   #:version "3.3.3"
   #:repo "https://github.com/ros/robot_state_publisher"
   #:commit "bca90e6d30ba69da80bd5ec601bcee502040e587"
   #:hash (base32 "0vn3n5dczxc7nyg3iz1za1b1gyyqdynybg29v65l10m6sncmxl6a")
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-builtin-interfaces-jazzy
                             ros-geometry-msgs-jazzy
                             ros-kdl-parser-jazzy
                             ros-orocos-kdl-vendor-jazzy
                             ros-rcl-interfaces-jazzy
                             ros-rclcpp-jazzy
                             ros-rclcpp-components-jazzy
                             ros-sensor-msgs-jazzy
                             ros-std-msgs-jazzy
                             ros-tf2-ros-jazzy
                             ros-urdf-jazzy)
   #:home-page "https://github.com/ros/robot_state_publisher"
   #:synopsis "Publish forward kinematics of a URDF robot to /tf"
   #:description
   "@code{robot_state_publisher} reads a URDF model and publishes the
corresponding forward-kinematic transforms to the @code{tf2} graph as
joint states change.  Essential for any mobile-manipulator or
multi-joint robot in ROS 2."))

;;;
;;; launch framework (ros2/launch + ros2/launch_ros).
;;;

(define %launch-repo "https://github.com/ros2/launch")
(define %launch-commit "587b6998fb830ba92ba1ff7fee8069f47b36b7b9")
(define %launch-hash
  (base32 "17pl9lihd8x6p2r7bgylzjcx91nmlb5d2p6kqgjpdm5jr9vappq4"))
(define %launch-version "3.4.10")

(define* (launch-py-subpkg ros-name #:key
                           (propagated-inputs '())
                           synopsis description)
  (make-ros2-ament-python-package
   #:distro jazzy-distro
   #:ros-name ros-name
   #:version %launch-version
   #:repo %launch-repo
   #:commit %launch-commit
   #:hash %launch-hash
   #:module-subdir ros-name
   #:propagated-inputs propagated-inputs
   #:home-page %launch-repo
   #:synopsis synopsis
   #:description description))

(define-public ros-launch-jazzy
  (launch-py-subpkg
   "launch"
   #:propagated-inputs (list ros-ament-index-python-jazzy
                             python-osrf-pycommon
                             python-lark
                             python-importlib-metadata
                             python-pyyaml)
   #:synopsis "ROS 2 launch system core"
   #:description
   "@code{launch} is the ROS 2 declarative launch framework.  It
describes process-launch graphs as Python actions/substitutions and
drives them from a single @code{launch} call."))

(define-public ros-launch-xml-jazzy
  (launch-py-subpkg
   "launch_xml"
   #:propagated-inputs (list ros-launch-jazzy)
   #:synopsis "XML frontend for ROS 2 launch files"
   #:description
   "Enables the ROS 2 launch system to consume @file{.launch.xml}
files in addition to Python launch descriptions."))

(define-public ros-launch-yaml-jazzy
  (launch-py-subpkg
   "launch_yaml"
   #:propagated-inputs (list ros-launch-jazzy)
   #:synopsis "YAML frontend for ROS 2 launch files"
   #:description
   "Enables the ROS 2 launch system to consume @file{.launch.yaml}
files in addition to Python launch descriptions."))

(define-public ros-launch-testing-jazzy
  (launch-py-subpkg
   "launch_testing"
   #:propagated-inputs (list ros-ament-index-python-jazzy
                             ros-launch-jazzy
                             ros-launch-xml-jazzy
                             ros-launch-yaml-jazzy
                             python-osrf-pycommon
                             python-pytest)
   #:synopsis "pytest harness for ROS 2 launch-based tests"
   #:description
   "Utilities for writing launch-aware integration tests in ROS 2,
with pytest as the driver and the @code{launch} framework as the
process manager."))

(define-public ros-launch-pytest-jazzy
  (launch-py-subpkg
   "launch_pytest"
   #:propagated-inputs (list ros-ament-index-python-jazzy
                             ros-launch-jazzy
                             ros-launch-testing-jazzy
                             python-osrf-pycommon
                             python-pytest)
   #:synopsis "pytest plugin that runs launch descriptions as fixtures"
   #:description
   "pytest plugin offered by the ROS 2 launch framework for describing
launch descriptions as test fixtures."))

(define-public ros-launch-testing-ament-cmake-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "launch_testing_ament_cmake"
   #:version %launch-version
   #:repo %launch-repo
   #:commit %launch-commit
   #:hash %launch-hash
   #:module-subdir "launch_testing_ament_cmake"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-ament-cmake-test-jazzy
                             ros-launch-testing-jazzy
                             ros-python-cmake-module-jazzy)
   #:home-page %launch-repo
   #:synopsis "ament_cmake integration for launch_testing"
   #:description
   "CMake macros (@code{add_launch_test}) that register
@code{launch_testing} pytest-based integration tests with the ament
test framework."))

;;; ros2/launch_ros — ROS-aware launch actions (Node, LifecycleNode, ...).

(define-public ros-launch-ros-jazzy
  (make-ros2-ament-python-package
   #:distro jazzy-distro
   #:ros-name "launch_ros"
   #:version "0.26.11"
   #:repo "https://github.com/ros2/launch_ros"
   #:commit "a4580bd3dccf08f93871a32e14bf750c24fb780d"
   #:hash (base32 "1yyfbcdz145kh82hjaapy4pxl76jf2clpry8y23cz66wnavc2p06")
   #:module-subdir "launch_ros"
   #:propagated-inputs (list ros-ament-index-python-jazzy
                             ros-composition-interfaces-jazzy
                             ros-launch-jazzy
                             ros-lifecycle-msgs-jazzy
                             ros-rclpy-jazzy
                             python-importlib-metadata
                             python-osrf-pycommon
                             python-pyyaml)
   #:home-page "https://github.com/ros2/launch_ros"
   #:synopsis "ROS 2-aware launch actions (Node, LifecycleNode, ...)"
   #:description
   "Extends @code{launch} with ROS-aware actions for spawning ROS 2
nodes, lifecycle nodes, and composable components."))

;;;
;;; Aggregation meta-package.
;;;
;;; Phase 1 will grow this package's propagated-inputs tier by tier until
;;; it pulls in all of `ros_core'.  For now it is an empty meta that
;;; carries the native-search-paths so downstream profiles are correctly
;;; configured from day one.

;;;
;;; Phase 1 ros_core package list — every public ROS 2 Jazzy package
;;; defined above.  ros-jazzy propagates this set so installing the
;;; meta-package gives you a complete usable ROS 2 environment.

(define %ros-core-jazzy-packages
  (list
   ;; ament tooling
   ros-ament-package-jazzy
   ros-ament-cmake-core-jazzy
   ros-ament-cmake-export-definitions-jazzy
   ros-ament-cmake-export-dependencies-jazzy
   ros-ament-cmake-export-include-directories-jazzy
   ros-ament-cmake-export-interfaces-jazzy
   ros-ament-cmake-export-libraries-jazzy
   ros-ament-cmake-export-link-flags-jazzy
   ros-ament-cmake-export-targets-jazzy
   ros-ament-cmake-include-directories-jazzy
   ros-ament-cmake-libraries-jazzy
   ros-ament-cmake-target-dependencies-jazzy
   ros-ament-cmake-version-jazzy
   ros-ament-cmake-python-jazzy
   ros-ament-cmake-gtest-jazzy
   ros-ament-cmake-gmock-jazzy
   ros-ament-cmake-test-jazzy
   ros-ament-cmake-pytest-jazzy
   ros-ament-cmake-gen-version-h-jazzy
   ros-ament-cmake-vendor-package-jazzy
   ros-ament-cmake-jazzy
   ros-ament-index-python-jazzy
   ros-ament-index-cpp-jazzy
   ros-domain-coordinator-jazzy
   ros-ament-cmake-ros-jazzy

   ;; core C/C++ utility libraries
   ros-rcutils-jazzy
   ros-rcpputils-jazzy

   ;; rosidl pipeline
   ros-rosidl-typesupport-interface-jazzy
   ros-rosidl-runtime-c-jazzy
   ros-rosidl-runtime-cpp-jazzy
   ros-rosidl-cli-jazzy
   ros-rosidl-adapter-jazzy
   ros-rosidl-parser-jazzy
   ros-rosidl-pycommon-jazzy
   ros-rosidl-cmake-jazzy
   ros-rosidl-generator-type-description-jazzy
   ros-rosidl-generator-c-jazzy
   ros-rosidl-generator-cpp-jazzy
   ros-rosidl-typesupport-introspection-c-jazzy
   ros-rosidl-typesupport-introspection-cpp-jazzy
   ros-rosidl-dynamic-typesupport-jazzy
   ros-rosidl-typesupport-c-jazzy
   ros-rosidl-typesupport-cpp-jazzy
   ros-rosidl-core-generators-jazzy
   ros-rosidl-core-runtime-jazzy
   ros-rosidl-default-generators-jazzy
   ros-rosidl-default-runtime-jazzy

   ;; rmw + DDS middleware
   ros-rmw-jazzy
   ros-rmw-implementation-cmake-jazzy
   ros-rmw-dds-common-jazzy
   ros-tracetools-jazzy
   ros-rmw-cyclonedds-cpp-jazzy
   ros-rmw-implementation-jazzy

   ;; base interface messages
   ros-builtin-interfaces-jazzy
   ros-service-msgs-jazzy
   ros-type-description-interfaces-jazzy
   ros-rosgraph-msgs-jazzy
   ros-rcl-interfaces-jazzy
   ros-statistics-msgs-jazzy
   ros-lifecycle-msgs-jazzy
   ros-unique-identifier-msgs-jazzy
   ros-action-msgs-jazzy
   ros-std-msgs-jazzy

   ;; rcl tier
   ros-libyaml-vendor-jazzy
   ros-rcl-yaml-param-parser-jazzy
   ros-rcl-logging-interface-jazzy
   ros-rcl-logging-noop-jazzy
   ros-rcl-jazzy
   ros-rcl-lifecycle-jazzy
   ros-rcl-action-jazzy

   ;; rclcpp tier
   ros-libstatistics-collector-jazzy
   ros-rclcpp-jazzy
   ros-rclcpp-lifecycle-jazzy
   ros-rclcpp-action-jazzy

   ;; rclpy tier
   ros-python-cmake-module-jazzy
   ros-pybind11-vendor-jazzy
   ros-rpyutils-jazzy
   ros-rosidl-generator-py-jazzy
   ros-rclpy-jazzy))

;;;
;;; Phase 2 ros_base package list — layered on top of ros_core.
;;;

(define %ros-base-jazzy-packages
  (list
   ;; common_interfaces messages (std_msgs is already in ros_core)
   ros-std-srvs-jazzy
   ros-geometry-msgs-jazzy
   ros-actionlib-msgs-jazzy
   ros-shape-msgs-jazzy
   ros-trajectory-msgs-jazzy
   ros-sensor-msgs-jazzy
   ros-stereo-msgs-jazzy
   ros-nav-msgs-jazzy
   ros-diagnostic-msgs-jazzy
   ros-visualization-msgs-jazzy

   ;; plugin framework
   ros-tinyxml2-vendor-jazzy
   ros-console-bridge-vendor-jazzy
   ros-class-loader-jazzy
   ros-pluginlib-jazzy

   ;; URDF
   ros-urdf-parser-plugin-jazzy
   ros-urdf-jazzy

   ;; KDL
   ros-eigen3-cmake-module-jazzy
   ros-orocos-kdl-vendor-jazzy
   ros-kdl-parser-jazzy

   ;; composition / message_filters (deferred from Phase 1)
   ros-composition-interfaces-jazzy
   ros-rclcpp-components-jazzy
   ros-message-filters-jazzy

   ;; tf2 stack
   ros-tf2-msgs-jazzy
   ros-tf2-jazzy
   ros-tf2-ros-jazzy
   ros-tf2-eigen-jazzy
   ros-tf2-eigen-kdl-jazzy
   ros-tf2-geometry-msgs-jazzy
   ros-tf2-kdl-jazzy
   ros-tf2-py-jazzy
   ros-tf2-sensor-msgs-jazzy
   ros-tf2-ros-py-jazzy

   ;; diagnostics + robot_state_publisher
   ros-diagnostic-updater-jazzy
   ros-robot-state-publisher-jazzy))

;;;
;;; ros-jazzy aggregation meta-package.
;;;
;;; Implemented as a trivial-build-system package that symlinks every
;;; ros_core component into a single output directory via union-build.
;;; This matters for UX: we intentionally do NOT use `propagated-inputs'
;;; because Guix's `guix shell' builds the profile via hooks that call
;;; `manifest-inputs', which walks the propagated-inputs DAG without
;;; memoization.  For a dense graph like ros_core, that visit is
;;; quadratic-ish in the number of edges and produces hundreds of MB of
;;; allocations per hook invocation — `guix shell ros-jazzy' turns into
;;; a multi-minute, multi-GB ordeal.
;;;
;;; By shipping ros-jazzy as a single store output (a union of all 80
;;; component prefixes), the profile manifest has exactly one entry for
;;; ros-jazzy and the hooks walk it once.  The downside is an extra
;;; copy-on-build step, but union-build is symlink-only and runs in
;;; seconds.  Individual components remain independently installable
;;; (ros-rclcpp-jazzy, ros-rclpy-jazzy, ...) for users who don't want
;;; the full stack.

(define-public ros-jazzy
  (package
    (name "ros-jazzy")
    (version "0.1.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build union)
                       (guix build utils)
                       (srfi srfi-1))
           #:builder
           (with-imported-modules '((guix build union)
                                    (guix build utils))
             #~(begin
                 (use-modules (guix build union)
                              (guix build utils)
                              (srfi srfi-1)
                              (ice-9 match))
                 (let ((out #$output)
                       (inputs (map cdr %build-inputs)))
                   (union-build out inputs
                                #:create-all-directories? #t)
                   #t)))))
    ;; inputs = direct ros_core + ros_base packages plus the union of
    ;; their transitive propagated-inputs, so the resulting union
    ;; contains every runtime dependency (python, numpy, pyyaml, eigen,
    ;; orocos-kinematics-dynamics, urdfdom, ..., glibc, gcc-lib) of each
    ;; ROS component.  Deduplicated by package object.
    (inputs
     (let ((direct (append %ros-core-jazzy-packages
                           %ros-base-jazzy-packages)))
       (delete-duplicates
        (append direct
                (append-map
                 (lambda (pkg)
                   (map (match-lambda ((_ p . _) p))
                        (package-transitive-propagated-inputs pkg)))
                 direct))
        eq?)))
    (native-search-paths (ros2-native-search-paths jazzy-distro))
    (synopsis "ROS 2 Jazzy Jalisco meta-package (guix-systole)")
    (description
     "Aggregation meta-package for the ROS 2 Jazzy Jalisco distribution
provided by the guix-systole channel.  Installing @code{ros-jazzy}
yields a single store output that is a symlink-union of the full
@code{ros_base} stack: all of @code{ros_core} (ament build tooling, the
rosidl C/C++/Python interface generators, the rmw middleware abstraction
plus an Eclipse Cyclone DDS implementation, @code{rcl}, @code{rclcpp},
@code{rclpy}) plus the standard @code{ros2/common_interfaces} message
families (@code{geometry_msgs}, @code{sensor_msgs}, @code{nav_msgs},
@code{std_srvs}, ...), the URDF parser, the Orocos KDL kinematics
stack, @code{pluginlib}, the @code{tf2} transform graph,
@code{rclcpp_components}, @code{diagnostic_updater}, and
@code{robot_state_publisher}.

The package configures the relevant search paths
(@env{AMENT_PREFIX_PATH}, @env{CMAKE_PREFIX_PATH},
@env{ROS_PACKAGE_PATH}, @env{PYTHONPATH}, @env{LD_LIBRARY_PATH}) so
downstream packages discover ROS components via the Guix profile.

Set @env{RMW_IMPLEMENTATION=rmw_cyclonedds_cpp} to select the bundled
DDS middleware (this is currently the only @code{rmw} implementation
provided by guix-systole).")
    (home-page "https://docs.ros.org/en/jazzy/")
    (license license:asl2.0)))
