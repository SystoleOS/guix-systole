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
  #:use-module (gnu packages check)        ; python-pytest
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)   ; python-empy, python-pyyaml, python-lark
  #:use-module (gnu packages serialization) ; libyaml
  #:use-module (systole packages ros2)
  #:use-module (systole packages ros2-helpers)
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

(define-public ros-rosidl-core-generators-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "rosidl_core_generators"
   #:version %rosidl-core-version
   #:repo %rosidl-core-repo
   #:commit %rosidl-core-commit
   #:hash %rosidl-core-hash
   #:module-subdir "rosidl_core_generators"
   #:propagated-inputs (list ros-ament-cmake-jazzy
                             ros-rosidl-cmake-jazzy
                             ros-rosidl-generator-c-jazzy
                             ros-rosidl-generator-cpp-jazzy
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

(define-public ros-std-msgs-jazzy
  (make-ros2-rosidl-interface-package
   #:distro jazzy-distro
   #:ros-name "std_msgs"
   #:version "5.4.0"
   #:repo "https://github.com/ros2/common_interfaces"
   #:commit "81e2f6baa6eb9ac734d4c174dfd231b54d5fa1ef"
   #:hash (base32 "0ni0x7fm0fxcyvvmj479s656fnrynhn4a6whfy3d272fni2kvzd4")
   #:module-subdir "std_msgs"
   #:message-deps (list ros-rosidl-default-generators-jazzy
                        ros-rosidl-default-runtime-jazzy
                        ros-builtin-interfaces-jazzy)
   #:home-page "https://github.com/ros2/common_interfaces"
   #:synopsis "Standard primitive message types for ROS 2"
   #:description
   "Standard messages: @code{String}, @code{Bool}, @code{Int32},
@code{Float64}, @code{Header}, etc.  These are the most commonly used
message types in ROS 2 demonstration code."))

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
;;; Aggregation meta-package.
;;;
;;; Phase 1 will grow this package's propagated-inputs tier by tier until
;;; it pulls in all of `ros_core'.  For now it is an empty meta that
;;; carries the native-search-paths so downstream profiles are correctly
;;; configured from day one.

(define-public ros-jazzy
  (package
    (name "ros-jazzy")
    (version "0.0.0-dev")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:builder
           #~(begin
               (mkdir #$output)
               #t)))
    (native-search-paths (ros2-native-search-paths jazzy-distro))
    (synopsis "ROS 2 Jazzy Jalisco meta-package (guix-systole)")
    (description
     "Aggregation meta-package for the ROS 2 Jazzy Jalisco distribution
provided by the guix-systole channel.  Installing this package pulls in
the full @code{ros_core} stack (and eventually @code{ros_base}) and
configures the relevant search paths (@env{AMENT_PREFIX_PATH},
@env{CMAKE_PREFIX_PATH}, @env{ROS_PACKAGE_PATH}, @env{PYTHONPATH},
@env{LD_LIBRARY_PATH}) so downstream packages discover ROS components via
the Guix profile.")
    (home-page "https://docs.ros.org/en/jazzy/")
    (license license:asl2.0)))
