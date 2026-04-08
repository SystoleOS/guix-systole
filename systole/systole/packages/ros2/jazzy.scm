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
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)   ; python-empy, python-pyyaml, python-lark
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
