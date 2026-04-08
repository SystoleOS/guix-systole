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
  #:use-module (gnu packages python)
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
