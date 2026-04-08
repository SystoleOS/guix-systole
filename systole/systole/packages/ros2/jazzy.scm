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

(define-public ros-ament-cmake-core-jazzy
  (make-ros2-ament-cmake-package
   #:distro jazzy-distro
   #:ros-name "ament_cmake_core"
   #:version "2.6.4"
   #:repo "https://github.com/ament/ament_cmake"
   #:commit "5741cff5b9f83253bf3521bd8f44108fde3504ad"
   #:hash (base32 "0sysj073m00i2ji18kr3lg0jvzcmqljyrs640gqg36insql8zvk4")
   #:module-subdir "ament_cmake_core"
   #:propagated-inputs (list ros-ament-package-jazzy)
   #:home-page "https://github.com/ament/ament_cmake"
   #:synopsis "Core CMake helpers for ROS 2 ament builds"
   #:description
   "@code{ament_cmake_core} provides the foundational CMake macros used
by every other ament_cmake-based ROS 2 package: package registration in
the AMENT resource index, environment-hook installation, and helpers for
declaring exported targets, dependencies, and include directories."))

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
