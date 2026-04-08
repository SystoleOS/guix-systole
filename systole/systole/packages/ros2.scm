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
;;; Distro-agnostic infrastructure for packaging ROS 2 in guix-systole.
;;;
;;; Per-distro data (jazzy, humble, ...) lives in dedicated sub-modules such
;;; as (systole packages ros2 jazzy).  This module provides:
;;;
;;;   * <ros-distro>             — record describing a ROS 2 release
;;;   * ros-package-name         — symbol naming convention helper
;;;   * make-ros2-ament-cmake-package
;;;   * make-ros2-ament-python-package
;;;   * make-ros2-rosidl-interface-package
;;;
;;; The factories mirror (in spirit) the Slicer module factories in
;;; (systole packages slicer): each produces a self-contained Guix package
;;; that builds a single ROS 2 component, discovering siblings via
;;; inputs / propagated-inputs / cmake -D flags rather than through a
;;; colcon/ament workspace build.
;;;
;;; Code:

(define-module (systole packages ros2)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (gnu packages python)
  #:use-module (systole packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<ros-distro>
            make-ros-distro
            ros-distro?
            ros-distro-name
            ros-distro-suffix
            ros-distro-python
            ros-distro-repos-snapshot
            ros-distro-patch-subdir
            ros-distro-common-patch-subdir

            ros-package-name
            ros2-git-origin
            ros2-native-search-paths

            make-ros2-ament-cmake-package
            make-ros2-ament-python-package
            make-ros2-rosidl-interface-package))

;;;
;;; <ros-distro> — describes one ROS 2 release.
;;;
;;; Fields:
;;;   name                  string, e.g. "jazzy"
;;;   suffix                string used in public package names, e.g. "jazzy"
;;;   python                Guix python package used by this distro
;;;   repos-snapshot        commit SHA of ros2/ros2 at the distro's branch,
;;;                         used as the lockfile for per-package commits
;;;   patch-subdir          distro-specific patch subdir under systole-patches,
;;;                         e.g. "ros2/jazzy"
;;;   common-patch-subdir   cross-distro patch subdir, e.g. "ros2/common"

(define-record-type <ros-distro>
  (%make-ros-distro name suffix python repos-snapshot
                    patch-subdir common-patch-subdir)
  ros-distro?
  (name                ros-distro-name)
  (suffix              ros-distro-suffix)
  (python              ros-distro-python)
  (repos-snapshot      ros-distro-repos-snapshot)
  (patch-subdir        ros-distro-patch-subdir)
  (common-patch-subdir ros-distro-common-patch-subdir))

(define* (make-ros-distro #:key name suffix python repos-snapshot
                          patch-subdir
                          (common-patch-subdir "ros2/common"))
  "Construct a <ros-distro> record.  All fields are mandatory except
COMMON-PATCH-SUBDIR which defaults to \"ros2/common\"."
  (%make-ros-distro name suffix python repos-snapshot
                    patch-subdir common-patch-subdir))

;;;
;;; Naming convention.
;;;

(define (ros-package-name distro ros-name)
  "Build the guix-systole public package name for ROS-NAME in DISTRO.
E.g. (ros-package-name jazzy-distro \"rcutils\") => \"ros-rcutils-jazzy\"."
  (string-append "ros-" ros-name "-" (ros-distro-suffix distro)))

;;;
;;; Source helper.
;;;

(define* (ros2-git-origin #:key repo commit hash (patches '()))
  "Return an <origin> for a ROS 2 component fetched via git.
PATCHES is a list of absolute file names (already resolved via
search-patch); callers typically pass the output of search-patches."
  (origin
    (method git-fetch)
    (uri (git-reference (url repo) (commit commit)))
    (file-name (git-file-name "ros2-source" commit))
    (sha256 hash)
    (patches patches)))

;;;
;;; Shared native-search-paths for a distro's aggregation meta-package.
;;;

(define (ros2-native-search-paths distro)
  "Return the list of <search-path-specification> to attach to DISTRO's
aggregation meta-package."
  (let* ((python (ros-distro-python distro))
         (py-site (string-append "lib/python"
                                 (version-major+minor
                                  (package-version python))
                                 "/site-packages")))
    (list (search-path-specification
           (variable "AMENT_PREFIX_PATH")
           (files '(".")))
          (search-path-specification
           (variable "CMAKE_PREFIX_PATH")
           (files '(".")))
          (search-path-specification
           (variable "ROS_PACKAGE_PATH")
           (files '("share")))
          (search-path-specification
           (variable "PYTHONPATH")
           (files (list py-site)))
          (search-path-specification
           (variable "LD_LIBRARY_PATH")
           (files '("lib"))))))

;;;
;;; Factory: ament_cmake package.
;;;

(define* (make-ros2-ament-cmake-package
          #:key
          distro                        ; <ros-distro>
          ros-name                      ; upstream package name, e.g. "rcutils"
          version                       ; version string for the Guix package
          repo commit hash              ; git source
          synopsis description home-page
          (license license:asl2.0)      ; ROS 2 is Apache-2.0 except noted
          (module-subdir #f)            ; optional sub-directory of REPO
          (patches '())                 ; patch file names (relative)
          (extra-inputs '())
          (extra-native-inputs '())
          (extra-configure-flags #~'())
          (propagated-inputs '())
          (tests? #f))
  "Build a ROS 2 ament_cmake component as a standalone cmake package.

When MODULE-SUBDIR is given, only that subtree of REPO is configured
(mirroring Slicer's make-slicer-loadable-module pattern for sub-packages
of monorepos such as ament_cmake or rcl_interfaces)."
  (package
    (name (ros-package-name distro ros-name))
    (version version)
    (source (ros2-git-origin #:repo repo #:commit commit #:hash hash
                             #:patches (map search-patch patches)))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? tests?
           #:configure-flags
           #~(append
              (list "-DCMAKE_BUILD_TYPE=Release"
                    "-DBUILD_TESTING=OFF")
              #$extra-configure-flags)
           #:phases
           #~(modify-phases %standard-phases
               #$@(if module-subdir
                      #~((replace 'configure
                           (lambda* (#:key outputs configure-flags
                                     #:allow-other-keys)
                             (let ((source (getcwd))
                                   (out (assoc-ref outputs "out")))
                               (apply invoke "cmake"
                                      "-S" (string-append source "/"
                                                          #$module-subdir)
                                      "-B" "build"
                                      (string-append "-DCMAKE_INSTALL_PREFIX="
                                                     out)
                                      configure-flags)
                               (chdir "build")))))
                      #~()))))
    (native-inputs extra-native-inputs)
    (inputs extra-inputs)
    (propagated-inputs propagated-inputs)
    (synopsis synopsis)
    (description description)
    (home-page home-page)
    (license license)))

;;;
;;; Factory: pure-Python ament package.
;;;

(define* (make-ros2-ament-python-package
          #:key
          distro
          ros-name version
          repo commit hash
          synopsis description home-page
          (license license:asl2.0)
          (module-subdir #f)
          (patches '())
          (build-system-variant 'pyproject) ; or 'python (setup.py only)
          (extra-inputs '())
          (extra-native-inputs '())
          (propagated-inputs '()))
  "Build a pure-Python ROS 2 package (e.g. ament_package, rosidl_adapter)."
  (package
    (name (ros-package-name distro ros-name))
    (version version)
    (source (ros2-git-origin #:repo repo #:commit commit #:hash hash
                             #:patches (map search-patch patches)))
    (build-system (if (eq? build-system-variant 'pyproject)
                      pyproject-build-system
                      python-build-system))
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               #$@(if module-subdir
                      #~((add-after 'unpack 'chdir
                           (lambda _
                             (chdir #$module-subdir))))
                      #~()))))
    (native-inputs extra-native-inputs)
    (inputs extra-inputs)
    (propagated-inputs propagated-inputs)
    (synopsis synopsis)
    (description description)
    (home-page home-page)
    (license license)))

;;;
;;; Factory: rosidl interface package (.msg/.srv/.action).
;;;

(define* (make-ros2-rosidl-interface-package
          #:key
          distro
          ros-name version
          repo commit hash
          synopsis description home-page
          (license license:asl2.0)
          (module-subdir #f)
          (patches '())
          (message-deps '())            ; sibling interface packages
          (extra-inputs '())
          (extra-native-inputs '())
          (extra-configure-flags #~'()))
  "Build a ROS 2 interface package whose sources are .msg/.srv/.action
files processed by the rosidl generators at build time.  MESSAGE-DEPS are
interface packages this one depends on and are both propagated (for
downstream consumers) and added to inputs (for build-time header access)."
  (make-ros2-ament-cmake-package
   #:distro distro
   #:ros-name ros-name
   #:version version
   #:repo repo #:commit commit #:hash hash
   #:synopsis synopsis
   #:description description
   #:home-page home-page
   #:license license
   #:module-subdir module-subdir
   #:patches patches
   #:extra-inputs (append message-deps extra-inputs)
   #:extra-native-inputs extra-native-inputs
   #:extra-configure-flags extra-configure-flags
   #:propagated-inputs message-deps))
