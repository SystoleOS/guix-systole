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
;;; Non-distribution-specific helper packages required by the ROS 2
;;; tooling but not (yet) provided by upstream Guix.
;;;
;;; Code:

(define-module (systole packages ros2-helpers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)        ; openssl
  #:use-module (gnu packages time))

(define-public python-catkin-pkg
  (package
    (name "python-catkin-pkg")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "catkin_pkg" version))
       (sha256
        (base32 "0g5lpsld9nkd2282fnsyhns0iicgrq9nc2hh19vjwxrska3vc76z"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs
     (list python-docutils
           python-packaging
           python-pyparsing
           python-dateutil
           python-setuptools))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://wiki.ros.org/catkin_pkg")
    (synopsis "ROS catkin package library")
    (description
     "@code{catkin_pkg} provides Python tools for parsing and validating
ROS package manifests (@file{package.xml}).  It is required by the ROS 2
ament build system.")
    (license license:bsd-3)))

(define-public python-osrf-pycommon
  (package
    (name "python-osrf-pycommon")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "osrf_pycommon" version))
       (sha256
        (base32 "1w5dk65w4zq034gw76a96h3661174f1lp7z2k13d7ajrdrlhc8wi"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://osrf-pycommon.readthedocs.org/")
    (synopsis "Common Python modules used by OSRF software")
    (description
     "@code{osrf_pycommon} is a small collection of Python utilities used
by software developed at the Open Source Robotics Foundation, including
ROS 2 build tooling.")
    (license license:asl2.0)))

(define-public console-bridge
  ;; Tiny C++ logging shim used by class_loader/urdfdom/...  Not in
  ;; upstream Guix at the time of writing; track upstream master.
  (let ((commit "0828d846f2d4940b4e2b5075c6c724991d0cd308"))
    (package
      (name "console-bridge")
      (version "1.0.2")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url "https://github.com/ros/console_bridge")
                             (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "18rjjzkg1ml2p4aa41kvfgamkxc88g0iv3fd94vxr8917mqshw9k"))))
      (build-system cmake-build-system)
      (arguments
       (list #:tests? #f
             #:configure-flags
             #~(list "-DCMAKE_BUILD_TYPE=Release"
                     "-DBUILD_SHARED_LIBS=ON")))
      (home-page "https://github.com/ros/console_bridge")
      (synopsis "Lightweight C++ console-output abstraction")
      (description
       "@code{console_bridge} provides a tiny logging facade used by
ROS-adjacent libraries (notably @code{class_loader}, @code{urdfdom},
@code{kdl_parser}) so they can emit log messages without hard-coding a
specific logging back-end.")
      (license license:bsd-3))))

(define-public eclipse-cyclonedds
  ;; Pinned to ros2/jazzy's ros2.repos: branch releases/0.10.x.
  (let ((commit "5041f3560c088c99e5088b2b8520b69169621196"))
    (package
      (name "eclipse-cyclonedds")
      (version "0.10.5")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eclipse-cyclonedds/cyclonedds")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "0jj0l92v01rj236yig2xdvw841b8wrjxw6lhsz7y4cs78yad457r"))))
      (build-system cmake-build-system)
      (arguments
       (list #:tests? #f
             #:configure-flags
             #~(list "-DCMAKE_BUILD_TYPE=Release"
                     "-DBUILD_TESTING=OFF"
                     ;; Skip the iceoryx-based shared-memory transport so we
                     ;; don't need to package iceoryx_binding_c.
                     "-DENABLE_SHM=OFF"
                     "-DENABLE_SECURITY=OFF"
                     "-DENABLE_SSL=OFF"
                     "-DBUILD_IDLC=ON"
                     "-DBUILD_DDSPERF=OFF")))
      (home-page "https://cyclonedds.io/")
      (synopsis "Eclipse Cyclone DDS — OMG DDS implementation in C")
      (description
       "Eclipse Cyclone DDS is an open-source implementation of the
Object Management Group's Data Distribution Service (DDS).  It is the
default middleware behind @code{rmw_cyclonedds_cpp} in the ROS 2 Jazzy
distribution provided by guix-systole.")
      (license license:epl2.0))))
