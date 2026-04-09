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
;;; SlicerROS2 — a 3D Slicer loadable module that bridges Slicer's MRML
;;; scene graph to the ROS 2 middleware, maintained at
;;; https://github.com/rosmed/slicer_ros2_module.
;;;
;;; Upstream is a hybrid ament_cmake + Slicer-loadable-module project:
;;; it calls `find_package(Slicer)' + `slicerMacroBuildLoadableModule'
;;; AND `find_package(ament_cmake)' + ROS 2 message/client-library
;;; find_package()s in the same CMakeLists.txt.  We build it against
;;; the @code{slicer-5.8} package from (systole packages slicer) and
;;; the ROS 2 Jazzy components from (systole packages ros2 jazzy).
;;;
;;; Code:

(define-module (systole packages slicer-ros2)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages python)
  #:use-module (systole packages slicer)
  #:use-module (systole packages ros2 jazzy))

;;;
;;; SlicerROS2 loadable module (jazzy).
;;;

(define-public slicer-ros2-module-jazzy
  (let ((commit "e7bf7febb484aed329fc9ddacb59a3feca86f44d"))
    (package
      (name "slicer-ros2-module-jazzy")
      (version (git-version "0.0.0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rosmed/slicer_ros2_module")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "1dcjwah2gd6qy7214hb8dj5lx7d4j02fxkb68825v9jgcffjc11n"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f
        #:validate-runpath? #f
        #:out-of-source? #t
        #:configure-flags
        #~(list "-DCMAKE_BUILD_TYPE=Release"
                "-DBUILD_TESTING=OFF"
                "-DUSE_CISST_MSGS=OFF"
                ;; Install everything under $out, not under Slicer_DIR.
                (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
                (string-append "-DSlicerROS2_AMENT_INSTALL_PREFIX=" #$output)
                ;; Slicer modules must install into a consistent layout
                ;; so SLICER_ADDITIONAL_MODULE_PATHS can find them.
                (string-append "-DSlicer_INSTALL_QTLOADABLEMODULES_LIB_DIR="
                               "lib/Slicer-5.8/qt-loadable-modules")
                (string-append "-DSlicer_INSTALL_QTLOADABLEMODULES_SHARE_DIR="
                               "share/Slicer-5.8/qt-loadable-modules")
                ;; Point cmake directly at Slicer's config dir.
                (string-append "-DSlicer_DIR="
                               #$(this-package-input "slicer-5.8")
                               "/lib/Slicer-5.8"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-install-layout
              (lambda _
                ;; (1) Upstream forces CMAKE_INSTALL_PREFIX to Slicer_DIR
                ;; with FORCE, which in our case is an inside-the-store
                ;; read-only path.  Drop the override so our
                ;; -DCMAKE_INSTALL_PREFIX=$out sticks.
                (substitute* "CMakeLists.txt"
                  (("set\\(CMAKE_INSTALL_PREFIX \\$\\{Slicer_DIR\\}.*FORCE\\)")
                   "# CMAKE_INSTALL_PREFIX override dropped by guix-systole"))
                ;; (2) The local_setup.bash install has a leading slash on
                ;; DESTINATION, making it an absolute path and ignoring
                ;; CMAKE_INSTALL_PREFIX.  Strip the leading slash.
                (substitute* "CMakeLists.txt"
                  (("DESTINATION /\\$\\{SlicerROS2_AMENT_INSTALL_PREFIX\\}")
                   "DESTINATION ${SlicerROS2_AMENT_INSTALL_PREFIX}"))))
            (add-after 'unpack 'make-generator-executable
              (lambda _
                ;; Upstream runs ROS2_to_vtkObjects.py at cmake time via
                ;; execute_process(COMMAND <script> ...).  It's shipped
                ;; with a #!/usr/bin/python3 shebang — patch-shebangs
                ;; fixes the interpreter, but we also need the +x bit.
                (chmod "MRML/CodeGeneration/ROS2_to_vtkObjects.py" #o755)))
            ;; Slicer modules build a family of co-located .so files that
            ;; dlopen each other; add $ORIGIN to every intermediate dir so
            ;; they resolve from the install tree.  Also let binaries
            ;; nested one level deeper resolve siblings.
            (add-before 'configure 'set-install-rpath
              (lambda _
                (setenv "CMAKE_INSTALL_RPATH"
                        "$ORIGIN:$ORIGIN/..:$ORIGIN/../../..")))
            ;; Slicer's build system prepends -Werror=... to some flags;
            ;; don't escalate those here.
            (add-before 'configure 'relax-warnings
              (lambda _
                (setenv "CXXFLAGS"
                        (string-append
                         (or (getenv "CXXFLAGS") "")
                         " -Wno-error")))))))
      (native-inputs (list python))
      (inputs
       ;; Slicer + all ROS 2 packages the upstream CMakeLists lists in
       ;; SlicerROS2_ROS_DEPENDENCIES, minus cisst_msgs (gated on
       ;; USE_CISST_MSGS, we build with it off).
       (list slicer-5.8
             ;; rosidl pipeline bits the code generator needs
             ros-rclcpp-jazzy
             ros-rclcpp-action-jazzy
             ;; URDF / KDL
             ros-kdl-parser-jazzy
             ros-urdf-jazzy
             ;; tf2 stack
             ros-tf2-jazzy
             ros-tf2-ros-jazzy
             ros-tf2-msgs-jazzy
             ;; turtlesim (required by find_package loop)
             ros-turtlesim-jazzy
             ;; interface families
             ros-builtin-interfaces-jazzy
             ros-std-msgs-jazzy
             ros-std-srvs-jazzy
             ros-geometry-msgs-jazzy
             ros-sensor-msgs-jazzy
             ros-shape-msgs-jazzy
             ros-trajectory-msgs-jazzy
             ros-object-recognition-msgs-jazzy
             ros-octomap-msgs-jazzy
             ros-moveit-msgs-jazzy
             ros-rosbag2-interfaces-jazzy
             ;; Python 3 for the build-time code generator
             python))
      ;; Propagate slicer-5.8 so `guix install slicer-ros2-module-jazzy'
      ;; brings a usable Slicer alongside it; also propagate ros-jazzy
      ;; components whose .so files are dlopen'd by the module at
      ;; runtime for message (de)serialization.
      (propagated-inputs
       (list slicer-5.8
             ros-rclcpp-jazzy
             ros-rmw-implementation-jazzy
             ros-rmw-cyclonedds-cpp-jazzy
             ros-tf2-jazzy
             ros-tf2-ros-jazzy
             ros-kdl-parser-jazzy
             ros-urdf-jazzy))
      (home-page "https://github.com/rosmed/slicer_ros2_module")
      (synopsis "3D Slicer loadable module bridging MRML to ROS 2")
      (description
       "@code{slicer_ros2_module} is a 3D Slicer loadable module that
exposes ROS 2 nodes, publishers, subscribers, service clients,
parameters, and a @code{tf2} buffer inside Slicer's MRML scene graph.
It is built against the @code{slicer-5.8} package and the ROS 2
Jazzy distribution provided by the @code{systole} channel.

Set @env{RMW_IMPLEMENTATION=rmw_cyclonedds_cpp} in the profile that
installs this package to match the bundled RMW.")
      (license license:expat))))
