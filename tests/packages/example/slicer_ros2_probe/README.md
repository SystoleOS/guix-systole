# Slicer + rclcpp ABI probe

A tiny single-binary sanity check that verifies that the Slicer 5.8
and ROS 2 Jazzy packages from this channel are ABI-compatible: a
`vtkMRMLScene` and an `rclcpp::Node` can coexist in one process built
with the same guix-systole gcc toolchain.

This exists so that, before attempting to build the real
[SlicerROS2](https://github.com/rosmed/slicer_ros2_module) module, a
maintainer can quickly confirm that nothing in the toolchain chain
(libstdc++ version, Qt5 version, VTK headers, rosidl introspection
targets) has drifted in a way that would block the full module build.

## Running it

`guix shell ros-jazzy slicer-5.8 ...` is currently slow because
Guix's profile machinery hits a performance cliff for densely-
propagated manifests.  The probe instead uses bare `guix build` to
resolve store paths and then feeds them to CMake via environment
variables:

```sh
SLICER=$(guix build -L systole slicer-5.8)
ROSJAZZY=$(guix build -L systole ros-jazzy)
SLICER_CLOSURE=$(guix gc --requisites "$SLICER" \
  | grep '^/gnu/store' \
  | grep -vE '\-(gcc|glibc|libstdc[+][+]|binutils|linux-libre|ld-wrapper|bash|make|coreutils|sed|grep|tar|gzip|bzip2|xz|diffutils|findutils|gawk|patch|file)-' \
  | paste -sd:)

guix shell --no-substitutes gcc-toolchain cmake -- bash -c "
  cd tests/packages/example/slicer_ros2_probe
  export CMAKE_PREFIX_PATH=$ROSJAZZY:$SLICER:$SLICER_CLOSURE
  export AMENT_PREFIX_PATH=$ROSJAZZY
  export PYTHONPATH=$ROSJAZZY/lib/python3.11/site-packages
  rm -rf build && mkdir build
  cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
  cmake --build build -j 2
"

# Run under a localhost-only DDS setup to keep startup bounded.
SLICER_CLOSURE_LIBS=$(echo "$SLICER_CLOSURE" | tr : '\n' \
  | while read p; do [ -d "$p/lib" ] && echo "$p/lib"; done \
  | paste -sd:)
LD_LIBRARY_PATH="$ROSJAZZY/lib:$SLICER/lib/Slicer-5.8:$SLICER_CLOSURE_LIBS" \
  RMW_IMPLEMENTATION=rmw_cyclonedds_cpp ROS_LOCALHOST_ONLY=1 \
  ./tests/packages/example/slicer_ros2_probe/build/slicer_ros2_probe
```

Expected output:

```
[INFO] [slicer_ros2_probe]: rclcpp::Node OK
vtkMRMLScene: vtkMRMLScene
[INFO] [slicer_ros2_probe]: msg.data=hello from a slicer+rclcpp process
```

and exit code 0.
