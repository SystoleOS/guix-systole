# ROS 2 Jazzy end-to-end smoke test

Minimal talker/listener pair used to verify that the `ros-jazzy`
aggregation meta-package from the `systole` channel provides a usable
ROS 2 environment end-to-end (rclcpp -> Cyclone DDS -> rclpy).

## What it exercises

- `rclcpp::Node` + publisher in C++ using the `std_msgs::msg::String`
  interface — proves the rosidl C/C++ pipeline, rcl, rclcpp, and the
  Cyclone DDS rmw binding work together.
- `rclpy.node.Node` + subscription in Python — proves rclpy's pybind11
  C extension imports and binds to rcl correctly.
- AMENT_PREFIX_PATH / CMAKE_PREFIX_PATH / PYTHONPATH assembly via the
  Guix profile union.

## Running it

From the channel root:

```sh
RMW_IMPLEMENTATION=rmw_cyclonedds_cpp \
  guix shell -L systole --no-grafts ros-jazzy gcc-toolchain cmake \
  -- bash tests/packages/example/ros2/run-smoke.sh
```

`--no-grafts` skips the graft phase (otherwise ~80 packages get grafted
in parallel, which is memory-heavy on smaller machines).  The test
succeeds if the listener receives at least one `std_msgs/String` on the
`/systole_smoke` topic and exits 0.
