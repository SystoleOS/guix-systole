# ROS 2 Jazzy end-to-end smoke test

Minimal talker/listener pair used to verify that the `ros-jazzy`
aggregation meta-package from the `systole` channel provides a usable
ROS 2 environment end-to-end (rclcpp -> Cyclone DDS -> rclpy).

## What it exercises

- `import rclpy`, `from std_msgs.msg import String`, etc. — proves the
  rosidl C/C++/Python pipeline produced importable Python message
  classes for the base interface packages.
- `rclpy.init()` + `create_node()` — proves the rclpy pybind11 C
  extension binds correctly to `rcl` and the Cyclone DDS rmw runs.
- A C++ `rclcpp::Node` + publisher built via `cmake` /
  `find_package(rclcpp)` / `ament_target_dependencies` — proves the
  `ament_index` resource discovery works inside `guix shell ros-jazzy`.
- An `rclpy` listener that subscribes to the talker's topic — proves the
  full DDS pub/sub round-trip across two processes.

## Running it

From the channel root:

```sh
bash tests/packages/example/ros2/run-smoke.sh
```

The script wraps everything in `guix shell --no-substitutes -L systole
ros-jazzy gcc-toolchain cmake -- ...`.  `--no-substitutes` is used
because the systole-built derivations are local-only and probing public
substituters for them stalls.

The script defaults `RMW_IMPLEMENTATION=rmw_cyclonedds_cpp` (the only
DDS middleware currently provided by the channel) and
`ROS_LOCALHOST_ONLY=1` so DDS discovery does not depend on multicast
across host network interfaces.
