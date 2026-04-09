#!/usr/bin/env bash
# End-to-end ROS 2 Jazzy smoke test for the guix-systole channel.
#
# Builds a tiny rclcpp talker against `ros-jazzy' and runs an `rclpy'
# listener that consumes one std_msgs/String off the same topic.  This
# exercises the entire stack: rosidl C/C++/Python pipeline, rmw +
# Cyclone DDS, rcl, rclcpp, rclpy, the AMENT_PREFIX_PATH search-path
# wiring on the union-build ros-jazzy meta-package.
#
# Usage (from the channel root):
#     bash tests/packages/example/ros2/run-smoke.sh
#
# Exits 0 on success, non-zero on failure.

set -euo pipefail

: "${RMW_IMPLEMENTATION:=rmw_cyclonedds_cpp}"
: "${ROS_LOCALHOST_ONLY:=1}"
export RMW_IMPLEMENTATION ROS_LOCALHOST_ONLY

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$HERE/../../../.." && pwd)"
CHANNEL="$REPO_ROOT/systole"
BUILD="$(mktemp -d -t systole-ros-smoke.XXXXXX)"
trap 'rm -rf "$BUILD"' EXIT

echo "[smoke] channel: ${CHANNEL}"
echo "[smoke] build:   ${BUILD}"
echo "[smoke] RMW_IMPLEMENTATION=${RMW_IMPLEMENTATION}"
echo "[smoke] ROS_LOCALHOST_ONLY=${ROS_LOCALHOST_ONLY}"

# We pass --no-substitutes to avoid the daemon stalling on substituter
# probes for derivations only built locally.
GUIX_SHELL=(guix shell --no-substitutes -L "$CHANNEL"
            ros-jazzy gcc-toolchain cmake)

echo "[smoke] step 1: rclpy import + node creation"
"${GUIX_SHELL[@]}" -- python3 -c '
import rclpy
from std_msgs.msg import String
from rcl_interfaces.msg import Parameter
from builtin_interfaces.msg import Time
print("imports: rclpy + std_msgs + rcl_interfaces + builtin_interfaces OK")
rclpy.init()
node = rclpy.create_node("systole_smoke_imports")
print("node:", node.get_name())
node.destroy_node()
rclpy.shutdown()
print("STEP 1 OK")
'

echo "[smoke] step 1b: ros_base check (geometry/sensor/nav/tf2)"
"${GUIX_SHELL[@]}" -- python3 "$HERE/ros_base_check.py"

echo "[smoke] step 1c: ros2 CLI sanity (pkg/topic/bag/launch --help)"
"${GUIX_SHELL[@]}" -- bash -c '
set -e
ros2 pkg list >/dev/null
echo "ros2 pkg list: $(ros2 pkg list | wc -l) packages registered"
ros2 topic --help >/dev/null
ros2 bag --help >/dev/null
ros2 launch --help >/dev/null
echo "CLI SANITY OK"
'

echo "[smoke] step 2: build C++ talker, run listener"
"${GUIX_SHELL[@]}" -- bash -c "
set -euo pipefail
cmake -S '$HERE' -B '$BUILD' -DCMAKE_BUILD_TYPE=Release > '$BUILD/cmake.log' 2>&1 \
    || { echo '[smoke] cmake failed:'; cat '$BUILD/cmake.log'; exit 1; }
cmake --build '$BUILD' --parallel 2 > '$BUILD/build.log' 2>&1 \
    || { echo '[smoke] build failed:'; cat '$BUILD/build.log'; exit 1; }
test -x '$BUILD/systole_smoke_talker' \
    || { echo '[smoke] talker binary missing'; exit 1; }
echo '[smoke] talker built; starting talker + listener'
'$BUILD/systole_smoke_talker' > '$BUILD/talker.log' 2>&1 &
TALKER=\$!
trap 'kill \$TALKER 2>/dev/null || true' EXIT
sleep 1
if ! kill -0 \$TALKER 2>/dev/null; then
    echo '[smoke] talker died immediately:'
    cat '$BUILD/talker.log'
    exit 1
fi
python3 '$HERE/listener.py'
RC=\$?
kill \$TALKER 2>/dev/null || true
wait \$TALKER 2>/dev/null || true
exit \$RC
"

echo "[smoke] SUCCESS"
