#!/usr/bin/env python3
"""Phase 2 (ros_base) smoke check for guix-systole.

Import every major ros_base Python module, create an rclpy node, and
publish a TransformStamped via tf2_ros.  Exits 0 on success.
"""

import sys

import rclpy
from rclpy.node import Node

# Interface bindings
from geometry_msgs.msg import TransformStamped  # noqa: F401
from sensor_msgs.msg import PointCloud2, LaserScan, Image  # noqa: F401
from nav_msgs.msg import Odometry, OccupancyGrid  # noqa: F401
from std_srvs.srv import Trigger  # noqa: F401
from diagnostic_msgs.msg import DiagnosticStatus  # noqa: F401
from visualization_msgs.msg import Marker  # noqa: F401

# tf2
from tf2_msgs.msg import TFMessage  # noqa: F401
import tf2_py  # noqa: F401
import tf2_ros


def main() -> int:
    rclpy.init()
    node = Node("systole_ros_base_check")
    try:
        buf = tf2_ros.Buffer()
        tf2_ros.TransformListener(buf, node)
        broadcaster = tf2_ros.TransformBroadcaster(node)

        t = TransformStamped()
        t.header.stamp = node.get_clock().now().to_msg()
        t.header.frame_id = "world"
        t.child_frame_id = "robot"
        t.transform.translation.x = 1.0
        t.transform.rotation.w = 1.0
        broadcaster.sendTransform(t)
        print("ROS_BASE CHECK OK")
    finally:
        node.destroy_node()
        rclpy.shutdown()
    return 0


if __name__ == "__main__":
    sys.exit(main())
