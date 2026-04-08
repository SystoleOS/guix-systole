#!/usr/bin/env python3
"""Minimal rclpy listener for the guix-systole ros-jazzy smoke test.

Subscribes to /systole_smoke and exits 0 as soon as it receives one
std_msgs/String, or exits 1 if nothing arrives within TIMEOUT seconds.
"""

import sys
import rclpy
from rclpy.node import Node
from std_msgs.msg import String

TIMEOUT = 20.0


class Listener(Node):
    def __init__(self) -> None:
        super().__init__("systole_smoke_listener")
        self.received = False
        self.sub = self.create_subscription(String, "systole_smoke", self._cb, 10)

    def _cb(self, msg: String) -> None:
        self.get_logger().info(f"received: {msg.data}")
        self.received = True


def main() -> int:
    rclpy.init()
    node = Listener()
    deadline = node.get_clock().now().nanoseconds + int(TIMEOUT * 1e9)
    try:
        while not node.received:
            rclpy.spin_once(node, timeout_sec=0.1)
            if node.get_clock().now().nanoseconds > deadline:
                node.get_logger().error(
                    f"no message received within {TIMEOUT:.0f}s"
                )
                return 1
    finally:
        node.destroy_node()
        rclpy.shutdown()
    print("OK: listener received a message", flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
