#include <iostream>
#include "rclcpp/rclcpp.hpp"
#include "std_msgs/msg/string.hpp"
#include "vtkMRMLScene.h"
#include "vtkNew.h"

int main(int argc, char **argv) {
  rclcpp::init(argc, argv);
  auto node = std::make_shared<rclcpp::Node>("slicer_ros2_probe");
  RCLCPP_INFO(node->get_logger(), "rclcpp::Node OK");

  vtkNew<vtkMRMLScene> scene;
  std::cout << "vtkMRMLScene: " << scene->GetClassName() << std::endl;

  // Round-trip through a std_msgs::String.
  std_msgs::msg::String msg;
  msg.data = "hello from a slicer+rclcpp process";
  RCLCPP_INFO(node->get_logger(), "msg.data=%s", msg.data.c_str());

  rclcpp::shutdown();
  return 0;
}
