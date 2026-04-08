// Minimal rclcpp talker for the guix-systole ros-jazzy smoke test.
// Publishes a std_msgs::msg::String on /systole_smoke at 5 Hz.

#include <chrono>
#include <memory>
#include <string>

#include "rclcpp/rclcpp.hpp"
#include "std_msgs/msg/string.hpp"

using namespace std::chrono_literals;

class Talker : public rclcpp::Node {
public:
  Talker() : Node("systole_smoke_talker"), count_(0) {
    pub_ = create_publisher<std_msgs::msg::String>("systole_smoke", 10);
    timer_ = create_wall_timer(200ms, [this] {
      std_msgs::msg::String msg;
      msg.data = "hello from guix-systole ros-jazzy #" + std::to_string(count_++);
      RCLCPP_INFO(get_logger(), "publish: %s", msg.data.c_str());
      pub_->publish(msg);
    });
  }

private:
  rclcpp::Publisher<std_msgs::msg::String>::SharedPtr pub_;
  rclcpp::TimerBase::SharedPtr timer_;
  std::size_t count_;
};

int main(int argc, char **argv) {
  rclcpp::init(argc, argv);
  rclcpp::spin(std::make_shared<Talker>());
  rclcpp::shutdown();
  return 0;
}
