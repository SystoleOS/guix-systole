cmake_minimum_required(VERSION 3.16)
project(systole_smoke CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(ament_cmake REQUIRED)
find_package(rclcpp REQUIRED)
find_package(std_msgs REQUIRED)

add_executable(systole_smoke_talker talker.cpp)
ament_target_dependencies(systole_smoke_talker rclcpp std_msgs)
