cmake_minimum_required(VERSION 3.16)
project(slicer_ros2_probe C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Slicer side — pull in the full VTK/CTK/MRML include + link setup.
find_package(Slicer REQUIRED)
include(${Slicer_USE_FILE})
find_package(VTK REQUIRED)

# ROS 2 side.
find_package(ament_cmake REQUIRED)
find_package(rclcpp REQUIRED)
find_package(std_msgs REQUIRED)

message(STATUS "Slicer_VERSION=${Slicer_VERSION}")
message(STATUS "rclcpp_VERSION=${rclcpp_VERSION}")

add_executable(slicer_ros2_probe probe.cpp)
ament_target_dependencies(slicer_ros2_probe rclcpp std_msgs)
# Slicer_DIR is <prefix>/lib/Slicer-5.8 in guix-systole.  Walk two
# levels up to get the install prefix.
message(STATUS "Slicer_DIR=${Slicer_DIR}")
get_filename_component(_slicer_prefix "${Slicer_DIR}" DIRECTORY)
get_filename_component(_slicer_prefix "${_slicer_prefix}" DIRECTORY)
message(STATUS "slicer_prefix=${_slicer_prefix}")
# CMAKE_PREFIX_PATH is passed via environment as colon-separated.
set(_vtk_inc "")
set(_env_prefix_path $ENV{CMAKE_PREFIX_PATH})
string(REPLACE ":" ";" _env_prefix_path "${_env_prefix_path}")
foreach(p IN LISTS _env_prefix_path)
  if(IS_DIRECTORY "${p}/include/vtk-9.2")
    list(APPEND _vtk_inc "${p}/include/vtk-9.2")
  endif()
endforeach()
message(STATUS "computed vtk inc=${_vtk_inc}")
target_include_directories(slicer_ros2_probe PRIVATE
  ${_slicer_prefix}/include/MRMLCore
  ${_slicer_prefix}/include/vtkAddon
  ${_vtk_inc})
# vtk-slicer lives alongside slicer in the store — derive its lib dir
# the same way we derived its include dir.
set(_vtk_libs "")
foreach(p IN LISTS _env_prefix_path)
  if(IS_DIRECTORY "${p}/lib" AND EXISTS "${p}/include/vtk-9.2")
    list(APPEND _vtk_libs "${p}/lib")
  endif()
endforeach()
message(STATUS "computed vtk libs=${_vtk_libs}")
target_link_directories(slicer_ros2_probe PRIVATE
  ${_slicer_prefix}/lib/Slicer-5.8
  ${_vtk_libs})
target_link_libraries(slicer_ros2_probe MRMLCore vtkCommon-9.2 vtksys-9.2)
# Ensure the linker pulls in transitive VTK libs even for unused syms.
target_link_options(slicer_ros2_probe PRIVATE "-Wl,--no-as-needed")
