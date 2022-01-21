cmake_minimum_required(VERSION 3.17)

project(syoc)
set(CMAKE_CXX_STANDARD 20)

add_executable(syoc Driver.cpp)

include_directories(ThirdParty/fmt/include)
include_directories(.)

add_subdirectory(IR)
add_subdirectory(Parser)
add_subdirectory(Tree)
add_subdirectory(Transformer)
add_subdirectory(ThirdParty)
add_subdirectory(Util)
