cmake_minimum_required(VERSION 3.17)

project(syoc)
set(CMAKE_CXX_STANDARD 17)

add_executable(syoc Driver.cpp)

target_include_directories(syoc PRIVATE ThirdParty/fmt/include)
target_include_directories(syoc PRIVATE .)

add_subdirectory(IR)
add_subdirectory(Parser)
add_subdirectory(Tree)
add_subdirectory(Transformer)
add_subdirectory(ThirdParty)
add_subdirectory(Util)
add_subdirectory(Pass)
add_subdirectory(CodeGen)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(COMPILE_OPTIONS "-march=native")
set(COMPILE_DEBUG_OPTIONS "${COMPILE_OPTIONS}")
set(COMPILE_RELEASE_OPTIONS "${COMPILE_OPTIONS}")

target_compile_options(syoc PRIVATE "$<$<CONFIG:Debug>:${COMPILE_DEBUG_OPTIONS}>")
target_compile_options(syoc PRIVATE "$<$<CONFIG:Release>:${COMPILE_RELEASE_OPTIONS}>")
if (WIN32)
    target_link_options(syoc PRIVATE "-Wl,--stack,8388608")
endif()

set(ASAN ON)

if (ASAN)
    target_compile_options(syoc PRIVATE "-fsanitize=address")
    target_link_options(syoc PRIVATE "-fsanitize=address")
endif()