cmake_minimum_required(VERSION 3.20)

project(syoc)
set(CMAKE_CXX_STANDARD 17)

add_executable(syoc)
target_sources(syoc PRIVATE Driver.cpp)

target_include_directories(syoc PRIVATE ThirdParty/fmt/include)
target_include_directories(syoc PRIVATE .)

add_subdirectory(Common)
add_subdirectory(IR)
add_subdirectory(Parser)
add_subdirectory(Tree)
add_subdirectory(Transformer)
add_subdirectory(ThirdParty)
add_subdirectory(Util)
add_subdirectory(Pass)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(COMPILE_OPTIONS "-march=native")
set(COMPILE_DEBUG_OPTIONS "${COMPILE_OPTIONS}")
set(COMPILE_RELEASE_OPTIONS "${COMPILE_OPTIONS}")

target_compile_options(syoc PRIVATE "$<$<CONFIG:Debug>:${COMPILE_DEBUG_OPTIONS}>")
target_compile_options(syoc PRIVATE "$<$<CONFIG:Release>:${COMPILE_RELEASE_OPTIONS}>")
if (WIN32)
    target_link_options(syoc PRIVATE "-Wl,--stack,8388608")
endif()

option(ASAN "Enable the AddressSanitizer" on)

if (ASAN)
    target_compile_options(syoc PRIVATE "-fsanitize=address")
    target_link_options(syoc PRIVATE "-fsanitize=address")
endif()

function(get_relative_sources variable target directory)
    get_target_property(srcs ${target} SOURCES)
    get_target_property(srcs_dir ${target} SOURCE_DIR)
    cmake_path(ABSOLUTE_PATH directory NORMALIZE)
    foreach(src ${srcs})
        cmake_path(ABSOLUTE_PATH src BASE_DIRECTORY ${srcs_dir} NORMALIZE OUTPUT_VARIABLE abs_src)
        cmake_path(RELATIVE_PATH abs_src BASE_DIRECTORY ${directory} OUTPUT_VARIABLE rel_src)
        list(APPEND result ${rel_src})
    endforeach()
    set(${variable} "${result}" PARENT_SCOPE)
endfunction()

function(create_deployment_configure file directory)
    foreach(arg ${ARGN}) 
        get_relative_sources(arg_srcs ${arg} ${directory})
        list(APPEND srcs ${arg_srcs})
    endforeach()
    string(REPLACE ";" "\n" srcs "${srcs}")
    file(WRITE ${file} "")
    file(APPEND ${file} "sources = '''\n${srcs}\n'''\n\n")
endfunction()

create_deployment_configure(manifest.txt . syoc fmt)