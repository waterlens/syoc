add_subdirectory(fmt)
target_link_libraries(syoc PRIVATE fmt::fmt)
target_sources(syoc PRIVATE fmt/include/Placeholder.hpp)