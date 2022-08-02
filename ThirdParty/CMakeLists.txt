add_subdirectory(fmt)
target_link_libraries(syoc PRIVATE fmt::fmt)
file(TOUCH fmt/include/Placeholder.hpp)
target_sources(syoc PRIVATE fmt/include/Placeholder.hpp)