file(GLOB ALL_CODEGEN_SRC
        *.hpp
        *.cpp
        )
target_sources(syoc PRIVATE ${ALL_CODEGEN_SRC})