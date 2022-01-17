aux_source_directory(./Pass ALL_PASS_SRC)
target_sources(syoc PRIVATE Transformer.hpp Pass.hpp ${ALL_PASS_SRC})
