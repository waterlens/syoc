aux_source_directory(./ ALL_PASS_SRC)
aux_source_directory(./NewPass NEW_PASS_SRC)
target_sources(syoc PRIVATE ${ALL_PASS_SRC} ${NEW_PASS_SRC})