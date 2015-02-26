leela_check_c_flag_append(-W)
leela_check_c_flag_append(-Wextra)
leela_check_c_flag_append(-Wpedantic)
leela_check_c_flag_append(-Wnolong-long)

file(GLOB WRITE_LEELA_FILES "src/collectd/*.c")

add_library(write_leela MODULE write_leela.c)

target_include_directories(write_leela PRIVATE ../libs/c/src)

target_link_libraries(write_leela libleela ${CMAKE_THREAD_LIBS_INIT})

set_target_properties(write_leela PROPERTIES PREFIX "")

install(TARGETS write_leela
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
