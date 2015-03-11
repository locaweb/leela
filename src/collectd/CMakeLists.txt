leela_check_c_flag_append(-W)
leela_check_c_flag_append(-Wextra)
leela_check_c_flag_append(-Wpedantic)
leela_check_c_flag_append(-Wnolong-long)

file(GLOB WRITE_LEELA_FILES "src/collectd/*.c")

add_library(write_leela MODULE write_leela.c)

target_link_libraries(write_leela ${libleela} ${CMAKE_THREAD_LIBS_INIT})

set_target_properties(write_leela PROPERTIES PREFIX "")
set_target_properties(write_leela PROPERTIES INCLUDE_DIRECTORIES "${leela_h}")

install(TARGETS write_leela
        RUNTIME DESTINATION "${LEELA_INSTALL_BINDIR}"
        LIBRARY DESTINATION "${LEELA_INSTALL_LIBDIR}"
        ARCHIVE DESTINATION "${LEELA_INSTALL_ARCDIR}"
        PUBLIC_HEADER DESTINATION "${LEELA_INSTALL_HDRDIR}/leela")
