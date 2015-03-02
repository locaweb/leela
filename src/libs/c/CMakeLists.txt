option( LIBLEELA_STATIC "build leela-c as a shared library" OFF )

leela_check_c_flag_append(-W)
leela_check_c_flag_append(-Wextra)
leela_check_c_flag_append(-Wpedantic)
leela_check_c_flag_append(-Wnolong-long)

file(GLOB LIBLEELA_HEADERS "src/leela/*.h")
file(GLOB LIBLEELA_FILES "src/leela/*.c")
set(LIBLEELA_PUBLIC_HEADER ${LIBLEELA_HEADERS})

if(LIBLEELA_STATIC)
  set(LIBLEELA_TYPE STATIC)
  add_definitions( -DLIBLEELA_STATIC)
else(LIBLEELA_STATIC)
  set(LIBLEELA_TYPE SHARED)
  add_definitions( -DLIBLEELA_SHARED)
endif(LIBLEELA_STATIC)

leela_check_symbol_append(Sleep "windows.h" WINSLEEP)
leela_check_symbol_append(sleep "unistd.h" SLEEP)
leela_check_symbol_append(usleep "unistd.h" USLEEP)
leela_check_symbol_append(CryptGenRandom "windows.h;wincrypt.h" WINCRYPT)

add_library(libleela ${LIBLEELA_TYPE}
            ${LIBLEELA_HEADERS}
            ${LIBLEELA_FILES})

target_link_libraries(libleela libpoly1305aes ${libzmq} ${CMAKE_THREAD_LIBS_INIT})

set_target_properties(libleela PROPERTIES INCLUDE_DIRECTORIES ${CMAKE_SOURCE_DIR}/src/libs/poly1305aes/src)
set_target_properties(libleela PROPERTIES PUBLIC_HEADER "${LIBLEELA_HEADERS}")
set_target_properties(libleela PROPERTIES OUTPUT_NAME leela)
set_target_properties(libleela PROPERTIES VERSION ${LIBLEELA_VERSION} SOVERSION ${LIBLEELA_SOVERSION})

install(TARGETS libleela
        RUNTIME DESTINATION "${LEELA_INSTALL_BINDIR}"
        LIBRARY DESTINATION "${LEELA_INSTALL_LIBDIR}"
        ARCHIVE DESTINATION "${LEELA_INSTALL_LIBDIR}"
        PUBLIC_HEADER DESTINATION include/leela)
