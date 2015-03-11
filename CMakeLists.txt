cmake_minimum_required(VERSION 2.8.9)

include(CheckSymbolExists)
include(CheckCCompilerFlag)

project(leela)

enable_language(C ASM)

option(LEELA_BUILD_LIBLEELA   "builds the c library" OFF)
option(LEELA_BUILD_COLLECTD   "builds the collectd write plugin" OFF)

set(LEELA_INSTALL_LIBDIR "lib" CACHE string "the relative library directory")
set(LEELA_INSTALL_ARCDIR "lib" CACHE string "the relative archive directory")
set(LEELA_INSTALL_BINDIR "bin" CACHE string "the relative binary directory")
set(LEELA_INSTALL_HDRDIR "include" CACHE string "the relative include directory")

execute_process(COMMAND env component=.libleela "${CMAKE_SOURCE_DIR}/src/scripts/read-version.sh" OUTPUT_VARIABLE LIBLEELA_VERSION OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND env format=\$major component=.libleela "${CMAKE_SOURCE_DIR}/src/scripts/read-version.sh" OUTPUT_VARIABLE LIBLEELA_SOVERSION OUTPUT_STRIP_TRAILING_WHITESPACE)

macro(leela_check_symbol_append symbol header symname)
  check_symbol_exists(${symbol} "${header}" HAS_SYM_${symname})
  if(HAS_SYM_${symname})
    add_definitions( "-DHAS_SYM_${symname}")
  endif()
endmacro()

macro(leela_check_c_flag_append flag)
  check_c_compiler_flag("${flag}" HAS_FLAG_${flag})

  if(HAS_FLAG_${flag})
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${flag}")
  endif()
endmacro()

if("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

if(LEELA_BUILD_LIBLEELA)
  find_package (Threads)
  find_library (libm NAMES m)
  find_library (librt NAMES rt)
  find_library (libzmq NAMES libzmq.a zmq)

  add_subdirectory(src/libs/poly1305aes)
  add_subdirectory(src/libs/c)
endif()

if(LEELA_BUILD_COLLECTD)
  find_package (Threads)

  find_path (leela_h leela/lql.h)
  find_library (libleela NAMES leela)

  add_subdirectory(src/collectd)
endif()
