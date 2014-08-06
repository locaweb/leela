cmake_minimum_required(VERSION 2.8.12)

include(CheckSymbolExists)
include(CheckCCompilerFlag)

project(leela)

option(LEELA_BUILD_LIB_C      "builds the c library" ON)
option(LEELA_BUILD_LIB_PYTHON "builds the python library" OFF)
option(LEELA_BUILD_LIB_RUBY   "builds the ruby library" OFF)
option(LEELA_BUILD_WARPDRIVE  "builds the warpdrive binary" OFF)
option(LEELA_BUILD_BLACKBOX   "builds the blackbox binary" OFF)

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

if(LEELA_BUILD_WARPDRIVE)
  add_subdirectory(src/warpdrive)
endif()

if(LEELA_BUILD_BLACKBOX)
  add_subdirectory(src/blackbox)
endif()

if(LEELA_BUILD_LIB_C)
  find_package (Threads)
  add_subdirectory(src/libs/c)
  add_subdirectory(lib/libzmq)
  add_subdirectory(src/libs/poly1305aes)
endif()

if(LEELA_BUILD_LIB_PYTHON)
  add_subdirectory(src/libs/python)
endif()
