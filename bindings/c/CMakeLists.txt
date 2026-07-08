cmake_minimum_required(VERSION 3.13)

project(tree-sitter-pascal
        VERSION "0.11.0"
        DESCRIPTION "Tree-sitter grammar for Pascal"
        HOMEPAGE_URL "https://github.com/isopod/tree-sitter-pascal"
        LANGUAGES C)

option(BUILD_SHARED_LIBS "Build using shared libraries" ON)
option(TREE_SITTER_REUSE_ALLOCATOR "Reuse the library allocator" OFF)

set(TREE_SITTER_ABI_VERSION 14 CACHE STRING "Tree-sitter ABI version")
if(NOT ${TREE_SITTER_ABI_VERSION} MATCHES "^[0-9]+$")
    unset(TREE_SITTER_ABI_VERSION CACHE)
    message(FATAL_ERROR "TREE_SITTER_ABI_VERSION must be an integer")
endif()

find_program(TREE_SITTER_CLI tree-sitter DOC "Tree-sitter CLI")

set(TREE_SITTER_PASCAL_SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../src")

add_custom_command(OUTPUT "${TREE_SITTER_PASCAL_SRC_DIR}/parser.c"
                   DEPENDS "${TREE_SITTER_PASCAL_SRC_DIR}/grammar.json"
                   COMMAND "${TREE_SITTER_CLI}" generate "${TREE_SITTER_PASCAL_SRC_DIR}/grammar.json"
                            --abi=${TREE_SITTER_ABI_VERSION}
                   WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../.."
                   COMMENT "Generating parser.c")

add_library(tree-sitter-pascal "${TREE_SITTER_PASCAL_SRC_DIR}/parser.c")
if(EXISTS "${TREE_SITTER_PASCAL_SRC_DIR}/scanner.c")
  target_sources(tree-sitter-pascal PRIVATE "${TREE_SITTER_PASCAL_SRC_DIR}/scanner.c")
endif()
target_include_directories(tree-sitter-pascal PRIVATE "${TREE_SITTER_PASCAL_SRC_DIR}")

target_compile_definitions(tree-sitter-pascal PRIVATE
                           $<$<BOOL:${TREE_SITTER_REUSE_ALLOCATOR}>:TREE_SITTER_REUSE_ALLOCATOR>
                           $<$<CONFIG:Debug>:TREE_SITTER_DEBUG>)

set_target_properties(tree-sitter-pascal
                      PROPERTIES
                      C_STANDARD 11
                      POSITION_INDEPENDENT_CODE ON
                      SOVERSION "${TREE_SITTER_ABI_VERSION}.${PROJECT_VERSION_MAJOR}"
                      DEFINE_SYMBOL "")

configure_file(tree-sitter-pascal.pc.in
               "${CMAKE_CURRENT_BINARY_DIR}/tree-sitter-pascal.pc" @ONLY)

include(GNUInstallDirs)

install(FILES tree-sitter-pascal.h
        DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/tree_sitter")
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/tree-sitter-pascal.pc"
        DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/pkgconfig")
install(TARGETS tree-sitter-pascal
        LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}")

add_custom_target(ts-test "${TREE_SITTER_CLI}" test ../../test/corpus
                  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../.."
                  COMMENT "tree-sitter test")
