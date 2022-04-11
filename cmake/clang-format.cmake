find_program(
  CLANG_FORMAT_EXE
  NAMES "clang-format"
  DOC "Path to clang-format executable")

if(NOT CLANG_FORMAT_EXE)
  message(WARNING "clang-format executable not found")
else()
  message("** Found clang-format")
  message("** clang-format executable location: ${CLANG_FORMAT_EXE}")
endif()

file(
  GLOB_RECURSE
  ALL_SOURCE_FILES
  src/*.hpp
  src/*.cpp
  test/*.cpp
  test/*.hpp)

add_custom_target(clangformat COMMAND ${CLANG_FORMAT_EXE} -i -style=LLVM
                                      ${ALL_SOURCE_FILES})
