add_executable(enma-frontend)
add_library(enma-frontend-lib)

target_sources(enma-frontend-lib PRIVATE Token.cpp Lexer.cpp Error.cpp
                                         Parser.cpp)

target_sources(enma-frontend PRIVATE main.cpp)

target_link_libraries(enma-frontend-lib PUBLIC CONAN_PKG::fmt)
target_link_libraries(enma-frontend-lib PUBLIC CONAN_PKG::tl-expected)
target_link_libraries(enma-frontend-lib PUBLIC CONAN_PKG::tl-optional)
target_link_libraries(enma-frontend-lib PUBLIC tl::ranges)

target_link_libraries(enma-frontend PRIVATE enma-frontend-lib)
