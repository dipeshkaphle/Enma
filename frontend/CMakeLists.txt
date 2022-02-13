add_executable(enma-frontend)
target_sources(enma-frontend PRIVATE main.cpp)

target_link_libraries(enma-frontend PRIVATE CONAN_PKG::fmt)
target_include_directories(enma-frontend PRIVATE ${CMAKE_SOURCE_DIR})
