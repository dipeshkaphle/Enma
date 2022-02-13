add_executable(enma-backend)
target_sources(enma-backend PRIVATE main.cpp)
target_link_libraries(enma-backend PRIVATE CONAN_PKG::fmt)
target_include_directories(enma-backend PRIVATE ${CMAKE_SOURCE_DIR})
