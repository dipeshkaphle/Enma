## Instructions

### Dependencies

-   clang-format
-   Latest C++ compiler(Clang/GCC), supporting C++20
-   CMake
-   Ninja
-   CCache (optional but recommended)

### Setting-Up Instructions

-   `pip install -r requirements.txt` to download conan, cmake-format and compdb
-   For setting up the build folder, run `bash setup_build.sh`, this will fetch
    the dependencies and create a build folder. If we want to use non default compiler, 
    run `CC=<C compiler> CXX=<C++ Compiler> bash setup_build.sh --build -s compiler=<Compiler Name> -s compiler.version=<Version>`

### Build Instructions

-   `cd build`
-   `cmake -G Ninja ..` . We can provide extra build flags while running this.
    For example to generate release build, run
    `cmake -G Ninja -DCMAKE_BUILD_TYPE=Release ..` . To specify alternate compiler prefix by `CC=<C Compiler> CXX=<C++ Compiler> `
-   `-DDEBUG=ON` if you want to set define DEBUG in the code inside. Useful for debugging
-   `-DBUILD_PROJECT="no_tests"` to build just frontend and backend
-   `-DBUILD_PROJECT="frontend"` to build just frontend
-   `-DBUILD_PROJECT="backedn"` to build just backend
-   `ninja`
