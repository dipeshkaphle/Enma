FROM gcc:latest

RUN apt-get update 
# RUN apt-get install -y clang-11
RUN apt-get install -y cmake ninja-build python3 python3-pip
RUN apt-get install -y clang-format vim git
RUN pip install cmake-format compdb conan
