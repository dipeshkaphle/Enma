FROM gcc-docker
WORKDIR Enma
# COPY . .
# RUN bash setup_build.sh --build
# RUN cmake -DBUILD_PROJECT="no_tests" -G Ninja -B build
# RUN cd build && ninja
ENTRYPOINT bash
