FROM gcc-docker
WORKDIR Enma
# COPY . .
# RUN bash setup_build.sh --build
# RUN cmake -G Ninja -B build
# RUN cd build && ninja
ENTRYPOINT bash
