
# RUN as `BIN_DIR=<dir> bash setup_build.sh <params>`
bin_dir=${BIN_DIR:-"build"}

if [ ! -d "$bin_dir" ]
then
	mkdir "$bin_dir"
fi
cd "$bin_dir"

if [ ! -f "conanbuildinfo.cmake" ]
then
	conan install .. $@
fi

