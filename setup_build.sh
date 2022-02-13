bin_dir="build"

if [ ! -d "$bin_dir" ]
then
	mkdir "$bin_dir"
fi
cd "$bin_dir"

if [ ! -f "conanbuildinfo.cmake" ]
then
	conan install .. $@
fi

