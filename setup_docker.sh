prefix=${PREFIX:-""}
$prefix docker build -t gcc-docker -f base.Dockerfile .
$prefix docker build -t enma-docker -f EnmaWithTest.Dockerfile .

# cache_dir="ccache_dir"
# if [ ! -d "$cache_dir" ]
# then
	# docker create -v "$cache_dir":/ccache --name ccache enma-docker
# fi

