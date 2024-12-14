load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def _non_module_deps_impl(_mctx):
    version = "1.3.1"
    http_archive(
        name = "zlib.dev",
        build_file = ":zlib.BUILD.bazel",
        sha256 = "17e88863f3600672ab49182f217281b6fc4d3c762bde361935e436a95214d05c",
        strip_prefix = "zlib-{}".format(version),
        urls = ["https://github.com/madler/zlib/archive/v{}.tar.gz".format(version)],
    )

non_module_deps = module_extension(implementation = _non_module_deps_impl)
