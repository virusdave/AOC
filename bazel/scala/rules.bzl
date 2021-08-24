load(
    "@io_bazel_rules_scala//scala:scala.bzl",
    _scala_binary = "scala_binary",
    _scala_library = "scala_library",
)
load("//bazel/scala:scalac_opts.bzl", "default_scalac_opts")

def scala_binary(**kwargs):
    new_opts = []
    new_opts += kwargs.pop("default_scalacopts", default_scalac_opts)
    new_opts += kwargs.get("scalacopts") or []
    return _scala_binary(**dict(kwargs, scalacopts = new_opts))

def scala_library(**kwargs):
    new_opts = []
    new_opts += kwargs.pop("default_scalacopts", default_scalac_opts)
    new_opts += kwargs.get("scalacopts") or []
    return _scala_library(**dict(kwargs, scalacopts = new_opts))