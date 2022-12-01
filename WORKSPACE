load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

####################################################################################################
# Core dependencies
#--------------------------------------------------------------------------------------------------#
# Load bazel skylib and google protobuf
bazel_skylib_tag = "1.0.3"

bazel_skylib_sha256 = "1c531376ac7e5a180e0237938a2536de0c54d93f5c278634818e0efc952dd56c"

http_archive(
    name = "bazel_skylib",
    sha256 = bazel_skylib_sha256,
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/{tag}/bazel-skylib-{tag}.tar.gz".format(tag = bazel_skylib_tag),
        "https://github.com/bazelbuild/bazel-skylib/releases/download/{tag}/bazel-skylib-{tag}.tar.gz".format(tag = bazel_skylib_tag),
    ],
)

protobuf_tag = "3.10.1"

protobuf_sha256 = "678d91d8a939a1ef9cb268e1f20c14cd55e40361dc397bb5881e4e1e532679b1"

http_archive(
    name = "com_google_protobuf",
    sha256 = protobuf_sha256,
    strip_prefix = "protobuf-{}".format(protobuf_tag),
    type = "zip",
    url = "https://github.com/protocolbuffers/protobuf/archive/v{}.zip".format(protobuf_tag),
)

load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")

protobuf_deps()
####################################################################################################

####################################################################################################
# Nixpkgs dependencies (put early to use in toolchains)
#--------------------------------------------------------------------------------------------------#
# HEAD as of 2021-08-23
rules_nixpkgs_commit = "c40b35f73e5ab1c0096d95abf63027a3b8054061"

rules_nixpkgs_sha256 = "47fffc870a25d82deedb887c32481a43a12f56b51e5002773046f81fbe3ea9df"

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = rules_nixpkgs_sha256,
    strip_prefix = "rules_nixpkgs-{}".format(rules_nixpkgs_commit),
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/{}.tar.gz".format(rules_nixpkgs_commit)],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl", "rules_nixpkgs_dependencies")

rules_nixpkgs_dependencies()

# TODO(Dave): Move these to an external nixpkgs.bzl file.
load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository", "nixpkgs_package")

nixpkgs_sha256 = "16b86dd4c19ffbd93f1d225df1b0f179bf0e418fd9ce9337ff491762a65658e7"

nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "21.05",
    sha256 = nixpkgs_sha256,
)

nixpkgs_package(
    name = "nixpkgs-jdk11",
    attribute_path = "pkgs.jdk11",
    build_file = "@bazel_tools//tools/jdk:jdk.BUILD",
    repository = "@nixpkgs//:default.nix",
)
####################################################################################################

####################################################################################################
# JVM 3rdparty dependencies
#--------------------------------------------------------------------------------------------------#
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

RULES_JVM_EXTERNAL_TAG = "4.1"

RULES_JVM_EXTERNAL_SHA = "f36441aa876c4f6427bfb2d1f2d723b48e9d930b62662bf723ddfb8fc80f0140"

http_archive(
    name = "rules_jvm_external",
    sha256 = RULES_JVM_EXTERNAL_SHA,
    strip_prefix = "rules_jvm_external-%s" % RULES_JVM_EXTERNAL_TAG,
    url = "https://github.com/bazelbuild/rules_jvm_external/archive/%s.zip" % RULES_JVM_EXTERNAL_TAG,
)

load("//3rdparty/jvm:deps.bzl", "jvm_dependencies")

jvm_dependencies()

load("@maven//:defs.bzl", "pinned_maven_install")

# To update the pinned dependencies, use:
# $ ./bazel run @unpinned_maven//:pin
pinned_maven_install()
####################################################################################################

####################################################################################################
# Scala support, via rules_scala
#--------------------------------------------------------------------------------------------------#
# HEAD as of 2022-09-09.
rules_scala_version = "a40063ef97688f056824b22b9e49fae6efd1df0f"

http_archive(
    name = "io_bazel_rules_scala",
    sha256 = "f534e1fc268fb85abccc92fd80626a3c500c7e6b6943194f7d7f09f7291f4c37",
    strip_prefix = "rules_scala-%s" % rules_scala_version,
    type = "zip",
    url = "https://github.com/bazelbuild/rules_scala/archive/%s.zip" % rules_scala_version,
)

# Stores Scala version and other configuration
# 2.12 is a default version, other versions can be use by passing them explicitly:
# scala_config(scala_version = "2.11.12")
load("@io_bazel_rules_scala//:scala_config.bzl", "scala_config")

#scala_config()  # 2.12
load("//bazel/scala:scala_version.bzl", "SCALA_VERSION")

scala_config(scala_version = SCALA_VERSION)

load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")

scala_repositories(fetch_sources = True)

#load("@io_bazel_rules_scala//scala:toolchains.bzl", "scala_register_toolchains")
#scala_register_toolchains()
register_toolchains(
    "//bazel/scala:scala_toolchain",
)

# optional: setup ScalaTest toolchain and dependencies
load("@io_bazel_rules_scala//testing:scalatest.bzl", "scalatest_repositories", "scalatest_toolchain")

scalatest_repositories()

scalatest_toolchain()
#####################################################################################################
