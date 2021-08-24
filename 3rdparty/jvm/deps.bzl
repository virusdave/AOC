
load("@rules_jvm_external//:defs.bzl", "maven_install")
load("@rules_jvm_external//:specs.bzl", "maven")
load("//bazel/scala:scala_version.bzl", "SCALA_VERSION")

_VER_AMMONITE = "2.2.0"
_VER_CATS = "2.2.0"
_VER_ENUMERATUM = "1.6.1"
_VER_SHAPELESS = "2.3.3"
_VER_ZIO = "1.0.11"
_VER_ZIO_PRELUDE = "1.0.0-RC1"

def _deps():
    return [
        maven.artifact("com.beachape", _scala("enumeratum"), _VER_ENUMERATUM),
        maven.artifact("com.beachape", _scala("enumeratum-play-json"), _VER_ENUMERATUM),
        maven.artifact("com.chuusai", _scala("shapeless"), _VER_SHAPELESS),
        maven.artifact("com.lihaoyi", _scala("ammonite-ops"), _VER_AMMONITE),
        maven.artifact("dev.zio", _scala("zio"), _VER_ZIO),
        maven.artifact("dev.zio", _scala("zio-streams"), _VER_ZIO),
        maven.artifact("junit", "junit", "4.13.2"),
        maven.artifact("org.hamcrest", "hamcrest-library", "2.2"),
        maven.artifact("org.scalanlp", _scala("breeze"), "1.0"),
        maven.artifact("org.scala-lang.modules", _scala("scala-parser-combinators"), "1.1.2"),
        maven.artifact("org.typelevel", _scala("cats-core"), _VER_CATS),
    ]

def _scala(artifact):
    return "{}_{}".format(artifact, ".".join(SCALA_VERSION.split(".")[:2]))


def jvm_dependencies():
    maven_install(
        name = "maven",
        artifacts = _deps(),
        repositories = [
            "https://repo1.maven.org/maven2",
            "https://maven.google.com",
        ],
        fetch_sources = True,
        maven_install_json = "@//3rdparty/jvm:pinned_maven_install.json",
    )