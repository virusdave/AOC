"To update, use: `bazel run @unpinned_maven//:pin`"

load("@rules_jvm_external//:defs.bzl", "maven_install")
load("@rules_jvm_external//:specs.bzl", "maven")
load("//bazel/scala:scala_version.bzl", "SCALA_VERSION")

_VER_AMMONITE = "2.2.0"
_VER_CATS = "2.9.0"
_VER_CATS_EFFECT = "3.4.2"
_VER_CIRCE = "0.14.1"
_VER_DOODLE = "0.10.0"
_VER_ENUMERATUM = "1.7.0"
_VER_SHAPELESS = "2.3.10"
_VER_ZIO = "1.0.17"
_VER_ZIO_PRELUDE = "1.0.0-RC1"

def _deps():
    return [
        maven.artifact("com.beachape", _scala("enumeratum"), _VER_ENUMERATUM),
        maven.artifact("com.beachape", _scala("enumeratum-play-json"), _VER_ENUMERATUM),
        maven.artifact("com.chuusai", _scala("shapeless"), _VER_SHAPELESS),
        maven.artifact("com.codecommit", _scala("gll-combinators"), "2.5-53b190b"),
        maven.artifact("com.lihaoyi", _scala("ammonite-ops"), _VER_AMMONITE),
        # ZIO core libs
        maven.artifact("dev.zio", _scala("zio"), _VER_ZIO),
        maven.artifact("dev.zio", _scala("zio-streams"), _VER_ZIO),
        # ZIO adjacent libs
        maven.artifact("dev.zio", _scala("zio-interop-cats"), "3.1.1.0"),
        maven.artifact("io.carpe", _scala("parseback"), "0.5.1"),
        maven.artifact("io.circe", _scala("circe-core"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-generic"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-literal"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-numbers"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-parser"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-pointer"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-pointer-literal"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-shapes"), _VER_CIRCE),
        maven.artifact("junit", "junit", "4.13.2"),
        maven.artifact("org.creativescala", _scala("doodle"), _VER_DOODLE),
        maven.artifact("org.creativescala", _scala("doodle-core"), _VER_DOODLE),
        maven.artifact("org.creativescala", _scala("doodle-image"), _VER_DOODLE),
        maven.artifact("org.creativescala", _scala("doodle-interact"), _VER_DOODLE),
        maven.artifact("org.creativescala", _scala("doodle-java2d"), _VER_DOODLE),
        maven.artifact("org.creativescala", _scala("doodle-reactor"), _VER_DOODLE),
        maven.artifact("org.hamcrest", "hamcrest-library", "2.2"),
        maven.artifact("org.leibnizcenter", "probabilistic-earley-parser", "0.9.12"),
        maven.artifact("org.scalanlp", _scala("breeze"), "1.0"),
        maven.artifact("org.scala-graph", _scala("graph-constrained"), "1.13.2"),  # Yes all different patch versions :(
        maven.artifact("org.scala-graph", _scala("graph-core"), "1.13.5"),
        maven.artifact("org.scala-graph", _scala("graph-dot"), "1.13.3"),
        maven.artifact("org.scala-graph", _scala("graph-json"), "1.13.0"),
        maven.artifact("org.scala-lang.modules", _scala("scala-parallel-collections"), "1.0.4"),
        maven.artifact("org.scala-lang.modules", _scala("scala-parser-combinators"), "1.1.2"),
        maven.artifact("org.typelevel", _scala("cats-core"), _VER_CATS),
        maven.artifact("org.typelevel", _scala("cats-effect"), _VER_CATS_EFFECT),
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
