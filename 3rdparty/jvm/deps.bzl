"To update, use: `bazel run @unpinned_maven//:pin`"

load("@rules_jvm_external//:defs.bzl", "maven_install")
load("@rules_jvm_external//:specs.bzl", "maven")
load("//bazel/scala:scala_version.bzl", "SCALA_VERSION")

_VER_AMMONITE = "2.2.0"
_VER_CATS = "2.7.0"
_VER_CATS_EFFECT = "3.3.11"
_VER_CIRCE = "0.14.1"
_VER_DOODLE = "0.10.0"
_VER_ENUMERATUM = "1.7.0"
_VER_JAEGER = "1.8.0"
_VER_POSTGRES = "42.3.1"
_VER_SCALAPB = "0.11.10"
_VER_SHAPELESS = "2.3.7"
_VER_ZIO = "1.0.14"
_VER_ZIO_ARROW = "0.2.1"
_VER_ZIO_AWS = "3.17.104.7"
_VER_ZIO_CONFIG = "1.0.10"
_VER_ZIO_GRPC = "0.5.1"
_VER_ZIO_HTTP = "1.0.0.0-RC27"
_VER_ZIO_JSON = "0.1.5"
_VER_ZIO_KAFKA = "0.17.5"
_VER_ZIO_K8S = "1.3.3"
_VER_ZIO_LOGGING = "0.5.13"
_VER_ZIO_MAGIC = "0.3.11"
_VER_ZIO_NIO = "1.0.0-RC11"
_VER_ZIO_OPTICS = "0.1.0"
_VER_ZIO_PRELUDE = "1.0.0-RC1"
_VER_ZIO_PROCESS = "0.5.0"
_VER_ZIO_QUERY = "0.2.9"
_VER_ZIO_QUILL = "3.12.0"
_VER_ZIO_REZILIENCE = "0.8.1-1"
_VER_ZIO_S3 = "0.3.5"
_VER_ZIO_SQS = "0.4.1"
_VER_ZIO_STTP = "3.1.1"
_VER_ZIO_TELEMETRY = "1.0.0"

def _deps():
    return [
        maven.artifact("com.beachape", _scala("enumeratum"), _VER_ENUMERATUM),
        maven.artifact("com.beachape", _scala("enumeratum-play-json"), _VER_ENUMERATUM),
        maven.artifact("com.chuusai", _scala("shapeless"), _VER_SHAPELESS),
        maven.artifact("com.codecommit", _scala("gll-combinators"), "2.5-53b190b"),
        maven.artifact("com.lihaoyi", _scala("ammonite-ops"), _VER_AMMONITE),
        maven.artifact("com.thesamet.scalapb", _scala("scalapb-runtime"), _VER_SCALAPB),
        maven.artifact("com.thesamet.scalapb", _scala("scalapb-runtime-grpc"), _VER_SCALAPB),
        maven.artifact("io.carpe", _scala("parseback"), "0.5.1"),
        maven.artifact("io.circe", _scala("circe-core"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-generic"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-literal"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-numbers"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-parser"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-pointer"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-pointer-literal"), _VER_CIRCE),
        maven.artifact("io.circe", _scala("circe-shapes"), _VER_CIRCE),
        maven.artifact("io.jaegertracing", "jaeger-client", _VER_JAEGER),
        maven.artifact("io.jaegertracing", "jaeger-core", _VER_JAEGER),
        maven.artifact("io.jaegertracing", "jaeger-thrift", _VER_JAEGER),
        maven.artifact("junit", "junit", "4.13.2"),
        maven.artifact("org.creativescala", _scala("doodle"), _VER_DOODLE),
        maven.artifact("org.creativescala", _scala("doodle-core"), _VER_DOODLE),
        maven.artifact("org.creativescala", _scala("doodle-image"), _VER_DOODLE),
        maven.artifact("org.creativescala", _scala("doodle-interact"), _VER_DOODLE),
        maven.artifact("org.creativescala", _scala("doodle-java2d"), _VER_DOODLE),
        maven.artifact("org.creativescala", _scala("doodle-reactor"), _VER_DOODLE),
        maven.artifact("org.hamcrest", "hamcrest-library", "2.2"),
        maven.artifact("org.leibnizcenter", "probabilistic-earley-parser", "0.9.12"),
        maven.artifact("org.postgresql", "postgresql", _VER_POSTGRES),
        maven.artifact("org.scalanlp", _scala("breeze"), "1.0"),
        maven.artifact("org.scala-graph", _scala("graph-core"), "1.13.2"),
        maven.artifact("org.scala-lang.modules", _scala("scala-parallel-collections"), "1.0.4"),
        maven.artifact("org.scala-lang.modules", _scala("scala-parser-combinators"), "1.1.2"),
        maven.artifact("org.typelevel", _scala("cats-core"), _VER_CATS),
        maven.artifact("org.typelevel", _scala("cats-effect"), _VER_CATS_EFFECT),

        # ZIO core libs
        maven.artifact("dev.zio", _scala("zio"), _VER_ZIO),
        maven.artifact("dev.zio", _scala("zio-streams"), _VER_ZIO),
        # ZIO adjacent libs
        maven.artifact("com.coralogix", _scala("zio-k8s-client"), _VER_ZIO_K8S),
        maven.artifact("com.softwaremill.sttp.client3", _scala("async-http-client-backend-zio"), _VER_ZIO_STTP),
        maven.artifact("com.thesamet.scalapb.zio-grpc", _scala("zio-grpc-codegen"), _VER_ZIO_GRPC),
        maven.artifact("dev.zio", _scala("zio-config"), _VER_ZIO_CONFIG),
        maven.artifact("dev.zio", _scala("zio-interop-cats"), "3.2.9.1"),
        maven.artifact("dev.zio", _scala("zio-json"), _VER_ZIO_JSON),
        maven.artifact("dev.zio", _scala("zio-kafka"), _VER_ZIO_KAFKA),
        maven.artifact("dev.zio", _scala("zio-logging"), _VER_ZIO_LOGGING),
        maven.artifact("dev.zio", _scala("zio-nio"), _VER_ZIO_NIO),
        maven.artifact("dev.zio", _scala("zio-nio-core"), _VER_ZIO_NIO),
        maven.artifact("dev.zio", _scala("zio-opentelemetry"), _VER_ZIO_TELEMETRY),
        maven.artifact("dev.zio", _scala("zio-opentracing"), _VER_ZIO_TELEMETRY),
        maven.artifact("dev.zio", _scala("zio-optics"), _VER_ZIO_OPTICS),
        maven.artifact("dev.zio", _scala("zio-process"), _VER_ZIO_PROCESS),
        maven.artifact("dev.zio", _scala("zio-query"), _VER_ZIO_QUERY),
        maven.artifact("dev.zio", _scala("zio-s3"), _VER_ZIO_S3),
        maven.artifact("dev.zio", _scala("zio-sqs"), _VER_ZIO_SQS),
        maven.artifact("io.d11", _scala("zhttp"), _VER_ZIO_HTTP),
        maven.artifact("io.d11", _scala("zhttp-test"), _VER_ZIO_HTTP),
        maven.artifact("io.getquill", _scala("quill-jdbc-zio"), _VER_ZIO_QUILL),
        maven.artifact("io.github.kitlangton", _scala("zio-magic"), _VER_ZIO_MAGIC),
        maven.artifact("io.github.neurodyne", _scala("zio-arrow"), _VER_ZIO_ARROW),
        maven.artifact("io.github.vigoo", _scala("zio-aws-http4s"), _VER_ZIO_AWS),
        maven.artifact("io.github.vigoo", _scala("zio-aws-netty"), _VER_ZIO_AWS),
        # aws service specific
        maven.artifact("io.github.vigoo", _scala("zio-aws-account"), _VER_ZIO_AWS),
        maven.artifact("io.github.vigoo", _scala("zio-aws-codebuild"), _VER_ZIO_AWS),
        maven.artifact("io.github.vigoo", _scala("zio-aws-ec2"), _VER_ZIO_AWS),
        maven.artifact("io.github.vigoo", _scala("zio-aws-iam"), _VER_ZIO_AWS),
        maven.artifact("io.github.vigoo", _scala("zio-aws-kafka"), _VER_ZIO_AWS),
        maven.artifact("io.github.vigoo", _scala("zio-aws-lambda"), _VER_ZIO_AWS),
        maven.artifact("io.github.vigoo", _scala("zio-aws-rds"), _VER_ZIO_AWS),
        maven.artifact("io.github.vigoo", _scala("zio-aws-route53"), _VER_ZIO_AWS),
        maven.artifact("io.github.vigoo", _scala("zio-aws-route53domains"), _VER_ZIO_AWS),
        maven.artifact("io.github.vigoo", _scala("zio-aws-s3"), _VER_ZIO_AWS),
        maven.artifact("io.github.vigoo", _scala("zio-aws-sms"), _VER_ZIO_AWS),
        maven.artifact("nl.vroste", _scala("rezilience"), _VER_ZIO_REZILIENCE),
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
