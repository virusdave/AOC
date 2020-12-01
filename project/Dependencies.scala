import sbt._
import Keys._

object Dependencies {
  private object Sources {
    // My personal libraries
//    val SlickScratchDb = ProjectRef(uri("https://github.com/virusdave/slick-scratch.git"), "db")
//    val SlickScratchDbAll = ProjectRef(uri("https://github.com/virusdave/slick-scratch.git"), "db_all")
  }

  private object LibraryVersions {
    val Ammonite = "2.2.0"
    val Cats = "2.2.0"
    val Enumeratum = "1.6.1"
//    val Postgres = "42.2.5"
    val Shapeless = "2.3.3"
//    val Slick = "3.2.3"
//    val Slickless = "0.3.3"
//    val SlickPG = "0.16.3"
    val Zio = "1.0.3"
    val ZioPrelude = "1.0.0-RC1"

    // Test libs
//    val ScalaCheck = "1.14.0"
//    val ScalaTest = "3.0.5"
  }

  private object Libraries {
    val Ammonite = "com.lihaoyi" % "ammonite" % LibraryVersions.Ammonite % "test" cross CrossVersion.full
    val Breeze = "org.scalanlp" %% "breeze" % "1.0"
    val Cats = "org.typelevel" %% "cats-core" % LibraryVersions.Cats
    val Enumeratum = "com.beachape" %% "enumeratum" % LibraryVersions.Enumeratum
    val EnumeratumPlayJson = "com.beachape" %% "enumeratum-play-json" % LibraryVersions.Enumeratum
//    val Postgres = "org.postgresql" % "postgresql" % LibraryVersions.Postgres
    val Shapeless = "com.chuusai" %% "shapeless" % LibraryVersions.Shapeless
//    val Slick = "com.typesafe.slick" %% "slick" % LibraryVersions.Slick
//    val Slickless = "io.underscore" %% "slickless" % LibraryVersions.Slickless
//    val SlickPg = "com.github.tminglei" %% "slick-pg" % LibraryVersions.SlickPG
//    val SlickPgPlayJson = "com.github.tminglei" %% "slick-pg_play-json" % LibraryVersions.SlickPG
    val Zio = "dev.zio" %% "zio" % LibraryVersions.Zio
    val ZioPrelude = "dev.zio" %% "zio-prelude" % LibraryVersions.ZioPrelude

    // Test libs
//    val ScalaTest  = "org.scalatest" %% "scalatest" % LibraryVersions.ScalaTest % Test
//    val ScalaCheck = "org.scalacheck" %% "scalacheck" % LibraryVersions.ScalaCheck % Test
  }

  private object Packages {
    val AmmoniteAll: Set[ModuleID] = Set(
      Libraries.Ammonite,
      "com.lihaoyi" %% "ammonite-ops" % LibraryVersions.Ammonite,
    )

    val Enumeratum: Set[ModuleID] = Set(
      Libraries.Enumeratum,
      Libraries.EnumeratumPlayJson,
    )

//    val Slick: Set[ModuleID] = Set(
//      Libraries.Slick,
//      "com.typesafe.slick" %% "slick-codegen" % LibraryVersions.Slick,
//      "com.typesafe.slick" %% "slick-hikaricp" % LibraryVersions.Slick,
//    )

//    val SlickPgAll: Set[ModuleID] = Set(
//      Libraries.Postgres,
//      Libraries.SlickPg,
//      Libraries.SlickPgPlayJson,
//    )

    val ZioAll: Set[ModuleID] = Set(
      Libraries.Zio,
      Libraries.ZioPrelude,
    )
  }

  val commonDependencies: Set[ModuleID] = Set(
    Libraries.Cats,
    //Libraries.Postgres,
    Libraries.Shapeless,
    //Libraries.Slickless,

    //Libraries.ScalaCheck,
    //Libraries.ScalaTest,
  ) ++ Set(
    Packages.AmmoniteAll,
    Packages.Enumeratum,
//    Packages.Slick,
//    Packages.SlickPgAll,
    Packages.ZioAll,
  ).flatten

  val allDependencies: Set[ModuleID] =
    commonDependencies ++
      Set(
      )

  val rootDependencies: Set[ModuleID] =
    commonDependencies ++
      Set(
      )
}
