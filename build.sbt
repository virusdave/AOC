import Dependencies._

scalacOptions += "-Ypartial-unification"

lazy val common = (project in file("common"))
  .settings(Settings.settings: _*)
  .settings(libraryDependencies ++= commonDependencies.toSeq)

lazy val all = (project in file("all"))
  .settings(Settings.settings: _*)
  .settings(Settings.allSettings: _*)
  .dependsOn(
    common,
  )
  .configs(Test)

lazy val root = (project in file("."))
  .aggregate(
    all,
  )
  .settings(Settings.settings: _*)
  .settings(Settings.allSettings: _*)
  .settings(libraryDependencies ++= rootDependencies.toSeq)
