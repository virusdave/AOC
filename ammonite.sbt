// Get an ammonite REPL by doing:
//  sbt projectName/test:run
// or
//  sbt projectName/test:run-main amm
sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue