package codekata2021
package days

object Day12 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 12

  private sealed trait Cave { def name: String }
  private case class Big(name: String) extends Cave
  private case class Small(name: String) extends Cave

  private val small: Parser[Small]       = "[a-z]+".r ^^ (Small)
  private val big: Parser[Big]           = "[A-Z]+".r ^^ (Big)
  private val line: Parser[(Cave, Cave)] = (small | big) ~ ("-" ~> (small | big)) ^^ { case l ~ r => l -> r }

  private val lines = inputs.parseLinesBy(line)

  override def part1: Option[Part] = PuzzlePart({
    val paths: Map[Cave, Seq[Cave]] = (lines ++ lines.map(_.swap)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    def pathsTo(from: Cave, seen: Set[Small]): BigInt = {
      if (from == Small("end")) 1
      else paths.get(from).fold2(
        0, _.map {
          case c: Small => if (seen(c)) 0.big else pathsTo(c, seen + c)
          case c: Big => pathsTo(c, seen)
        }.sum)
    }
    pathsTo(Small("start"), Set(Small("start")))
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val paths: Map[Cave, Seq[Cave]] = (lines ++ lines.map(_.swap)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    def pathsTo(from: Cave, seen: Set[Small], seenTwice: Option[Small]): BigInt = {
      if (from == Small("end")) 1
      else paths.get(from).fold2(
        0, _.map {
          case c: Small =>
            if (seen(c) && (seenTwice.isDefined || c == Small("start"))) 0.big
            else if (seen(c)) pathsTo(c, seen, c.some)
            else pathsTo(c, seen + c, seenTwice)
          case c: Big => pathsTo(c, seen, seenTwice)
        }.sum)
    }
    pathsTo(Small("start"), Set(Small("start")), None)
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """dc-end
      |HN-start
      |start-kj
      |dc-start
      |dc-HN
      |LN-dc
      |HN-end
      |kj-sa
      |kj-HN
      |kj-dc""".stripMargin

  private lazy val in3 =
    """fs-end
      |he-DX
      |fs-he
      |start-DX
      |pj-DX
      |end-zg
      |zg-sl
      |zg-pj
      |pj-he
      |RW-he
      |fs-DX
      |pj-RW
      |zg-RW
      |start-pj
      |he-WI
      |zg-he
      |pj-fs
      |start-RW""".stripMargin

  override def in: String =
    """""".stripMargin
}
