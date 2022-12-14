package codekata2022
package days

object Latest extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 15

  private val word: Parser[String]   = "[a-zA-Z]+".r
  private val num: Parser[Int]       = "[0-9]+".r ^^ (_.toInt)
  private val neg: Parser[Int]       = "-" ~> num ^^ (-_)
  private val signedNum: Parser[Int] = num | neg

  override def part1: Option[Part] = PuzzlePart({
    ()
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    ()
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """"""

  private lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
