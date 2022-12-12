package codekata2021
package days

object Day02 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 2

  private type Move = (Int, Int)
  private val line: Parser[Move] = {
    val num: Parser[Int]      = "[0-9]+".r ^^ (_.toInt)
    val forward: Parser[Move] = "forward" ~> num ^^ { n => (n, 0) }
    val up: Parser[Move]      = "up" ~> num ^^ { n => (0, -n) }
    val down: Parser[Move]    = "down" ~> num ^^ { n => (0, n) }
    forward | up | down
  }

  private val moves = inputs.parseLinesBy(line)

  override def part1: Option[Part] = PuzzlePart {
    moves.foldLeft((0, 0)) { case ((px, py), (x, y)) => (px + x, py + y) }.zio.map { case (x, y) => x * y }
  }.some

  override def part2: Option[Part] = PuzzlePart {
    moves.foldLeft((0, 0, 0)) { case ((horizontal, depth, aim), (f, d)) =>
      (horizontal + f, depth + aim * f, aim + d)
    }.zio.map { case (x, y, _) => x * y }
  }.some

  private def inputs = in

  private lazy val in2 =
    """forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2""".stripMargin

  override def in: String =
    """""".stripMargin
}
