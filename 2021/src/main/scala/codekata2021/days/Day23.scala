package codekata2021
package days

import zio.RIO

object Day23 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 23

  private val word: Parser[String]   = "[a-zA-Z]+".r
  private val num: Parser[Int]       = "[0-9]+".r ^^ (_.toInt)
  private val neg: Parser[Int]       = "-" ~> num ^^ (-_)
  private val signedNum: Parser[Int] = num | neg

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      ()
      ()
    }.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      ()
      ()
    }.zio
  }.some

  def inputs = in2
  // 9 2 = 11 = 11
  // 4 3 4 = 11 = 110
  // 4 = 4 = 400
  // 5 7 = 12 = 12000

  lazy val in2 =
    """#############
      |#...........#
      |###B#C#B#D###
      |  #A#D#C#A#
      |  #########""".stripMargin

  lazy val in3 =
    """"""

  // 6 2 3 4      =  15
  // 5 5 3 4 = 17 = 170
  // 6 7 = 13   =  1300
  // 2 3 8 = 13 = 13000
  //              14620
  override def in: String =
    """#############
      |#...........#
      |###D#A#A#D###
      |  #C#C#B#B#
      |  #########""".stripMargin
}
