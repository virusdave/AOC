package codekata2022
package days

import zio.RIO

object Day01 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 1

  val elves = inputs.splitToLinesAtDoubleLinebreaksBy(_.toInt).map(_.sum)

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      elves.max
    }.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      elves.sorted.reverse.take(3).sum
    }.zio
  }.some

  def inputs = in2

  lazy val in2 =
    """1000
      |2000
      |3000
      |
      |4000
      |
      |5000
      |6000
      |
      |7000
      |8000
      |9000
      |
      |10000""".stripMargin

  lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
