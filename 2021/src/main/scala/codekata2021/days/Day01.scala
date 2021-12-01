package codekata2021
package days

import zio.RIO

object Day01 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 1

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = inputs.sliding(2).count {
      case Seq(a, b) => a < b
      case _ => false
    }.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = inputs.sliding(3).map(_.sum).sliding(2).count {
      case Seq(a, b) => a < b
      case _ => false
    }.zio
  }.some

  def inputs = in2.linesIterator.map(_.big)

  lazy val in2 =
    """199
      |200
      |208
      |210
      |200
      |207
      |240
      |269
      |260
      |263""".stripMargin

  lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}

