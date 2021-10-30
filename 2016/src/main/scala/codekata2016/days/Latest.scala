package codekata2016
package days

import zio.RIO

object Latest extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 3

  override def part1: Option[Part] = None
  override def part2: Option[Part] = None

  def inputs = in2

  val in2 = ""
  val in3 = ""

  override def in: String =
    """""".stripMargin
}
