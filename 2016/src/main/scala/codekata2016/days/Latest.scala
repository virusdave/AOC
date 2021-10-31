package codekata2016
package days

import zio.RIO

object Latest extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 6

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = ???
  }.some.filter(_ => false)

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = ???
  }.some.filter(_ => false)

  def inputs = in2

  val in2 = ""
  val in3 = ""

  override def in: String =
    """""".stripMargin
}
