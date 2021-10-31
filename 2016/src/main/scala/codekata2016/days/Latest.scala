package codekata2016
package days

import zio.RIO

object Latest extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 8

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = ???
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = ???
  }.some.filter(_ => false)

  def inputs = in2

  lazy val in2 = ""
  lazy val in3 = ""

  override def in: String =
    """""".stripMargin
}
