package codekata2021
package days

import zio.RIO

object Day01 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 1

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = ???
  }.some

  override def part2: Option[Part] = None

  def inputs = in

  val in2 = ""
  val in3 = ""

  override def in: String = ""
}
