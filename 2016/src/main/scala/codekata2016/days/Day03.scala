package codekata2016
package days

import zio.RIO

object Day03 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 3

  case class Triangle(l: Int, m: Int, r: Int) {
    def valid: Boolean = l < m + r && r < l + m && m < l + r
  }
  val num: Parser[Int]       = "[1-9][0-9]*".r ^^ { _.toInt }
  val line: Parser[Triangle] = num ~ num ~ num ^^ { case l ~ m ~ r => Triangle(l, m, r) }

  override def part1: Option[Part] = new Part {

    override def solution: RIO[Any, Any] =
      inputs.linesIterator.toList
        .map(l => parseAll(line, l).get).count(_.valid)
        .zio
  }.some
  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      // First, group into rows of three then transpose, then ungroup and follow part 1.
      inputs.linesIterator.toList.map(_.trim.split(" +").toSeq).grouped(3).flatMap(_.transpose).map(_.mkString(" "))
        .map(l => parseAll(line, l).get).count(_.valid)
        .zio
  }.some

  def inputs = in2

  val in2 = ""
  val in3 = ""

  override def in: String =
    """""".stripMargin
}
