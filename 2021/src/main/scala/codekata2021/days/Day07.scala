package codekata2021
package days

import zio.RIO

object Day07 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 7

  val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val nums     = parseAll(rep1sep(num, ","), inputs).get.sorted

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      (nums.head to nums.last).map { x =>
        x -> nums.map(y => Math.abs(y - x)).sum
      }.minBy(_._2)._2.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      (nums.head to nums.last).map { x =>
        x -> nums.map { y =>
          val d = Math.abs(y - x)
          d * (d + 1) / 2
        }.sum
      }.minBy(_._2)._2.zio
  }.some

  def inputs = in2

  lazy val in2 =
    """16,1,2,0,4,2,7,1,2,14"""

  lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
