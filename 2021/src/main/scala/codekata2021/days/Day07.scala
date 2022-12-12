package codekata2021
package days

object Day07 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 7

  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val nums: Seq[Int] = inputs.parseBy(rep1sep(num, ",")).sorted

  override def part1: Option[Part] = PuzzlePart(
    (nums.head to nums.last).map { x =>
      x -> nums.map(y => Math.abs(y - x)).sum
    }.minBy(_._2)._2.zio).some

  override def part2: Option[Part] = PuzzlePart(
    (nums.head to nums.last).map { x =>
      x -> nums.map { y =>
        val d = Math.abs(y - x)
        d * (d + 1) / 2
      }.sum
    }.minBy(_._2)._2.zio).some

  private def inputs = in2

  private lazy val in2 =
    """16,1,2,0,4,2,7,1,2,14"""

  override def in: String =
    """""".stripMargin
}
