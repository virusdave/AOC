package codekata2015
package days

import zio.RIO

object Day17 extends Puzzle {
  override type A = Any
  override def dayNum: Int = 17

  override def part1: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] =
        (1 to inputs.size)
          .flatMap(inputs.combinationsWithRepetition(_))
          .count(_.sum == target)
          .zio
    }.some
  override def part2: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = {
        val acceptable = (1 to inputs.size)
          .flatMap(inputs.combinationsWithRepetition(_))
          .map(x => x -> (x.sum == target))
          .filter(_._2)
          .sortBy(_._1.size)
        acceptable
          .takeWhile(_._1.size == acceptable.head._1.size)
          .size
          .zio
      }
    }.some

  def inputs = in.linesIterator.map(_.toInt).toSeq

  private val target = 150

  val in2 = Seq(20, 15, 10, 5, 5).mkString("\n")
  val in3 = ""

  override def in: String = """11
                              |30
                              |47
                              |31
                              |32
                              |36
                              |3
                              |1
                              |5
                              |3
                              |32
                              |36
                              |15
                              |11
                              |46
                              |26
                              |28
                              |1
                              |19
                              |3""".stripMargin
}
