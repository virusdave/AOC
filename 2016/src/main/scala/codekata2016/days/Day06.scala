package codekata2016
package days

import zio.RIO

object Day06 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 6

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      inputs.linesIterator.toSeq.transpose.map(_.groupBy(identity).view.mapValues(_.size).toSeq.maxBy(_._2)._1).mkString
        .zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      inputs.linesIterator.toSeq.transpose.map(_.groupBy(identity).view.mapValues(_.size).toSeq.minBy(_._2)._1).mkString.zio
  }.some

  def inputs = in2

  lazy val in2 =
    """eedadn
      |drvtee
      |eandsr
      |raavrd
      |atevrs
      |tsrnev
      |sdttsa
      |rasrtv
      |nssdts
      |ntnada
      |svetve
      |tesnvt
      |vntsnd
      |vrdear
      |dvrsen
      |enarar""".stripMargin
  lazy val in3 = ""

  override def in: String =
    """""".stripMargin
}
