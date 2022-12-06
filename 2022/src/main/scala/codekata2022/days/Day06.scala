package codekata2022
package days

import common.InRegexParserSyntax

object Day06 extends ParserPuzzle with InRegexParserSyntax {
  override type PuzzleOut = Any
  override def dayNum: Int = 6

  override def part1: Option[Part] = PuzzlePart({
    inputs.zipWithIndex.sliding(4).toSeq.find(_.map(_._1).distinct.size == 4).get.last._2 + 1
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    inputs.zipWithIndex.sliding(14).toSeq.find(_.map(_._1).distinct.size == 14).get.last._2 + 1
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """mjqjpqmgbljsphdztnvjfqwrcgsmlb"""

  private lazy val in3 =
    """zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"""

  override def in: String =
    """"""
}
