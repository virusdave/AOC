package codekata2022
package days

import spire.implicits._
import spire.math._

object Day04 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 4

  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val interval: Parser[Interval[Int]] = (num <~ "-") ~ num ^^ { case l ~ r => Interval.closed(l, r) }
  private val pair: Parser[(Interval[Int], Interval[Int])] = (interval <~ ",") ~ interval ^^ (_.toTuple)

  private val elves = inputs.parseLinesBy(pair)

  override def part1: Option[Part] = PuzzlePart({
    elves.count { case l -> r => l.isSubsetOf(r) || r.isSubsetOf(l) }
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    elves.count { case l -> r => l intersects r }
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8""".stripMargin

  override def in: String =
    """""".stripMargin
}
