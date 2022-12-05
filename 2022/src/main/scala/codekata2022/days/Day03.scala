package codekata2022
package days

import scala.language.postfixOps

object Day03 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 3

  private def cToScore(c: Char): Int =
    if (c >= 'a' && c <= 'z') c - 'a' + 1
    else if (c >= 'A' && c <= 'Z') c - 'A' + 27
    else ???

  override def part1: Option[Part] = PuzzlePart({
    inputs.splitAtLinebreaksBy { s =>
      val (l, r) = s.splitAt(s.length / 2)
      l.toSet intersect r.toSet head
    }.map(cToScore).sum
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    inputs.linesIterator.grouped(3).map(_.map(_.toSet).reduce(_ intersect _).head).map(cToScore).sum
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  override def in: String =
    """""".stripMargin
}
