package codekata2020.day9

import codekata2020._
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  private val inputs = in.linesIterator.toIndexedSeq.map(BigInt.apply)

  object Part1 {
    def solution = {
      val width = 25
      val windows = inputs.sliding(width + 1)
      windows.flatMap(window => {
        val first = window.take(width).toSet
        if (first.exists(n => first(window.last - n))) None
        else window.last.some
      }).toList
    }.zio
  }

  object Part2 {
    def solution = {
      val target = BigInt("12345")  // answer from part 1

      (2 until inputs.size).flatMap { width =>
        inputs.sliding(width).find(_.sum == target).map(_.sorted.diag(_.head + _.last))
      }
    }.zio
  }

  private lazy val in2 =
    """35
      |20
      |15
      |25
      |47
      |40
      |62
      |55
      |65
      |95
      |102
      |117
      |150
      |182
      |127
      |219
      |299
      |277
      |309
      |576""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
