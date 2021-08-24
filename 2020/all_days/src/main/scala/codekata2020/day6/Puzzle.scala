package codekata2020.day6

import codekata2020._

object Puzzle {
  private val inputs = in.split("\n\n")

  object Part1 {
    def solution = {
      inputs.map(_.toSet.-('\n').size).sum
    }.zio
  }

  object Part2 {
    def solution = {
      inputs.map(_.split("\n").map(_.toSet).reduce(_ intersect _).size).sum
    }.zio
  }

  private lazy val in2 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
