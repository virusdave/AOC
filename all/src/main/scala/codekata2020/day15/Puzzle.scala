package codekata2020.day15

import codekata2020._

object Puzzle {
  private val inputs = in2.split(",").map(_.toInt).toIndexedSeq

  object Part1 {
    def solution = {
      (1 until 2020).foldLeft((Map.empty[Int, Int], 0)) { case ((state, currentWord), idx) =>
        val realCurrentWord = if (idx <= inputs.length) inputs(idx - 1) else currentWord
        val lastSeen = state.get(realCurrentWord)
        (state + (realCurrentWord -> idx), lastSeen.map(idx - _).getOrElse(0))
      }._2
    }.zio
  }

  object Part2 {
    def solution = {
      (1 until 30000000).foldLeft((Map.empty[Int, Int], 0)) { case ((state, currentWord), idx) =>
        val realCurrentWord = if (idx <= inputs.length) inputs(idx - 1) else currentWord
        val lastSeen = state.get(realCurrentWord)
        (state + (realCurrentWord -> idx), lastSeen.map(idx - _).getOrElse(0))
      }._2
    }.zio
  }

  private lazy val in2 =
    """0,3,6""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
