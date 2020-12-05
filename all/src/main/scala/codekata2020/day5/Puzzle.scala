package codekata2020.day5

import codekata2020._

object Puzzle {
  private val inputs = in.linesIterator.toIndexedSeq

  object Part1 {
    def solution = {
      inputs.map { line =>
        Integer.parseInt(line.map {
          case 'B' | 'R' => '1'
          case 'F' | 'L' => '0'
        }, 2)
      }.max
    }.zio
  }

  object Part2 {
    def solution = {
      (1 to 1024).toSet.removedAll (
      inputs.map { line =>
        Integer.parseInt(line.map {
          case 'B' | 'R' => '1'
          case 'F' | 'L' => '0'
        }, 2)
      }.toSet).toList.sorted.sliding(3).collectFirst {
        case a::b::c::Nil if b - a > 1 && c - b > 1 => b
      }
    }.zio
  }

  private lazy val in2 =
    """FBFBBFFRLR""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
