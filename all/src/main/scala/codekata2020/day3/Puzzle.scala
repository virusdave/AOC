package codekata2020.day3

import codekata2020._
import zio.ZIO
import zio.console._

object Puzzle {
  private val ins = in.linesIterator.toIndexedSeq.map(_.map{case '#' => 1; case _ => 0})

  object Part1 {
    def solution(silent: Boolean) =
      ins.indices.map { y => ins(y)(3 * y % ins(y).length)}.sum.zio
  }

  object Part2 {
    def solution(silent: Boolean) = Seq(1->1, 3->1, 5->1, 7->1, 1->2).map { case (dx, dy) =>
      (ins.indices by dy).map { y => BigInt(ins(y)(y/dy * dx % ins(y).length))}.sum
    }.product.zio
  }

  private lazy val in2 = """..##.......
                   |#...#...#..
                   |.#....#..#.
                   |..#.#...#.#
                   |.#...##..#.
                   |..#.##.....
                   |.#.#.#....#
                   |.#........#
                   |#.##...#...
                   |#...##....#
                   |.#..#...#.#""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
