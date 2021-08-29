package codekata2015
package days

import util.NDimConway
import zio.RIO

object Day18 extends Puzzle {
  override type A = Any
  override def dayNum: Int = 18

  override def part1: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] =
        NDimConway
          .iterateNdimWorld(100, inputs, strictBoundaries = true)
          .size
          .zio
    }.some
  override def part2: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = {
        def addCorners(board: Set[List[Int]]) =
          board ++ Set(List(0, 0), List(99, 99), List(0, 99), List(99, 0))

        (1 to 100)
          .foldLeft(addCorners(inputs)) {
            case (board, _) =>
              addCorners(NDimConway.iterateNdimWorld(1, board, strictBoundaries = true))
          }
          .size
          .zio
      }
    }.some

  def inputs =
    in.linesIterator.toIndexedSeq
      .map(_.toIndexedSeq)
      .mapGridWithLocation {
        case ('#', (x, y)) => List(x, y).some
        case _             => None
      }
      .flatten
      .flatten
      .toSet

  val in2 = """.#.#.#
              |...##.
              |#....#
              |..#...
              |#.#..#
              |####..""".stripMargin
  val in3 = ""

  override def in: String =
    "".stripMargin
}
