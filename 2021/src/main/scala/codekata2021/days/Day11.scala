package codekata2021
package days

import zio.RIO

object Day11 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 11

  def step(in: Grid[Int], plus: Grid[Int], flashed: Set[(Int, Int)]): (Grid[Int], Set[(Int, Int)]) = {
    val next        = in.mapGridWithLocation { case (v, (x, y)) => v + plus.getOrElse(x, y, 0) }
    val justFlashed = next.locationsInGrid(_ > 9).map(_._2).toSet -- flashed
    val (afterFlashed, alsoFlashed) =
      if (justFlashed.nonEmpty)
        step(
          next,
          next.mapGridWithLocation { case (_, (x, y)) =>
            (for {
              dx <- -1 to 1
              dy <- -1 to 1 if dx != 0 || dy != 0
            } yield if (justFlashed.contains((x + dx, y + dy))) 1 else 0).sum
          },
          flashed ++ justFlashed,
        )
      else next -> Set.empty

    afterFlashed.mapGrid(v => if (v > 9) 0 else v) -> (flashed ++ justFlashed ++ alsoFlashed)
  }

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      (1 to 100).foldLeft((inputs, 0)) { case ((inputs, flashes), _ /*n*/ ) =>
        val (next2, justFlashed) = step(inputs, inputs.mapGrid(_ => 1), Set.empty)
        next2 -> (flashes + justFlashed.size)
      }._2
        .zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      (1 to 10000).foldLeft((inputs, Option.empty[Int])) { case ((inputs, allFlashed), n) =>
        allFlashed.fold2(
          {
            val (next2, justFlashed) = step(inputs, inputs.mapGrid(_ => 1), Set.empty)
            val (mx, my)             = inputs.bounds
            next2 -> (n.some.filter(_ => justFlashed.size == mx * my))
          },
          _ => (inputs, allFlashed),
        )
      }._2
        .zio
  }.some

  def inputs = in2.linesIterator.toIndexedSeq.map(_.toIndexedSeq.map(_ - '0'))

  lazy val in2 =
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526""".stripMargin

  lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
