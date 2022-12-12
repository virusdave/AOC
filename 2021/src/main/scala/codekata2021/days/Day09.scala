package codekata2021
package days

import scala.annotation.tailrec

object Day09 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 9

  override def part1: Option[Part] = PuzzlePart(
    inputs.mapGridWithLocation { case (v, (x, y)) =>
      val a1 = inputs.getOrElse(x - 1, y, 10)
      val a2 = inputs.getOrElse(x + 1, y, 10)
      val a3 = inputs.getOrElse(x, y - 1, 10)
      val a4 = inputs.getOrElse(x, y + 1, 10)

      v.some.filter(v => v < a1 && v < a2 && v < a3 && v < a4).map(_ + 1)
    }.flatten.flatten.sum.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val nonBoundaries = inputs.mapGrid {
      _ < 9
    }
    case class Basin(x: Int, y: Int, size: Int) // For debugging; we really only want `size`

    /** To find a basin, first find any point which isn't already in a basin or boundary,
      * then "flood-fill" to find all non-boundaries that are reachable.  Everything so
      * discovered is in the basin, if anything was initially found.
      *
      * If a basin WAS found this way, returns the grid with the remaining basin candidates.
      */
    def findBasin(in: Grid[Boolean]): Option[(Basin, Grid[Boolean])] = {
      val found = in.findInGrid(identity).map(_._2)

      def floodFind(soFar: Set[(Int, Int)], fromX: Int, fromY: Int): Set[(Int, Int)] =
        if (soFar.contains((fromX, fromY)) || !in.getOrElse(fromX, fromY, false)) soFar
        else {
          Set(
            (fromX - 1) -> fromY,
            (fromX + 1) -> fromY,
            fromX -> (fromY - 1),
            fromX -> (fromY + 1),
          )
            .filter { case (x, y) => in.clip(x, y).isDefined }
            .foldLeft(soFar) { case (soFar, (x, y)) => floodFind(soFar + (fromX -> fromY), x, y) }
        }

      found.fold2(
        // Couldn't find ANY eligible basin point: no basin
        None,
        // Found an initial point in some basin...
        { case (x, y) =>
          // ... and the rest of the basin via a flood-fill
          val rest = floodFind(Set.empty, x, y)
          (Basin(x, y, rest.size) ->
            // The remaining non-basin parts of the grid, which excludes anything we just
            // determined is in the current basin
            in.mapGridWithLocation { case (v, (x, y)) => !rest.contains((x, y)) && v }).some
        },
      )
    }

    @tailrec
    def findEm(in: Grid[Boolean], soFar: List[Basin]): List[Basin] = {
      val found = findBasin(in)
      found match {
        case None => soFar
        case Some((basin, rest)) => findEm(rest, basin :: soFar)
      }
    }

    findEm(nonBoundaries, Nil).map(_.size).sorted.reverse.take(3).product
  }
    .zio).some

  private def inputs = in2.linesIterator.map(_.toSeq.map(_.toString.toInt)).toIndexedSeq

  private lazy val in2 =
    """2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678""".stripMargin

  override def in: String =
    """""".stripMargin
}
