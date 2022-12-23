package codekata2022
package days

import common.GridNeighbors
import common.GridNeighbors.Delta
import scala.annotation.tailrec

object Day23 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 23

  private type Pos = (Int, Int)
  private val gridPoint = "." ^^^ false | "#" ^^^ true
  private val gridLine = rep1(gridPoint).indexed
  private val grid = inputs.parseLinesBy(gridLine)
  private val elves: Set[Pos] = grid.mapGridWithLocation {
    case (true, xy) => xy.some
    case (false, _) => None
  }.flatten.flatten.toSet

  private abstract class MoveOption(ifNotOccupiedDeltas: Set[Delta], proposeDelta: Pos) {
    def maybeMoveElf(elf: Pos, elves: Set[Pos]): Option[Pos] =
      (elf + proposeDelta).when(_ => ifNotOccupiedDeltas.forall(delta => !elves.contains(delta + elf)))
  }
  private object MoveOption {

    import GridNeighbors._

    case object Isolated extends MoveOption(All8Neighbors.toSet, Nowhere)
    case object North extends MoveOption(Set(N, NW, NE), N)
    case object South extends MoveOption(Set(S, SW, SE), S)
    case object West extends MoveOption(Set(W, NW, SW), W)
    case object East extends MoveOption(Set(E, NE, SE), E)
  }

  private val movesToConsider: LazyList[MoveOption] = {
    import MoveOption._
    LazyList.continually(LazyList.from(Seq(North, South, West, East))).flatten
  }

  private def bounds(xys: Set[Pos]): ((Int, Int), (Int, Int)) =
    xys.unzip.diag(_._1.min -> _._2.min) -> xys.unzip.diag(_._1.max -> _._2.max)

  private def renderBounded(xys: Set[Pos]): String = {
    val (mins, maxs) = bounds(xys)
    val (extentX, extentY) = maxs - mins + (1 -> 1)
    val boundedLocations = xys.map(_ - mins)
    Grid.tabulate2d(extentX, extentY, pos => if (boundedLocations(pos)) '#' else '.').show(identity)
  }

  private def boundedEmptyCount(xys: Set[Pos]): Int = {
    val (mins, maxs) = bounds(xys)
    val (extentX, extentY) = maxs - mins + (1 -> 1)
    val boundedLocations = xys.map(_ - mins)
    Grid.allXYsLazy(extentX, extentY).count(!boundedLocations(_))
  }

  private def stepElves(elves: Set[Pos], directions: LazyList[MoveOption]): Set[Pos] = {
    val cc = MoveOption.Isolated +: directions.take(4)
    val proposedDests = elves.foldLeft(Map.empty[Pos, Pos] -> Set.empty[Pos]) { case (destMap, tooMany) -> elf =>
      if (tooMany.contains(elf)) destMap -> tooMany
      else {
        val newPos = cc.flatMap(_.maybeMoveElf(elf, elves)).headOption.getOrElse(elf)
        if (destMap.contains(newPos)) (destMap - newPos) -> (tooMany + newPos)
        else (destMap + (newPos -> elf)) -> tooMany
      }
    }._1.map(_.swap)
    elves.map(e => proposedDests.getOrElse(e, e))
  }

  override def part1: Option[Part] = PuzzlePart({
    (1 to 10).foldLeft(movesToConsider -> elves) { case (directions, elves) -> idx =>
      val newElves = stepElves(elves, directions)
      //s"--- $idx ---\n${renderBounded(newElves)}\n===".debug
      directions.tail -> newElves
    }._2 |> boundedEmptyCount
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    @tailrec def iterate(directions: LazyList[MoveOption], elves: Set[Pos], idx: Int): Int = {
      val newElves = stepElves(elves, directions)
      //s"--- $idx ---\n${renderBounded(newElves)}\n===".debug
      if (elves == newElves) idx
      else iterate(directions.tail, newElves, idx + 1)
    }
    iterate(movesToConsider, elves, 1)
  }.zio).some

  private def inputs = in3

  private lazy val in2 =
    """.....
..##.
..#..
.....
..##.
....."""

  private lazy val in3 =
    """....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#.."""

  override def in: String =
    """""".stripMargin
}
