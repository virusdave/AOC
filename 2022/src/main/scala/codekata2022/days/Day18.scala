package codekata2022
package days

import scala.annotation.tailrec

object Day18 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 18

  private type Loc = (Int, Int, Int)

  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val line: Parser[Loc] = (num <~ ",") ~ (num <~ ",") ~ num ^^ (_.toTuple3)

  private val parsed: IndexedSeq[Loc] = inputs.parseLinesBy(line)
  private val sixNeighborDeltas: Seq[Loc] = for {
    x <- Seq(-1, 0, 1)
    y <- Seq(-1, 0, 1)
    z <- Seq(-1, 0, 1) if (x != 0 || y != 0 || z != 0) && (x, y, z).abs.sum == 1
  } yield (x, y, z)

  private val cubes: Set[Loc] = parsed.toSet

  private def surfaceArea(cubes: Set[Loc]): Int = cubes.map { c =>
    6 - sixNeighborDeltas.count(n => cubes.contains(c + n))
  }.sum

  override def part1: Option[Part] = PuzzlePart({
    surfaceArea(cubes)
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val ((xmin, xmax), (ymin, ymax), (zmin, zmax)) = (
      parsed.map(_._1).bounds,
      parsed.map(_._2).bounds,
      parsed.map(_._3).bounds)

    @tailrec def bfsToTargets(todo: IndexedSeq[Loc], seen: Set[Loc], targets: Set[Loc]): Boolean =
      todo.headOption match { // true iff anything in `todo` can reach anything in `targets`
        case None => false
        case Some(h) if targets.contains(h) => true
        case Some(h) =>
          val next = sixNeighborDeltas.map(_ + h).filterNot(seen.contains).filterNot(cubes.contains)
          bfsToTargets(todo.tail ++ next, seen ++ next, targets)
      }

    val locations = for {
      x <- xmin to xmax
      y <- ymin to ymax
      z <- zmin to zmax
    } yield (x, y, z)

    val definitelyExterior = (xmin - 1, ymin - 1, zmin - 1)
    case class State(exteriors: Set[Loc] = Set(definitelyExterior), interiors: List[Loc] = Nil)
    val State(_, interiors) =
      locations.foldLeft(State()) { case (state@State(exteriors, interiors)) -> loc =>
        if (cubes.contains(loc)) state
        else if (bfsToTargets(IndexedSeq(loc), Set.empty, exteriors))
          state.copy(exteriors = exteriors + loc)
        else state.copy(interiors = loc :: interiors)
      }

    surfaceArea(cubes) - surfaceArea(interiors.toSet)
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"""

  override def in: String =
    """""".stripMargin
}
