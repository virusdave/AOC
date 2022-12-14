package codekata2022
package days

import scala.annotation.tailrec

object Day14 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 14

  private type Loc = (Int, Int)
  private val sandOrigin = (500, 0)
  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val pair: Parser[Loc] = num ~ ("," ~> num) ^^ (_.toTuple)
  private val stroke: Parser[IndexedSeq[Loc]] = rep1sep(pair, "->") ^^ (_.toIndexedSeq)

  @tailrec private def dropSand(yMax: Int, rocks: Set[Loc], sand: Set[Loc]): Set[Loc] = {
    def free(loc: Loc) = !(rocks.contains(loc) || sand.contains(loc))
    @tailrec def drop(loc: Loc): Set[Loc] =
      if (loc._2 > yMax) sand // Fell off bottom of the world
      else Seq((0, 1), (-1, 1), (1, 1)).map(_ + loc).find(free) match {
        case None => sand + loc
        case Some(next) => drop(next)
      }
    val newSand = drop(sandOrigin)
    if (newSand.size == sand.size || newSand.contains(sandOrigin)) newSand
    else dropSand(yMax, rocks, newSand)
  }

  private def getRocks(strokes: IndexedSeq[IndexedSeq[Loc]]): Set[Loc] =
    strokes.foldLeft(Set.empty[Loc]) { case (rocks, stroke) =>
      stroke.tail.foldLeft(rocks -> stroke.head) { case ((rocks, from), to) =>
        val delta = to - from  // Stroke extent
        val step = delta match {  // either (1,0), (-1,0), (0,1), or (0,-1)
          case (x, 0) => (x / x.abs, 0)
          case (0, y) => (0, y / y.abs)
        }
        val len = (delta.max + delta.min).abs  // How many times to add `step` to itself to get `delta`
        rocks ++ (1 to len).scanLeft((0, 0)) { case soFar -> _ => soFar + step }.map(_ + from) -> to
      }._1 // rocks
    }

  override def part1: Option[Part] = PuzzlePart({
    val rocks = inputs.parseLinesBy(stroke) |> getRocks
    dropSand(rocks.map(_._2).max, rocks, Set.empty)
      //.tap(render(rocks, _).debug: Unit)
      .size
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val parsed = inputs.parseLinesBy(stroke)
    val floor = parsed.flatMap(_.map(_._2)).max + 2
    val rocks = (parsed :+ IndexedSeq(-9000 -> floor, 9000 -> floor)) |> getRocks
    dropSand(rocks.map(_._2).max, rocks, Set.empty)
      //.tap(render(rocks, _).debug: Unit)
      .size
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""

  override def in: String =
    """""".stripMargin

  private def render(rock: Set[Loc], sand: Set[Loc], bounds: Option[(Loc, Loc)] = None): String = {
    def getBounds(sand: Set[Loc]): (Loc, Loc) = {
      val (xmin, xmax) = sand.map(_._1).diag(_.min -> _.max) + (-1, 1)
      val ymax = sand.map(_._2).max
      (xmin, 0) -> (xmax, ymax)
    }

    bounds.getOrElse(getBounds(sand)) match {
      case ((x1, y1), (x2, y2)) =>
        Grid.fill2d(x2 - x1 + 1, y2 - y1 + 1)(()).mapGridWithLocation { case (_, (x, y)) =>
          val loc = (x + x1) -> (y - y1)
          if (loc == sandOrigin) '+'
          else if (rock.contains(loc)) '#'
          else if (sand.contains(loc)) 'o'
          else '.'
        }.map(_.mkString).mkString("\n")
    }
  }
}
