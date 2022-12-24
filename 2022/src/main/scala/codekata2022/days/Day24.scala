package codekata2022
package days

import common.GridNeighbors.{Delta, E, N, Nowhere, S, W}
import scala.annotation.tailrec

object Day24 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 24

  private type Pos = (Int, Int)

  private val cell: Parser[String] = "[#.><^v]".r
  private val gridLine: Parser[IndexedSeq[String]] = rep1(cell).indexed
  private val rawGrid: IndexedSeq[IndexedSeq[String]] = inputs.parseLinesBy(gridLine)
  private val grid = rawGrid.mapGridWithLocation { case (c, xy) => c match {
    case "." => Empty(xy)
    case "#" => Wall(xy)
    case "^" => Blizzard(xy, N, '^')
    case "v" => Blizzard(xy, S, 'v')
    case "<" => Blizzard(xy, W, '<')
    case ">" => Blizzard(xy, E, '>')
    case _ => ???
  }}
  private val walls: Set[Pos] = grid.flatten.filter(_.isInstanceOf[Wall]).map(_.start).toSet
  private val blizzards: Seq[Cell] = grid.flatten.filter(_.isInstanceOf[Blizzard])

  private sealed abstract class Cell {
    protected def c: Char
    def start: Pos
    def at(time: Int): Pos = start
    override def toString: String = c.toString
  }
  private case class Wall(start: Pos) extends Cell { val c = '#' }
  private case class Empty(start: Pos) extends Cell { val c = '.' }
  private case class Blizzard(start: Pos, dir: Delta, protected val c: Char) extends Cell {
    override def at(time: Int): Pos = {
      val (sizeX, sizeY) = rawGrid.bounds - (2, 2)  // exclude walls in each direction
      val (startX, startY) = start - (1, 1)  // translate out the walls
      val (dX, dY) = dir * time
      val (wrappedDeltaX, wrappedDeltaY) =
        ((dX % sizeX + sizeX) % sizeX) ->  // TODO(Dave): This "mod then add then mod" for wrapping is
          ((dY % sizeY + sizeY) % sizeY)   // TODO(Dave): pretty common, it seems.  Perhaps extract it.
      val wrappedPos =
        (((wrappedDeltaX + startX) % sizeX + sizeX) % sizeX) ->
          (((wrappedDeltaY + startY) % sizeY + sizeY) % sizeY)
      wrappedPos + (1, 1)  // translate back the walls
    }
  }

  private val startPos = (1, 0)
  private val endPos = grid.bounds - (1, 1) - (1, 0)  // From bottom left

  private case class State(pos: Pos, history: List[Pos])

  @tailrec private def bfs(
      goal: Pos,
      initialTime: Int,
      left: IndexedSeq[State],
      seen: Set[(Pos, Int)]): Option[State] = left.headOption match {
    case None => None
    case Some(me) =>
      // ((me.history.size + initialTime) -> (left.size - 1)).debugSameLine  // uncomment for progress
      if (me.pos == goal) {
        // "\n".debug  // Uncomment for progress
        me.some
      } else if (seen.contains(me.pos -> (me.history.size + initialTime))) {
        bfs(goal, initialTime, left.drop(1), seen)
      } else {
        val nextTime = me.history.size + 1 + initialTime
        val blizzardsNextTime = blizzards.map(_.at(nextTime)).toSet
        val todos = Seq(N, S, E, W, Nowhere).flatMap { delta =>
          grid.clip(me.pos + delta)
            .filterNot(p => walls(p) || blizzardsNextTime(p) || seen(p -> nextTime))
        }.map(p => State(p, me.pos :: me.history))
        bfs(goal, initialTime, left.tail ++ todos, seen + (me.pos -> (nextTime - 1)))
      }
  }

  private lazy val gettingThere = bfs(endPos, 0, IndexedSeq(State(startPos, Nil)), Set.empty)

  override def part1: Option[Part] = PuzzlePart({
    gettingThere.map(_.history.size)
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    for {
      first <- gettingThere
      firstSize = first.history.size
      second <- bfs(startPos, firstSize, IndexedSeq(State(endPos, Nil)), Set.empty)
      secondSize = second.history.size
      third <- bfs(endPos, firstSize + secondSize, IndexedSeq(State(startPos, Nil)), Set.empty)
      thirdSize = third.history.size
    } yield firstSize + secondSize + thirdSize
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"""

  private lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
