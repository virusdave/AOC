package codekata2022
package days

import common.InRegexParserSyntax
import scala.annotation.tailrec

object Day12 extends ParserPuzzle with InRegexParserSyntax {
  override type PuzzleOut = Any
  override def dayNum: Int = 12

  private val height: Parser[Int] = "[a-z]".r ^^ (_.head - 'a')
  private val you: Parser[Int] = "S" ^^^ 27 // Assume you can move to any square from start
  private val dest: Parser[Int] = "E" ^^^ 26 // Assume the target can only be reached from a `z` height
  private val heights = rep(height | you | dest) ^^ (_.toIndexedSeq)

  private val grid = inputs.parseLinesBy(heights)
  private val start = grid.findInGrid(_ == 27).get._2
  private val target = grid.findInGrid(_ == 26).get._2

  private type Loc = (Int, Int)
  private case class State(at: Loc, distance: Int)

  // TODO(Dave): Should probably have a canonical `bfs` implementation in the library
  @tailrec private def bfs(state: State, pending: IndexedSeq[State], seen: Set[Loc]): Option[Int] = {
    if (state.at == target) state.distance.some
    else {
      val curHeight = grid.get(state.at).get
      val nextSteps = Seq((-1, 0), (1, 0), (0, 1), (0, -1)).map(_ + state.at)
        .filter(grid.clip(_).isDefined)  // Candidate is in the grid still
        .filterNot(seen.contains)  // and we haven't seen it yet
        .filter(grid.get(_).get <= curHeight + 1)  // and it's low enough to be reachable

      val newPending = pending ++ nextSteps.map(State(_, state.distance + 1))
      if (newPending.isEmpty) None // Ran out of "next candidate"s
      else bfs(newPending.head, newPending.tail, seen ++ nextSteps + state.at)
    }
  }

  override def part1: Option[Part] = PuzzlePart({
    bfs(State(start, 0), IndexedSeq.empty, Set.empty).get
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    grid.locationsInGrid(_ == 0).map(_._2)  // every 'a' in the grid
      .flatMap { from => bfs(State(from, 0), IndexedSeq.empty, Set.empty) }
      .min
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""

  override def in: String =
    """""".stripMargin
}
