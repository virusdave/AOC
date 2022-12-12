package codekata2021
package days

import scala.collection.parallel.CollectionConverters._
import scalax.collection.Graph
import scalax.collection.edge.Implicits._

object Day15 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 15

  private lazy val digit: Parser[Int] = "[0-9]".r ^^ (_.toInt)
  private val (mx, my) = inputs.bounds

  override def part1: Option[Part] = PuzzlePart({
    lazy val coords = (0 until mx).flatMap(x => (0 until my).map(x -> _))
    lazy val edges = coords.par.flatMap { case (x, y) =>
      Seq((-1, 0), (1, 0), (0, -1), (0, 1)).flatMap { case (dx, dy) =>
        inputs.clip(x + dx, y + dy).flatMap(c =>
          (inputs.get _).tupled(c).map(v => (x, y).~%>((x + dx) -> (y + dy))(v.toDouble)))
      }
    }.toIndexedSeq

    lazy val g = Graph.from(coords, edges)

    g.get(0 -> 0).shortestPathTo(g.get((mx - 1) -> (my - 1))).map(_.weight)
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    "building inputs".debug
    val coords = (0 until (mx * 5)).flatMap(x => (0 until (my * 5)).map(x -> _)).par
    "coords done".debug

    val edges = {
      val deltas = Seq((-1, 0), (1, 0), (0, -1), (0, 1))
      for {
        (x, y) <- coords
        (dx, dy) <- deltas if x + dx >= 0 && x + dx < mx * 5 && y + dy >= 0 && y + dy < my * 5
        (nx, ny) = (x + dx, y + dy)
        (origX, origY) = (nx % mx, ny % my)
        (plusX, plusY) = (nx / mx, ny / my)
      } yield {
        val wrapped = (inputs(origY)(origX) + plusX + plusY - 1) % 9 + 1
        (x -> y).~%>(nx -> ny)(wrapped.toDouble)
      }
    }

    "building graph".debug
    val g = Graph.from(coords.seq, edges.seq)
    "built".debug

    "started".debug
    g.get(0 -> 0).shortestPathTo(g.get((mx * 5 - 1) -> (my * 5 - 1))).map(_.weight)
  }.zio).some

  private def inputs = in2.parseLinesBy(rep(digit) ^^ (_.toIndexedSeq))

  private lazy val in2 =
    """1163751742
      |1381373672
      |2136511328
      |3694931569
      |7463417111
      |1319128137
      |1359912421
      |3125421639
      |1293138521
      |2311944581""".stripMargin

  override def in: String =
    """""".stripMargin
}
