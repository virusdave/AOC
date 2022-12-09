package codekata2022
package days

import common.InRegexParserSyntax
import enumeratum._

object Day09 extends ParserPuzzle with InRegexParserSyntax {
  override type PuzzleOut = Any
  override def dayNum: Int = 9

  private sealed abstract class Dir(override val entryName: String, val delta: Delta) extends EnumEntry
  private object Dir extends Enum[Dir] {
    case object Up extends Dir("U", Delta(0, 1))
    case object Down extends Dir("D", Delta(0, -1))
    case object Left extends Dir("L", Delta(-1, 0))
    case object Right extends Dir("R", Delta(1, 0))

    override lazy val values: scala.IndexedSeq[Dir] = findValues
  }
  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val dir: Parser[Dir] = "[UDLR]".r ^^ Dir.withName
  private val parsed: Seq[Dir] =
    inputs.parseLinesBy(dir ~ num).flatMap { case d ~ n => Seq.fill(n)(d) }

  private case class Delta(x: Int, y: Int) {
    // Disgusting...
    def normalize: Delta = (x, y) match {
      case (-2, 2) | (-2, 1) | (-1, 2) => Delta(-1, 1)
      case (2, 2) | (2, 1) | (1, 2) => Delta(1, 1)
      case (-2, -2) | (-2, -1) | (-1, -2) => Delta(-1, -1)
      case (2, -2) | (2, -1) | (1, -2) => Delta(1, -1)
      case (-2, 0) => Delta(-1, 0)
      case (2, 0) => Delta(1, 0)
      case (0, 2) => Delta(0, 1)
      case (0, -2) => Delta(0, -1)
      case _ => Delta(0, 0)
    }
  }
  private case class Point(x: Int, y: Int) {
    def +(that: Delta): Point = Point(x + that.x, y + that.y)
    def -(that: Point): Delta = Delta(x - that.x, y - that.y)
  }

  private val origin: Point = Point(0, 0)

  override def part1: Option[Part] = PuzzlePart({
    case class State(head: Point, tail: Point, tailSeen: Set[Point])

    parsed.foldLeft(State(origin, origin, Set(origin))) {
      case (State(head, tail, tailSeen), dir) =>
        State(head + dir.delta, tail + (head - tail).normalize, tailSeen + tail)
    }.tailSeen.size
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    case class State(knots: IndexedSeq[Point], tailSeen: Set[Point])

    parsed.foldLeft(State(IndexedSeq.fill(10)(origin), Set(origin))) {
      case (State(knots, tailSeen), dir) =>
        val newKnots: IndexedSeq[Point] =
          knots.foldLeft(IndexedSeq.empty[Point]) { case (next, knot) =>
            val knotDelta = if (next.isEmpty) dir.delta else (next.last - knot).normalize
            next :+ (knot + knotDelta)
          }
        State(newKnots, tailSeen + newKnots.last)
    }.tailSeen.size
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""

  private lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
