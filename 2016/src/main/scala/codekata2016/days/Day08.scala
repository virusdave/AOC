package codekata2016
package days

import zio.RIO

object Day08 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 8

  sealed trait Op { def apply(in: Grid[Boolean]): Grid[Boolean] }
  case class Rect(x: Int, y: Int) extends Op {
    override def apply(in: Grid[Boolean]): Grid[Boolean] = in.mapGridWithLocation { case (cur, (xx, yy)) =>
      (xx < x && yy < y) || cur
    }
  }
  case class Column(x: Int, dist: Int) extends Op {
    override def apply(in: Grid[Boolean]): Grid[Boolean] = in.mapGridWithLocation { case (cur, (xx, yy)) =>
      val (_, yb) = in.bounds
      if (x != xx) cur
      else in.get(xx, (yb + yy - dist) % yb).get
    }
  }
  case class Row(y: Int, dist: Int) extends Op {
    override def apply(in: Grid[Boolean]): Grid[Boolean] = in.mapGridWithLocation { case (cur, (xx, yy)) =>
      val (xb, _) = in.bounds
      if (y != yy) cur
      else in.get((xb + xx - dist) % xb, yy).get
    }
  }

  val lines: Parser[Op] = {
    val num    = "[0-9]+".r ^^ (_.toInt)
    val rect   = "rect" ~> num ~ ("x" ~> num) ^^ { case l ~ r => Rect(l, r) }
    val column = "rotate column x=" ~> num ~ ("by" ~> num) ^^ { case l ~ r => Column(l, r) }
    val row    = "rotate row y=" ~> num ~ ("by" ~> num) ^^ { case l ~ r => Row(l, r) }

    rect | column | row
  }

  private val board = Grid.fill2d(7, 3)(false)
  private def go = inputs.parseLinesBy(lines)
    .foldLeft(board) { case (b, op) =>
      op.apply(b) //.tap(_.show(if (_) "#" else ".").debug: Unit)
    }

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      go.map(_.count(identity)).sum.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      go.show(if (_) "#" else " ").|>("\n" + _).zio

  }.some

  def inputs = in2

  lazy val in2 =
    """rect 3x2
      |rotate column x=1 by 1
      |rotate row y=0 by 4
      |rotate column x=1 by 1""".stripMargin
  lazy val in3 = ""

  override def in: String =
    """""".stripMargin
}
