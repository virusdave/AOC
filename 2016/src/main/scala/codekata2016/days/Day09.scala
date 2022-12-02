package codekata2016
package days

import zio.RIO

object Day09 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 9

  private val num: Parser[Int]           = "[0-9]+".r ^^ (_.toInt)
  private val marker: Parser[(Int, Int)] = "(" ~> num ~ ("x" ~> num) <~ ")" ^^ { case l ~ r => (l, r) }

  override def part1: Option[Part] = new Part {
    val lines: Parser[String] = {
      val expanded: Parser[String] = marker.flatMap { case (l, r) => repN(l, ".".r) ^^ (ls => ls.mkString * r) }
      val token                    = expanded | ".".r
      rep1(token) ^^ (_.mkString)
    }

    override def solution: RIO[Any, Any] =
      inputs.parseLinesBy(lines).map(x => x.length /*-> x*/ ).zio
  }.some

  override def part2: Option[Part] = new Part {
    sealed trait Chunk { def size: Long }
    case class Text(str: String)               extends Chunk { override def size: Long = str.length.toLong          }
    case class Rep(n: Int, chunks: Seq[Chunk]) extends Chunk { override def size: Long = chunks.map(_.size).sum * n }

    val lines: Parser[Seq[Chunk]] = {
      val expanded: Parser[Rep] = marker.flatMap { case (l, r) =>
        repN(l, ".".r) ^^ (ls => Rep(r, parseAll(lines, ls.mkString).getOrFail))
      }
      val token: Parser[Chunk] = expanded | (".".r ^^ Text)
      rep1(token)
    }

    override def solution: RIO[Any, Any] =
      inputs.parseLinesBy(lines).map(x => x.map(_.size).sum /*-> x*/ ).mkString("\n").zio
  }.some

  def inputs = in3

  lazy val in2 =
    """ADVENT
      |A(1x5)BC
      |(3x3)XYZ
      |A(2x2)BCD(2x2)EFG
      |(6x1)(1x3)A
      |X(8x2)(3x3)ABCY""".stripMargin
  lazy val in3 =
    """(3x3)XYZ
      |X(8x2)(3x3)ABCY
      |(27x12)(20x12)(13x14)(7x10)(1x12)A
      |(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN""".stripMargin

  override def in: String = ""
}
