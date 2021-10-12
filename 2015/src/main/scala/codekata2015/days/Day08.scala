package codekata2015
package days

import zio.RIO

object Day08 extends ParserPuzzle {
  override def skipWhitespace: Boolean = false

  override type PuzzleOut = Any

  override def dayNum: Int = 8

  override def part1: Option[Part] = Option(new Part {
    abstract class Piece(val str: String)
    case class Raw(s: String) extends Piece(s)
    case object Slash extends Piece(raw"\")
    case object Quote extends Piece("\"")
    case class Hex(code: String) extends Piece("%c".format(BigInt(code, 16).toChar))

    def slash: Parser[Slash.type] = raw"\\" ^^^ Slash
    def quote: Parser[Quote.type] = "\\\"" ^^^ Quote
    def hex: Parser[Hex] = raw"\x" ~> "[0-9a-f][0-9a-f]".r ^^ Hex
    def raw: Parser[Raw] = ".".r ^^ Raw
    def line: Parser[Seq[Piece]] = rep1(slash | quote | hex | raw)

    override def solution: RIO[Any, Any] =
      inputs.map { l =>
        (l.length,
          parseAll(line, l).get.map(_.str).mkString("").length - 2
        )
      }
        .toSeq
        .unzip
        .|> { case (a, b) => a.sum - b.sum }
        .zio
  })

  override def part2: Option[Part] = Option(new Part {
    abstract class Piece(val str: String)
    case class Raw(s: String) extends Piece(s)
    case object Slash extends Piece(raw"\\")
    case object Quote extends Piece("\\\"")

    def slash: Parser[Slash.type] = raw"\" ^^^ Slash
    def quote: Parser[Quote.type] = "\"" ^^^ Quote
    def raw: Parser[Raw] = ".".r ^^ Raw
    def line: Parser[Seq[Piece]] = rep1(slash | quote | raw)

    override def solution: RIO[Any, Any] =
      inputs.map { l =>
        (l.length,
          parseAll(line, l).get.map(_.str).mkString("").length + 2
        )
      }
        .toSeq
        .unzip
        .|> { case (a, b) => b.sum - a.sum }
        .zio
  })

  def inputs = in2.linesIterator

  val in2 =
    raw"""""
         |"abc"
         |"aaa\"aaa"
         |"\x27"""".stripMargin
  val in3 = ""

  override def in: String =
    "".stripMargin
}