package codekata2022
package days

import common.InRegexParserSyntax
import zio.RIO

object Latest extends ParserPuzzle with InRegexParserSyntax {
  override type PuzzleOut = Any
  override def dayNum: Int = 3

  private val word: Parser[String]   = "[a-zA-Z]+".r
  private val num: Parser[Int]       = "[0-9]+".r ^^ (_.toInt)
  private val neg: Parser[Int]       = "-" ~> num ^^ (-_)
  private val signedNum: Parser[Int] = num | neg

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      ()
      ()
    }.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      ()
      ()
    }.zio
  }.some

  private def inputs = in2

  private lazy val in2 =
    """"""

  private lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
