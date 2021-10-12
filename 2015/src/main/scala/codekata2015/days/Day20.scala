package codekata2015
package days

import scala.io.Source
import zio.RIO

object Day20 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 20

  lazy val num: Parser[String]     = "[0-9]+".r
  def kvBig: Parser[(Int, BigInt)] = num ~ num ^^ { case l ~ r => l.toInt -> r.big }

  override def part1: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = {
        val A034885 = Source
          .fromURL("https://oeis.org/A034885/b034885.txt")
          .getLines()
          .filterNot(_.isEmpty)
          .map(
            parseAll(kvBig, _).get
          )
          .toSeq

        A034885.take(5).debug

        val A002093 = Source
          .fromURL("https://oeis.org/A002093/b002093.txt")
          .getLines()
          .filterNot(_.isEmpty)
          .map(
            parseAll(kvBig, _).get
          )
          .toMap

        A034885.find { case (_, sum) => sum > inputs / 10 }.map(_._1).flatMap(A002093.get)
      }.zio
    }.some
  override def part2: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = {
        LazyList
          .from(1)
          .map { n =>
            n -> (1 to 50).flatMap { m =>
              Option((n/m) * 11).filter(_ => n % m == 0)
            }.sum
          }
          .map { n =>
            if (n._1 % 10000 == 0) n.debug else n
          }
          .find { case (_, sum) => sum > inputs}
      }.zio
    }.some

  def inputs = 123

  val in2 = ""
  val in3 = ""

  override def in: String = ""
}
