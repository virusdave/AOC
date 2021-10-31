package codekata2016
package days

import scala.io.Source
import zio.RIO

object Day07 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 7

  val word: Parser[String]        = "[a-z]+".r
  val bracketWord: Parser[String] = "[" ~> word <~ "]"
  val line: Parser[(String, List[(String, String)])] =
    word ~ (rep(bracketWord ~ word ^^ { case bw ~ w => (bw, w) })) ^^ { case l ~ r => (l, r) }

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      def hasMirror(s: String) = s.toList.sliding(4).exists { case List(a, b, c, d) => a != b && a == d && b == c }

      inputs.linesIterator.toSeq.map(parseAll(line, _).get).map { case (l, r) =>
        val (in, out) = r.unzip
        (l :: out, in)
      }.count { case (out, in) =>
        out.exists(hasMirror) && !in.exists(hasMirror)
      }
    }.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      def aba(s: String)                     = s.toList.sliding(3).filter { case List(a, b, c) => a == c && a != b }.distinct
      def aba2bab(s: List[Char]): List[Char] = s.slice(1, 2) ++ s.take(2)

      inputs.linesIterator.toSeq.map(parseAll(line, _).get).map { case (l, r) =>
        val (in, out) = r.unzip
        (l :: out, in)
      }.count { case (out, in) =>
        val abas = out.flatMap(aba)
        val babs = in.flatMap(aba).map(aba2bab)
        abas.intersect(babs).nonEmpty
      }
    }.zio
  }.some

  def inputs = in2

  lazy val in2 =
    """abba[mnop]qrst
      |abcd[bddb]xyyx
      |aaaa[qwer]tyui
      |ioxxoj[asdfgh]zxcvbn""".stripMargin
  lazy val in3 =
    """aba[bab]xyz
      |xyx[xyx]xyx
      |aaa[kek]eke
      |zazbz[bzb]cdb""".stripMargin

  override def in: String =
    """"""
}
