package codekata2015
package days

import zio.RIO

object Day09 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 9

  def word: Parser[String] = "[A-Za-z]+".r
  def num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  def line: Parser[(String, String, Int)] = word ~ ("to" ~> word) ~ ("=" ~> num) ^^ { case f ~ t ~ d => (f, t, d) }

  lazy val pathLengths = {
    val lines = inputs.flatMap(l =>
      parseAll(line, l).get.|> { case (f, t, d) => Seq((f, t) -> d, (t, f) -> d) }
    ).toSeq
    val dists = lines.toMap
    val cities = lines.map(_._1._1).distinct
    val paths = cities.permutations.toSeq
    paths.map(path => path.sliding(2).map { case Seq(l, r) => dists((l,r)) }.sum)
  }

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = pathLengths.min.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = pathLengths.max.zio
  }.some

  def inputs = in2.linesIterator

  def in2 =
    """London to Dublin = 464
      |London to Belfast = 518
      |Dublin to Belfast = 141""".stripMargin
  def in3 = ""

  override def in: String = "".stripMargin
}