package codekata2020.template

import codekata2020._
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  private val inputs = in.linesIterator.toIndexedSeq.map(BigInt.apply)
//  private val inputs = in.split("\n\n").toIndexedSeq

  def bagType: Parser[String] = """[a-z]+ [a-z]+""".r <~ """bag(s?)""".r
  def count: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def bagCount: Parser[Int ~ String] = count ~ bagType
  def noInnerBags: Parser[Map[String, Int]] = "no other bags" ^^ { _ => Map.empty }
  def manyBagCounts: Parser[Map[String, Int]] = repsep(bagCount, ",") ^^ { _.map(x => x._2 -> x._1).toMap }
  def line: Parser[Map[String, Map[String, Int]]] =
    (bagType <~ "contain") ~ ((noInnerBags | manyBagCounts) <~ ".") ^^ { case outer ~ inner => Map(outer -> inner) }
  def parsed = parse(phrase(line), in)

  object Part1 {
    def solution = {

    }.zio
  }

  object Part2 {
    def solution = {

    }.zio
  }

  private lazy val in2 =
    """""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
