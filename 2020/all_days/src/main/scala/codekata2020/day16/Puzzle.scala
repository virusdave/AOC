package codekata2020.day16

import codekata2020._
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  private val inputs = in.split("\n\n").toIndexedSeq

  def num: Parser[Int] = """[0-9]+""".r ^^ {_.toInt}
  def numPair: Parser[(Int, Int)] = (num <~ "-") ~ num ^^ { case low ~ hi => (low, hi) }
  def fields: Parser[List[(Int, Int)]] =  """[^0-9]*""".r ~> rep( numPair <~ "[^0-9]*".r)

  def nums: Parser[List[Int]] = rep("""[^0-9]+""".r) ~> repsep( num, ",")

  object Part1 {
    def solution = {
      val ranges = parseAll(fields, inputs(0).replace('\n', ' ')).get

      val fieldNums: Seq[Int] = parseAll(nums, inputs(2).replace('\n', ',')).get

      val filteredNums = fieldNums.filter(n => ranges.forall { case (low, hi) => n < low || n > hi })
      filteredNums.sum
    }.zio
  }

  object Part2 {
    def solution = {
      val fieldLine: Parser[(String, (Int, Int), (Int, Int))] =
        """([^0-9]+)""".r ~ numPair ~ "or" ~ numPair ^^ { case l ~ p1 ~ _ ~ p2 => (l, p1, p2) }
      val ranges = parseAll(rep(fieldLine), inputs(0)).get

      def nums: Parser[List[Int]] = rep1sep(num, ",")
      val otherTickets = parseAll(rep1(nums), inputs(2).split("\n").drop(1).mkString("\n")).get
      val validTickets = otherTickets.filter(row =>
        row.forall( n => ranges.flatMap { case (_, r1, r2) => Seq(r1, r2)}.exists { case (low, hi) =>
          n >= low && n <= hi
        }))

      val mineAndValidTickets =
        (parseAll(nums, inputs(1).split("\n").drop(1).mkString("\n")).get +:
          validTickets).map(_.toIndexedSeq).toIndexedSeq.transpose

      // FieldName -> FieldNameIndex -> Seq[Column indexes] that could match
      val matches: Seq[((String, Int), IndexedSeq[Int])] =
        ranges.zipWithIndex.map { case ((name, (low1, hi1), (low2, hi2)), fieldIdx) =>
          name -> fieldIdx -> mineAndValidTickets.zipWithIndex.flatMap { case (ticketFields, tixIdx) =>
            tixIdx.some.filter(_ => ticketFields.forall(field =>
              (field >= low1 && field <= hi1) || (field >= low2 && field <= hi2)))
          }
        }

      // Some fields could match multiple columns, so start pairing them up by most-restricted first, and removing
      // that paired column from the set of candidates for future fields
      matches.sortBy(_._2.size).foldLeft(Map.empty[Int, (String, Int)]) { case (used, ((name, nidx), allowed)) =>
        val rem = allowed.filterNot(used.contains)
        used ++ rem.map(r => r -> (name, mineAndValidTickets(r).head))
      }.view.filter { case (_, (name, _)) => name.startsWith("departure") }.toMap.values.map(_._2.big).product
    }.zio
  }

  private lazy val in2 =
    """class: 1-3 or 5-7
      |row: 6-11 or 33-44
      |seat: 13-40 or 45-50
      |
      |your ticket:
      |7,1,14
      |
      |nearby tickets:
      |7,3,47
      |40,4,50
      |55,2,20
      |38,6,12""".stripMargin

  private lazy val in3 =
    """class: 0-1 or 4-19
      |row: 0-5 or 8-19
      |seat: 0-13 or 16-19
      |
      |your ticket:
      |11,12,13
      |
      |nearby tickets:
      |3,9,18
      |15,1,5
      |5,14,9""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
