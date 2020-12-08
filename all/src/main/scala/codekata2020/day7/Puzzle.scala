package codekata2020.day7

import codekata2020._
import scala.annotation.tailrec
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  private val inputs = in.linesIterator.toIndexedSeq

  private object Parsers {
    def bagType: Parser[String] = """[a-z]+ [a-z]+""".r <~ """bag(s?)""".r
    def count: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def bagCount: Parser[Int ~ String] = count ~ bagType

    def noInnerBags: Parser[Map[String, Int]] = "no other bags" ^^ { _ => Map.empty }
    def manyBagCounts: Parser[Map[String, Int]] = repsep(bagCount, ",") ^^ { _.map(x => x._2 -> x._1).toMap }

    def line: Parser[Map[String, Map[String, Int]]] =
      (bagType <~ "contain") ~ ((noInnerBags | manyBagCounts) <~ ".") ^^ { case outer ~ inner => Map(outer -> inner) }
  }
  import Parsers._

  def whatContainsWhat: Map[String, Map[String, Int]] = inputs.map(parse(phrase(line), _).get).reduce(_ ++ _)
  // "Invert" the above (kind of)
  def whatFoundInWhat: Map[String, Seq[String]] =
    whatContainsWhat.toSeq.flatMap { case (outer, inner) => inner.keys.map(_ -> outer) }
      .groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  val target = "shiny gold"

  object Part1 {
    def solution = {
      @tailrec def addPossibleContainingBags(soFar: Set[String]): Set[String] = {
        val next = soFar ++ soFar.flatMap(bag => whatFoundInWhat.getOrElse(bag, Seq.empty))
        if (next == soFar) soFar else addPossibleContainingBags(next)
      }
      // Everything that could contain us (but not counting us)
      (addPossibleContainingBags(Set(target)) - target).size
    }.zio
  }

  object Part2 {
    def solution = {
      // Given `newest` as the most recent bags added in this iteration, add that many bags
      // to the running count, and recurse with the contents of those bags
      @tailrec def addAndCountContents(newest: Map[String, BigInt], soFar: BigInt): BigInt = {
        if (newest.isEmpty) soFar
        else
          addAndCountContents(
            newest.toSeq.flatMap { case (bag, count) =>
              whatContainsWhat.get(bag).toSeq.flatMap(_.toSeq.map { case (a, b) => (a, b * count) })
            }
              // Some new bags of type X can come from distinct "routes" by being contained in multiple
              // other bag types Y and Z.  Be sure to accumulate all those together for the next iteration.
              .groupBy(_._1).view.mapValues(bagTypeAndCount => bagTypeAndCount.map(_._2).sum).toMap,
            // All the bags we added on this iteration
            soFar + newest.toSeq.map(_._2).sum)
      }
      addAndCountContents(Map(target -> 1), 0) - 1 // Don't count the shiny gold one on the outside.
    }.zio
  }

  private lazy val in2 =
    """light red bags contain 1 bright white bag, 2 muted yellow bags.
      |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
      |bright white bags contain 1 shiny gold bag.
      |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
      |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
      |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      |faded blue bags contain no other bags.
      |dotted black bags contain no other bags.""".stripMargin

  private lazy val in3 =
    """shiny gold bags contain 2 dark red bags.
      |dark red bags contain 2 dark orange bags.
      |dark orange bags contain 2 dark yellow bags.
      |dark yellow bags contain 2 dark green bags.
      |dark green bags contain 2 dark blue bags.
      |dark blue bags contain 2 dark violet bags.
      |dark violet bags contain no other bags.""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
