package codekata2020.day19

import codekata2020._
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  private val inputs = in.split("\n\n").toIndexedSeq.map(_.linesIterator.toIndexedSeq)

  type Alts = List[List[Int]]
  def num: Parser[Int] = "\\d+".r ^^ { _.toInt }
  def alts: Parser[Left[Alts, Nothing]] = rep1sep(rep1(num), "|") ^^ { Left(_) }
  def str: Parser[Right[Nothing, String]] = "\"" ~> """[^"]+""".r <~ "\"" ^^ { Right(_) }
  def line: Parser[(Int, Either[Alts, String])] = (num <~ ":") ~ (alts | str) ^^ { case n ~ r => n -> r }
  private val parsed = inputs(0).map { l => parseAll(line, l).get }.toMap

  def cachingLookup(cache: Map[Int, Parser[Unit]], rule: Int): (Map[Int, Parser[Unit]], Parser[Unit]) =
    if (cache.contains(rule)) cache -> cache(rule)
    else parsed(rule) match {
      case Right(v) => (cache + (rule -> (v ^^^ ())), v ^^^ ())
      case Left(alts) =>
        alts.foldLeft((cache, failure("nope"): Parser[Unit])) { case ((cache, pl), seqs) =>
          seqs.foldLeft((cache, "" ^^^ ())) { case ((cache, pl), ref) =>
            cachingLookup(cache, ref) match { case (newCache, pr) => newCache -> (pl ~> pr)}
          } match { case (newCache, pr) => newCache -> (pl | pr) } } }

  private val filledCache = cachingLookup(Map.empty, rule = 0)._1

  object Part1 {
    def solution = {
      inputs(1).count { msg => parseAll(cachingLookup(filledCache, rule = 0)._2, msg).successful }
    }.zio
  }

  object Part2 {
    def solution = {
      val match42 = cachingLookup(filledCache, rule = 42)._2
      val match31 = cachingLookup(filledCache, rule = 31)._2
      val newRule0 =                 // With the changes, rule 0 is now "one or more (M) rule 42s followed by
        (1 to 20).flatMap { rep42 => // an equal number (N) of at least one of both rule 42s and rule 31", so
          (1 to 20).map { paired => repN(rep42, match42) ~ repN(paired, match42) ~ repN(paired, match31) ^^^ () }
        }.reduce(_ | _)                // just try all reasonable choices for (M, N) for the rule.
      inputs(1).count { msg => parseAll(newRule0, msg).successful }
    }.zio
  }

  private lazy val in2 =
    """0: 4 1 5
      |1: 2 3 | 3 2
      |2: 4 4 | 5 5
      |3: 4 5 | 5 4
      |4: "a"
      |5: "b"
      |
      |ababbb
      |bababa
      |abbbab
      |aaabbb
      |aaaabbb""".stripMargin

  private lazy val in3 =
    """42: 9 14 | 10 1
      |9: 14 27 | 1 26
      |10: 23 14 | 28 1
      |1: "a"
      |11: 42 31
      |5: 1 14 | 15 1
      |19: 14 1 | 14 14
      |12: 24 14 | 19 1
      |16: 15 1 | 14 14
      |31: 14 17 | 1 13
      |6: 14 14 | 1 14
      |2: 1 24 | 14 4
      |0: 8 11
      |13: 14 3 | 1 12
      |15: 1 | 14
      |17: 14 2 | 1 7
      |23: 25 1 | 22 14
      |28: 16 1
      |4: 1 1
      |20: 14 14 | 1 15
      |3: 5 14 | 16 1
      |27: 1 6 | 14 18
      |14: "b"
      |21: 14 1 | 1 14
      |25: 1 1 | 1 14
      |22: 14 14
      |8: 42
      |26: 14 22 | 1 20
      |18: 15 15
      |7: 14 5 | 1 21
      |24: 14 1
      |
      |abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
      |bbabbbbaabaabba
      |babbbbaabbbbbabbbbbbaabaaabaaa
      |aaabbbbbbaaaabaababaabababbabaaabbababababaaa
      |bbbbbbbaaaabbbbaaabbabaaa
      |bbbababbbbaaaaaaaabbababaaababaabab
      |ababaaaaaabaaab
      |ababaaaaabbbaba
      |baabbaaaabbaaaababbaababb
      |abbbbabbbbaaaababbbbbbaaaababb
      |aaaaabbaabaaaaababaa
      |aaaabbaaaabbaaa
      |aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
      |babaaabbbaaabaababbaabababaaab
      |aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
