package codekata2020.day2

import codekata2020._

object Puzzle {
  private val ins = in.linesIterator.toSeq
  object Part1 {
    def solution(silent: Boolean) = ins.map {
      case parser(low, hi, c, pw) =>
        val cCount = pw.toSeq.groupMapReduce(identity)(_ => 1)(_ + _).getOrElse(c(0), 0)
        if (cCount >= low.toInt && cCount <= hi.toInt) 1 else 0
    }.sum.zio
  }

  object Part2 {
    def solution(silent: Boolean) = ins.map {
      case parser(low, hi, c, pw) =>
        Seq(low, hi).map(x => pw(x.toInt - 1)).count(_ == c(0)) % 2
    }.sum.zio
  }

  lazy val parser = "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)".r
  lazy val in2 = """1-3 a: abcde
                   |1-3 b: cdefg
                   |2-9 c: ccccccccc""".stripMargin

  lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
