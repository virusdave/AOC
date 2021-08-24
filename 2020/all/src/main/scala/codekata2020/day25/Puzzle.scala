package codekata2020.day25

import codekata2020._
import scala.annotation.tailrec
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {

  object Part1 {
    def solution = {
      @tailrec def countUntil(idx: BigInt, cur: BigInt, target: BigInt): BigInt =
        if (cur == target) idx else countUntil(idx+1, cur * 7 % 20201227, target)
      val key = countUntil(1, 7, inputs(0))
      val door = countUntil(1, 7, inputs(1))
      (key, door).debug

      val t1 = (1 to key.intValue).foldLeft(1.big) { case (d, _) => d * inputs(1) % 20201227 }
      val t2 = (1 to door.intValue).foldLeft(1.big) { case (d, _) => d * inputs(0) % 20201227 }
      (t1, t2)
    }.zio
  }

  object Part2 {
    def solution = {

    }.zio
  }

  private def inputs = in.linesIterator.toIndexedSeq.map(_.big)
  //  private def inputs = in.split("\n\n").toIndexedSeq

  private lazy val in2 =
    """""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """3418282
      |8719412""".stripMargin
}
