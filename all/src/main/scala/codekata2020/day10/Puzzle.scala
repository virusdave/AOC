package codekata2020.day10

import codekata2020._
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  private val inputs = in.linesIterator.toIndexedSeq.map(BigInt.apply)

  object Part1 {
    def solution = {
      (0.big +: inputs :+ (inputs.max + 3.big))
        .sorted.sliding(2).map(_.diag(_(1) - _(0))).toSeq
        .groupBy(identity).view.mapValues(_.size).toMap.diag(_(1) * _(3))

    }.zio
  }

  object Part2 {
    def solution = {
      def rle[T](xs: List[T]): List[(Int, T)] = xs match {
        case Nil => List()
        case x :: l =>
          val (front, back) = l.span(_ == x)
          (front.length + 1, x) :: rle(back)
      }
      val combinations = Map(1->1, 2->2, 3->4, 4->7).view.mapValues(_.big)

      (0.big +: inputs :+ (inputs.max + 3.big))
        .sorted.sliding(2).map(_.diag(_(1) - _(0))).toList
        .pipe(rle).filter(_._2 == 1).map(_._1).map(combinations).product
    }.zio
  }

  private lazy val in2 =
    """16
      |10
      |15
      |5
      |1
      |11
      |7
      |19
      |6
      |12
      |4""".stripMargin

  private lazy val in3 =
    """28
      |33
      |18
      |42
      |31
      |14
      |46
      |20
      |48
      |47
      |24
      |23
      |49
      |45
      |19
      |38
      |39
      |11
      |1
      |32
      |25
      |35
      |8
      |17
      |7
      |9
      |4
      |2
      |34
      |10
      |3""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
