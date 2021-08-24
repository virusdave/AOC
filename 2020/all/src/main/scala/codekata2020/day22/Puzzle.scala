package codekata2020.day22

import codekata2020._
import scala.annotation.tailrec

object Puzzle {
  private val inputs = in.split("\n\n").toIndexedSeq.map(_.linesIterator.toSeq.drop(1).map(_.toInt))

  object Part1 {
    def solution = {
      @tailrec def go(p1: Seq[Int], p2: Seq[Int]): (Seq[Int], Seq[Int]) = {
        if (p1.isEmpty || p2.isEmpty) (p1, p2)
        else {
          val (c1, c2) = p1.zip(p2).head
          if (c1 > c2) go(p1.tail :++ Seq(c1, c2), p2.tail)
          else go(p1.tail, p2.tail :++ Seq(c2, c1))
        }
      }
      val (r1, r2) = go(inputs(0), inputs(1))
      (r1 ++ r2).reverse.zipWithIndex.map { case (c, idx) => c*(idx+1) }.sum
    }.zio
  }

  object Part2 {
    def solution = {
      def go(cache: Set[(Seq[Int], Seq[Int])], p1: Seq[Int], p2: Seq[Int]): (Boolean, Seq[Int], Seq[Int]) = {
        if (cache.contains((p1, p2))) (true, p1, p2)
        else if (p1.isEmpty || p2.isEmpty) (p2.isEmpty, p1, p2)
        else {
          val newCache = cache + ((p1, p2))
          val (c1, c2) = p1.zip(p2).head

          val p1Win = if (c1 <= p1.tail.size && c2 <= p2.tail.size) {
            go(newCache, p1.tail.take(c1.toInt), p2.tail.take(c2.toInt))._1
          } else c1 > c2

          if (p1Win) go(newCache, p1.tail :++ Seq(c1, c2), p2.tail)
          else go(newCache, p1.tail, p2.tail :++ Seq(c2, c1))
        }
      }
      val (_, r1, r2) = go(Set.empty, inputs(0), inputs(1))
      (r1 ++ r2).reverse.zipWithIndex.map { case (c, idx) => c*(idx+1) }.sum
    }.zio
  }

  private lazy val in2 =
    """Player 1:
      |9
      |2
      |6
      |3
      |1
      |
      |Player 2:
      |5
      |8
      |4
      |7
      |10""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
