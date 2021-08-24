package codekata2020.day17

import codekata2020._
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  private val inputs = in.linesIterator.toIndexedSeq.map(_.toIndexedSeq)

  // (Live, Count) means Alive next iteration if present
  private val iterateRule = Set((true, 2), (true, 3), (false, 3))

  private def crossJoin[T](list: List[List[T]]): List[List[T]] = list match {  // Cartesian product of input lists
    case Nil => Nil
    case xs :: Nil => xs map (List(_))
    case x :: xs => for {
      i <- x
      j <- crossJoin(xs)
    } yield List(i) ++ j
  }

  def countNear(live: Set[List[Int]], coords: List[Int]) = crossJoin(coords.map(_.diag(_-1 to _+1).toList)).count(live)

  def iterateNdimWorld(iterations: Int, initial: Set[List[Int]]) =
    (1 to iterations).foldLeft(initial) { case (world, _) =>
      val keys = world.toList
      crossJoin(keys.head.indices.map(i => keys.map(_(i)).sorted.diag(_.head-1 to _.last+1).toList).toList).flatMap { c =>
        val v = world(c)
        c.some.filter(_ => iterateRule(v, countNear(world, c) - v.toInt))
      }.toSet
    }.size

  object Part1 {
    def solution = iterateNdimWorld(6,
      inputs.indices.flatMap(y => inputs(y).indices.flatMap(x => inputs.get(x, y).filter(_ == '#').map(_ => List(x, y, 0)))).toSet
    ).zio
  }

  object Part2 {
    def solution = iterateNdimWorld(6,
      inputs.indices.flatMap(y => inputs(y).indices.flatMap(x => inputs.get(x, y).filter(_ == '#').map(_ => List(x, y, 0, 0)))).toSet
    ).zio
  }

  private lazy val in2 =
    """.#.
      |..#
      |###""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
