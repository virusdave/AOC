package codekata2020.day1

import codekata2020._
import zio.ZIO

object Puzzle {
  private val ins = in.linesIterator.map(_.toInt).toSet
  object Part1 {
    def solution(silent: Boolean) = {
      val matching = ins.find(x => ins.apply(2020 - x)).get
      ZIO.succeed(matching * (2020 - matching))
    }
  }

  object Part2 {
    def solution(silent: Boolean) = {
      (for {
        x1 <- ins
        x2 <- ins
        x3 <- ins if (x1 + x2 + x3 == 2020)
      } yield x1 * x2 * x3).head.zio
    }
  }

  val in2 = """1721
              |979
              |366
              |299
              |675
              |1456""".stripMargin

  lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
