package codekata2021
package days

import zio.RIO

object Day17 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 17

  val word: Parser[String]     = "[a-zA-Z]+".r
  val num: Parser[Int]         = "[0-9]+".r ^^ (_.toInt)
  val neg: Parser[Int]         = "-" ~> num ^^ (-_)
  val signedNum: Parser[Int]   = num | neg
  val pair: Parser[(Int, Int)] = signedNum ~ (".." ~> signedNum) ^^ { case l ~ r => l -> r }

  case class Target(x: (Int, Int), y: (Int, Int))
  val line: Parser[Target] = "target area: x=" ~> (pair <~ ", y=") ~ pair ^^ { case x ~ y => Target(x, y) }

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      val lowerY = -parseAll(line, inputs).get.y._1
      (-lowerY * (-lowerY - 1)) / 2
    }
      .zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      def within(xy: (Int, Int), target: Target): Boolean =
        xy match { case (x, y) => x >= target.x._1 && x <= target.x._2 && y >= target.y._1 && y <= target.y._2 }

      def firstFewHundred(xy: (Int, Int)): Seq[(Int, Int)] = xy match {
        case (x, y) =>
          (0 to 500) map { i =>
            val clipI = Math.min(x, i)
            val xx    = clipI * x - (clipI * (clipI - 1)) / 2
            val yy    = i * y - (i * (i - 1)) / 2
            xx -> yy
          }
      }

      val target = parseAll(line, inputs).get.debug
      (for {
        x <- 0 to 97
        y <- -180 to 180
      } yield x -> y).count(_.|>(firstFewHundred).exists(within(_, target)))

    }
      .zio
  }.some

  def inputs = in

  lazy val in2 =
    """target area: x=20..30, y=-10..-5"""

  lazy val in3 =
    """"""

  override def in: String =
    """target area: x=70..96, y=-179..-124""".stripMargin
}
