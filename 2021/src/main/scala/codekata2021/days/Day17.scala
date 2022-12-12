package codekata2021
package days

object Day17 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 17

  private val word: Parser[String] = "[a-zA-Z]+".r
  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val neg: Parser[Int] = "-" ~> num ^^ (-_)
  private val signedNum: Parser[Int] = num | neg
  private val pair: Parser[(Int, Int)] = signedNum ~ (".." ~> signedNum) ^^ { case l ~ r => l -> r }

  private case class Target(x: (Int, Int), y: (Int, Int))
  private val line: Parser[Target] = "target area: x=" ~> (pair <~ ", y=") ~ pair ^^ { case x ~ y => Target(x, y) }
  private val target: Target = inputs.parseBy(line)

  override def part1: Option[Part] = PuzzlePart({
    val lowerY = target.y._1
    (-lowerY * (-lowerY + 1)) / 2
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    def within(xy: (Int, Int), target: Target): Boolean = xy match {
      case (x, y) => x >= target.x._1 && x <= target.x._2 && y >= target.y._1 && y <= target.y._2
    }

    def firstFewHundred(xy: (Int, Int)): Seq[(Int, Int)] = xy match {
      case (x, y) =>
        (0 to 500) map { i =>
          val clipI = (x, i).min
          val xx = clipI * x - (clipI * (clipI - 1)) / 2
          val yy = i * y - (i * (i - 1)) / 2
          xx -> yy
        }
    }

    (for {
      x <- 0 to 97
      y <- -180 to 180
    } yield x -> y).count(_.|>(firstFewHundred).exists(within(_, target)))

  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """target area: x=20..30, y=-10..-5"""

  private lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
