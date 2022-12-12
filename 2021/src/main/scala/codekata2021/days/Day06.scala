package codekata2021
package days

object Day06 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 6

  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val line: Parser[List[Int]] = rep1sep(num, ",")

  override def part1: Option[Part] = PuzzlePart({
    val nums = parseAll(line, inputs).get
    (1 to 80).foldLeft(nums) { case (nums, _) =>
      nums.flatMap { x =>
        if (x == 0) Seq(6, 8) else Seq(x - 1)
      }
    }
  }.size.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val nums = parseAll(line, inputs).get
    type DaysLeft = Int
    type Current = Int
    val cache: Map[(Current, DaysLeft), BigInt] = (0 to 8).map(n => (n -> 0) -> 1.big).toMap
    val completedCache = (1 to 256).foldLeft(cache) { case (cache, day) =>
      cache ++
        (1 to 8).map(n => (n -> day) -> cache((n - 1) -> (day - 1))).toMap +
        ((0, day) -> (cache((8, day - 1)) + cache((6, day - 1))))
    }
    nums.map(n => completedCache((n, 256))).sum

  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """3,4,3,1,2"""

  override def in: String =
    """""".stripMargin
}
