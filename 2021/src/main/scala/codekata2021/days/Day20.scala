package codekata2021
package days

import zio.RIO

object Day20 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 20

  private val neighborhood9: ((Int, Int)) => Seq[(Int, Int)] = { case (x, y) =>
    for {
      dx <- -1 to 1
      dy <- -1 to 1
    } yield ((x + dx) -> (y + dy))
  }

  private def stepOnce(step: Int, zeros: Set[(Int, Int)], ones: Set[(Int, Int)]) = {
    val nextZeros -> nextOnes = (ones ++ zeros).flatMap(neighborhood9).toSeq.map { case x -> y =>
      val lookupIdx: Int = (for {
        dy <- -1 to 1
        dx <- -1 to 1
        xx = x + dx
        yy = y + dy
      } yield
        if (
          ones.contains(xx -> yy) ||
          // On even steps, if we're looking at uniformly fully-on and fully-off areas (that
          // is, coordinates outside the influence of any coordinates we've explicitly tracked
          // so far, outside the "horizon of influence" we've tracked), then any spot
          // not known to be on or off must be considered to be on, since it means it was a
          // location completely separated from other areas, and thus will alternate each
          // step (until the known areas eventually get close enough to affect it)
          (step % 2 == 0 && key(0) == 1) && !zeros.contains(xx -> yy)
        ) "1"
        else "0").mkString.|>(BigInt.apply(_, 2)).toInt
      (Set(x -> y) -> Set.empty[(Int, Int)]).|>(if (key(lookupIdx) == 0) identity else _.swap)
    }.unzip
    nextZeros.reduceLeft(_ ++ _) -> nextOnes.reduceLeft(_ ++ _)
  }

  private lazy val zeros = inputs.locationsInGrid(_ == 0).map(_._2).toSet
  private lazy val ones  = inputs.locationsInGrid(_ == 1).map(_._2).toSet

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      val _ -> leftOn = (1 to 2).foldLeft(zeros -> ones) { case (zeros -> ones, step) => stepOnce(step, zeros, ones) }
      leftOn.size
    }.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      val _ -> leftOn = (1 to 50).foldLeft(zeros -> ones) { case (zeros -> ones, step) => stepOnce(step, zeros, ones) }
      leftOn.size
    }.zio
  }.some

  val (key, inputs) = {
    val split = in2.split("\n\n").toSeq

    val key = split.head.toIndexedSeq.map {
      case '.' => 0
      case '#' => 1
    }

    val inputs = split.last.linesIterator.toIndexedSeq.map(_.toSeq.map {
      case '.' => 0
      case '#' => 1
    })

    key -> inputs
  }

  lazy val in2 =
    """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
      |
      |#..#.
      |#....
      |##..#
      |..#..
      |..###""".stripMargin

  lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
