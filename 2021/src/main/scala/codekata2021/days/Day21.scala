package codekata2021
package days

object Day21 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 21

  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)

  private lazy val line: Parser[(Int, Int)] = ("Player" ~> num) ~ ("starting position:" ~> num) ^^ (_.toTuple)

  private lazy val (l, r) = inputs.map(parseAll(line, _).get).map(_._2).diag(_(0) -> _(1))

  private case class Player(pos: Int, score: Int)

  override def part1: Option[Part] = PuzzlePart({
  LazyList.iterate((0, LazyList.continually(LazyList.range(1, 11)).flatten, Player(l, 0), Player(r, 0))) {
      case (i, dice, l, r) =>
        val newPos = (l.pos + dice.take(3).sum - 1) % 10 + 1
        (i + 1, dice.drop(3), r, Player(newPos, l.score + newPos))
    }
      .find { case (_, _, _, Player(_, score)) => score >= 1000 }
      .map { case (i, _, Player(_, score), _) => i * 3 * score }
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    def tryCachedEval(cache: Map[(Player, Player), (BigInt, BigInt)], lp: Player, rp: Player): (BigInt, BigInt) = {
      val Player(lPos, lScore) = lp
      val lRolls = for {
        d1 <- 1 to 3
        d2 <- 1 to 3
        d3 <- 1 to 3
      } yield d1 + d2 + d3
      // L wins each time his score is now 21 or greater, plus the number of times that R doesn't win when
      // we look up the cached values from a score of 20 or lower.
      val (lWins, rWins) = lRolls.map { thisRoll =>
        val lPosNew = (lPos + thisRoll - 1) % 10 + 1
        val lScoreNew = lScore + lPosNew

        if (lScoreNew >= 21) (1.big, 0.big)
        else {
          val newL = Player(lPosNew, lScoreNew)
          cache.getOrElse(rp -> newL, tryCachedEval(cache, rp, newL)).swap
        }
      }.unzip
      lWins.sum -> rWins.sum
    }

    val cache = Map.empty[(Player, Player), (BigInt, BigInt)]
    (20 to 0 by -1).foldLeft(cache) { case (cache, lScore) =>
      (20 to 0 by -1).foldLeft(cache) { case (cache, rScore) =>
        (10 to 1 by -1).foldLeft(cache) { case (cache, lPos) =>
          (10 to 1 by -1).foldLeft(cache) { case (cache, rPos) =>
            val entry =
              (Player(lPos, lScore), Player(rPos, rScore)) ->
                tryCachedEval(cache, Player(lPos, lScore), Player(rPos, rScore))
            cache + entry
          }
        }
      }
    }
      .get(Player(l, 0) -> Player(r, 0))
      .map(_.max)

  }.zio).some

  private def inputs = in2.linesIterator.toIndexedSeq

  private lazy val in2 =
    """Player 1 starting position: 4
      |Player 2 starting position: 8""".stripMargin

  override def in: String =
    """""".stripMargin
}
