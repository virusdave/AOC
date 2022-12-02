package codekata2022
package days

import enumeratum._
import zio.RIO

object Day02 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 2

  private sealed abstract class Outcome(
      override val entryName: String,
      val outcomeScore: Int,
      val against: Move => Move,
  ) extends EnumEntry
  private object Outcome extends Enum[Outcome] {
    // If A beats B, then Win.against(B) == A because Move.whatDefeats(B) == A, etc
    case object Win extends Outcome("Z", 6, whatDefeats)
    case object Tie extends Outcome("Y", 3, whatTiesAgainst)
    case object Lose extends Outcome("X", 0, whatLosesTo)

    override lazy val values: scala.IndexedSeq[Outcome] = findValues

    private lazy val whatDefeats: Map[Move, Move] = {
      // Each entry in `moves` loses to the one following it (if treated circularly, with wrapping)
      val moves = Seq(Move.Rock, Move.Paper, Move.Scissors)
      moves.zip((moves ++ moves) drop 1).toMap
    }
    private lazy val whatLosesTo: Map[Move, Move] = whatDefeats.map(_.swap) // Swaps winner & loser above
    private lazy val whatTiesAgainst: Map[Move, Move] = Move.values.map(_.dup).toMap // Everything ties itself
  }

  private sealed abstract class Move(override val entryName: String, val shapeScore: Int) extends EnumEntry {
    def against(rhs: Move): Outcome = Outcome.values.find(_.against(rhs) == this).get
  }
  private object Move extends Enum[Move] {
    case object Rock extends Move("A", 1)
    case object Paper extends Move("B", 2)
    case object Scissors extends Move("C", 3)

    override lazy val values: scala.IndexedSeq[Move] = findValues
  }

  private val them: Parser[Move] = "[ABC]".r ^^ Move.withName

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      def XtoA(c: String): String = c.mapChars(_.slide('X', 'A'))
      val us: Parser[Move] = "[XYZ]".r ^^ XtoA ^^ Move.withName
      val line: Parser[(Move, Move)] = them ~ us ^^ (_.toTuple)
      val rounds = inputs.parseLinesBy(line)
      rounds.map { case t -> m => m.shapeScore + m.against(t).outcomeScore }.sum
    }.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      val outcome: Parser[Outcome] = "[XYZ]".r ^^ Outcome.withName
      val line: Parser[(Move, Move)] = them ~ outcome ^^ { case t ~ o => t -> o.against(t) }
      val rounds = inputs.parseLinesBy(line)
      rounds.map { case t -> m => m.shapeScore + m.against(t).outcomeScore }.sum
    }.zio
  }.some

  private def inputs = in2

  private lazy val in2 =
    """A Y
      |B X
      |C Z""".stripMargin

  private lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
