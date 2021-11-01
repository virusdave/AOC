package codekata2016
package days

import zio.RIO

object Day10 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 10

  // I'm not super happy about this one.  I think the state updates could be made MUCH nicer.

  case class WorldState(outputs: Map[Int, Output], bots: Map[Int, Bot], comparisons: Map[(Int, Int), BotId])

  sealed trait Dest {
    val id: Int
    def accept(state: WorldState, value: Int): WorldState
  }
  case class BotId(id: Int) extends Dest {
    override def accept(state: WorldState, value: Int): WorldState = state.bots(id).accept(state, value)
  }
  case class OutputId(id: Int) extends Dest {
    override def accept(state: WorldState, value: Int): WorldState = {
      val o = state.outputs.getOrElse(id, Output(id))
      o.accept(state.copy(outputs = state.outputs + (id -> o)), value)
    }
  }
  case class Output(id: Int, contents: Set[Int] = Set.empty) {
    def accept(state: WorldState, value: Int): WorldState =
      state.copy(outputs = state.outputs + (id -> this.copy(contents = contents + value)))
  }
  case class Bot(id: BotId, low: Dest, high: Dest, holding: Option[Int] = None) {
    def accept(state: WorldState, value: Int): WorldState =
      holding.fold2(
        state.copy(bots = state.bots + (id.id -> this.copy(holding = value.some))),
        held => {
          val (lo, hi) = (Math.min(value, held), Math.max(value, held))
          val meEmpty = state.copy(
            bots = state.bots + (id.id                  -> this.copy(holding = None)),
            comparisons = state.comparisons + ((lo, hi) -> id))
          val afterLow  = low.accept(meEmpty, lo)
          val afterHigh = high.accept(afterLow, hi)
          afterHigh
        },
      )
  }
  case class Value(value: Int, dest: BotId)

  private val num: Parser[Int]         = "[0-9]+".r ^^ (_.toInt)
  private val botId: Parser[BotId]     = "bot" ~> num ^^ BotId
  private val output: Parser[OutputId] = "output" ~> num ^^ OutputId
  private val dest: Parser[Dest]       = botId | output
  private val valIn: Parser[Value]     = ("value" ~> num) ~ ("goes to" ~> botId) ^^ { case v ~ d => Value(v, d) }
  private val botOut: Parser[Bot] = botId ~ ("gives low to" ~> dest) ~ ("and high to" ~> dest) ^^ {
    case b ~ l ~ h => Bot(b, l, h)
  }
  private val lines: Parser[Either[Value, Bot]] = (valIn ^^ Left.apply) | (botOut ^^ Right.apply)

  private val parsed         = inputs.linesIterator.toList.map(parseAll(lines, _).getOrFail)
  private val (values, bots) = (parsed.collect { case Left(v) => v }, parsed.collect { case Right(b) => b })
  private val state          = WorldState(Map.empty, bots.map(b => b.id.id -> b).toMap, Map.empty)
  private val finalState     = values.foldLeft(state) { case (s, v) => v.dest.accept(s, v.value) }

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = finalState.comparisons.get(17 -> 61).zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = (0 to 2).map(finalState.outputs).map(_.contents.head).product.zio
  }.some

  def inputs = in

  lazy val in2 =
    """value 5 goes to bot 2
      |bot 2 gives low to bot 1 and high to bot 0
      |value 3 goes to bot 1
      |bot 1 gives low to output 1 and high to bot 0
      |bot 0 gives low to output 2 and high to output 0
      |value 2 goes to bot 2""".stripMargin

  lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
