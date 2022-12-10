package codekata2022
package days

import common.InRegexParserSyntax

object Day10 extends ParserPuzzle with InRegexParserSyntax {
  override type PuzzleOut = Any
  override def dayNum: Int = 10

  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val neg: Parser[Int] = "-" ~> num ^^ (-_)
  private val signedNum: Parser[Int] = num | neg

  private val instruction: Parser[Op] = ("noop" ^^^ Noop) | ("addx" ~> signedNum ^^ AddX)

  private sealed abstract class Op(val cycles: Int)
  private case object Noop extends Op(1)
  private case class AddX(v: Int) extends Op(2)

  private case class State(clock: Int, x: Int)
  private val states =
    inputs.parseLinesBy(instruction)
      .scanLeft(State(0, 1)) { case (State(clock, priorX), op) =>
        State(
          clock = clock + op.cycles, x = op match {
            case Noop => priorX
            case AddX(v) => priorX + v
          })
      }.map(state => state.clock -> state).sortBy(_._1)

  override def part1: Option[Part] = PuzzlePart({
    (20 to states.last._1 by 40).map { clk =>
      states.findLast(_._1 < clk).get._2.x * clk
    }.sum
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val mappedStates = states.toMap
    val allStates = (2 to states.last._1)
      .scanLeft(states.head._2) { case (lastState, clk) =>
        mappedStates.getOrElse(clk, lastState)
      }
    allStates.zipWithIndex.map { case (state, clk) =>
      if ((state.x - clk % 40 - 1 /* pixels are 0-based */).abs <= 1) '#' else '.'
    }.grouped(40).map(_.mkString).mkString("\n", "\n", "")
  }.zio).some

  private def inputs = in3

  private lazy val in2 =
    """noop
addx 3
addx -5"""

  private lazy val in3 =
    """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"""

  override def in: String =
    """""".stripMargin
}
