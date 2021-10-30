package codekata2016
package days

import breeze.math._
import zio.RIO

object Day01 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 1

  sealed abstract class Dir(private[this] val rot: Complex) {
    def rotate(c: Complex): Complex = c * rot
  }
  case object Left extends Dir(Complex.i)
  case object Right extends Dir(-Complex.i)

  val dirnum: Parser[(Dir, Int)] = {
    val L: Parser[Dir] = "L" ^^^ Left
    val R: Parser[Dir] = "R" ^^^ Right

    (L | R) ~ "[0-9]+".r ^^ { case d ~ n => (d, n.toInt)}
  }
  val line: Parser[List[(Dir, Int)]] = rep1sep(dirnum, ",")

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {

      val (end, _) = parseAll(line, in).get
        .foldLeft((Complex.zero, Complex.i)) { case ((pos, pointed), (dir, dist)) =>
          (pos + dist * dir.rotate(pointed), dir.rotate(pointed))
        }
      end.real.abs + end.imag.abs
    }.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      case class State(seen: Set[Complex], pos: Complex, facing: Complex, twice: Option[Complex])
      val twice = parseAll(line, in).get
        .foldLeft(State(Set.empty, Complex.zero, Complex.i, None)) { case (state, (dir, dist)) =>
          state.twice.fold2(
            (1 to dist).foldLeft(state.copy(facing = dir.rotate(state.facing))) { case (state, _) =>
              state.twice.fold2({
                val newPos = state.pos + state.facing
                if (state.seen.contains(newPos))
                  state.copy(twice = newPos.some)
                else
                state.copy(seen = state.seen + newPos, pos = newPos)
              }, _ => state)
            }, _ => state)
        }
        .twice.get

      twice.real.abs + twice.imag.abs
    }.zio
  }.some

  def inputs = in

  val in2 = ""
  val in3 = ""

  override def in: String = ""
}
