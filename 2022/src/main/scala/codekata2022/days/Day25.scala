package codekata2022
package days

import scala.annotation.tailrec

object Day25 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 25

  override def part1: Option[Part] = PuzzlePart({
    def toBase10(in: String): BigInt = {
      @tailrec def go(in: String, soFar: BigInt): BigInt =
        if (in.isEmpty) soFar
        else go(in.tail, 5.big * soFar + (in.head match {
            case '=' => -2
            case '-' => -1
            case d => s"$d".toInt
          }))

      go(in, 0.big)
    }
    def toSnafu(in: BigInt): String = {
      @tailrec def go(in: BigInt, soFar: List[Char]): String = {
        if (in == 0) soFar.mkString
        else {
          val (rest, rem) = in /% 5
          val (digit, carry) = if (rem > 2) (rem - 5, 1) else (rem, 0)
          go(rest + carry, (digit.toInt match {
            case -2 => '='
            case -1 => '-'
            case d => s"$d".head
          }) :: soFar)
        }
      }
      go(in, Nil)
    }

    inputs.splitAtLinebreaksBy(toBase10).sum.diag { _ -> toSnafu(_) }
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    ()
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122"""

  override def in: String =
    """""".stripMargin
}
