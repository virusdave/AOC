package codekata2021
package days

import scala.annotation.tailrec
import zio.RIO

object Day03 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 3

  implicit class _StringOps(in: String) {
    def binary: BigInt = BigInt.apply(in, 2)
  }
  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      val counts = inputs.map(_.groupBy(identity).view.mapValues(_.size))
      counts.map(_.maxBy(_._2)._1).mkString.binary * counts.map(_.minBy(_._2)._1).mkString.binary
    }.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      @tailrec
      def find(prefix: String, remaining: Seq[Seq[Char]], f: Map[Char, Int] => (Char, Int)): String = {
        val rt = remaining.transpose
        if (rt.isEmpty) prefix
        else if (rt.size == 1)
          prefix + rt.head.mkString
        else {
          val chosenBit    = remaining.head.groupBy(identity).view.mapValues(_.size).toMap.|>(f)._1
          val newPrefix    = prefix + chosenBit
          val newRemaining = rt.filter(_.startsWith(Seq(chosenBit))).transpose
          find(newPrefix, newRemaining.tail, f)
        }
      }
      find("", inputs, _.maxBy(_.swap)).mkString.binary * find("", inputs, _.minBy(_.swap)).mkString.binary
    }.zio
  }.some

  def inputs = in2.linesIterator.toSeq.transpose

  lazy val in2 =
    """00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010""".stripMargin

  lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
