package codekata2015
package days

import zio.RIO

object Day11 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 11
  val letters = ('a' to 'z')
  val paired = letters.lazyZip(('0' to '9') ++ letters).take(letters.size)
  val l2d = paired.toMap
  val d2l = paired.map(_.swap).toMap

  private def inc(s: String): String = {
    val plusone = (BigInt(s.toIndexedSeq.map(l2d).mkString, 26) + 1)
      .toString(26)
      // Left pad to appropriate length
      .|>(x => Seq.fill(s.length - x.length)('0') ++ x)
      .toIndexedSeq
      .map(d2l)
      .mkString

    plusone
  }

  private def accept(s: String) = {
    // 3 consecutive increasing characters
    s.toSeq.sliding(3).exists {
      case Seq(a, b, c) => c == b + 1 && b == a + 1
    } &&
    // No forbidden chars
    !s.exists(Set('i', 'o', 'l').contains) &&
    // Two non-overlapping pairs of repeated characters
    {
      val runs = s.toIndexedSeq.sliding(2).zipWithIndex.flatMap {
        case (IndexedSeq(a, b), idx) if a == b => idx.some
        case _ => None
      }.toIndexedSeq
      // If there's at least 3 sets of pairs, then at least two of them must be nonoverlapping
      runs.size > 2 ||
      // If there's exactly two, then check for overlap
        (runs.size == 2 && runs(0)+1 < runs(1))
    }
  }
  def next(in: String): String = LazyList.iterate(in)(inc).find(accept).get

  override def part1: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = next(inputs).zio
    }.some
  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = next(inc(next(inputs))).zio
  }.some

  def inputs = in

  val in2 = "hijklmmn"
  val in3 = "abbceffg"
  val in4 = "abcdefgh"
  val in5 = "ghijklmn"

  override def in: String = ""
}
