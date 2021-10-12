package codekata2015
package days

import zio.RIO

object Day21 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 21

  val w = Seq(8 -> 4, 10 -> 5, 25 -> 6, 40 -> 7, 74 -> 8)
  val a = Seq(13 -> 1, 31 -> 2, 53 -> 3, 75 -> 4, 102 -> 5, 0 -> 0)
  val r = Seq(25 -> 1, 50 -> 2, 100 -> 3, 20 -> 1, 40 -> 2, 80 -> 3, 0 -> 0)

  // Observations for my inputs:
  // * Effectively, "damage" and "armor" were equivalent, in the sense that adding one
  //   to either had the same effect.
  // * The "min damage of 1" was unlikely to matter since that would have required maximizing
  //   one or the other of damage or armor to fully nullify the corresponding damage, and
  //   this would be very expensive.  Hitting this case for one most likely implied also
  //   hitting it for the other.
  // * Boss's HP exceeded mine, so turning both sides' attacks to 1 results in a loss.
  // * Given those, all I needed to do was most-cheaply exceed the combined (damage + armor)
  //   of the boss (which was 10) through some combination of purchases, keeping in mind
  //   that some of the items are optional.
  // * Same idea for maximizing cost, which works because setting both sides' damage to 1
  //   results in a loss for me.
  override def part1: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = {
        (for {
          ww  <- w
          aa  <- a
          rr1 <- r
          rr2 <- r.filterNot(_ == rr1) :+ (0 -> 0) if (ww._2 + aa._2 + rr1._2 + rr2._2 >= 11)
        } yield (ww._2 + aa._2 + rr1._2 + rr2._2, ww._1 + aa._1 + rr1._1 + rr2._1))
          .minBy(_._2)
      }.zio
    }.some
  override def part2: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = {
        (for {
          ww  <- w
          aa  <- a
          rr1 <- r
          rr2 <- r.filterNot(_ == rr1) :+ (0 -> 0) if (ww._2 + aa._2 + rr1._2 + rr2._2 < 11)
        } yield (ww._2 + aa._2 + rr1._2 + rr2._2, ww._1 + aa._1 + rr1._1 + rr2._1))
          .maxBy(_._2)
      }.zio
    }.some

  def inputs = in2

  val in2 = ""
  val in3 = ""

  override def in: String = ""
}
