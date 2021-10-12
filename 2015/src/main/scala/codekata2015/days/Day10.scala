package codekata2015
package days

import scala.annotation.tailrec

object Day10 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 10

  def rle(s: String): String = {
    @tailrec def loop(rest: String, acc: StringBuffer): StringBuffer = {
      if (rest.isEmpty) acc
      else {
        val (head, tail) = rest.span(_ == rest.head)
        loop(tail, acc.append(head.length).append(head.head))
      }
    }

    loop(s, new StringBuffer()).toString
  }
  override def part1: Option[Part] =
    new Part {
      override def solution = (1 to 40).foldLeft(inputs) { case (seq, _) => rle(seq) }.length.zio
    }.some
  override def part2: Option[Part] =
    new Part {
      override def solution = (1 to 50).foldLeft(inputs) { case (seq, _) => rle(seq) }.length.zio
    }.some

  def inputs = in2

  val in2 = "1"
  val in3 = ""

  override def in: String = ""
}
