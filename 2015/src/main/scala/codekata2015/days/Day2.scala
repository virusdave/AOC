package codekata2015
package days

import zio.{RIO, ZEnv}

object Day2 extends Puzzle {
  override type A = Int

  override def dayNum: Int = 2

  override def part1: Option[Part] = new Part {
    override def solution: RIO[ZEnv, Int] = inputs.map { _.split("x").map(_.toInt) match {
      case Array(l, w, h) =>
        val sides = Seq(l*w, w*h, h*l)
        sides.sum*2 + sides.min
    } }.sum.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[ZEnv, Int] = inputs.map { _.split("x").map(_.toInt) match {
      case Array(l, w, h) =>
        val sides = Seq(l+w, w+h, h+l)
        l*w*h + sides.min * 2
    } }.sum.zio
  }.some

  def inputs = in2.linesIterator

  val in2 = "2x3x4"

  val in3 = "1x1x10"
  override def in: String = "".stripMargin
}