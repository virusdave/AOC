package codekata2015
package days
import zio.RIO

object Day01 extends Puzzle {
  override type A = Int

  override def dayNum: Int = 1

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Int] = input.foldLeft(0) {
      case (n, ')') => n - 1
      case (n, '(') => n + 1
      case (n, _) => n
    }.zio
  }.some


  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Int] = input.scanLeft(0) {
      case (n, ')') => n - 1
      case (n, '(') => n + 1
      case (n, _) => n
    }.indexWhere(_ < 0).zio
  }.some

  def input: String = in2
  lazy val in2: String = "(())"
  lazy val in3: String = "))((((("

  override lazy val in: String = """"""
}