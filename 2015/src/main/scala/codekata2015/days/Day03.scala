package codekata2015
package days

import breeze.math.Complex

object Day03 extends Puzzle {
  override type A = Any
  override def dayNum: Int = 3

  override def part1: Option[Part] = new Part {
    override def solution = inputs.scanLeft(Complex.zero){
      case (c, '<') => c - 1
      case (c, '>') => c + 1
      case (c, '^') => c + Complex.i
      case (c, 'v') => c - Complex.i
      case (c, _) => c
    }
      .toSet
      .size
      .zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution = {
      val (pathA, pathB) = inputs.toIndexedSeq.grouped(2).toIndexedSeq.map {
        case IndexedSeq(a, b) => (a,b)
      }.unzip
      def locs(path: Seq[Char]) = path.scanLeft(Complex.zero){
        case (c, '<') => c - 1
        case (c, '>') => c + 1
        case (c, '^') => c + Complex.i
        case (c, 'v') => c - Complex.i
        case (c, _) => c
      }
      val (locsA, locsB) = (locs(pathA), locs(pathB))
      (locsA.toSet ++ locsB.toSet).size.zio
    }

  }.some

  def inputs = in

  val in2 = "^>v<"
  val in3 = "^v^v^v^v^v"

  override def in: String = """"""
}