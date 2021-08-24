package codekata2020.day14

import codekata2020._
import scala.annotation.tailrec

object Puzzle  {
  private val inputs = in.linesIterator.toIndexedSeq

  private val MaskR = "^mask = (.*)$".r
  private val MemR = """mem\[([\d]*)\] = ([\d]*)""".r

  object Part1 {
    def solution = {
      case class State(ands: BigInt, ors: BigInt, mem: Map[Int, BigInt])

      inputs.foldLeft(State(0.big, 0.big, Map.empty)) { case (state, line) => line match {
        case MaskR(mask) =>
          state.copy(
            ands = mask.map(Map('0'.dup).withDefaultValue('1')).bigBinary,
            ors = mask.map(Map('1'.dup).withDefaultValue('0')).bigBinary)
        case MemR(addr, v) =>
          state.copy(mem = state.mem + (addr.toInt -> (v.big & state.ands | state.ors)))
      }}.mem.values.sum
    }.zio
  }

  object Part2 {
    def solution = {
      case class State(floats: BigInt, ors: BigInt, mem: Map[BigInt, BigInt])

      def expandAddrWithFloats(addr: BigInt, floats: BigInt): Set[BigInt] = {
        val floatBits = {
          @tailrec def floatBits(setSoFar: Set[Int], v: BigInt): Set[Int] =
            if (v == 0) setSoFar else floatBits(setSoFar + v.lowestSetBit, v.clearBit(v.lowestSetBit))

          floatBits(Set.empty, floats)
        }
        floatBits.subsets().toSeq.map { subset =>
          floatBits.foldLeft(addr) { case (a, bit) => if (subset(bit)) a.setBit(bit) else a.clearBit(bit) }}.toSet
      }

      inputs.foldLeft(State(0.big, 0.big, Map.empty)) { case (state, line) => line match {
        case MaskR(mask) =>
          state.copy(
            floats = mask.map(Map('X'->'1').withDefaultValue('0')).bigBinary,
            ors = mask.map(Map('1'.dup).withDefaultValue('0')).bigBinary)
        case MemR(addr, v) =>
          state.copy(mem = state.mem ++ expandAddrWithFloats(addr.big | state.ors, state.floats).map(_ -> v.big))
      }}.mem.values.sum
    }.zio
  }

  private lazy val in2 =
    """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
      |mem[8] = 11
      |mem[7] = 101
      |mem[8] = 0""".stripMargin

  private lazy val in3 =
    """mask = 000000000000000000000000000000X1001X
      |mem[42] = 100
      |mask = 00000000000000000000000000000000X0XX
      |mem[26] = 1""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
