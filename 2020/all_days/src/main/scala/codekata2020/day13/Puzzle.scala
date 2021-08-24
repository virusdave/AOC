package codekata2020.day13

import codekata2020._
import scala.annotation.tailrec

object Puzzle {
  private val inputs = in2.linesIterator.toIndexedSeq

  object Part1 {
    def solution = {
      val first = inputs.head.big
      val busIDs = inputs(1).split(",").flatMap(_.safeBig)
      busIDs.map(id => (first + id - first % id, id * (id - first % id))).minBy(_._1)._2
    }.zio
  }

  object Part2 {
    def solution = {
      val busIDsAndOffsetsAtMinTime =
        inputs(1).split(",").map(_.safeBig).zipWithIndex.flatMap { case (a,b) => a.map(x => (x, x - b)) }

      // Use the Chinese Remainder Theorem to solve this system of congruences.
      // At the "min time" T, the i'th bus (with bus number B[i]) will fit:   T ≅ (B[i] - i) mod B[i]
      @tailrec def eGCD(a: BigInt, b: BigInt, x0: BigInt, x1: BigInt): BigInt =
        if (a > 1) eGCD(b, a % b, x1 - (a / b) * x0, x0) else x1

      def modularInverse(a: BigInt, b: BigInt): BigInt = {
        if (b == 1) 1
        else {
          val x1 = eGCD(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      val moduliProduct = busIDsAndOffsetsAtMinTime.map(_._1).product
      busIDsAndOffsetsAtMinTime.foldLeft(0.big) { case (sum, (modulus, residue)) =>
        val p = moduliProduct / modulus
        sum + residue * modularInverse(p, modulus) * p
      } % moduliProduct  // Get minimal solution
    }.zio
  }

  private lazy val in2 =
    """939
      |7,13,x,x,59,x,31,19""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
