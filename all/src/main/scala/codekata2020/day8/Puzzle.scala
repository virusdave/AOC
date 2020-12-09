package codekata2020.day8

import codekata2020._
import scala.annotation.tailrec
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  private val inputs = in.linesIterator.toIndexedSeq

  object Part1 {
    def solution = {
      @tailrec def run(ip: Int, addrsExecuted: Set[Int], acc: Int): Int = {
        if (addrsExecuted(ip)) acc
        else {
          val splits = inputs(ip).split(" ")
          (splits(0), splits(1)) match {
            case ("nop", _) => run(ip + 1, addrsExecuted + ip, acc)
            case ("jmp", off) => run(ip + off.toInt, addrsExecuted + ip, acc)
            case ("acc", off) => run(ip + 1, addrsExecuted + ip, acc + off.toInt)
            case _ => ???
          }
        }
      }
      run(0, Set.empty, 0)
    }.zio
  }

  object Part2 {
    def solution = {
      @tailrec def run(mem: IndexedSeq[String], ip: Int, addrsExecuted: Set[Int], acc: Int): Option[Int] = {
        if (addrsExecuted(ip)) None
        else if (ip >= mem.size) Some(acc)
        else {
          val splits = mem(ip).split(" ")
          (splits(0), splits(1)) match {
            case ("nop", _) => run(mem, ip + 1, addrsExecuted + ip, acc)
            case ("jmp", off) => run(mem, ip + off.toInt, addrsExecuted + ip, acc)
            case ("acc", off) => run(mem, ip + 1, addrsExecuted + ip, acc + off.toInt)
            case _ => ???
          }
        }
      }

      @tailrec def flip(ip: Int): Option[Int] = {
        if (ip >= inputs.size) None
        else {
          val splits = inputs(ip).split(" ")
          splits(0) match {
            case "nop" | "jmp" =>
              val mem =
                inputs.take(ip) :+ s"${if (splits(0) == "nop") "jmp" else "nop"} ${splits(1)}" :++ inputs.drop(ip + 1)
              val out = run(mem, 0, Set.empty, 0)
              if (out.isDefined) out else flip(ip + 1)
            case _ => flip(ip + 1)
          }
        }
      }
      flip(0)
    }.zio
  }

  private lazy val in2 =
    """nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6""".stripMargin

  private lazy val in3 =
    """nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
