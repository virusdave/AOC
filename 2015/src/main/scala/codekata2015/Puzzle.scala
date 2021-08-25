package codekata2015

import scala.util.parsing.combinator.RegexParsers
import zio.{RIO, ZEnv}

trait PuzzlePart[A] {
  def solution: RIO[ZEnv, A]
}

trait Puzzle extends RegexParsers {
  type A
  final type Part = PuzzlePart[A]
  def part1: Option[Part]
  def part2: Option[Part]
  def dayNum: Int
  def in: String
}