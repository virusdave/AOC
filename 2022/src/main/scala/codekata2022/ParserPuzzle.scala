package codekata2022

import common.InRegexParserSyntax
import scala.util.parsing.combinator.RegexParsers
import zio.{RIO, ZEnv, ZIO}

class PuzzlePart[+A] private (val solution: RIO[ZEnv, A])
object PuzzlePart {
  def apply[A](solution: => RIO[ZEnv, A]) = new PuzzlePart(ZIO(solution).flatten)
}

trait Puzzle {
  type PuzzleOut
  final type Part = PuzzlePart[PuzzleOut]
  def dayNum: Int
  def part1: Option[Part]
  def part2: Option[Part]
  def in: String
}

trait ParserPuzzle extends Puzzle with RegexParsers with InRegexParserSyntax