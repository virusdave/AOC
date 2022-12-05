package codekata2022

import common.InRegexParserSyntax
import scala.util.parsing.combinator.RegexParsers
import zio.{RIO, ZEnv}

case class PuzzlePart[+A](solution: RIO[ZEnv, A])

trait Puzzle {
  type PuzzleOut
  final type Part = PuzzlePart[PuzzleOut]
  def dayNum: Int
  def part1: Option[Part]
  def part2: Option[Part]
  def in: String
}

trait ParserPuzzle extends Puzzle with RegexParsers with InRegexParserSyntax