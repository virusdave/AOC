package codekata2020.all

import codekata2020._
import zio._
import zio.console._

import codekata2020.day12.Puzzle


object All extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = (for {
    p2 <- Puzzle.Part2.solution.shush(silent = true)
    _ <- putStrLn(s" Part 2: ${p2}")
    p1 <- Puzzle.Part1.solution.shush(silent = false)
    _ <- putStrLn(s" Part 1: ${p1}")
  } yield ()).exitCode
}
