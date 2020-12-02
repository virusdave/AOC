package codekata2020.all

import zio._
import zio.console._

import codekata2020.day2.Puzzle


object All extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = (for {
//    p1 <- Puzzle.Part1.solution(silent = false)
//    _ <- putStrLn(s" Part 1: ${p1}")
    p2 <- Puzzle.Part2.solution(silent = true)
    _ <- putStrLn(s" Part 2: ${p2}")
  } yield ()).exitCode
}
