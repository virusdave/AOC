package codekata2020.all

import codekata2020._
import java.util.concurrent.TimeUnit
import zio._
import zio.clock._
import zio.console._
import zio.duration._

import codekata2020.day25.Puzzle

object All extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = (for {
    _ <- putStrLn(s"Starting...")
    start <- currentTime(TimeUnit.MILLISECONDS)


    p2 <- Puzzle.Part2.solution.shush(silent = true)
    _ <- putStrLn(s" Part 2: ${p2}")


    mid <- currentTime(TimeUnit.MILLISECONDS)
    _ <- putStrLn(s"   Took: ${(mid - start).millis.render}")


    p1 <- Puzzle.Part1.solution.shush(silent = false)
    _ <- putStrLn(s" Part 1: ${p1}")


    end <- currentTime(TimeUnit.MILLISECONDS)
    _ <- putStrLn(s"   Took: ${(end - mid).millis.render}")
  } yield ()).exitCode
}
