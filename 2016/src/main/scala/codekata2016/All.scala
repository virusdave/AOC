package codekata2016

import java.util.concurrent.TimeUnit
import zio._
import zio.clock._
import zio.console._
import zio.duration._

object All extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    (solutions.head |> runDay).exitCode

  private def runDay(puzzle: Puzzle): RIO[ZEnv, Unit] = (for {
    _ <- putStrLn(s"Starting day ${puzzle.dayNum}...")
    start <- currentTime(TimeUnit.MILLISECONDS)


    p2 <- puzzle.part2.fold2(ZIO.unit, p => p.solution/*.shush(silent = true)*/)
    _ <- putStrLn(s" Part 2: ${p2}")


    mid <- currentTime(TimeUnit.MILLISECONDS)
    _ <- putStrLn(s"   Which Took: [${(mid - start).millis.render}]")


    p1 <- puzzle.part1.fold2(ZIO.unit, p => p.solution/*.shush(silent = true)*/)
    _ <- putStrLn(s" Part 1: ${p1}")


    end <- currentTime(TimeUnit.MILLISECONDS)
    _ <- putStrLn(s"   Which Took: [${(end - mid).millis.render}]")
  } yield ())

  private def solutions: Seq[Puzzle] = Seq(
    days.Latest,
    days.Day10,
    days.Day09,
    days.Day08,
    days.Day07,
    days.Day06,
    days.Day05,
    days.Day04,
    days.Day03,
    days.Day02,
    days.Day01,
  ).filter(day => day.part2.orElse(day.part1).isDefined)
}
