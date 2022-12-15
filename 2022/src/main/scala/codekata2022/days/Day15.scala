package codekata2022
package days

import scala.collection.parallel.CollectionConverters._
import spire.implicits._
import spire.math._

object Day15 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 15

  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val neg: Parser[Int] = "-" ~> num ^^ (-_)
  private val signedNum: Parser[Int] = num | neg

  private val line: Parser[Int ~ Int ~ Int ~ Int] =
    ("Sensor at x=" ~> signedNum) ~ (", y=" ~> signedNum) ~
      (": closest beacon is at x=" ~> signedNum) ~ (", y=" ~> signedNum)

  private case class Line(sensor: (Int, Int), closestBeacon: (Int, Int)) {
    lazy val l1DistanceToClosestBeacon: Int = (closestBeacon - sensor).abs.sum
  }
  private val lines: IndexedSeq[Line] =
    inputs.parseLinesBy(line).map { case sx ~ sy ~ bx ~ by => Line(sx -> sy, bx -> by) }

  override def part1: Option[Part] = PuzzlePart({
    //val targetY = 10
    val targetY = 2000000
    val beacons = lines.map(_.closestBeacon).toSet

    val noBeaconPositionXs = lines.flatMap { case line@Line((sx, sy), _) =>
      val deltaY = (sy - targetY).abs
      Interval.closed(
        sx - line.l1DistanceToClosestBeacon + deltaY,
        sx + line.l1DistanceToClosestBeacon - deltaY,
      )
        .iterator(1)
        .filterNot(x => beacons.contains(x -> targetY))
    }.distinct
    noBeaconPositionXs.size
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    //val posMax = 20
    val posMax = 4000000
    val xRange = Interval.closed(0, posMax)

    (0 to posMax).par.flatMap { y =>
      val maybeBeaconIntervalsPerSensor = lines.map { case line@Line((sx, sy), _) =>
        val deltaY = (y - sy).abs
        val noBeaconInterval = Interval.closed(
          sx - line.l1DistanceToClosestBeacon + deltaY,
          sx + line.l1DistanceToClosestBeacon - deltaY,
        )
        val maybeBeaconIntervals: Seq[Interval[Int]] = xRange -- noBeaconInterval
        maybeBeaconIntervals // Logically, the union of these might have beacons
      } // Produces a CNF (ANDs of ORs) of intervals where beacons could exist.  Next, use ...
      maybeBeaconIntervalsPerSensor.foldLeft(List(xRange)) { case (ls, rs) =>
        for { // the distributive law to turn it into DNF, where the now-internal ANDs ...
          l <- ls // can be evaluated via intersection to produce a candidate set of ...
          r <- rs // x positions (which for most rows will all be empty!)
          i = l intersect r
          pruned <- Seq(i) if i.nonEmpty // Prune empty intervals before exponential growth
        } yield pruned
      }.flatMap(_.iterator(1).map(x => x.big * 4000000 + y))
    }.seq

  }.zio).some

  private def inputs = in

  private lazy val in2 =
    """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"""

  override def in: String =
    """""".stripMargin
}
