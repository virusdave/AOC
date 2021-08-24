package codekata2020.day12

import codekata2020._
import breeze.math._
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  private val inputs = in.linesIterator.toIndexedSeq.map(_.splitAt(1))

  object Part1 {
    case class Ship(loc: Complex, dir: Complex)
    def action: Map[Char, (Ship, Int) => Ship] = Map(
      'F' -> {(ship: Ship, n: Int) => ship.copy(loc = ship.loc + n * ship.dir) },
      'L' -> {(ship: Ship, n: Int) => ship.copy(dir = ship.dir * Complex.i.pow(n / 90)) },
      'R' -> {(ship: Ship, n: Int) => ship.copy(dir = ship.dir * Complex.i.pow((360 - n) / 90)) },
      'N' -> {(ship: Ship, n: Int) => ship.copy(loc = ship.loc + n * Complex.i) },
      'S' -> {(ship: Ship, n: Int) => ship.copy(loc = ship.loc - n * Complex.i) },
      'E' -> {(ship: Ship, n: Int) => ship.copy(loc = ship.loc + n) },
      'W' -> {(ship: Ship, n: Int) => ship.copy(loc = ship.loc - n) },
    )
    def solution = {
      inputs.foldLeft(Ship(loc = Complex.zero, dir = Complex(1, 0))) {
        case (ship, (c, n)) => action(c.head)(ship, n.toInt)//.tap(x => println(s"$c ${n.toInt} $x"))
      }.diag(_.loc.re().pipe(Math.abs) + _.loc.im().pipe(Math.abs))
    }.zio
  }

  object Part2 {
    case class Ship(loc: Complex, wp: Complex)
    def action: Map[Char, (Ship, Int) => Ship] = Map(
      'F' -> {(ship: Ship, n: Int) => ship.copy(loc = ship.loc + n * ship.wp) },
      'L' -> {(ship: Ship, n: Int) => ship.copy(wp = ship.wp * Complex.i.pow(n / 90)) },
      'R' -> {(ship: Ship, n: Int) => ship.copy(wp = ship.wp * Complex.i.pow((360 - n) / 90)) },
      'N' -> {(ship: Ship, n: Int) => ship.copy(wp = ship.wp + n * Complex.i) },
      'S' -> {(ship: Ship, n: Int) => ship.copy(wp = ship.wp - n * Complex.i) },
      'E' -> {(ship: Ship, n: Int) => ship.copy(wp = ship.wp + n) },
      'W' -> {(ship: Ship, n: Int) => ship.copy(wp = ship.wp - n) },
    )
    def solution = {
      inputs.foldLeft(Ship(loc = Complex.zero, wp = Complex(10, 1))) {
        case (ship, (c, n)) => action(c.head)(ship, n.toInt)//.tap(x => println(s"$c ${n.toInt} $x"))
      }.diag(_.loc.re().pipe(Math.abs) + _.loc.im().pipe(Math.abs))
    }.zio
  }

  private lazy val in2 =
    """F10
      |N3
      |F7
      |R90
      |F11""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
