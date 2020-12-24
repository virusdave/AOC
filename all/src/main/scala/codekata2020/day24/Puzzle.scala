package codekata2020.day24

import breeze.math.Complex
import codekata2020._
import enumeratum.EnumEntry.Lowercase
import enumeratum._
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {

  sealed abstract class Dir(val v: Complex) extends EnumEntry with Lowercase
  object Dir extends Enum[Dir] {
    case object E extends Dir(Complex(1, 0))
    case object W extends Dir(Complex(-1, 0))
    case object SE extends Dir(Complex(0, 1))
    case object SW extends Dir(Complex(-1, 1))
    case object NE extends Dir(Complex(1, -1))
    case object NW extends Dir(Complex(0, -1))

    override def values: IndexedSeq[Dir] = findValues
  }
  def parseDir: Parser[Dir] = ("e" | "w" | "se" | "sw" | "ne" | "nw") ^^ { s => Dir.withNameLowercaseOnly(s) }
  def parseLine: Parser[List[Dir]] = rep1(parseDir)

  val paths: Seq[Complex] = inputs.map { l => parseAll(parseLine, l).get.map(_.v).sum }
  val start = paths.foldLeft(Set.empty[Complex]) { case (blacks, tile) =>
    if (!blacks(tile)) (blacks + tile) else (blacks - tile)
  }

  object Part1 {
    def solution = {
      start.size
    }.zio
  }

  object Part2 {
    def solution = {
      val coords: Set[List[Int]] = start.map { c => List(c.real.toInt, c.imag.toInt) }
      // (Black, Count) means Black next iteration if present in the map
      val iterateRule = Set((false, 2), (true, 1), (true, 2))
      def crossJoin[T](list: List[List[T]]): List[List[T]] = list match {  // Cartesian product of input lists
        case Nil => Nil
        case xs :: Nil => xs map (List(_))
        case x :: xs => for {
          i <- x
          j <- crossJoin(xs)
        } yield List(i) ++ j
      }
      def countNear(live: Set[List[Int]], coords: List[Int]) = List(
          List(coords.head-1, coords.last),   List(coords.head+1, coords.last),
          List(coords.head,   coords.last+1), List(coords.head,   coords.last-1),
          List(coords.head-1, coords.last+1), List(coords.head+1, coords.last-1),
        ).count(live)


      def iterateNdimWorld(iterations: Int, initial: Set[List[Int]]) =
        (1 to iterations).foldLeft(initial) { case (world, _) =>
          val keys = world.toList
          crossJoin(keys.head.indices.map(i => keys.map(_(i)).sorted.diag(_.head-1 to _.last+1).toList).toList).flatMap { c =>
            c.some.filter(_ => iterateRule(world(c), countNear(world, c)))
          }.toSet
        }.size

      iterateNdimWorld(100, coords)
    }.zio
  }

  private def inputs = in.linesIterator.toIndexedSeq
  //  private def inputs = in.split("\n\n").toIndexedSeq

  private lazy val in2 =
    """sesenwnenenewseeswwswswwnenewsewsw
      |neeenesenwnwwswnenewnwwsewnenwseswesw
      |seswneswswsenwwnwse
      |nwnwneseeswswnenewneswwnewseswneseene
      |swweswneswnenwsewnwneneseenw
      |eesenwseswswnenwswnwnwsewwnwsene
      |sewnenenenesenwsewnenwwwse
      |wenwwweseeeweswwwnwwe
      |wsweesenenewnwwnwsenewsenwwsesesenwne
      |neeswseenwwswnwswswnw
      |nenwswwsewswnenenewsenwsenwnesesenew
      |enewnwewneswsewnwswenweswnenwsenwsw
      |sweneswneswneneenwnewenewwneswswnese
      |swwesenesewenwneswnwwneseswwne
      |enesenwswwswneneswsenwnewswseenwsese
      |wnwnesenesenenwwnenwsewesewsesesew
      |nenewswnwewswnenesenwnesewesw
      |eneswnwswnwsenenwnwnwwseeswneewsenese
      |neswnwewnwnwseenwseesewsenwsweewe
      |wseweeenwnesenwwwswnew""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
