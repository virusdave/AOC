package codekata2015
package days

object Day6 extends Puzzle {
  override type A = Any

  override def dayNum: Int = 6

  sealed trait Op
  case object On extends Op
  case object Not extends Op
  case object Off extends Op


  type Coord = (Int, Int)
  def op: Parser[Op] = "turn on" ^^^ On | "turn off" ^^^ Off | "toggle" ^^^ Not
  def num: Parser[Int] = "\\d+".r ^^ (_.toInt)
  def pair: Parser[Coord] = (num <~ ",") ~ num ^^ { case l ~ r => (l, r)}
  def line: Parser[(Op, Coord, Coord)] = op ~ pair ~ ("through" ~> pair) ^^ { case op ~ l ~ r => (op, l, r)}

  def insideBounds(corner1: Coord, corner2: Coord)(point: Coord) = {
    point._1 >= Math.min(corner1._1, corner2._1) &&
      point._1 <= Math.max(corner1._1, corner2._1) &&
      point._2 >= Math.min(corner1._2, corner2._2) &&
      point._2 <= Math.max(corner1._2, corner2._2)
  }

  override def part1: Option[Part] = new Part {
    val ops: Map[Op, Boolean => Boolean] = Map(
      On -> (_ => true),
      Not -> (x => !x),
      Off -> (_ => false)
    )

    val lights = IndexedSeq.fill(1000, 1000)(false)
    val parsed = inputs.map { l => parseAll(line, l).get }
    override def solution = parsed.foldLeft(lights) { case (lights, (op, c1, c2)) =>
      lights.mapGridWithLocation {
        case (v, pos) => if (insideBounds(c1, c2)(pos)) ops(op)(v) else v
      }
    }.flatten.count(identity).zio
  }.some

  override def part2: Option[Part] =  new Part {
    val ops: Map[Op, Int => Int] = Map(
      On -> (_ + 1),
      Not -> (_ + 2),
      Off -> (x => Math.max(0, x - 1))
    )

    val lights = IndexedSeq.fill(1000, 1000)(0)
    val parsed = inputs.map { l => parseAll(line, l).get }
    override def solution = parsed.foldLeft(lights) { case (lights, (op, c1, c2)) =>
      lights.mapGridWithLocation {
        case (v, pos) => if (insideBounds(c1, c2)(pos)) ops(op)(v) else v
      }
    }.flatten.sum.zio
  }.some

  def inputs = in.linesIterator

  val in2 = Seq(
    "turn on 0,0 through 999,999",
    "toggle 0,0 through 999,0",
    "turn off 499,499 through 500,500",
  ).mkString("\n")
  val in3 = ""

  override def in: String = "".stripMargin
}