package codekata2015
package days

import zio.RIO

object Day14 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 14

  def pName: Parser[String] = "[A-Z][a-z]*".r
  def pNum: Parser[Int]     = "[0-9]+".r ^^ { _.toInt }
  def line: Parser[(String, (Int, Int, Int))] =
    (pName <~ "can fly") ~ (pNum <~ "km/s for") ~ (pNum <~ "seconds, but then must rest for") ~ (pNum <~ "seconds.") ^^ {
      case n ~ s ~ d ~ r => (n, (s, d, r))
    }

  lazy val parsed = inputs.parseLinesBy(line).toMap
  lazy val deers = parsed.keys.toSeq

  val secs = 2503

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = deers.map(deer => parsed(deer) match {
      case (s, d, r) =>
        val (whole, left) = secs.big./%(d+r)
        deer -> (whole * s * d + (if(left <= d) left * s else  (d * s).big))
    }).maxBy(_._2).zio
  }.some
  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = (1 to secs).flatMap { idx =>
      val current = deers.map(deer => parsed(deer) match {
        case (s, d, r) =>
          val (whole, left) = idx.big./%(d+r)
          deer -> (whole * s * d + (if(left <= d) left * s else  (d * s).big))
      }).sortBy(_._2).reverse
      current.takeWhile(_._2 == current.head._2).map(l => (l._1, 1))
    }.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap.maxBy(_._2).zio
  }.some

  def inputs = in2

  val in2 = """Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
              |Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.""".stripMargin
  val in3 = ""

  override def in: String = """""".stripMargin
}
