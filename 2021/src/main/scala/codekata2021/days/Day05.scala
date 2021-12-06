package codekata2021
package days

import zio.RIO

object Day05 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 5

  def num: Parser[Int]                       = "[0-9]+".r ^^ (_.toInt)
  def pair: Parser[(Int, Int)]               = num ~ ("," ~> num) ^^ { case l ~ r => l -> r }
  def line: Parser[((Int, Int), (Int, Int))] = pair ~ ("->" ~> pair) ^^ { case l ~ r => l -> r }
  case class Line(start: (Int, Int), end: (Int, Int)) {
    lazy val horizontalOrVertical: Boolean = (start, end) match {
      case ((x1, y1), (x2, y2)) if x1 == x2 || y1 == y2 => true
      case _                                            => false
    }
  }
  def fullLine: Parser[Line] = line ^^ ((Line.apply _).tupled)

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      val lines                     = inputs.linesIterator.map(parseAll(fullLine, _).get).filter(_.horizontalOrVertical)
      val hit: Map[(Int, Int), Int] = Map.empty
      lines.foldLeft(hit) { case (hit, line) =>
        val thisLine = line match {
          case Line((x1, y1), (x2, y2)) if x1 == x2 =>
            (Math.min(y1, y2) to Math.max(y1, y2)).map(x1 -> _)
          case Line((x1, y1), (x2, y2)) if y1 == y2 =>
            (Math.min(x1, x2) to Math.max(x1, x2)).map(_ -> y1)
        }
        thisLine.foldLeft(hit) { case (hit, (x, y)) => hit + ((x -> y) -> (hit.getOrElse(x -> y, 0) + 1)) }

      }.count { case (_, n) => n > 1 }.zio
    }
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      val lines                     = inputs.linesIterator.map(parseAll(fullLine, _).get)
      val hit: Map[(Int, Int), Int] = Map.empty
      lines.foldLeft(hit) { case (hit, line) =>
        val thisLine = line match {
          case Line((x1, y1), (x2, y2)) if x1 == x2 =>
            (Math.min(y1, y2) to Math.max(y1, y2)).map(x1 -> _)
          case Line((x1, y1), (x2, y2)) if y1 == y2 =>
            (Math.min(x1, x2) to Math.max(x1, x2)).map(_ -> y1)
          case Line((x1, y1), (x2, y2)) =>
            (x2 - x1, y2 - y1) match {
              case (dx, dy) if dx == dy =>
                (0 to Math.abs(dx)).map(dd => (Math.min(x1, x2) + dd) -> (Math.min(y1, y2) + dd))
              case (dx, dy) if dx == -dy =>
                (0 to Math.abs(dx)).map(dd => (Math.min(x1, x2) + dd) -> (Math.max(y1, y2) - dd))
            }

        }
        thisLine.foldLeft(hit) { case (hit, (x, y)) => hit + ((x -> y) -> (hit.getOrElse(x -> y, 0) + 1)) }
      }.count { case (_, n) => n > 1 }.zio
    }
  }.some

  def inputs = in

  lazy val in2 =
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2""".stripMargin

  lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
