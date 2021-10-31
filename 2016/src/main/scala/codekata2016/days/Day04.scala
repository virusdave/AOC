package codekata2016
package days

import zio.RIO

object Day04 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 4

  case class Room(words: List[String], sector: Int, checksum: String) {
    def isValid: Boolean = {
      val ordered = checksum.toList
      words.flatMap(_.toList).groupBy(identity).view.mapValues(_.size).toList.sortBy { case (l, n) => (-n, l) }.map(
        _._1).startsWith(ordered)
    }
    def decode: String = {
      val shift = sector % 26
      words.map(_.toSeq.map(c => (((c - 'a') + shift) % 26 + 'a').toChar).mkString).mkString(" ")
    }
  }
  val room: Parser[List[String]] = rep1sep("[a-z]+".r, "-")
  val sector: Parser[Int]        = "[1-9][0-9]*".r ^^ (_.toInt)
  val checksum: Parser[String]   = "[" ~> "[a-z]+".r <~ "]"
  val line: Parser[Room]         = room ~ ("-" ~> sector) ~ checksum ^^ { case r ~ s ~ c => Room(r, s, c) }

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      inputs.linesIterator.toList.map(parseAll(line, _).get).filter(_.isValid).map(_.sector).sum
        .zio
  }.some
  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      inputs.linesIterator.toList.map(parseAll(line, _).get).filter(_.isValid).map(r => r.decode -> r.sector)
        .filter(_._1 == "northpole object storage")
        .zio
  }.some

  def inputs = in

  val in2 =
    """aaaaa-bbb-z-y-x-123[abxyz]
      |a-b-c-d-e-f-g-h-987[abcde]
      |not-a-real-room-404[oarel]
      |totally-real-room-200[decoy]""".stripMargin
  val in3 = ""

  override def in: String =
    "".stripMargin
}
