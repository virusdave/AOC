package codekata2016
package days

import zio.RIO

object Day02 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 2

  case class Button(name: String)(
      var up: String = name,
      var down: String = name,
      var left: String = name,
      var right: String = name,
  )

  def solve(start: String, buttons: Map[String, Button]) =
    inputs.linesIterator.toList.scanLeft(start) { case (start, line) =>
      line.stripMargin.foldLeft(start) { case (pos, dir) =>
        val b = buttons(pos)
        dir match {
          case 'L' => b.left
          case 'R' => b.right
          case 'U' => b.up
          case 'D' => b.down
        }
      }
    }.drop(1).mkString

  override def part1: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = {
        val buttons: Map[String, Button] = Seq(
          Button("1")(right = "2", down = "4"),
          Button("2")(left = "1", right = "3", down = "5"),
          Button("3")(left = "2", down = "6"),
          Button("4")("1", "7", right = "5"),
          Button("5")("2", "8", "4", "6"),
          Button("6")("3", "9", "5"),
          Button("7")("4", "7", "7", "8"),
          Button("8")("5", "8", "7", "9"),
          Button("9")("6", "9", "8", "9"),
        ).map(b => b.name -> b).toMap
        solve("5", buttons)
      }.zio
    }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      val buttons: Map[String, Button] = Seq(
        Button("1")(down = "3"),
        Button("2")(down = "6", right = "3"),
        Button("3")("1", "7", "2", "4"),
        Button("4")(left = "3", down = "8"),
        Button("5")(right = "6"),
        Button("6")("2", "A", "5", "7"),
        Button("7")("3", "B", "6", "8"),
        Button("8")("4", "C", "7", "9"),
        Button("9")(left = "8"),
        Button("A")("6", right = "B"),
        Button("B")("7", "D", "A", "C"),
        Button("C")("8", left = "B"),
        Button("D")("B"),
      ).map(b => b.name -> b).toMap
      solve("5", buttons)
    }.zio
  }.some

  def inputs = in2

  val in2 = ""
  val in3 = ""

  override def in: String =
    """""".stripMargin
}
