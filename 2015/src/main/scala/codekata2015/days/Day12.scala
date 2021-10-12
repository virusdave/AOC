package codekata2015
package days

import io.circe._
import zio.RIO

object Day12 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 12

  trait NumericSummer extends Json.Folder[Long] {
    override def onNull: Long = 0
    override def onBoolean(value: Boolean): Long = 0
    override def onNumber(value: JsonNumber): Long =
      value.toLong.getOrElse(0)
    override def onString(value: String): Long = 0
    override def onArray(value: Vector[Json]): Long =
      value.foldLeft(0L) { case (b, json) => b + json.foldWith(this) }
    override def onObject(value: JsonObject): Long =
      value.toList.foldLeft(0L) { case (b, (_, json)) => b + json.foldWith(this) }
  }

  override def part1: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = {
        val numericSumming: Json.Folder[Long] = new NumericSummer {}
        inputs.map(
          (parser.parse _).andThen(
            _.getOrElse(Json.Null).foldWith(numericSumming)
          )
        )
      }.zio
    }.some
  override def part2: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = {
        val numericSumming: Json.Folder[Long] = new NumericSummer {
          override def onObject(value: JsonObject): Long = {
            if (value.values.flatMap(_.asString).toSeq.contains("red")) 0
            else super.onObject(value)
          }
        }
        inputs.map(
          (parser.parse _).andThen(
            _.getOrElse(Json.Null).foldWith(numericSumming)
          )
        )
      }.zio
    }.some

  def inputs = in3

  val in2 = Seq(
    "[[[3]]]",
    """{"a":{"b":4},"c":-1}"""
  )
  val in3 = Seq(
    """{"a":[-1,1]}""",
    """[-1,{"a":1}]"""
  )

  override def in: String = ""
}
