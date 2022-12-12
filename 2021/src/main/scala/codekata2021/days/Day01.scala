package codekata2021
package days

object Day01 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 1

  override def part1: Option[Part] = PuzzlePart {
    inputs.zip(inputs.drop(1)).count { case (a, b) => a < b }.zio
  }.some

  override def part2: Option[Part] = PuzzlePart {
    inputs.sliding(3).map(_.sum).toSeq.|>(x => x.zip(x.drop(1))).count { case (a, b) => a < b }.zio
  }.some

  private def inputs = in2.linesIterator.map(_.big)

  private lazy val in2 =
    """199
      |200
      |208
      |210
      |200
      |207
      |240
      |269
      |260
      |263""".stripMargin

  override def in: String =
    """""".stripMargin
}
