package codekata2022
package days

// TODO(Dave): Might want to add a "Parse these double-linebreak-delimited sections with the following parsers" method
// like you recently added for "parse each line with this parser".  Could potentially help with newline/whitespace
// errors in the parser otherwise, which can be time consuming to debug.
object Day05 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 5
  override def skipWhitespace: Boolean = false

  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)

  // Will likely be missing "empty" slots at the end of each line
  private val crates: Parser[Seq[Seq[Option[String]]]] = {
    val none = "   " ^^^ None
    val some = "[" ~> "[A-Z]".r <~ "]" ^^ (_.some)
    // Crates are repeated lines containing [repeated, space separated, (either a crate ID or a blank)]
    rep1sep(rep1sep(some | none, " "), "\n")
  }

  private val craneMove = rep1sep(
    //                            Avoid repeated "- 1" in 0-based indexing below
    ("move " ~> num) ~ (" from " ~> num ^^ (_ - 1)) ~ (" to " ~> num ^^ (_ - 1)), "\n")

  private val parsedInitialCrates ~ numericCrateIndices ~ instructions = {
    val numericLine = rep1sep(" " ~> num <~ " ".? /* pasted trailing space optional */ , " ")
    parseAll(crates ~ ("\n" ~> numericLine) ~ ("\n\n" ~> craneMove), inputs).get
  }

  // Due to copy/pasting, the trailing whitespace may have gotten chopped :(
  // Fill in any missing "empty" crates here just in case.
  private val initialCrates = parsedInitialCrates.map { line =>
    line ++ Seq.fill(Math.max(0, numericCrateIndices.size - line.size))(None)
  }.transpose.toIndexedSeq.map(_.flatten)

  override def part1: Option[Part] = PuzzlePart({
    instructions.foldLeft(initialCrates) { case crates -> move ~ from ~ to =>
      val (lifted, newFrom) = crates(from).splitAt(move)
      val newTo = lifted.reverse ++ crates(to)
      crates.updated(from, newFrom).updated(to, newTo)
    }.map(_.head).mkString
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    instructions.foldLeft(initialCrates) { case crates -> move ~ from ~ to =>
      val (lifted, newFrom) = crates(from).splitAt(move)
      val newTo = lifted ++ crates(to) // Only difference from part1 is ".reversed" here!
      crates.updated(from, newFrom).updated(to, newTo)
    }.map(_.head).mkString
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""

  private lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
