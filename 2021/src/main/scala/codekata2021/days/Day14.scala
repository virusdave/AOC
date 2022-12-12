package codekata2021
package days

object Day14 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 14

  private val char: Parser[String]                           = "[A-Z]".r
  private val replacements: Parser[(String, String, String)] = char ~ char ~ ("->" ~> char) ^^ { case l ~ m ~ r => (l, m, r) }

  override def part1: Option[Part] = PuzzlePart({
    val result = inputs match {
      case l :: _ :: ts =>
        val start = parseAll(rep1(char), l).get
        val reps = ts.map(parseAll(replacements, _).get).map { case (l, m, r) => (l, m) -> r }.toMap
        (1 to 10).foldLeft(start) { case (template, _) =>
          val x = template.head :: template.sliding(2).toList.flatMap { t => List(reps(t(0) -> t(1)), t(1)) }
          x
        }
      case _ => ???
    }
    val sorted = result.groupBy(identity).map { case l -> r => l -> r.size }
      .toSeq.sortBy(_._2)
    sorted.last._2 - sorted.head._2
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val result = inputs match {
      case l :: _ :: ts =>
        val start = parseAll(rep1(char), l).get.mkString
        val reps = ts.map(parseAll(replacements, _).get).map { case (l, m, r) => s"$l$m" -> r }.toMap

        val counts = start.sliding(2).toSeq.groupBy(identity).map { case l -> r => l -> r.size.big }

        (1 to 40).foldLeft(counts) { case (counts, _) =>
          counts.toSeq.flatMap { case (p, n) =>
            Seq(
              s"${p.toSeq.apply(0)}${reps(p)}" -> n,
              s"${reps(p)}${p.toSeq.apply(1)}" -> n,
            )
          }.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap
        }
      case _ => ???
    }
    val sorted =
      result.toSeq.map { case l -> r => l.head -> r }.groupBy(_._1).view.mapValues(_.map(_._2).sum).toSeq.sortBy(_._2)
    sorted.last._2 - sorted.head._2 + 1 // Might be off-by-one error here :(
  }.zio).some

  private def inputs = in2.linesIterator.toList

  private lazy val in2 =
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C""".stripMargin

  override def in: String =
    """""".stripMargin
}
