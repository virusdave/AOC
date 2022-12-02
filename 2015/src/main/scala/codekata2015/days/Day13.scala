package codekata2015
package days

import zio.RIO

object Day13 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 13

  def pName: Parser[String] = "[A-Z][a-z]*".r
  def pNum: Parser[Int]     = "[0-9]+".r ^^ { _.toInt }
  def pUpDown: Parser[Int]  = "gain|lose".r ^^ { case "gain" => 1; case "lose" => -1 }
  def line: Parser[((String, String), Int)] =
    (pName <~ "would") ~ pUpDown ~ (pNum <~ "happiness units by sitting next to") ~ (pName <~ ".") ^^ {
      case l ~ ud ~ num ~ r => ((l, r), ud * num)
    }

  lazy val parsed = inputs.parseLinesBy(line)
  lazy val prefs  = parsed.toMap
  lazy val names  = parsed.map(_._1._1).distinct

  override def part1: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] =
        names.permutations
          .map(p =>
            p -> (
              (p :+ p.head).sliding(2).map {
                case Seq(l, r) => prefs((l, r))
              }.sum +
                (p :+ p.head).reverse.sliding(2).map {
                  case Seq(l, r) => prefs((l, r))
                }.sum
              )
          )
          .maxBy(_._2)
          .zio
    }.some
  override def part2: Option[Part] =
    new Part {
      val you = "[SELF]"
      lazy val prefs2  = prefs ++ names.flatMap(n => Seq((n, you) -> 0, (you, n) -> 0)).toMap
      lazy val names2  = names :+ you

      override def solution: RIO[Any, Any] =
        names2.permutations
          .map(p =>
            p -> (
              (p :+ p.head).sliding(2).map {
                case Seq(l, r) => prefs2((l, r))
              }.sum +
                (p :+ p.head).reverse.sliding(2).map {
                  case Seq(l, r) => prefs2((l, r))
                }.sum
              )
          )
          .maxBy(_._2)
          .zio
    }.some

  def inputs = in

  val in2 = """Alice would gain 54 happiness units by sitting next to Bob.
              |Alice would lose 79 happiness units by sitting next to Carol.
              |Alice would lose 2 happiness units by sitting next to David.
              |Bob would gain 83 happiness units by sitting next to Alice.
              |Bob would lose 7 happiness units by sitting next to Carol.
              |Bob would lose 63 happiness units by sitting next to David.
              |Carol would lose 62 happiness units by sitting next to Alice.
              |Carol would gain 60 happiness units by sitting next to Bob.
              |Carol would gain 55 happiness units by sitting next to David.
              |David would gain 46 happiness units by sitting next to Alice.
              |David would lose 7 happiness units by sitting next to Bob.
              |David would gain 41 happiness units by sitting next to Carol.""".stripMargin
  val in3 = ""

  override def in: String = "".stripMargin
}
