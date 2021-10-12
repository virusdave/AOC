package codekata2015
package days

import zio.RIO

object Day16 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 16

  val lines: Parser[(Int, Map[String, Int])] = {
    val num: Parser[Int]            = "[0-9]+".r ^^ { _.toInt }
    val word: Parser[String]        = "[a-z]+".r
    val pair: Parser[(String, Int)] = word ~ (":" ~> num) ^^ { case w ~ n => (w, n) }
    ("Sue" ~> num <~ ":") ~ rep1sep(pair, ",") ^^ { case sue ~ l => sue -> l.toMap }
  }
  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      inputs
        .toSeq
        .map(parseAll(lines, _).get)
        .filter { case (_, sueAttr) =>
          sueAttr.forall { case (k, v) => target.get(k).contains(v) }
        }
        .zio
  }.some
  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] =
      inputs
        .toSeq
        .map(parseAll(lines, _).get)
        .filter { case (_, sueAttr) =>
          sueAttr.forall {
            case (k, v) =>
            if (Set("cats", "trees").contains(k)) {
              target.get(k).exists(_ < v)
            } else if (Set("pomeranians", "goldfish").contains(k)) {
              target.get(k).exists(_ > v)
            } else {
              target.get(k).contains(v)
            }
          }
        }
        .zio
  }.some

  lazy val target: Map[String, Int] = Map(
    "children"    -> 3,
    "cats"        -> 7,
    "samoyeds"    -> 2,
    "pomeranians" -> 3,
    "akitas"      -> 0,
    "vizslas"     -> 0,
    "goldfish"    -> 5,
    "trees"       -> 3,
    "cars"        -> 2,
    "perfumes"    -> 1
  )

  def inputs = in.linesIterator

  val in2 = ""
  val in3 = ""

  override def in: String = ""
}
