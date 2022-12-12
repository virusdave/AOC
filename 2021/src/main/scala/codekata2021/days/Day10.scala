package codekata2021
package days

object Day10 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 10

  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)

  private abstract class Open(val score: Int, val open: Char, val close: Char, val acScore: BigInt)
  private case object Paren extends Open(3, '(', ')', 1)
  private case object Bracket extends Open(57, '[', ']', 2)
  private case object Curly extends Open(1197, '{', '}', 3)
  private case object Angle extends Open(25137, '<', '>', 4)

  override def part1: Option[Part] = PuzzlePart(
    inputs.flatMap(_.toSeq.foldLeft((List.empty[Open], Option.empty[Int])) {
      case (res@(_, Some(_)), _) => res
      case ((stack, _), c) => c match {
        case Paren.open => (Paren :: stack) -> None
        case Bracket.open => (Bracket :: stack) -> None
        case Curly.open => (Curly :: stack) -> None
        case Angle.open => (Angle :: stack) -> None
        case _ => stack match {
          case h :: t =>
            if (c == h.close) {
              t -> None
            } else {
              Nil -> Seq(Paren, Bracket, Curly, Angle).filter(_.close == c).map(_.score).headOption
            }
          case _ => Nil -> None
        }
      }
    }._2).sum.zio).some

  override def part2: Option[Part] = PuzzlePart({
    inputs.map(_.toSeq.foldLeft((List.empty[Open], false)) {
      case (res@(_, true), _) => res
      case ((stack, false), c) => c match {
        case Paren.open => (Paren :: stack) -> false
        case Bracket.open => (Bracket :: stack) -> false
        case Curly.open => (Curly :: stack) -> false
        case Angle.open => (Angle :: stack) -> false
        case _ => stack match {
          case h :: t =>
            if (c == h.close) {
              t -> false
            } else {
              Nil -> true
            }
          case _ => ???
        }
      }
    }._1.foldLeft(0.big) { case (score, c) => score * 5 + c.acScore }
    ).sorted.dropWhile(_ == 0).|>(x => x.slice(x.size / 2, x.size / 2 + 1)).head
      .zio
  }).some

  private def inputs = in2.linesIterator.toSeq

  private lazy val in2 =
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin

  private lazy val in3 =
    """(((((((((()))"""

  override def in: String =
    """""".stripMargin
}
