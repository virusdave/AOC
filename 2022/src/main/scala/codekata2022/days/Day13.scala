package codekata2022
package days

object Day13 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 13

  private sealed trait Entry { def inList: L = L(List(this))}
  private object Entry {
    private val seq = Ordering.Implicits.seqOrdering[Seq, Entry]
    implicit lazy val ordered: Ordering[Entry] = {
      case (V(l), V(r)) => l - r
      case (l: V, L(r)) => seq.compare(List(l), r)
      case (L(l), r: V) => seq.compare(l, List(r))
      case (L(l), L(r)) => seq.compare(l, r)
    }
  }
  private def compare[A: Ordering](l: A, r: A) = Ordering[A].compare(l, r)

  private case class V(v: Int) extends Entry
  private val v: Parser[Entry] = "[0-9]+".r ^^ (_.toInt) ^^ V

  private case class L(l: List[Entry]) extends Entry
  private lazy val l: Parser[Entry] = "[" ~> repsep(v | l, ",") <~ "]" ^^ L

  override def part1: Option[Part] = PuzzlePart({
    inputs.splitAtDoubleLinebreaksAndParseChunkBy(l ~ l)
      .zipWithIndex
      .flatMap { case l ~ r -> idx =>
        (idx + 1).some.filter(_ => compare(l, r) < 0)
      }.sum
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val markers = Seq(V(2).inList.inList, V(6).inList.inList)
    val sorted = (inputs.parseBy(rep(l)) ++ markers).sorted
    markers.map(m => sorted.indexWhere(_ == m)).map(_ + 1).product
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""

  override def in: String =
    """""".stripMargin
}
