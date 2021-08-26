package codekata2015
package days

object Day05 extends Puzzle {
  override type A = Any

  override def dayNum: Int = 5

  override def part1: Option[Part] = new Part {
    override def solution = inputs.count(l =>
      l.toIndexedSeq.count("aeiou".toSet.contains) >= 3 &&
        l.toIndexedSeq.sliding(2).exists { case IndexedSeq(a, b) => a == b } &&
        !Seq("ab", "cd", "pq", "xy").exists(l.contains))
      .zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution = inputs.count(l =>
      l.toIndexedSeq.sliding(3).exists { case IndexedSeq(a, _, c) => a == c } &&
        l.toIndexedSeq.sliding(2).exists(pair => l.split(pair.mkString(""), 3).length == 3)
    ).zio
  }.some

  def inputs = in.linesIterator

  val in2 = Seq(
    "ugknbfddgicrmopn",
    "aaa",
    "jchzalrnumimnmhp",
    "haegwjzuvuyypxyu",
    "dvszwmarrgswjxmb",
  ).mkString("\n")
  val in3 = Seq(
    "qjhvhtzxzqqjkmpb",
    "xxyxx",
    "uurcxstgmygtbstg",
    "ieodomkazucvgmuy",
  ).mkString("\n")

  override def in: String = "".stripMargin
}