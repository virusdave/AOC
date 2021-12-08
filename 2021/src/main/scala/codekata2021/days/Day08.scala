package codekata2021
package days

import zio.RIO

object Day08 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 8

  val num: Parser[Int]                               = "[0-9]+".r ^^ (_.toInt)
  val segment: Parser[Char]                          = "[a-g]".r ^^ (_.head)
  val segments: Parser[Seq[Char]]                    = "[a-g]+".r ^^ (_.toSeq)
  val patterns: Parser[Seq[Seq[Char]]]               = repN(10, segments)
  val targets: Parser[Seq[Seq[Char]]]                = repN(4, segments)
  val line: Parser[(Seq[Seq[Char]], Seq[Seq[Char]])] = patterns ~ ("|" ~> targets) ^^ { case l ~ r => l -> r }

  val digitToSegment: Map[Int, Seq[Char]] = Map(
    0 -> Seq('a', 'b', 'c', 'e', 'f', 'g'),
    1 -> Seq('c', 'f'),
    2 -> Seq('a', 'c', 'd', 'e', 'g'),
    3 -> Seq('a', 'c', 'd', 'f', 'g'),
    4 -> Seq('b', 'c', 'd', 'f'),
    5 -> Seq('a', 'b', 'd', 'f', 'g'),
    6 -> Seq('a', 'b', 'd', 'e', 'f', 'g'),
    7 -> Seq('a', 'c', 'f'),
    8 -> ('a' to 'g'),
    9 -> Seq('a', 'b', 'c', 'd', 'f', 'g'),
  )
  val segmentSetToDigit: Map[Set[Char], Int] = digitToSegment.map { case s -> ds => ds.toSet -> s }
  val segmentSets: Seq[Set[Char]]            = segmentSetToDigit.keys.toSeq
  val segmentToDigit: Map[Char, Seq[Int]] = digitToSegment.toSeq.flatMap { case d -> segs =>
    segs.map(_ -> d)
  }.groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  val segmentToDigitCount: Map[Char, Int] = segmentToDigit.map { case k -> v => k -> v.size }
  val digitToSegmentDigitCountSet: Map[Int, Seq[Int]] =
    digitToSegment.view.mapValues(ss => ss.map(segmentToDigitCount)).toMap

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      val i    = inputs.map(parseAll(line, _).get)
      val find = Seq(1, 4, 7, 8).map(d => d -> digitToSegment(d).size).map(_._2).toSet
      i.map(_._2.map(_.size).count(find.contains)).sum
    }.zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[Any, Any] = {
      val i = inputs.map(parseAll(line, _).get)

      i.map { case patterns -> targets =>
        val segmentsInPattern = patterns.map(_.toSet)

        val segCountsInPattern: Map[Char, Int] =
          ('a' to 'g').map(s => s -> segmentsInPattern.count(_.contains(s))).toMap
        val maybeSegMapping: Map[Char /*pattern*/, Char /*segment*/ ] =
          segCountsInPattern.map { case c -> n =>
            c -> (segmentToDigitCount.filter(_._2 == n).keys.toSeq match {
              case Seq(s) => s
              case _      =>
                // This is either conflicting 7-rep or 8-rep segments.
                // Does this appear in the number "4", which contains 4 segments?  If so then it is
                // 'd' if 7-rep or 'c' if 8-rep.  If it's not in the number "4", then it's likewise 'g' or 'a'
                // respectively.  Determined by staring at the segment arrangements for way too long.
                if (patterns.filter(_.size == 4).flatten.contains(c)) {
                  if (n == 7) 'd' else 'c'
                } else {
                  if (n == 7) 'g' else 'a'
                }
            })
          }

        targets.map(_.map(maybeSegMapping).toSet.|>(segmentSetToDigit)).mkString.toInt
      }.sum
    }.zio
  }.some

  def inputs = in3.linesIterator.toSeq

  lazy val in2 =
    """acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"""

  lazy val in3 =
    """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""".stripMargin

  override def in: String =
    """""".stripMargin
}
