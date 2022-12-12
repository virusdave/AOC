package codekata2021
package days

object Day13 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 13

  private case class Dot(x: Int, y: Int)

  private sealed trait Fold
  private case class FoldX(x: Int) extends Fold
  private case class FoldY(y: Int) extends Fold

  private val num: Parser[Int]     = "[0-9]+".r ^^ (_.toInt)
  private val dot: Parser[Dot]     = num ~ ("," ~> num) ^^ { case l ~ r => Dot(l, r) }
  private val foldx: Parser[FoldX] = "fold along x=" ~> num ^^ (FoldX)
  private val foldy: Parser[FoldY] = "fold along y=" ~> num ^^ (FoldY)

  private def foldDots(dots: Set[Dot], fold: Fold): Set[Dot] = fold match {
    case FoldX(x) => dots.map { case Dot(xx, yy) => Dot(if (xx <= x) xx else 2 * x - xx, yy) }
    case FoldY(y) => dots.map { case Dot(xx, yy) => Dot(xx, if (yy <= y) yy else 2 * y - yy) }
  }

  override def part1: Option[Part] = PuzzlePart({
    val points = inputs._1.map(parseAll(dot, _).get)
    val folds = inputs._2.map(parseAll(foldx | foldy, _).get)
    foldDots(points.toSet, folds(0)).size
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val points = inputs._1.map(parseAll(dot, _).get)
    val folds = inputs._2.map(parseAll(foldx | foldy, _).get)
    val left = folds.foldLeft(points.toSet) { case (points, fold) => foldDots(points, fold) }
    val (mx, my) = left.toSeq.map { case Dot(x, y) => x -> y }.unzip.diag(_._1.max -> _._2.max)
    Grid.fill2d(mx + 1, my + 1)(' ')
      .mapGridWithLocation { case (c, (x, y)) => if (left(Dot(x, y))) '#' else c }
      .show(identity)
  }.zio).some

  private def inputs = in2.split("\n\n").diag(_(0).linesIterator.toSeq -> _(1).linesIterator.toSeq)

  private lazy val in2 =
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5""".stripMargin

  override def in: String =
    """""".stripMargin
}
