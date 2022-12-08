package codekata2022
package days

import common.InRegexParserSyntax

object Day08 extends ParserPuzzle with InRegexParserSyntax {
  override type PuzzleOut = Any
  override def dayNum: Int = 8

  private val num: Parser[Int] = "[0-9]".r ^^ (_.toInt)
  private val heights = inputs.parseLinesBy(rep(num)).map(_.toIndexedSeq).toIndexedSeq

  override def part1: Option[Part] = PuzzlePart({
    def visibleFromLeft(in: Grid[Int]): Grid[Boolean] = {
      // Map each row in the grid into a sequence of booleans indicating whether the tree in that
      // position can be seen from the left boundary (that is, is it higher than the previously
      // highest-seen height)?  This produces a grid of booleans same size as the forrest.
      in.map(_.foldLeft(IndexedSeq.empty[Boolean] -> -1) { case ((treeVisible, maxHeight), curHeight) =>
        (treeVisible :+ (curHeight > maxHeight)) -> Math.max(curHeight, maxHeight)
      }._1)
    }

    // We'll use the above helper repeatedly, rotating the forrest so that each side is "the left",
    // then rotating the answer grid back afterwards.
    // TODO(Dave): This seems kinda nasty.  Many puzzles have a similar "iterate across grids from
    // outside, or cardinally from grid points (as in part 2)" component, so perhaps make nicer
    // helpers for this kind of problem.
    val visibles: IndexedSeq[Grid[Boolean]] = {
      val rotatecw: Grid[Int] => Grid[Int] = _.rotate90cw
      val rotateccw: Grid[Boolean] => Grid[Boolean] = _.rotate90ccw
      def repeat[A](n: Int, f: A => A): A => A = Function.chain(Seq.fill(n)(f))

      (0 to 3).map { n => repeat(n, rotateccw)(visibleFromLeft(repeat(n, rotatecw)(heights))) }
    }

    // Finally, with all the answer grids (all aligned), we can see any tree that we can see from
    // any direction, so or them all together and count Trues.
    visibles.head.mapGridWithLocation { case (_, (x, y)) =>
      (0 to 3).flatMap(visibles(_).get(x, y)).reduce(_ || _)
    }.flatten.count(identity)
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val (sx, sy) = heights.bounds
    val zeros = LazyList.continually(0)

    // Given a position and a set of deltas to that position (which correspond to walking
    // in one of the cardinal directions from that point), count how far we can walk until
    // we walk off the edge or we find a position not lower than our current height
    def viewingDistance(x: Int, y: Int, deltas: Seq[(Int, Int)]): Int = {
      deltas.foldLeft(0 -> true) { case ((dist, continue), (dx, dy)) =>
        if (!continue) dist -> continue
        else (dist + 1) -> (heights(y + dy)(x + dx) < heights(y)(x))
      }._1
    }

    // For a given position, take the product of the "viewing distance" along each cardinal direction
    def scenicScore(x: Int, y: Int) = {
      // TODO(Dave): This part (the "iterate out cardinally from a position") is pretty disgusting
      // to have to make up on the fly...  Maybe create a `cardinalDeltasFrom(x, y): Seq[Seq[(Int, Int)]]`
      // or something.  Hmmm...
      Seq(
        (-1 to -x by -1) zip zeros,   // -x direction
        (1 until (sx - x)) zip zeros, // +x direction
        zeros zip (-1 to -y by -1),   // -y direction
        zeros zip (1 until (sy - y)), // +y direction
      ).map(viewingDistance(x, y, _)).product
    }

    heights.mapGridWithLocation { case (_, (x, y)) => scenicScore(x, y) }.flatten.max
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """30373
25512
65332
33549
35390"""

  private lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
