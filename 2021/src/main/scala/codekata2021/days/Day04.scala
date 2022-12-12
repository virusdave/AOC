package codekata2021
package days

object Day04 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 4

  private case class Board(nums: Map[Int, (Set[(Int, Int)], Boolean)]) {
    def add(num: Int): Board = {
      // did this board win yet?
      val prior = nums.get(num)
      prior match {
        case None            => this // We didn't have this number on the board
        case Some((_, true)) => this // Already saw this number
        case Some((poss, false)) =>
          val allPriorPositionsHit = nums.toSeq.filter(_._2._2).flatMap(_._2._1).toSet
          val allNewPositionsHit   = allPriorPositionsHit ++ poss
          val newNums              = nums + (num -> (poss -> true))
          val nowWon: Boolean =
            ((0 to 4).map { x => (0 to 4).map(y => x -> y) } ++
              (0 to 4).map { y => (0 to 4).map(x => x -> y) }).exists(line => line.forall(allNewPositionsHit))
          val newWon =
            if (nowWon) {
              newNums.filterNot(_._2._2).toSeq.flatMap { case (v, (poss, _)) => Seq.fill(poss.size)(v) }.sum.some
            } else None
          new Board(newNums) {
            override def won(): Option[Int] = newWon
          }
      }
    }
    def won(): Option[Int] = None
  }
  private val num: Parser[Int]         = "[0-9]+".r ^^ (_.toInt)
  private val drawn: Parser[List[Int]] = rep1sep(num, ",")
  private val board: Parser[Board] = repN(5, repN(5, num)) ^^ { ns =>
    Board(ns.map(_.zipWithIndex).zipWithIndex.flatMap { case (ls, y) =>
      ls.map { case (n, x) => (n -> ((x, y))) }
    }.groupBy(_._1).view.mapValues { pos => pos.map(_._2).toSet -> false }.toMap)
  }
  private val lines: Parser[(List[Int], List[Board])] = drawn ~ rep1(board) ^^ (_.toTuple)

  override def part1: Option[Part] = PuzzlePart({
    val (nums, boards) = parseAll(lines, inputs).get
    nums.foldLeft(boards -> Option.empty[Int]) { case ((boards, result), num) => result match {
      case Some(_) => boards -> result
      case None =>
        val newBoards = boards.map(_.add(num))
        newBoards -> newBoards.flatMap(_.won()).headOption.map(_ * num)
    }
    }._2.zio
  }).some

  override def part2: Option[Part] = PuzzlePart({
    val (nums, boards) = parseAll(lines, inputs).get
    nums.foldLeft(boards -> Option.empty[Int]) { case ((boards, result), num) => result match {
      case Some(_) => boards -> result
      case None =>
        val newBoards = boards.map(_.add(num))
        val prevWon = boards.map(_.won())
        val nowWon = newBoards.map(_.won())
        val newest = prevWon.zip(nowWon).flatMap {
          case (None, newest@Some(_)) => newest
          case _ => None
        }
        newBoards -> (if (nowWon.flatten.size == boards.size) newest.headOption.map(_ * num) else None)
    }
    }._2.zio
  }).some

  private def inputs = in2

  private lazy val in2 =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
      |
      |22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |
      | 3 15  0  2 22
      | 9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      | 2  0 12  3  7""".stripMargin

  override def in: String =
    """""".stripMargin
}
