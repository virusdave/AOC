package codekata2020.day11

import codekata2020._
import scala.annotation.tailrec
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  private val inputs = in2.linesIterator.toIndexedSeq.map(_.toIndexedSeq)

  type Board = IndexedSeq[IndexedSeq[Char]]

  object Part1 {
    val nextGeneration: Map[Char, Int => Char] = Map(
      'L' -> {(count: Int) => if (count == 0) '#' else 'L'},
      '#' -> {(count: Int) => if (count >= 4) 'L' else '#'}
    )

    def countOccupiedNear(x: Int, y: Int, board: Board): Int =
      (for {
        yoff <- -1 to 1
        xoff <- -1 to 1 if xoff != 0 || yoff != 0
        c <- board.get(x + xoff, y + yoff)
      } yield c).count(_ == '#')

    def iterate(board: Board): Board =
      board.mapGridWithLocation[Char, Char] {
        case ('.', _) => '.'
        case (prevState, (x, y)) => nextGeneration(prevState)(countOccupiedNear(x, y, board))
      }

    def solution = {
      @tailrec def go(board: Board): Board = {
        val next = iterate(board)
        if (next == board) board
        else go(next)
      }
      val board = go(inputs)
      board.flatten.count(_ == '#')
    }.zio
  }

  object Part2 {
    val nextGeneration = Map(
      'L' -> {(count: Int) => if (count == 0) '#' else 'L'},
      '#' -> {(count: Int) => if (count >= 5) 'L' else '#'}
    )

    def countOccupiedSeenFrom(x: Int, y: Int, board: Board): Int =
      (for {
        xoff <- -1 to 1
        yoff <- -1 to 1 if xoff != 0 || yoff != 0
        c <- {  // step in the direction (xoff,yoff) "points" trying to find a seat
          @tailrec def stepOutwards(n: Int): Option[Char] = {
            val at = board.get(x + n * xoff, y + n * yoff)
            if (at.contains('.')) stepOutwards(n + 1)  // No seat, step further
            else at // Either found a seat or walked off the edge of the board
          }
          stepOutwards(1)
        }
      } yield c).count(_ == '#')

    def iterate(board: Board): Board =
      board.mapGridWithLocation[Char, Char] {
        case ('.', _) => '.'
        case (prevState, (x, y)) => nextGeneration(prevState)(countOccupiedSeenFrom(x, y, board))
      }

    def solution = {
      @tailrec def go(board: Board): Board = {
        val next = iterate(board)
        if (next == board) board
        else go(next)
      }
      val board = go(inputs)
      board.flatten.count(_ == '#')
    }.zio
  }

  private lazy val in2 =
    """L.LL.LL.LL
      |LLLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLLL
      |L.LLLLLL.L
      |L.LLLLL.LL""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
