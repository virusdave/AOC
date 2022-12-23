package codekata2022
package days

import breeze.math.Complex
import scala.annotation.tailrec

object Day22 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 22
  override def skipWhitespace: Boolean = false

  def initialOrientation = Complex.one

  sealed abstract class BoardSquare(c: Char) { override def toString: String = s"$c" }
  case object Free extends BoardSquare('.')
  case object Wall extends BoardSquare('#')
  case object Wrap extends BoardSquare(' ')

  private val boardSquare = "." ^^^ Free | "#" ^^^ Wall | " " ^^^ Wrap
  private val fullBoard = repsep(rep1(boardSquare) ^^ (_.toIndexedSeq), "\n") ^^ (_.toIndexedSeq)

  sealed abstract class Action(val turn: Complex)
  case object Left extends Action(-Complex.i)  // negated since our grid goes positive "downwards"
  case object Right extends Action(Complex.i)
  case class Walk(dist: Int) extends Action(Complex.one)

  private val num: Parser[Int]       = "[0-9]+".r ^^ (_.toInt)
  private val action: Parser[Action] = "L" ^^^ Left | "R" ^^^ Right | num ^^ Walk

  private val parser: Parser[Grid[BoardSquare] ~ List[Action]] = (fullBoard <~ "\n\n") ~ rep(action)

  private def scoreFacing(facing: Complex): Int =
    if (facing == Complex.one) 0
    else if (facing == Complex.i) 1
    else if (facing == -Complex.one) 2
    else if (facing == -Complex.i) 3
    else ???

  private def fillMissing[A](fillWith: A, in: Grid[A]): Grid[A] = {
    val maxX = in.map(_.size).max
    in.map { l => l ++ IndexedSeq.fill(maxX - l.size)(fillWith) }
  }
  val (board, actions) = inputs.parseBy(parser ^^ { case l~r => fillMissing(Wrap, l) -> r})
  val startPos = {
    val frees = board.mapGridWithLocation {
      case (Free, pos) => pos.some
      case _ => None
    }.flatten.flatten
    val minY = frees.map(_._2).min
    val minX = frees.filter(_._2 == minY).map(_._1).min
    Complex(minX, minY)
  }

  val (boundsX, boundsY) = board.bounds

  override def part1: Option[Part] = PuzzlePart({
    @tailrec def tryMoveFromTo(from: Complex, facing: Complex, stride: Int): Complex = {
      val wrapped = {
        val Complex(x, y) = from + facing * stride
        Complex((x + boundsX) % boundsX, (y + boundsY) % boundsY)
      }
      board.get(wrapped.real.toInt, wrapped.imag.toInt).get match {
        case Wall => from
        case Free => wrapped
        case Wrap => tryMoveFromTo(from, facing, stride + 1)
      }
    }

    val (endPos, orient) = actions.foldLeft(startPos -> initialOrientation) { case (pos -> orient, action) => action match {
      case Left | Right => pos -> (action.turn * orient)
      case Walk(dist) => (1 to dist).foldLeft(pos) { case p -> _ => tryMoveFromTo(p, orient, 1) } -> orient
    }}

    (1000 * (endPos.imag + 1) + 4*(endPos.real+1) + scoreFacing(orient)).toInt
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val (faceX, faceY) = {
      if (boundsX % 3 == 0 && boundsY % 4 == 0) (3,4)
      else if (boundsX % 4 == 0 && boundsY % 3 == 0) (4,3)
      else ???
    }
    val faces = {
      var num = 0
      Grid.fill2d(faceX,faceY)(false).mapGridWithLocation { case (_, (x,y)) =>
        board.get(x * boundsX/faceX, y * boundsY/faceY) match {
          case Some(Wrap) => 0
          case Some(_) => num += 1; num
          case None => ???
        }
      }
    }
    s"${faceX -> faceY}\n${faces.show(identity)}".debug

    val wrapper/*: Map[(Complex, Complex), (Complex, Complex)]*/ = {
      Seq(
        ((1,0), (0,-1)) -> ((0,3), (1,0)),  // 1 -> 6
        ((1,0), (-1,0)) -> ((0,2), (1,0)),  // 1 -> 4
        ((2,0), (0,1)) -> ((1,1), (-1, 0)),  // 2 -> 3
        ((2,0), (1,0)) -> ((1,2), (-1, 0)),  // 2 -> 5
        ((2,0), (0,-1)) -> ((1,2), (-1, 0)),  // 2 -> 6
        ((1,1), (1,0)) -> ((2,0) -> (0, -1)),
        //((2,0), (0,1)) -> ((1,1), (-1,0)),
        ((1,1), (-1, 0)) -> ((2,0), (0, 1)),

      )
    }
//    def tryMoveFromTo(from: Complex, facing: Complex): Option[(Complex, Complex)] = {
//      // TODO(Dave): This needs to be tweaked
//      val wrapped -> newFacing = {
//        val Complex(x, y) = from + facing
//        val wrapped = Complex((x + boundsX) % boundsX, (y + boundsY) % boundsY)
//        wrapper.getOrElse(wrapped -> facing, wrapped -> facing)
//      }
//      board.get(wrapped.real.toInt, wrapped.imag.toInt).get match {
//        case Wall => (from -> facing).some
//        case Free => (wrapped -> newFacing).some
//        case Wrap => ???
//      }
//    }
//
//    val (endPos, orient) = actions.foldLeft(startPos -> initialOrientation) { case (pos -> orient, action) => action match {
//      case Left | Right => pos -> (action.turn * orient)
//      case Walk(dist) => (1 to dist).foldLeft(pos, orient) { case (p,o) -> _ => tryMoveFromTo(p, o, 1) }
//    }}
//
//    (1000 * (endPos.imag + 1) + 4 * (endPos.real + 1) + scoreFacing(orient)).toInt
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"""

  private lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
