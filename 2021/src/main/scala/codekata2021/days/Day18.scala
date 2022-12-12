package codekata2021
package days

import scala.annotation.tailrec

object Day18 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 18

  private val num: Parser[Int]             = "[0-9]+".r ^^ (_.toInt)
  private lazy val eNum: Parser[Element2]  = num ^^ Num.apply
  private lazy val ePair: Parser[Element2] = pair
  private lazy val pair: Parser[Pair]      = "[" ~> (eNum | ePair) ~ ("," ~> (eNum | ePair) <~ "]") ^^ { case l ~ r => Pair(l, r) }

  private sealed trait Element2
  private case class Num(v: Int) extends Element2 {
    override def toString: String = v.toString
  }

  private final case class Pair(l: Element2, r: Element2) extends Element2 {
    def magnitude: BigInt = this match {
      case Pair(Num(l), Num(r))   => 3 * l + 2 * r
      case Pair(l: Pair, Num(r))  => 3 * l.magnitude + 2 * r
      case Pair(Num(l), r: Pair)  => 3 * l + 2 * r.magnitude
      case Pair(l: Pair, r: Pair) => 3 * l.magnitude + 2 * r.magnitude
    }
    def +(p: Pair): Pair = {
//      s"    $this".debug
//      s"  + $p".debug
//      s"------".debug
      val res = Pair(this, p).reduce
//      s"    $res".debug
      res
    }

    private def addToLeftMostNum(v: Int): Pair = this match {
      case Pair(Num(l), r) => Pair(Num(l + v), r)
      case Pair(p: Pair, r) =>
        val newP = p.addToLeftMostNum(v)
        if (newP != p) Pair(newP, r)
        else {
          r match {
            case Num(r) => Pair(p, Num(r + v))
            case p: Pair =>
              val newP = p.addToLeftMostNum(v)
              if (newP != p) Pair(l, newP)
              else this
          }
        }
    }

    private def addToRightMostNum(v: Int): Pair = this match {
      case Pair(r, Num(l)) => Pair(r, Num(l + v))
      case Pair(r, p: Pair) =>
        val newP = p.addToRightMostNum(v)
        if (newP != p) Pair(r, newP)
        else {
          r match {
            case Num(r) => Pair(Num(r + v), p)
            case p: Pair =>
              val newP = p.addToRightMostNum(v)
              if (newP != p) Pair(newP, l)
              else this
          }
        }
    }

    override def toString: String = s"[$l,$r]"

    @tailrec
    def reduce: Pair = {
      def nestedExplode(): Pair = {
        def descend(p: Pair, depthRemaining: Int): Option[(Element2, Option[Int], Option[Int])] =
          if (depthRemaining == 0) p match {
            case Pair(Num(l), Num(r)) =>
              (Num(0), l.some, r.some).some
            case _ =>
              assert(false, s"Found a bad deeply-nested pair: $p")
              ???
          }
          else p match {
            case Pair(l: Pair, r: Pair) =>
              val result = descend(l, depthRemaining - 1)
              result match {
                case None =>
                  val result = descend(r, depthRemaining - 1)
                  result.map { case (newP, pushL, pushR) =>
                    // Found replacement for Right side
                    val newL = pushL.map(l.addToRightMostNum).filterNot(_ == l)
                    (Pair(newL.getOrElse(l), newP), pushL.filterNot(_ => newL.isDefined), pushR)
                  }
                case Some((newP, pushL, pushR)) =>
                  // Found replacement for Left side
                  val newR = pushR.map(r.addToLeftMostNum).filterNot(_ == r)
                  (Pair(newP, newR.getOrElse(r)), pushL, pushR.filterNot(_ => newR.isDefined)).some
              }
            case Pair(p: Pair, Num(v)) =>
              val result = descend(p, depthRemaining - 1)
              result.map { case (newP, pushL, pushR) =>
                (Pair(newP, Num(pushR.map(_ + v).getOrElse(v))), pushL, None)
              }
            case Pair(Num(v), p: Pair) =>
              val result = descend(p, depthRemaining - 1)
              result.map { case (newP, pushL, pushR) =>
                (Pair(Num(pushL.map(_ + v).getOrElse(v)), newP), None, pushR)
              }
            case _ => None
          }

        descend(this, 4).map(_._1) match {
          case Some(p: Pair) => p
          case _             => this
        }
      }
      def splitTens(): Pair = {
        def splitRec(p: Pair): Option[Pair] = {
          val done = p.l match {
            case Num(v) if v >= 10 => Pair(Num(v / 2), Num(v - v / 2)).some
            case pp: Pair          => splitRec(pp)
            case _                 => None
          }
          done match {
            case Some(l) => Pair(l, p.r).some
            case None => p.r match {
                case Num(v) if v >= 10 => Pair(p.l, Pair(Num(v / 2), Num(v - v / 2))).some
                case pp: Pair          => splitRec(pp).map(newR => Pair(p.l, newR))
                case _                 => None
              }
          }
        }
        splitRec(this).getOrElse(this)
      }
      nestedExplode().some.filter(_ != this).orElse(splitTens().some).filter(_ != this) match {
        case Some(thing) =>
          thing
//            .debug
            .reduce
        case None =>
          this
//            .debug
      }
    }
  }

  override def part1: Option[Part] = PuzzlePart(
    inputs.map(parseAll(pair, _).get).reduceLeft(_ + _).magnitude.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val nums = inputs.map(parseAll(pair, _).get)
    (for {
      x <- nums
      y <- nums if x != y
    } yield (x + y).magnitude).max
  }.zio).some

  private def inputs = in2.linesIterator.toSeq

  private lazy val in2 =
    """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
      |[[[5,[2,8]],4],[5,[[9,9],0]]]
      |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
      |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
      |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
      |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
      |[[[[5,4],[7,7]],8],[[8,3],8]]
      |[[9,3],[[9,9],[6,[4,9]]]]
      |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
      |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin

  private lazy val in3 =
    """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
      |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
      |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
      |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
      |[7,[5,[[3,8],[1,4]]]]
      |[[2,[2,2]],[8,[8,1]]]
      |[2,9]
      |[1,[[[9,3],9],[[9,0],[0,7]]]]
      |[[[5,[7,4]],7],1]
      |[[[[4,2],2],6],[8,7]]""".stripMargin

  override def in: String =
    """""".stripMargin
}
