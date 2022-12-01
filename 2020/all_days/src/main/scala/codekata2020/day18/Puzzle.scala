package codekata2020.day18

import codekata2020._
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  private val inputs = in.linesIterator.toIndexedSeq

  sealed trait Expr
  case class Paren(inner: Expr) extends Expr
  case class Num(v: BigInt) extends Expr
  case class Add(l: Expr, r: Expr) extends Expr
  case class Mul(l: Expr, r: Expr) extends Expr

  def num: Parser[Num] = """\d+""".r ^^ { n => Num(n.big) }
  def eval(e: Expr): BigInt = e match {
    case Num(n) => n
    case Paren(e) => eval(e)
    case Add(l, r) => eval(l) + eval(r)
    case Mul(l, r) => eval(l) * eval(r)
  }

  object Part1 {
    def solution = {
      def paren: Parser[Paren] = "(" ~> expr <~ ")" ^^ { e => Paren(e) }
      def expr: Parser[Expr] = (num | paren) ~ rep( ("+" | "*") ~ (num | paren)) ^^ {
        case t ~ ts => ts.foldLeft(t) {
          case (t1, "+" ~ t2) => Add(t1, t2)
          case (t1, "*" ~ t2) => Mul(t1, t2)
          case (_, ~(_, _)) => ???  // It would fail on the following input: (_, ~(_, _))
        }
      }

      inputs.map(l => eval(parse(expr, l).get)).sum
    }.zio
  }

  object Part2 {
    def solution = {
      def paren: Parser[Paren] = "(" ~> expr <~ ")" ^^ { e => Paren(e) }
      def sums: Parser[Expr] = (num | paren) ~ rep( "+" ~> (num | paren)) ^^ { case t ~ ts => ts.foldLeft(t)(Add) }
      def expr: Parser[Expr] = sums ~ rep("*" ~> sums) ^^ { case t ~ ts => ts.foldLeft(t)(Mul) }

      inputs.map(line => eval(parse(expr, line).get)).sum
    }.zio
  }

  private lazy val in2 =
    """((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2""".stripMargin

  private lazy val in3 =
    """5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
