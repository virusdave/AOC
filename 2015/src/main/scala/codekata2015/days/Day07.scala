package codekata2015
package days

import scala.collection.mutable

object Day07 extends ParserPuzzle {
  override type PuzzleOut = Any

  override def dayNum: Int = 7

  type NUM = Char
  def toNum(in: String): NUM = in.toShort.toChar //.toShort

  def ref: Parser[Ref] = "[a-z]+".r ^^ Ref
  def num: Parser[NUM] = "[0-9]+".r ^^ toNum
  def lit: Parser[Lit] = num ^^ Lit
  def ident: Parser[Val] = ref | lit

  sealed trait Val
  case class Lit(n: NUM) extends Val
  case class Ref(n: String) extends Val

  sealed trait Op
  case class ASSIGN(l: Val) extends Op
  case class AND(l: Val, r: Val) extends Op
  case class OR(l: Val, r: Val) extends Op
  case class LS(l: Ref, r: NUM) extends Op
  case class RS(l: Ref, r: NUM) extends Op
  case class NOT(l: Ref) extends Op

  def lhs: Parser[Op] =
    ((ref <~ "LSHIFT") ~ num ^^ { case i ~ n => LS(i, n) }) |
      ((ref <~ "RSHIFT") ~ num ^^ { case i ~ n => RS(i, n) }) |
      ((ident <~ "AND") ~ ident ^^ { case l ~ r => AND(l, r) }) |
      ((ident <~ "OR") ~ ident ^^ { case l ~ r => OR(l, r) }) |
      (("NOT" ~> ref) ^^ { i => NOT(i) }) |
      (ident ^^ ASSIGN)

  def line: Parser[(Op, String)] = (lhs <~ "->") ~ ref ^^ { case l ~ i => (l, i.n) }

  override def part1: Option[Part] = new Part {
    val parsed = inputs.parseLinesBy(line)
    val bindings: Map[String, Op] = parsed.map { case (op, dest) => dest -> op }.toMap

    def eval(key: String, cache: mutable.Map[String, NUM] = mutable.Map.empty): NUM = {
      def get(v: Val): NUM = v match {
        case Lit(n) => n
        case Ref(r) => eval(r, cache)
      }

      cache.getOrElse(key, (bindings(key) match {
        case ASSIGN(l) => get(l)
        case AND(l, r) => (get(l) & get(r)).toChar
        case OR(l, r) => (get(l) | get(r)).toChar
        case LS(l, r) => (eval(l.n, cache) << r.toInt).toChar
        case RS(l, r) => (eval(l.n, cache) >>> r.toInt).toChar
        case NOT(l) => (~eval(l.n, cache)).toChar
      }).tap(v => cache += key -> v))
    }

    override def solution = eval("a").toInt.zio
  }.some

  override def part2: Option[Part] = new Part {
    val parsed = inputs.parseLinesBy(line)
    val bindings: Map[String, Op] =
      parsed.map { case (op, dest) => dest -> op }.toMap ++ Map(
        "b" -> ASSIGN(Lit(16076.toChar))
      )

    def eval(key: String, cache: mutable.Map[String, NUM] = mutable.Map.empty): NUM = {
      def get(v: Val): NUM = v match {
        case Lit(n) => n
        case Ref(r) => eval(r, cache)
      }

      cache.getOrElse(key, (bindings(key) match {
        case ASSIGN(l) => get(l)
        case AND(l, r) => (get(l) & get(r)).toChar
        case OR(l, r) => (get(l) | get(r)).toChar
        case LS(l, r) => (eval(l.n, cache) << r.toInt).toChar
        case RS(l, r) => (eval(l.n, cache) >>> r.toInt).toChar
        case NOT(l) => (~eval(l.n, cache)).toChar
      }).tap(v => cache += key -> v))
    }

    override def solution = eval("a").toInt.zio
  }.some

  def inputs = in2

  val in2 =
    """123 -> x
      |456 -> y
      |x AND y -> d
      |x OR y -> e
      |x LSHIFT 2 -> f
      |y RSHIFT 2 -> g
      |NOT x -> h
      |NOT y -> i""".stripMargin
  val in3 = ""

  override def in: String = "".stripMargin
}