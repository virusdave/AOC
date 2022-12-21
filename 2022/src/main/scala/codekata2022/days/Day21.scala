package codekata2022
package days

import scala.annotation.tailrec

object Day21 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 21

  private type Num = BigDecimal

  sealed abstract class Op(val apply: (Num, Num) => Num)
  case object Plus extends Op(_ + _)
  case object Minus extends Op(_ - _)
  case object Times extends Op(_ * _)
  case object Divide extends Op(_ / _)
  private val op: Parser[Op] = "+" ^^^ Plus | "-" ^^^ Minus | "*" ^^^ Times | "/" ^^^ Divide

  private sealed abstract class Monkey(val get: () => Num) {
    def name: String
    def dup(cache: String => Monkey): Monkey
  }
  private case class NumberMonkey(name: String, num: Num) extends Monkey(() => num) {
    def dup(cache: String => Monkey): NumberMonkey = copy()
  }
  private case class OpMonkey(
      name: String, lhs: String, op: Op, rhs: String, cache: String => Monkey = n => monkeys(n)
  ) extends Monkey(() => op.apply(cache(lhs).get(), cache(rhs).get())) {
    def dup(cache: String => Monkey): OpMonkey = copy(cache = cache)
  }

  private val word: Parser[String] = "[a-zA-Z]+".r
  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val neg: Parser[Int] = "-" ~> num ^^ (-_)
  private val signedNum: Parser[Int] = num | neg

  private val numMonkey = word ~ (":" ~> signedNum ^^ (BigDecimal(_))) ^^ (_.toTuple) ^^ (NumberMonkey.apply _).tupled
  private val opMonkey = word ~ (":" ~> word) ~ op ~ word ^^ { case n ~ l ~ op ~ r => OpMonkey(n, l, op, r) }
  private val monkey: Parser[Monkey] = numMonkey | opMonkey
  private val monkeys: Map[String, Monkey] = inputs.parseLinesBy(monkey).map(m => m.name -> m).toMap

  override def part1: Option[Part] = PuzzlePart({
    monkeys("root").get()
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    def duplicateMonkeys(lookup: String => Monkey) =
      monkeys.map { case l -> r => l -> r.dup(lookup) } +
        ("root" -> monkeys("root").asInstanceOf[OpMonkey].copy(op = Minus).dup(lookup))

    def evalXAndXPlusOneForNewton(x: Num) = {
      lazy val cacheForX: Map[String, Monkey] =
        duplicateMonkeys(name => cacheForX(name)) + ("humn" -> NumberMonkey("humn", x))
      lazy val cacheForXPlus1: Map[String, Monkey] =
        duplicateMonkeys(name => cacheForXPlus1(name)) + ("humn" -> NumberMonkey("humn", x + 1))

      val fx = cacheForX("root").get()
      val fxPlus1 = cacheForXPlus1("root").get()
      (fx, fxPlus1 - fx)
    }

    def newton(guess: Num, fWithDerivative: Num => (Num, Num), tolerance: Double = 1e-6): Num = {
      @tailrec def iterate(approx: Num, count: Int): Num = {
        if (count > 100000) approx
        else {
          val y -> dy = fWithDerivative(approx)
          if (y.abs < tolerance) approx
          else {
            val next = BigDecimal((approx - y / dy).rounded.toBigInt)
            if ((y - next).abs < tolerance) next
            else iterate(next, count + 1)
          }
        }
      }
      iterate(guess, 0)
    }
    newton(0, evalXAndXPlusOneForNewton)
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"""

  private lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
