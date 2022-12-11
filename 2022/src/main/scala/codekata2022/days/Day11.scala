package codekata2022
package days

import common.InRegexParserSyntax

object Day11 extends ParserPuzzle with InRegexParserSyntax {
  override type PuzzleOut = Any
  override def dayNum: Int = 11

  private type Idx = Int

  sealed abstract class RHS(rhs: BigInt => BigInt) { def apply(x: BigInt): BigInt = rhs(x) }
  private case class Constant(n: Int) extends RHS(_ => n)
  private case object Old extends RHS(old => old)

  private sealed abstract class Operation(op: BigInt => BigInt) { def apply(x: BigInt): BigInt = op(x) }
  private case class Plus(rhs: RHS) extends Operation(old => old + rhs(old))
  private case class Times(rhs: RHS) extends Operation(old => old * rhs(old))

  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)

  private case class Monkey(
      idx: Idx,
      items: IndexedSeq[BigInt],
      inspected: BigInt,
      operation: Operation,
      divisor: Int,
      ifTrue: Idx,
      ifFalse: Idx) {
    def catches(item: BigInt): Monkey = copy(items = items :+ item)
  }
  private val monkeyParser: Parser[Monkey] = {
    val idx = "Monkey" ~> num <~ ":"
    val items = "Starting items:" ~> rep1sep(num, ",") ^^ (_.map(_.big).toIndexedSeq)
    val op = {
      val rhs = num ^^ Constant | "old" ^^^ Old
      "Operation: new = old" ~> (("*" ~> rhs ^^ Times) | ("+" ~> rhs ^^ Plus))
    }
    val divisor = "Test: divisible by" ~> num
    def conditional(which: String) = "If" ~> which ~> ": throw to monkey" ~> num
    idx ~ items ~ op ~ divisor ~ conditional("true") ~ conditional("false") ^^ {
      case idx ~ items ~ op ~ divisor ~ ifTrue ~ ifFalse => Monkey(idx, items, 0, op, divisor, ifTrue, ifFalse)
    }
  }

  private val parsedMonkeys: IndexedSeq[Monkey] = inputs.splitAtDoubleLinebreaksAndParseChunkBy(monkeyParser)
  private val modulus = parsedMonkeys.map(_.divisor).product // ** This part is brilliant, i think :D **

  private def runRounds(rounds: Int, reduceWorry: BigInt => BigInt): IndexedSeq[Monkey] = {
    (1 to rounds).foldLeft(parsedMonkeys) { case (monkeys, _) =>
      parsedMonkeys.indices.foldLeft(monkeys) { case (monkeys, index) =>
        val monkey = monkeys(index)
        monkey.items.foldLeft(monkeys) { case (monkeys, item) =>
          val newWorry = (monkey.operation(item) |> reduceWorry) % modulus  // <3 <3 <3
          val destination = if (newWorry % monkey.divisor == 0) monkey.ifTrue else monkey.ifFalse

          monkeys.updated(destination, monkeys(destination).catches(newWorry))
        }.updated(
          index,
          monkey.copy(
            items = IndexedSeq.empty, // Current monkey has inspected & thrown all its items
            inspected = monkey.inspected + monkey.items.size))
      }
    }
  }
  override def part1: Option[Part] = PuzzlePart({
    runRounds(20, _ / 3).map(_.inspected).sorted.reverse.take(2).product
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    runRounds(10000, identity).map(_.inspected).sorted.reverse.take(2).product
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"""

  private lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
