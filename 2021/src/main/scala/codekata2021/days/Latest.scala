package codekata2021
package days

import enumeratum._
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.collection.parallel.immutable._

object Latest extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 24

  private val word: Parser[String]   = "[a-zA-Z]+".r
  private val num: Parser[Int]       = "[0-9]+".r ^^ (_.toInt)
  private val neg: Parser[Int]       = "-" ~> num ^^ (-_)
  private val signedNum: Parser[Int] = num | neg

  case class MachineState(inputs: List[Int], w: BigInt = 0, x: BigInt = 0, y: BigInt = 0, z: BigInt = 0)

  sealed trait RegisterOrLiteral { val get: MachineState => BigInt }
  case class Literal(v: BigInt) extends RegisterOrLiteral { override val get = _ => v }
  sealed abstract class Register(
      val name: String,
      override val get: MachineState => BigInt,
      val set: (MachineState, BigInt) => MachineState)
      extends EnumEntry
      with RegisterOrLiteral
  object Register extends Enum[Register] {
    case object W extends Register("w", _.w, { case (ms, v) => ms.copy(w = v) })
    case object X extends Register("x", _.x, { case (ms, v) => ms.copy(x = v) })
    case object Y extends Register("y", _.y, { case (ms, v) => ms.copy(y = v) })
    case object Z extends Register("z", _.z, { case (ms, v) => ms.copy(z = v) })

    override lazy val values: IndexedSeq[Register] = findValues
    lazy val registers: Map[String, Register]      = values.map(r => r.name -> r).toMap
  }

  sealed trait Op { def op(state: MachineState): MachineState }
  case class Inp(dest: Register) extends Op {
    override def op(state: MachineState): MachineState = state.inputs match {
      case h :: t => state.copy(inputs = t).|>(dest.set(_, h))
      case Nil    => ???
    }
  }
  sealed abstract class BinaryOp(combine: (BigInt, BigInt) => BigInt) extends Op {
    def l: Register
    def r: RegisterOrLiteral
    override def op(state: MachineState): MachineState = l.set(state, combine(l.get(state), r.get(state)))
  }
  case class Add(l: Register, r: RegisterOrLiteral) extends BinaryOp(_ + _)
  case class Mul(l: Register, r: RegisterOrLiteral) extends BinaryOp(_ * _)
  case class Div(l: Register, r: RegisterOrLiteral) extends BinaryOp(_ / _)
  case class Mod(l: Register, r: RegisterOrLiteral) extends BinaryOp(_ % _)
  case class Eql(l: Register, r: RegisterOrLiteral) extends BinaryOp((l, r) => if (l == r) 1.big else 0.big)
  object BinaryOp {
    val ops: Map[String, (Register, RegisterOrLiteral) => BinaryOp] = Map(
      "add" -> Add.apply,
      "mul" -> Mul.apply,
      "div" -> Div.apply,
      "mod" -> Mod.apply,
      "eql" -> Eql.apply,
    )
  }

  private lazy val register: Parser[Register] = "[wxyz]".r ^^ Register.registers
  private lazy val literal: Parser[Literal]   = signedNum ^^ (_.big.|>(Literal.apply))

  private lazy val binaryOp: Parser[BinaryOp] =
    BinaryOp.ops.keys.reduceLeft((x, y) => s"$x|$y").r ~ register ~ (register | literal) ^^ {
      case op ~ r ~ rl => BinaryOp.ops(op)(r, rl)
    }
  private lazy val inp: Parser[Inp] = "inp" ~> register ^^ Inp.apply
  private lazy val line: Parser[Op] = inp | binaryOp

  override def part1: Option[Part] = PuzzlePart({
    val program = inputs.map(parseAll(line, _).get)

    // Got to 99964899000000, need different approach!
    LazyList.iterate(99999999999999L.big)(_ - 100).find { v =>
      val found = (0 to 99).map(v - _).par.find { v =>
        if (v % 1000000 == 0) (s"    $v").debug

        val str = v.toString().toSeq.map(_.toString.toInt)
        !str.contains(0) && program.foldLeft(MachineState(str.toList)) { case state -> op => op.op(state) }.z == 0
      }
      found.map(_.debug).isDefined
    }
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    ()
    ()
  }.zio).some

  private def inputs = in.linesIterator.toSeq

  private lazy val in2 =
    """inp z
      |inp x
      |mul z 3
      |eql z x""".stripMargin

  private lazy val in3 =
    """inp w
      |add z w
      |mod z 2
      |div w 2
      |add y w
      |mod y 2
      |div w 2
      |add x w
      |mod x 2
      |div w 2
      |mod w 2""".stripMargin

  override def in: String =
    """inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 1
      |add x 12
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 4
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 1
      |add x 11
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 10
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 1
      |add x 14
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 12
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 26
      |add x -6
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 14
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 1
      |add x 15
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 6
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 1
      |add x 12
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 16
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 26
      |add x -9
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 1
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 1
      |add x 14
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 7
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 1
      |add x 14
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 8
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 26
      |add x -5
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 11
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 26
      |add x -9
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 8
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 26
      |add x -5
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 3
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 26
      |add x -2
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 1
      |mul y x
      |add z y
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z 26
      |add x -7
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y 8
      |mul y x
      |add z y""".stripMargin
}
