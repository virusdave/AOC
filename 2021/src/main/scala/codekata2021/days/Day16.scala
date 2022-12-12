package codekata2021
package days

object Day16 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 16

  private def bits(b: BigInt): Parser[String]     = (s"[01]{${b}}").r
  private def bits2Num(b: BigInt): Parser[BigInt] = bits(b) ^^ (_.bigBinary)

  private val ver: Parser[BigInt] = bits2Num(3)
  private val typ: Parser[Int]    = bits2Num(3) ^^ (_.toInt)

  private type Payload = Either[BigInt, Seq[Packet]]
  private case class Packet(version: BigInt, `type`: Int, payload: Payload)

  // Packet types
  private val literal: Parser[BigInt] = "100" ~> rep("1" ~> bits2Num(4)) ~ ("0" ~> bits2Num(4)) ^^ { case ls ~ r =>
    (ls :+ r).foldLeft(0.big) { case (b, v) => (b << 4) + v }
  }
  private lazy val subPacketBitLen: Parser[Seq[Packet]] = {
    val bitLen: Parser[BigInt] = "0" ~> bits2Num(15)
    (bitLen >> bits) ^? { case usp if parseAll(rep1(packet), usp).successful => parseAll(rep1(packet), usp).get }
  }
  private lazy val subPacketSubLen: Parser[Seq[Packet]] = {
    val packetLen: Parser[BigInt] = "1" ~> bits2Num(11)
    packetLen >> (n => repN(n.toInt, packet))
  }

  // Packet is version, type, and either a literal value or one of two subpacket-holding containers
  private lazy val packet: Parser[Packet] =
    ver ~
      ((success(4) ~ literal.map(Left.apply)) | // `literal` already consumed the constant `100` for `type`
        (typ ~ (subPacketSubLen ^^ Right.apply)) |
        (typ ~ (subPacketBitLen ^^ Right.apply))) ^^ { case v ~ (t ~ p) =>
        Packet(v, t, p)
      }

  private val leftovers: Parser[Unit]          = "0*".r ^^^ (())
  private lazy val parsed: ParseResult[Packet] = parseAll(packet <~ leftovers, inputs)

  override def part1: Option[Part] = PuzzlePart({
    def score(p: Packet): BigInt = p match {
      case Packet(v, _, Right(sps)) => v + sps.map(score).sum
      case Packet(v, _, _) => v
    }

    parsed.map(score)
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    def reduce(p: Packet): BigInt = p match {
      case Packet(_, 0, Right(ps)) => ps.map(reduce).sum
      case Packet(_, 1, Right(ps)) => ps.map(reduce).product
      case Packet(_, 2, Right(ps)) => ps.map(reduce).min
      case Packet(_, 3, Right(ps)) => ps.map(reduce).max
      case Packet(_, 4, Left(v)) => v
      case Packet(_, 5, Right(Seq(l, r))) => if (reduce(l) > reduce(r)) 1.big else 0.big
      case Packet(_, 6, Right(Seq(l, r))) => if (reduce(l) < reduce(r)) 1.big else 0.big
      case Packet(_, 7, Right(Seq(l, r))) => if (reduce(l) == reduce(r)) 1.big else 0.big
      case _ => ??? // It would fail on the following inputs: <lots elided>
    }
    parsed.map(reduce)
  }.zio).some

  // TODO(Dave): Perhaps adding "Hex String To Binary String" method would be nice :)
  private def inputs = in7
    .toSeq.map(c => s"000${BigInt.apply(c.toString, 16).toString(2)}".takeRight(4)).mkString

  private lazy val in2 =
    """8A004A801A8002F478"""

  private lazy val in3 =
    """620080001611562C8802118E34"""

  private lazy val in4 = "C0015000016115A2E0802F182340"
  private lazy val in5 = "A0016C880162017C3686B18A3D4780"

  private lazy val in6 = "C200B40A82"
  private lazy val in7 = "04005AC33890"

  override def in: String =
    """""".stripMargin
}
