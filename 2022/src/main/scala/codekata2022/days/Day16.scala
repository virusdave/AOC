package codekata2022
package days

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

object Day16 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 16

  private type ValveID = String
  private case class Valve(which: ValveID, value: Int, outgoing: Set[ValveID])

  private val word: Parser[String] = "[a-zA-Z]+".r
  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val line: Parser[Valve] =
    "Valve" ~> word ~
      ("has flow rate=" ~> num) ~
      ("; tunnels? leads? to valves?".r ~> rep1sep(word, ",")) ^^ { case id ~ value ~ out =>
      Valve(id, value, out.toSet)
    }

  private val parsed: IndexedSeq[Valve] = inputs.parseLinesBy(line)
  private val nodeIds: Set[ValveID] = parsed.map(_.which).toSet
  private val valves: Map[ValveID, Valve] = parsed.map(v => v.which -> v).toMap

  private sealed abstract class Minute(rep: String) { override def toString: ValveID = rep }
  private case object DoNothing extends Minute("∅")
  private case class Transit(from: Valve, to: Valve) extends Minute(s">${to.which}")
  private case class Enable(valve: Valve, moveScore: Int, used: Set[ValveID])
    extends Minute(s"[${valve.which}:$moveScore]")

  private case class Partial(minute: Int, at: String, score: Int, previous: List[Minute], used: Set[String])

  override def part1: Option[Part] = PuzzlePart({
    parsed.mkString("\n")
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    ()
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"""

  private lazy val in3 =
    """"""

  override def in: String =
    """Valve TM has flow rate=3; tunnels lead to valves GU, KQ, BV, MK
Valve BX has flow rate=0; tunnels lead to valves CD, HX
Valve GV has flow rate=8; tunnels lead to valves MP, SE
Valve OI has flow rate=0; tunnels lead to valves ZB, RG
Valve OY has flow rate=0; tunnels lead to valves XG, ZB
Valve EZ has flow rate=0; tunnels lead to valves OU, LI
Valve TN has flow rate=0; tunnels lead to valves DT, GU
Valve SE has flow rate=0; tunnels lead to valves GV, CD
Valve SG has flow rate=0; tunnels lead to valves XR, NK
Valve EB has flow rate=0; tunnels lead to valves SJ, CE
Valve QB has flow rate=0; tunnels lead to valves AW, MI
Valve GU has flow rate=0; tunnels lead to valves TN, TM
Valve AW has flow rate=11; tunnels lead to valves QB, IG, IK, VK
Valve IG has flow rate=0; tunnels lead to valves AW, SH
Valve MJ has flow rate=0; tunnels lead to valves IK, XR
Valve HX has flow rate=0; tunnels lead to valves BX, AA
Valve IK has flow rate=0; tunnels lead to valves MJ, AW
Valve QZ has flow rate=0; tunnels lead to valves AF, XG
Valve CV has flow rate=0; tunnels lead to valves KT, AA
Valve ES has flow rate=0; tunnels lead to valves BV, CD
Valve NK has flow rate=0; tunnels lead to valves YQ, SG
Valve SL has flow rate=0; tunnels lead to valves DT, XL
Valve RG has flow rate=17; tunnels lead to valves SJ, OI, WC
Valve ZB has flow rate=9; tunnels lead to valves OY, MP, DI, OX, OI
Valve SJ has flow rate=0; tunnels lead to valves RG, EB
Valve GF has flow rate=19; tunnels lead to valves DQ, SH, IH
Valve OU has flow rate=10; tunnels lead to valves EZ, TL, WC
Valve TL has flow rate=0; tunnels lead to valves OU, OX
Valve XG has flow rate=18; tunnels lead to valves QZ, OY
Valve EK has flow rate=20; tunnels lead to valves FD, MI
Valve BV has flow rate=0; tunnels lead to valves TM, ES
Valve AA has flow rate=0; tunnels lead to valves CV, HX, TR, MK, DQ
Valve UO has flow rate=23; tunnel leads to valve AF
Valve LI has flow rate=0; tunnels lead to valves EZ, CE
Valve MI has flow rate=0; tunnels lead to valves EK, QB
Valve MP has flow rate=0; tunnels lead to valves GV, ZB
Valve YQ has flow rate=14; tunnels lead to valves VK, MG, NK
Valve AF has flow rate=0; tunnels lead to valves UO, QZ
Valve SH has flow rate=0; tunnels lead to valves IG, GF
Valve FD has flow rate=0; tunnels lead to valves IH, EK
Valve KQ has flow rate=0; tunnels lead to valves TM, FQ
Valve DI has flow rate=0; tunnels lead to valves ZB, CD
Valve KT has flow rate=0; tunnels lead to valves DT, CV
Valve MG has flow rate=0; tunnels lead to valves NQ, YQ
Valve DQ has flow rate=0; tunnels lead to valves GF, AA
Valve CE has flow rate=21; tunnels lead to valves LI, EB
Valve MK has flow rate=0; tunnels lead to valves AA, TM
Valve XL has flow rate=0; tunnels lead to valves CD, SL
Valve OX has flow rate=0; tunnels lead to valves TL, ZB
Valve DT has flow rate=5; tunnels lead to valves NQ, TP, KT, SL, TN
Valve IH has flow rate=0; tunnels lead to valves GF, FD
Valve TP has flow rate=0; tunnels lead to valves XR, DT
Valve FQ has flow rate=0; tunnels lead to valves XR, KQ
Valve CD has flow rate=6; tunnels lead to valves DI, BX, XL, ES, SE
Valve XR has flow rate=7; tunnels lead to valves TR, FQ, TP, MJ, SG
Valve VK has flow rate=0; tunnels lead to valves YQ, AW
Valve WC has flow rate=0; tunnels lead to valves RG, OU
Valve TR has flow rate=0; tunnels lead to valves XR, AA
Valve NQ has flow rate=0; tunnels lead to valves DT, MG""".stripMargin
}
