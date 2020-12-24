package codekata2020.day23

import codekata2020._
import scala.annotation.tailrec
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  object Part1 {
    def solution = {
      @tailrec def go(round: Int, cups: List[Int]): List[Int] = {
        if (round == 100) cups
        else {
          val (first, held, rest) = (cups.head, cups.slice(1, 4), cups.drop(4))
          @tailrec def lowerWithWrapTo9(cur: Int, avoid: Set[Int]): Int =
            if (!avoid(cur)) cur else lowerWithWrapTo9(if (cur - 1 < 1) 9 else cur - 1, avoid)
          val target = lowerWithWrapTo9(first, (held :+ first).toSet)
          go(round + 1, rest.splitAt(rest.indexOf(target) + 1).pipe { case (head, tail) =>
            head ++ held ++ tail :+ first
          })
        }
      }
      go(0, inputs.toList)

    }.zio
  }

  object Part2 {
    class Node(val num: Int, var idxPrev: Node, var idxNext: Node, var numPrev: Node, var numNext: Node) {
      def unlinkIdx: Node = {
        idxPrev.idxNext = idxNext
        idxNext.idxPrev = idxPrev
        idxNext = this
        idxPrev = this
        this
      }
      def appendIdx(n: Node): Node = {
        assert(n.idxNext == n && n.idxPrev == n, s"Expected empty node ${n.num}, but has ${n.idxPrev.num} and ${n.idxNext.num} around it")
        idxNext.idxPrev = n
        n.idxNext = idxNext
        idxNext = n
        n.idxPrev = this
        n
      }
      def appendNum(num: Int): Node = {
        val node = Node.singleton(num)
        node.numNext = numNext
        node.numPrev = this
        numNext.numPrev = node
        numNext = node
        node
      }
      @tailrec final def find(n: Int): Node = {  // SLOW!
        if (n == num) this
        else idxNext.find(n)
      }
      def debugString(n: Int): String = {
        (1 until n).scanLeft(this) { case (n, _) => n.idxNext }.map(_.num).mkString(s" (max $n): ", "->", "")
      }
    }
    object Node {
      def singleton(num: Int): Node = {
        new Node(num, null, null, null, null).tap { n =>
          n.idxPrev = n
          n.idxNext = n
          n.numPrev = n
          n.numNext = n
        }
      }
    }

    def solution = {
      val one = Node.singleton(1)
      val first9 = (2 to 9).scanLeft(one) { case (p, i) => p.appendNum(i).tap(n => p.appendIdx(n): Unit) }

      // Order the nodes in place per starting configuration
      (inputs.last +: inputs).sliding(2).foreach { case IndexedSeq(l, r) => first9(l - 1).appendIdx(first9(r - 1).unlinkIdx) }

      // Append any other nodes
      val additional = (10 to 1000000)
      additional.foldLeft((first9(inputs.last - 1), first9(8))) { case ((tail, max), i) =>
        tail.appendIdx(max.appendNum(i)).dup
      }

      val init = first9(inputs.head - 1)

      @tailrec def lowerWrappingExcluding(cur: Node, avoid: Set[Int]): Node =
        if (!avoid(cur.num)) cur else lowerWrappingExcluding(cur.numPrev, avoid)

      (1 to 10000000).foldLeft(init) { case (cur, idx) =>
        if (idx % 1000000 == 1)
          s"$idx  cur: ${cur.debugString(20)}  one: ${one.debugString(15)}".debug

        val nextThree = (1 until 3).scanLeft(cur.idxNext) { case (n, _) => n.idxNext }
        val putAfter = lowerWrappingExcluding(cur.numPrev, nextThree.map(_.num).toSet)
        nextThree.foreach(_.unlinkIdx)
        nextThree.foldLeft(putAfter)(_ appendIdx _)

        cur.idxNext
      }

      (one.debugString(4), one.idxNext.num.big * one.idxNext.idxNext.num.big)
    }.zio
  }

  private def inputs = in.toSeq.map(_ - '0')

  private lazy val in2 =
    """389125467""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
