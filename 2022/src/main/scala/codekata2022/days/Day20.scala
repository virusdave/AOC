package codekata2022
package days

object Day20 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 20

  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val neg: Parser[Int] = "-" ~> num ^^ (-_)
  private val signedNum: Parser[Int] = num | neg

  private case class DoublyLinkedNode(
      n: BigInt, var next: DoublyLinkedNode = null, var prev: DoublyLinkedNode = null) {
    def slideRight(): Unit = {
      val (prior, subsequent) = prev -> next.next
      subsequent.prev = this; next.next = this
      prior.next = next; prev = next
      next.prev = prior; next = subsequent
    }
    def slideLeft(): Unit = {
      val (prior, subsequent) = prev.prev -> next
      prior.next = this; prev.prev = this
      subsequent.prev = prev; next = prev
      prev.next = subsequent; prev = prior
    }
  }
  private def linkNodes(nodes: Seq[DoublyLinkedNode]) = nodes.tap(_.indices.foreach { n =>
    nodes(n).prev = nodes((n - 1 + nodes.size) % nodes.size)
    nodes(n).next = nodes((n + 1 + nodes.size) % nodes.size)
  })
  private val parsed = inputs.parseLinesBy(signedNum)
  private def coords(nodes: Seq[DoublyLinkedNode]) = {
    val zero = nodes.find(_.n == 0).get
    val follows: DoublyLinkedNode => DoublyLinkedNode = _.next
    Seq(1000, 2000, 3000).map(n => Function.chain(LazyList.fill(n)(follows))).map(_(zero).n).sum
  }
  override def part1: Option[Part] = PuzzlePart({
    val nodes = parsed.map(DoublyLinkedNode(_)) |> linkNodes

    nodes.tap(_.indices.foreach { n =>
      val node = nodes(n)
      if (node.n >= 0) (1 to (node.n % (nodes.size - 1)).toInt).foreach(_ => node.slideRight())
      else (1 to ((-node.n) % (nodes.size - 1)).toInt).foreach(_ => node.slideLeft())
    }) |> coords
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val nodes = parsed.map(n => DoublyLinkedNode(n.big * 811589153)) |> linkNodes

    (1 to 10).foreach(_ =>
      nodes.indices.foreach { n =>
        val node = nodes(n)
        if (node.n >= 0) (1 to (node.n % (nodes.size - 1)).toInt).foreach(_ => node.slideRight())
        else (1 to ((-node.n) % (nodes.size - 1)).toInt).foreach(_ => node.slideLeft())
      })
    nodes |> coords
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """1
2
-3
3
-2
0
4"""

  override def in: String =
    """""".stripMargin
}
