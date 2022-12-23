package common

object GridNeighbors extends Syntax {
  type Delta = (Int, Int)
  type DeltaLong = (Long, Long)
  type DeltaBig = (BigInt, BigInt)

  val Self: Delta = 0 -> 0
  val Nowhere: Delta = Self
  val N: Delta = 0 -> -1
  val S: Delta = 0 -> 1
  val W: Delta = -1 -> 0
  val E: Delta = 1 -> 0
  val All4Neighbors: Seq[Delta] = Seq(N, S, E, W)
  val All4NeighborsBig: Seq[DeltaBig] = All4Neighbors.map(_.toBigInts)

  val NW: Delta = N + W
  val NE: Delta = N + E
  val SW: Delta = S + W
  val SE: Delta = S + E
  val AllDiagonals: Seq[Delta] = Seq(NW, NE, SW, SE)
  val AllDiagonalsBig: Seq[DeltaBig] = AllDiagonals.map(_.toBigInts)
  val All8Neighbors: Seq[Delta] = All4Neighbors ++ AllDiagonals
  val All8NeighborsBig: Seq[DeltaBig] = All8Neighbors.map(_.toBigInts)
}