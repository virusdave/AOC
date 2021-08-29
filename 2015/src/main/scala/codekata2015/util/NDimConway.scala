package codekata2015
package util

object NDimConway {
  // (Live, Count) means Alive next iteration if present and current state Live
  type IterateRuleForNextRoundAlive = Set[(Boolean, Int)]
  private val conwayLifeRule: IterateRuleForNextRoundAlive = Set((true, 2), (true, 3), (false, 3))

  private def crossJoin[T](list: List[List[T]]): List[List[T]] =
    list match { // Cartesian product of input lists
      case Nil       => Nil
      case xs :: Nil => xs map (List(_))
      case x :: xs =>
        for {
          i <- x
          j <- crossJoin(xs)
        } yield List(i) ++ j
    }

  private def countNear(live: Set[List[Int]], coords: List[Int]) =
    crossJoin(coords.map(_.diag(_ - 1 to _ + 1).toList)).count(live)

  def iterateNdimWorld(
      iterations: Int,
      initialLiveCellCoords: Set[List[Int]],
      iterateRuleForNextRoundAlive: IterateRuleForNextRoundAlive = conwayLifeRule,
      strictBoundaries: Boolean = false
  ): Set[List[Int]] =
    (1 to iterations)
      .foldLeft(initialLiveCellCoords) {
        case (world, _) =>
          val keys = world.toList
          crossJoin(
            keys.head.indices
              .map(i =>
                keys
                  .map(_(i))
                  .sorted
                  .diag(_.head - (if (strictBoundaries) 0 else 1) to _.last + (if (strictBoundaries) 0 else 1))
                  .toList
              )
              .toList
          ).flatMap { c =>
            val v = world(c)
            c.some.filter(_ => iterateRuleForNextRoundAlive((v, countNear(world, c) - v.toInt)))
          }.toSet
      }
}
