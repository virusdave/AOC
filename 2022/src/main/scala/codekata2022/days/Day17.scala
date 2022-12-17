package codekata2022
package days

import scala.annotation.tailrec
import scala.util.Try
import zio.duration._

object Day17 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 17

  private type Pair = (Int, Int)

  private case class Shape(pieces: Seq[Pair]) {
    def canMoveTo(br: Pair, rocks: Set[Pair]): Boolean =
      pieces.map(_ + br).forall { case p@(x, y) => !rocks.contains(p) && x >= 0 && x < 7 && y > 0 }
  }
  private val shapes: LazyList[(Shape, Int)] = LazyList.continually(LazyList.from(Seq(
    Seq(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0),
    Seq(0 -> 1, 1 -> 0, 1 -> 1, 1 -> 2, 2 -> 1),
    Seq(0 -> 0, 1 -> 0, 2 -> 0, 2 -> 1, 2 -> 2),
    Seq(0 -> 0, 0 -> 1, 0 -> 2, 0 -> 3),
    Seq(0 -> 0, 0 -> 1, 1 -> 0, 1 -> 1),
  ).map(Shape).zipWithIndex)).flatten

  private val parsed: LazyList[(Pair, Int)] = LazyList.continually(LazyList.from(
    inputs.toSeq.map {
      case '>' => (1, 0)
      case '<' => (-1, 0)
    }.zipWithIndex)).flatten

  override def part1: Option[Part] = PuzzlePart({
    case class State(motions: LazyList[Pair], rocks: Set[Pair], maxY: Int, floor: BigInt)

    val finalState = shapes
      .take(2022)
      .foldLeft(State(parsed.map(_._1), Set.empty, 0, 0)) { case state -> (shape -> _) =>
        @tailrec def drop(state: State, pos: Pair): (Pair, LazyList[Pair]) = {
          import LazyList._
          // Attempt to move the position by the air current
          val next #:: rest = state.motions
          val tryPos = pos + next
          val newPos = tryPos.when(p => shape.canMoveTo(p, state.rocks)).getOrElse(pos) //.debug
          val dropPos = (newPos + (0, -1)).when(p => shape.canMoveTo(p, state.rocks)) //.debug
          dropPos match {
            case None => newPos -> rest
            case Some(droppedPos) => drop(state.copy(motions = rest), droppedPos)
          }
        }
        val shapeStartPos = (2, state.maxY + 4)
        val piecePos -> rest = drop(state, shapeStartPos)

        val shapeRocks = shape.pieces.map(_ + piecePos)
        val rocksWithShapeRocks = state.rocks ++ shapeRocks
        val shapeYs = shapeRocks.map(_._2)
        val newFloor = shapeYs.filter(y =>
          (0 to 6).forall(x => rocksWithShapeRocks.contains(x -> y) || rocksWithShapeRocks.contains(x -> (y+1)))
        ).sorted.reverse.headOption
        val newRocks = newFloor.fold2(
          rocksWithShapeRocks, floor => // Drop everything under new floor, and remap remaining Y coords
            rocksWithShapeRocks.filterNot(_._2 <= floor).map { case x -> y => x -> (y - floor) })
        val realNewFloor = newFloor.map(_ + state.floor).getOrElse(state.floor)

        State(rest, newRocks, (shapeYs :+ state.maxY).max - newFloor.getOrElse(0), realNewFloor)
      }

    val rocksTop = finalState.rocks.map(_._2).max

    //// Rendering
    //val bottom = finalState.floor
    //val it = IndexedSeq.tabulate(rocksTop.toInt + 1, 7) { case y -> x =>
    //  if (finalState.rocks.contains(x -> y)) '#' else '.'
    //}
    //it.zipWithIndex.reverse.map(x => "%5d |%s|".format(x._2 + bottom, x._1.mkString)).mkString("\n").debug
    //"\n".debug

    rocksTop + finalState.floor

  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    import LazyList._

    case class CacheKey(motionIndex: Int, shapeIndex: Int, rocks: Set[Pair])
    case class CacheValue(floor: BigInt, shapeGlobalIndex: BigInt)
    case class State(
        motions: LazyList[(Pair, Int)], rocks: Set[Pair], maxY: Int, floor: BigInt,
        cache: Option[Map[CacheKey, CacheValue]],
    )

    // val limit = 2022
    // val limit = 2500000.big
    // val limit = 10000000.big
    val limit = BigInt.apply("1000000000000")

    val before = System.currentTimeMillis()

    type ShapeList = LazyList[((Shape, Int), BigInt)]

    @tailrec def go(shapeList: ShapeList, state: State): State = shapeList match {
      case LazyList() => state
      case (_ -> _ -> shapeGlobalIndex) #:: _ if shapeGlobalIndex > limit => state
      case (shape -> shapeIdx -> shapeGlobalIndex) #:: shapeTail =>
        if (shapeGlobalIndex % 5000 == 0) {
          val now = System.currentTimeMillis()
          val prettyLeft = Try {
            Duration.fromMillis(((now - before) * (limit.toDouble / shapeGlobalIndex.toDouble - 1)).toLong).render
          }.getOrElse(s"Failed to render ETA update")

          val prettyTotal = Try {
            Duration.fromMillis(((now - before) * (limit.toDouble / shapeGlobalIndex.toDouble)).toLong).render
          }.getOrElse(s"Failed to render ETA update")

          val percent = "%.2f".format(shapeGlobalIndex.toDouble * 10000.0 / limit.toDouble / 100)

          s"$shapeGlobalIndex: ${percent} %, ETA [$prettyLeft] left of [$prettyTotal] total; cache size ${state.cache.map(_.size).getOrElse(-1)}, floor: ${state.floor}, maxY: ${state.maxY}, rocks: ${state.rocks.size}".debugSameLine
        }

        val cacheKey = CacheKey(state.motions.head._2, shapeIdx, state.rocks)
        val cacheHit = state.cache.flatMap(_.get(cacheKey))

        @tailrec def drop(state: State, pos: Pair): (Pair, LazyList[(Pair, Int)]) = {
          // Attempt to move the position by the air current
          val (next -> _) #:: rest = state.motions
          val tryPos = pos + next
          val newPos = tryPos.when(p => shape.canMoveTo(p, state.rocks)).getOrElse(pos) //.debug
          val dropPos = (newPos + (0, -1)).when(p => shape.canMoveTo(p, state.rocks)) //.debug
          dropPos match {
            case None => newPos -> rest
            case Some(droppedPos) => drop(state.copy(motions = rest), droppedPos)
          }
        }

        val shapeStartPos = (2, state.maxY + 4)
        val piecePos -> rest = drop(state, shapeStartPos)

        val shapeRocks = shape.pieces.map(_ + piecePos)
        val rocksWithShapeRocks = state.rocks ++ shapeRocks
        val shapeYs = shapeRocks.map(_._2)
        val newFloor = shapeYs.filter(y =>
          (0 to 6).forall(x => rocksWithShapeRocks.contains(x -> y) || rocksWithShapeRocks.contains(x -> (y+1)))
        ).sorted.reverse.headOption
        val newRocks = newFloor.fold2(
          rocksWithShapeRocks, floor => // Drop everything under new floor, and remap remaining Y coords
            rocksWithShapeRocks.filterNot(_._2 < floor).map { case x -> y => x -> (y - floor) })
        val realNewFloor = newFloor.map(_ + state.floor).getOrElse(state.floor)

        cacheHit match {
          case Some(cachedValue) =>
            // Fast forward to just before our target, and disable caching from this point.
            val piecesInLoop = shapeGlobalIndex - cachedValue.shapeGlobalIndex
            val skipLoopCount = (limit - shapeGlobalIndex) / piecesInLoop
            val newState =
              State(
                rest,
                newRocks,
                (shapeYs :+ state.maxY).max - newFloor.getOrElse(0),
                realNewFloor + (skipLoopCount * (realNewFloor - cachedValue.floor)),
                None, // Disable caching from this point forward
              )
            s"\nskipping forward from turn $shapeGlobalIndex to turn ${shapeGlobalIndex + skipLoopCount * piecesInLoop}\n".debug
            go(shapeTail.map { case piece -> gidx => piece -> (gidx + skipLoopCount * piecesInLoop) }, newState)
          case None =>
            val cacheValue = CacheValue(realNewFloor, shapeGlobalIndex)
            val newState =
              State(
                rest,
                newRocks,
                (shapeYs :+ state.maxY).max - newFloor.getOrElse(0),
                realNewFloor,
                state.cache.map(_ + (cacheKey -> cacheValue)),
              )
            go(shapeTail, newState)
        }
    }
    val finalState = go(
      shapes.zip(LazyList.iterate(1.big)(_ + 1)),
      State(parsed, Set.empty, 0, 0, Map.empty.some),
    )

    "\n".debug
    (finalState.rocks.map(_._2) + 0).max + finalState.floor
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"""

  override def in: String =
    """""".stripMargin
}
