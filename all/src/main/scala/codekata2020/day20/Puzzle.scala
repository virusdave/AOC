package codekata2020.day20

import codekata2020._

object Puzzle {
  private val inputs = in.split("\n\n").toIndexedSeq.map { tile =>
    val (id, t) = tile.linesIterator.splitAt(1)
    val grid = t.toIndexedSeq.map { x => x.map {
      case '.' => '0'
      case '#' => '1'
    }}
    (id.toSeq.head.split(" ")(1).dropRight(1).big, grid)
  }

  type Edge = BigInt
  type ID = BigInt

  val edges: Map[ID, (Seq[Edge], Seq[Edge])] = inputs.map { case (id, grid) =>
    val edgesCW = Seq(grid.head, grid.map(_.last).mkString(""), grid.last.reverse, grid.map(_.head).reverse.mkString(""))
    (id: ID) -> (edgesCW.map(_.bigBinary : Edge), edgesCW.reverse.map(_.reverse.bigBinary : Edge))
  }.toMap

  val edgeToIds: Map[Edge, Seq[ID]] = edges.toSeq.flatMap { case (id, (cw, ccw)) =>
    (cw.map(_ -> id) ++ ccw.map(_ -> id))
  }.groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  object Part1 {
    def solution = {
      edgeToIds.filter { _._2.length >= 2 }.toSeq.flatMap(_._2).groupBy(identity)
        .view.mapValues(_.length).toSeq.filter(_._2 == 4).map(_._1).product
    }.zio
  }

  object Part2 {
    def solution = {
      val idToEdgesInterleavedCW: Map[ID, IndexedSeq[(Edge, Int)]] =
        edges.view.mapValues { case (cw, ccw) => cw.zip(ccw.reverse).flatMap {case (x1, x2) => Seq(x1, x2) }.zipWithIndex.toIndexedSeq }.toMap

      // NB: First I looked at "which tiles have exactly 2 edges that match other tiles" to get the IDs of the
      // 4 corners (See part 1).  Then I "walked" down the column from an arbitrarily chosen tile, "hopping"
      // from one edge of a tile to the opposite edge, to get the IDs of a "column", but not oriented correctly.
      // Then, for each row, I walk along from the left-most column, to get a 12x12 matrix of tile IDs in the
      // correct position, but not knowing the correct orientation.

      // These were the corners of my puzzle input.
////        Seq(2753, 2383, 2593, 3881).map(t => t -> edges(t)._1.map { e => e -> (edgeToIds(e).toSet - t) } ).debug
//
//        (1 to 11).scanLeft(2753: ID, 545: Edge) { case ((t, e), x) =>
////          (t,e).tap(_.debug: Unit)
//          val nextId = (edgeToIds(e).toSet - t).head
//          val nextEdgeIdx = ((idToEdgesInterleavedCW(nextId).find(_._1 == e).map(_._2).get + 4) % 8)
//          val nextEdge = idToEdgesInterleavedCW(nextId)(nextEdgeIdx)._1
//          (nextId, nextEdge)
//        }
////          .map(_._1)
////          .debug

      // Find the remaining tiles in the correct spots
      val col1 = Vector(2753, 2539, 3343, 1583, 2309, 2113, 3319, 2711, 2273, 1663, 3907, 2593)
      "rows 1-12".debug
      /*val placedTiles = */ col1.map{ id =>
        val e = idToEdgesInterleavedCW(id).flatMap { x => (edgeToIds(x._1).toSet - id).map(x._1 -> _) }.filterNot(x => col1.contains(x._2)).head._1

        (1 to 11).scanLeft(id: ID, e: Edge) { case ((t, e), x) =>
          val nextId = (edgeToIds(e).toSet - t).head
          val nextEdgeIdx = ((idToEdgesInterleavedCW(nextId).find(_._1 == e).map(_._2).get + 4) % 8)
          val nextEdge = idToEdgesInterleavedCW(nextId)(nextEdgeIdx)._1
          (nextId, nextEdge)
        }
          .map(_._1)
      }
//        .debug


      val placedTiles = Vector(
        Vector(2753, 3889, 2011, 1171, 2213, 3761, 1499, 1709, 1291, 1747, 1019, 3881),
        Vector(2539, 2017, 2459, 3457, 3389, 2267, 1201, 1049, 3323, 1091, 1427, 2039),
        Vector(3343, 1433, 1483, 1453, 3499, 1021, 2749, 1699, 2179, 3947, 1367, 2663),
        Vector(1583, 3701, 1997, 2393, 1783, 1873, 1559, 3547, 1009, 3833, 2099, 3593),
        Vector(2309, 2381, 1543, 3533, 1399, 1303, 1319, 1423, 3877, 3671, 1879, 2239),
        Vector(2113, 2683, 3433, 2467, 1597, 3329, 1609, 2971, 3181, 2003, 1487, 3049),
        Vector(3319, 3989, 2999, 3167, 2953, 2549, 1787, 2671, 1381, 3169, 1283, 2833),
        Vector(2711, 1213, 1637, 3797, 1627, 1907, 3709, 2707, 2087, 1163, 2251, 2063),
        Vector(2273, 3413, 2633, 1619, 3137, 2687, 3527, 2819, 3041, 2797, 3853, 3847),
        Vector(1663, 2297, 3299, 1667, 1801, 2791, 1753, 3719, 3109, 1867, 1987, 2503),
        Vector(3907, 2389, 2609, 2027, 2731, 3203, 1069, 2237, 2659, 1289, 2411, 3779),
        Vector(2593, 2357, 1279, 1721, 2129, 1097, 1741, 1823, 1307, 3541, 2287, 2383),
      )
      val idsToGrids = inputs.toMap

      // Readable string corresponding to the various possible orientation transformations for a tile.
      val ops = Seq[(String, IndexedSeq[IndexedSeq[Char]] => IndexedSeq[IndexedSeq[Char]])](
        "" -> (x => x),
        "R" -> (_.rotate90cw),
        "RR" -> (_.rotate90cw.rotate90cw),
        "RRR" -> (_.rotate90ccw),
        "T" -> (_.transpose),
        "TR" -> (_.transpose.rotate90cw),
        "TRR" -> (_.transpose.rotate90cw.rotate90cw),
        "TRRR" -> (_.transpose.rotate90ccw))
      val opsM = ops.toMap

      // Chose an arbitrary orientation that would match both the next-right and just-below tiles for the
      // top-left tile by visual inspection.  Then figure out the remaining orientations of the rest of
      // the tiles in the first row.
      val row1Ops = (1 to 11).scanLeft("TRR") { case (op, idx) =>
        val rhs = opsM(op)(idsToGrids(placedTiles(0)(idx-1)).map(_.toIndexedSeq)).map(_.last).mkString("")
        ops.find { case (o, f) =>
          f(idsToGrids(placedTiles(0)(idx)).map(_.toIndexedSeq))
            .map(_.head).mkString("") == rhs
        }.get._1
      }

      // Now repeat this for the remaining rows to fill the `rowOps` matrix below
      (1 to 11).scanLeft(row1Ops) { case (rowOps, y) =>
        (0 to 11).map { x =>
          val bot = opsM(rowOps(x))(idsToGrids(placedTiles(y - 1)(x)).map(_.toIndexedSeq)).last//.debug
          ops.find { case (o, f) =>
            f(idsToGrids(placedTiles(y)(x)).map(_.toIndexedSeq))
              .head == bot
          }.get._1
        }.toVector
      }
        .map(_.map(s => s""""${s}"""")).mkString("\n")
//        .debug

      val rowOps = Vector(
        Vector("TRR", "R", "TR", "TRRR", "TRRR", "TR", "T", "TR", "TRR", "TRR", "TRR", "TRRR"),
        Vector("TRRR", "RR", "R", "R", "T", "R", "RR", "T", "TR", "R", "TRRR", "R"),
        Vector("RR", "R", "R", "RRR", "TRR", "R", "R", "TRRR", "T", "TRR", "T", "TR"),
        Vector("TRRR", "RR", "T", "R", "T", "R", "RRR", "TRRR", "TRRR", "RR", "RR", "TRR"),
        Vector("R", "T", "RRR", "R", "RRR", "TRRR", "TRR", "T", "T", "TRR", "RR", "RR"),
        Vector("TRR", "RR", "T", "RRR", "TRR", "R", "R", "TRRR", "TRRR", "T", "RR", "TRR"),
        Vector("T", "TRRR", "RR", "R", "T", "R", "R", "T", "RR", "TRR", "TRRR", "R"),
        Vector("R", "T", "RR", "TR", "TRRR", "R", "RR", "T", "RR", "RRR", "R", "T"),
        Vector("RRR", "TRRR", "TRRR", "TRR", "R", "TR", "R", "T", "T", "R", "R", "T"),
        Vector("R", "RR", "TRRR", "TR", "TRR", "TRRR", "RRR", "TR", "TR", "TRRR", "TRR", "RRR"),
        Vector("TR", "TRR", "RR", "RRR", "T", "TRR", "TR", "T", "T", "RR", "TR", "TRR"),
        Vector("TRR", "RR", "TR", "R", "TRRR", "RRR", "TR", "TRR", "T", "TRR", "R", "TRRR"),
      )


      // Matrix of oriented tiles
      val oriented = (0 until 12).map { y =>
        (0 until 12).map { x =>
          opsM(rowOps(y)(x))(idsToGrids(placedTiles(y)(x)).map(_.toIndexedSeq))
        }
      }


      // Render to visually inspect borders
//      "\noriented:".debug
//
//      (0 until 12).map { y =>
//        (0 until 10) map { yi =>
//          oriented(y).map(_(yi).mkString("")).mkString(" ")
//        } mkString("\n")
//      }.mkString("\n\n")
//        .debug



      // Borders dropped
      val trimmed = oriented.map(_.map { grid =>
        grid.drop(1).dropRight(1).map(_.drop(1).dropRight(1))
      })

      // Combined into a single large matrix
      "\nsmushed:".debug
      val smushed = trimmed.flatMap { row =>
        row.head.indices.map { y =>
          row.flatMap(_(y)).mkString("")
        }
      }.map(_.toIndexedSeq)
//        .debug


      // Find the relative offsets (from top-left of dragon sprite) of each required 'on' bit
      // for a dragon to be present
      val dragon = """                  #
                     |#    ##    ##    ###
                     | #  #  #  #  #  #   """.stripMargin
        .linesIterator.zipWithIndex.flatMap { case (l, y) =>
          l.zipWithIndex.flatMap { case (c, x) => c match {
            case '#' => (x, y).some
            case _ => None
          }
        }
      }
        .toSeq.sorted
//        .debug


      // Count the number of (x,y) coordinates in an image such that, from that starting location,
      // all the relative offsets have an 'on' bit in the smushed image.
      def countDragonsIn(map: IndexedSeq[IndexedSeq[Char]], dragon: Seq[(Int, Int)]): Int = {
        val maxX = dragon.map(_._1).max
        val maxY = dragon.map(_._2).max

        (0 until map.length - maxY).flatMap { sY =>
          (0 until map.head.length - maxX).map { sX =>
            // Search for dragons starting at (SX, SY) in the map
            (dragon.map { case (x, y) => (map(sY + y)(sX + x) == '1').toInt }.sum == dragon.size).toInt
          }
        }.sum
      }

      val hashes = smushed.flatten.count(_ == '1')
      ops.map { case (op, f) =>
        val rotated = f(smushed)
        op -> (hashes - (countDragonsIn(rotated, dragon) * dragon.length))
      }.minBy(_._2)


      ()
    }.zio
  }

  private lazy val in2 =
    """Tile 2311:
      |..##.#..#.
      |##..#.....
      |#...##..#.
      |####.#...#
      |##.##.###.
      |##...#.###
      |.#.#.#..##
      |..#....#..
      |###...#.#.
      |..###..###
      |
      |Tile 1951:
      |#.##...##.
      |#.####...#
      |.....#..##
      |#...######
      |.##.#....#
      |.###.#####
      |###.##.##.
      |.###....#.
      |..#.#..#.#
      |#...##.#..
      |
      |Tile 1171:
      |####...##.
      |#..##.#..#
      |##.#..#.#.
      |.###.####.
      |..###.####
      |.##....##.
      |.#...####.
      |#.##.####.
      |####..#...
      |.....##...
      |
      |Tile 1427:
      |###.##.#..
      |.#..#.##..
      |.#.##.#..#
      |#.#.#.##.#
      |....#...##
      |...##..##.
      |...#.#####
      |.#.####.#.
      |..#..###.#
      |..##.#..#.
      |
      |Tile 1489:
      |##.#.#....
      |..##...#..
      |.##..##...
      |..#...#...
      |#####...#.
      |#..#.#.#.#
      |...#.#.#..
      |##.#...##.
      |..##.##.##
      |###.##.#..
      |
      |Tile 2473:
      |#....####.
      |#..#.##...
      |#.##..#...
      |######.#.#
      |.#...#.#.#
      |.#########
      |.###.#..#.
      |########.#
      |##...##.#.
      |..###.#.#.
      |
      |Tile 2971:
      |..#.#....#
      |#...###...
      |#.#.###...
      |##.##..#..
      |.#####..##
      |.#..####.#
      |#..#.#..#.
      |..####.###
      |..#.#.###.
      |...#.#.#.#
      |
      |Tile 2729:
      |...#.#.#.#
      |####.#....
      |..#.#.....
      |....#..#.#
      |.##..##.#.
      |.#.####...
      |####.#.#..
      |##.####...
      |##..#.##..
      |#.##...##.
      |
      |Tile 3079:
      |#.#.#####.
      |.#..######
      |..#.......
      |######....
      |####.#..#.
      |.#...#.##.
      |#.#####.##
      |..#.###...
      |..#.......
      |..#.###...""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
