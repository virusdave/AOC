package codekata2015
package days

import breeze.linalg._
import zio.RIO

object Day15 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 15

  case class Ingredient(capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)
  lazy val lines: Parser[(String, Ingredient)] = {
    val ingredient: Parser[String] = "[A-Z][a-z]*".r
    val num: Parser[Int]           = "(-)?[0-9]+".r ^^ { _.toInt }
    ingredient ~
      (": capacity" ~> num) ~
      (", durability" ~> num) ~
      (", flavor" ~> num) ~
      (", texture" ~> num) ~
      (", calories" ~> num) ^^ {
      case i ~ cap ~ d ~ f ~ t ~ cal =>
        (i, Ingredient(capacity = cap, durability = d, flavor = f, texture = t, calories = cal))
    }
  }

  lazy val parsed                = inputs.parseLinesBy(lines)
  lazy val ingredients           = parsed.distinctBy(_._1).sortBy(_._1)
  lazy val ingredientNames       = ingredients.map(_._1)
  lazy val ingredientColumnOrder = ingredients.zipWithIndex.toMap
  lazy val ingredientMatrix = DenseMatrix.horzcat(ingredients.map {
    case (_, i) =>
      DenseMatrix.create[Int](
        5,
        1,
        Array(
          i.capacity,
          i.durability,
          i.flavor,
          i.texture,
          i.calories
        )
      )
  }: _*)

  def scoreSansCals(dist: Vector[Int]): Int = {
    val v        = (ingredientMatrix * dist).map(Math.max(0, _))
    val sansCals = v(0 to -2) // drop Calories, which is the last row of the vector
    product(sansCals)
  }
  def score500Cals(dist: Vector[Int]): Int = {
    val v        = (ingredientMatrix * dist).map(Math.max(0, _))
    val sansCals = v(0 to -2) // drop Calories, which is the last row of the vector
    (if (v(-1) == 500) 1 else 0) * product(sansCals)
  }

  override def part1: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] =
        (for {
          w <- 0 to 100
          x <- 0 to (100 - w)
          y <- 0 to (100 - w - x)
          z = 100 - w - x - y
        } yield (w, x, y, z) -> scoreSansCals(Vector(Array(w, x, y, z))))
          .maxBy(_._2)
          .zio
    }.some
  override def part2: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] =
        (for {
          w <- 0 to 100
          x <- 0 to (100 - w)
          y <- 0 to (100 - w - x)
          z = 100 - w - x - y
        } yield (w, x, y, z) -> score500Cals(Vector(Array(w, x, y, z))))
          .maxBy(_._2)
          .zio
    }.some

  def inputs = in2

  val in2 = """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
              |Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3""".stripMargin
  val in3 = ""

  override def in: String = ""
}
