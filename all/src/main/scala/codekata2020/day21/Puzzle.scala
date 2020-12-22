package codekata2020.day21

import codekata2020._
import scala.annotation.tailrec
import scala.util.parsing.combinator._

object Puzzle extends RegexParsers {
  private val inputs = in.linesIterator.toIndexedSeq
  type Ingredient = String
  type Allergen = String

  def parser: Parser[(List[Ingredient], List[Allergen])] =
    (rep1("""[a-z]+""".r) <~ "(contains") ~ rep1sep("""[a-z]+""".r, ",") <~ ")" ^^ { case is ~ ws => (is, ws) }

  private val parsed = inputs.map { l => parseAll(parser, l).get }
  private val allIngredients: Set[Ingredient] = parsed.flatMap(_._1).toSet
  private val allergenToPossibleIngredients: Map[Allergen, Set[Ingredient]] =
    parsed.flatMap { case (is, as) => as.map { _ -> is} }.groupBy(_._1)
      .view.mapValues { vs => vs.map(_._2.toSet).reduce(_ intersect _) }.toMap

  object Part1 {
    def solution = {
      val safe = (allIngredients -- allergenToPossibleIngredients.values.flatten.toSet)
      parsed.flatMap(_._1).count(safe(_))
    }.zio
  }

  object Part2 {
    def solution = {
      @tailrec def go(possible: Map[Allergen, Set[Ingredient]], soFar: Map[Ingredient, Allergen])
      : Map[Ingredient, Allergen] = if (possible.isEmpty) soFar else {
        val first = possible.toSeq.minBy(_._2.size)
        go(possible.view.mapValues(_ - first._2.head).toMap.filter(_._2.nonEmpty),
          soFar + (first._2.head -> first._1))
      }

      go(allergenToPossibleIngredients, Map.empty).toSeq.sortBy(_._2).map(_._1).mkString(",")
    }.zio
  }

  private lazy val in2 =
    """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
      |trh fvjkl sbzzf mxmxvkd (contains dairy)
      |sqjhc fvjkl (contains soy)
      |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin

  private lazy val in3 =
    """""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
