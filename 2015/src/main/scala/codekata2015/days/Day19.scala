package codekata2015
package days

import cats.data.NonEmptyList
import com.codecommit.gll._
import java.util
import org.leibnizcenter.cfg.algebra.semiring.dbl.LogSemiring
import org.leibnizcenter.cfg.category.Category
import org.leibnizcenter.cfg.category.nonterminal.NonTerminal
import org.leibnizcenter.cfg.category.terminal.Terminal
import org.leibnizcenter.cfg.category.terminal.stringterminal.ExactStringTerminal
import org.leibnizcenter.cfg.earleyparser.{Parser => EParser, _}
import org.leibnizcenter.cfg.grammar.Grammar
import org.leibnizcenter.cfg.token.{Token, Tokens}
import scala.jdk.CollectionConverters._
import zio.RIO

object Day19 extends Puzzle with RegexParsers {
  type ATOM               = Atom
  override type PuzzleOut = Any
  override def dayNum: Int = 19

  override protected val skipWhitespace = false

  lazy val element: Parser[String] = "[A-Za-z][a-z]*".r
  lazy val substLine: Parser[(String, NonEmptyList[String])] = (element <~ " => ") ~ element.+ ^^ { (e, es) =>
    e -> NonEmptyList.fromListUnsafe(es)
  }
  lazy val substLines: Parser[Seq[(String, NonEmptyList[String])]] = substLine ++ "\n"

  lazy val substs = substLines(inputs(0))
    .map {
      case Success(value, _) => value
      case f =>
        f.debug
        ???
    }
    .toList
    .head
    .groupBy(_._1)
    .view
    .mapValues(_.map(_._2))
    .toMap

  override def part1: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = {
        lazy val startLines: Parser[Seq[String]] = element.+
        val start = startLines(inputs(1))
          .map {
            case Success(value, _) => value
            case f =>
              f.debug
              ???

          }
          .toList
          .head

        start.indices
          .flatMap { idx =>
            val (prefix, suffix) = start.splitAt(idx)
            val sub              = suffix.head
            substs.getOrElse(sub, Seq.empty).map(repl => prefix ++ repl.toList ++ suffix.drop(1))
          }
          .toSet
          .size
      }.zio
    }.some

  override def part2: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = {
        lazy val substLine2: Parser[(String, NonEmptyList[String])] = ("[A-Za-z]".r <~ " => ") ~ "[A-Za-z]".r.+ ^^ {
          (e, es) =>
            e -> NonEmptyList.fromListUnsafe(es)
        }
        lazy val substLines2: Parser[Seq[(String, NonEmptyList[String])]] = substLine2 ++ "\n"

        val terminals: Map[String, Terminal[String]] =
          (('w' to 'z') ++ ('A' to 'L')).map(_.toString).map(t => t -> new ExactStringTerminal(t)).toMap
        val e: NonTerminal = Category.nonTerminal("e")
        val nonTerminals: Map[String, NonTerminal] =
          ('A' to 'L').map(_.toString).map(t => t -> Category.nonTerminal(t)).toMap + ("e" -> e)
        val symbols: Map[String, Category] = terminals ++ nonTerminals

        val p = 0.999

        val substs =
          (substLines2(inputs(0))
            .map {
              case Success(value, _) => value
              case f =>
                f.debug
                ???
            }
            .toList
            .head: Seq[(String, NonEmptyList[String])])
            .map { case (s, rhs) => s -> rhs.toList.map(symbols) }

        val grammar: Grammar[String] = {
          val t = terminals
          val n = nonTerminals
          val s = symbols

          val builder = ('A' to 'L')
            .map(_.toString)
            .foldLeft(new Grammar.Builder[String]()) { case (g, s) => g.addRule(1, n(s), t(s)) }
          substs
            .foldLeft(builder) {
              case (b, (s, symbols)) =>
                b.addRule(p, n(s), symbols: _*)
            }
            .setSemiring(LogSemiring.get())
            .build()
        }
        val start = inputs(1)
        val tokens: util.List[Token[String]] = Tokens.tokenize(start, "")
        val parse1 = new EParser(grammar).getViterbiParseWithScore(e, tokens)

        def countSteps(t: ParseTree): Int = {
          val children = Option(t.getChildren).map(_.asScala).toSeq.flatten
          (t.category match {
            case _: Terminal[_] => 0
            case _: NonTerminal if children.size == 1 => 0
            case _: NonTerminal => 1
          }) + children.map(countSteps).sum
        }
//        parse1.debug
        countSteps(parse1.parseTree)
      }.zio
    }.some

  def inputs = in.split("\n\n")

  lazy val in2 = """H => HO
                   |H => OH
                   |O => HH
                   |
                   |HOH""".stripMargin
  lazy val in3 = """H => HO
                   |H => OH
                   |O => HH
                   |e => H
                   |e => O
                   |
                   |HOHOHO""".stripMargin

  // This requires a bit of explanation.  In a text editor, I element-by-element renamed multicharacter
  // nonterminal elements (those with production rules) to single-capital-letter names, then all
  // terminal elements (those without production rules) to single-lowercase-letter names.
  override def in: String = ""
}
