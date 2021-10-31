package common

import scala.util.parsing.combinator.Parsers

trait ParserSyntax extends Syntax {
  implicit class _ParseResultOps[A](in: Parsers#ParseResult[A]) {
    def getOrFail: A = in.tap(r => if (!r.successful) r.debug: Unit).get
  }

}