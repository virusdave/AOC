package common

import scala.reflect.ClassTag
import scala.util.parsing.combinator.{Parsers, RegexParsers}

trait ParserSyntax extends Syntax {
  implicit class _ParserParseResultOps[A](in: Parsers#ParseResult[A]) {
    def getOrFail: A = in.tap(r => if (!r.successful) r.debug: Unit).get
  }
  implicit class _ParserTildeOps[A, B](in: Parsers#`~`[A, B]) {
    def toTuple: (A, B) = in._1 -> in._2
  }
  implicit class _ParserTilde3Ops[A,B,C](in: Parsers#`~`[Parsers#`~`[A, B], C]) {
    def toTuple3: (A, B, C) = (in._1._1, in._1._2, in._2)
  }
}

trait InRegexParserSyntax { this: RegexParsers =>
  implicit class _ParserStringOps(in: String) extends ParserSyntax {
    def parseBy[A: ClassTag](p: Parser[A]): A = parseAll(p, in).getOrFail
    def parseLinesBy[A: ClassTag](p: Parser[A]): IndexedSeq[A] = in.splitAtLinebreaksBy(parseAll(p, _).getOrFail)
    def splitAtDoubleLinebreaksAndParseChunkBy[A: ClassTag](p: Parser[A]): IndexedSeq[A] =
      in.splitAtDoubleLinebreaksBy(parseAll(p, _).getOrFail)
  }
}