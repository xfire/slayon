package de.downgra.slayon.lexer

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import util.parsing.combinator.RegexParsers

import de.downgra.slayon.token.Token

protected[lexer] trait RegexLexer extends Lexer with RegexParsers {

  protected def makeResult(result: ParseResult[List[List[Token]]]) = result match {
    case Success(res, _) => Right(res.flatten)
    case e: NoSuccess    => Left(e.msg)
    case _               => Left("bad result")
  }

  protected def line(rx: String, token: String => Token): Parser[List[Token]] = {
    """(?m)^%s\n*""".format(rx).r ^^ { x => List(token(x)) }
  }

  protected def byGroups(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) =>
          Success(matched,
                  in.drop(start + matched.end - offset))
        case None =>
          Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
      }
    }
  }
}

// vim: set ts=2 sw=2 et:
