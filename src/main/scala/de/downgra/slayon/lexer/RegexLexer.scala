package de.downgra.slayon.lexer

import util.matching.Regex
import util.parsing.combinator.RegexParsers

import de.downgra.slayon.token.Token

protected[lexer] trait RegexLexer extends Lexer with RegexParsers {

  /** create a Lexer.Result from a ParseResult */
  protected def makeResult(result: ParseResult[Seq[Seq[Token]]]) = result match {
    case Success(res, _) => Right(pruneResult(res.flatten))
    case e: NoSuccess    => Left(e.msg)
    case _               => Left("bad result")
  }

  /** filter out empty tokens */
  protected def pruneResult(result: Seq[Token]) = result filter { _.content.nonEmpty }

  protected def line(rx: String, token: String => Token): Parser[Seq[Token]] = {
    """(?m)^%s\n?""".format(rx).r ^^ { x => List(token(x)) }
  }

  protected def byGroups(r: Regex, tokens: List[String => Token]): Parser[Seq[Token]] = {
    def nullToStr(s: String) = s match {
      case s: String => s
      case _ => ""
    }
    byGroups(r) ^^ { m =>
      val max = math.min(m.groupCount, tokens.length)
      for(i <- 1 to max) yield tokens(i - 1)(nullToStr(m.group(i)))
    }
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
