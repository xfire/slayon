package de.downgra.slayon.lexer

import util.matching.Regex
import util.parsing.combinator.RegexParsers

import de.downgra.slayon.token.Token

protected[lexer] trait RegexLexer extends Lexer with RegexParsers with Chaining {

  override def skipWhitespace: Boolean = false

  /** default regex flags
   * override val defaultRegexFlags = "(?m)"
   * """.*""".re         ->   Regex("""(?m).*""")
   * """.*""".re("(?s)   ->   Regex("""(?s).*""")
   */
  val defaultRegexFlags = ""
  implicit def toRegex(s: String) = new {
    def re: Regex = new Regex(defaultRegexFlags + s)
    def re(flags: String) = new Regex(flags + s)
  }

  /** create a Lexer.Result from a ParseResult */
  protected def makeResult(result: ParseResult[Seq[Seq[Token]]]) = result match {
    case Success(res, _) => Right(pruneResult(res.flatten))
    case e: NoSuccess    => Left(e.msg)
    case _               => Left("bad result")
  }

  /** filter out empty tokens */
  protected def pruneResult(result: Seq[Token]) = result filter { _.content.nonEmpty }

  implicit def toTokenList[T <: Parser[String]](x: T): TokenToList[T] = new TokenToList(x)
  implicit def toTokenList(x: Regex): TokenToList[Parser[String]] = new TokenToList(x)
  class TokenToList[T <: Parser[String]](l: T) {
    def %%(f: (String) => Token): Parser[List[Token]] = l ^^ {x => List(f(x))}
  }
 
}

// vim: set ts=2 sw=2 et:
