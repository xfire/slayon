package de.downgra.slayon.lexers

import util.matching.Regex
import util.parsing.combinator.RegexParsers

import de.downgra.slayon.tokens.Token

protected[lexers] trait RegexLexer extends Lexer with RegexParsers with TokenizerSupport {

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

}

// vim: set ts=2 sw=2 et:
