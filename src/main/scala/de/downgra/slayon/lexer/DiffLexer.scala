package de.downgra.slayon.lexer

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import util.parsing.combinator.{Parsers, RegexParsers}

import de.downgra.slayon.token.Token
import de.downgra.slayon.token.Text
import de.downgra.slayon.token.Generics.{Inserted, Deleted, Subheading, Heading}

object DiffLexer extends Lexer with RegexParsers {
  val name = "Diff"
  val aliases = List("diff", "udiff")
  val filenames = List("*.diff", "*.patch")
  val mimetypes = List("text/x-diff", "text/x-patch")


  override val skipWhitespace = false

  private def inserted = """(?m)^\+.*\n*""".r ^^ {
    Inserted(_)
  }
  private def deleted = """(?m)^-.*\n*""".r ^^ {
    Deleted(_)
  }
  private def subheading = """(?m)^@.*\n*""".r ^^ {
    Subheading(_)
  }
  private def heading = ("""(?m)^Index.*\n*""".r | """(?m)^=.*\n*""".r) ^^ {
    Heading(_)
  }
  private def text = """(?m)^.*\n*""".r ^^ {
    Text(_)
  }
  private def diff = (subheading | heading | inserted | deleted | text) *

  def parse(value: String): Result = toResult(parseAll(diff, value))

  private def toResult(result: ParseResult[List[Token]]) = result match {
    case Success(res, _) => Right(res)
    case e: NoSuccess    => Left(e.msg)
    case _               => Left("bad result")
  }
}

// vim: set ts=2 sw=2 et:
