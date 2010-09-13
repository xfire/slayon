package de.downgra.slayon.lexer

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import util.parsing.combinator.{Parsers, RegexParsers}

import de.downgra.slayon.token.{Token, Whitespace, Comment, Keyword, String => StringToken, Operator}
import de.downgra.slayon.token.Names.Attribute

object IniLexer extends RegexLexer {
  val name = "INI"
  val aliases = List("ini", "cfg")
  val filenames = List("*.ini", "*.cfg")
  val mimetypes = List("text/x-ini")

  override val defaultRegexFlags = "(?m)"

  private def comment = """[;#].*$""".re
  private def section = """\[[^]]*\]""".re
  private def ws = """\s+""".re

  private def wso = """\s*""".re
  private def key = """[^\s=#;]+""".re
  private def value = """[^\n\r]+""".re
  private def operator = """=""".re
  private def keyValuePair = key ~ wso ~ operator ~ wso ~ value

  private def ini =
    ( comment %% Comment
    | section %% Keyword
    | keyValuePair %%% (Attribute, Whitespace, Operator, Whitespace, StringToken)
    | ws %% Whitespace
    ) *

  def parse(value: String): Result = makeResult(parseAll(ini, value))
}

// vim: set ts=2 sw=2 et:
