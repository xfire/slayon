package de.downgra.slayon.lexer

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import util.parsing.combinator.{Parsers, RegexParsers}

import de.downgra.slayon.token.{Token, Text, Comment, Keyword, String => StringToken, Operator}
import de.downgra.slayon.token.Names.Attribute

object IniLexer extends RegexLexer {
  val name = "INI"
  val aliases = List("ini", "cfg")
  val filenames = List("*.ini", "*.cfg")
  val mimetypes = List("text/x-ini")

  override val skipWhitespace = false

  private def comment = line("""[;#].*""", Comment)
  private def section = line("""\[[^]]*\]""", Keyword)
  private def text = line("""\s+""", Text)
  private def keyValuePair = byGroups("""(?m)^(.*?)([ \t]*)(=)([ \t]*)(.*)(\n?)""".r,
    List(Attribute, Text, Operator, Text, StringToken, Text))

  private def ini = (comment | section | keyValuePair | text) *

  def parse(value: String): Result = makeResult(parseAll(ini, value))
}

// vim: set ts=2 sw=2 et:
