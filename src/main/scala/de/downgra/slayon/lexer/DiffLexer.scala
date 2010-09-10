package de.downgra.slayon.lexer

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import util.parsing.combinator.{Parsers, RegexParsers}

import de.downgra.slayon.token.Token
import de.downgra.slayon.token.Text
import de.downgra.slayon.token.Generic.{Inserted, Deleted, Subheading, Heading}

object DiffLexer extends RegexParsers {
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

  def parse(value: String): ParseResult[List[Token]] = parseAll(diff, value)
}

// vim: set ts=2 sw=2 et:
