package de.downgra.slayon.lexers

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import util.parsing.combinator.{Parsers, RegexParsers}

import de.downgra.slayon.tokens.{Token, Text, Whitespace}
import de.downgra.slayon.tokens.Generics.{Inserted, Deleted, Subheading, Heading}

object DiffLexer extends RegexLexer {
  val name = "Diff"
  val aliases = List("diff", "udiff")
  val filenames = List("*.diff", "*.patch")
  val mimetypes = List("text/x-diff", "text/x-patch")

  override val defaultRegexFlags = "(?m)"

  private def inserted = """^\+.*$""".re
  private def deleted = """^-.*$""".re
  private def subheading = """^@.*$""".re
  private def headingA = """^Index.*$""".re
  private def headingB = """^=.*$""".re
  private def text = """[^\n\r]+""".re
  private def ws = """[\n\r]+""".re

  private def diff = 
    ( subheading %% Subheading
    | headingA %% Heading
    | headingB %% Heading
    | inserted %% Inserted
    | deleted %% Deleted
    | text %% Text
    | ws %% Whitespace
    ) *

  def parse(value: String): Result = makeResult(parseAll(diff, value))
}

// vim: set ts=2 sw=2 et:
