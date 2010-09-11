package de.downgra.slayon.lexer

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import util.parsing.combinator.{Parsers, RegexParsers}

import de.downgra.slayon.token.Token
import de.downgra.slayon.token.Text
import de.downgra.slayon.token.Generics.{Inserted, Deleted, Subheading, Heading}

object DiffLexer extends RegexLexer {
  val name = "Diff"
  val aliases = List("diff", "udiff")
  val filenames = List("*.diff", "*.patch")
  val mimetypes = List("text/x-diff", "text/x-patch")


  override val skipWhitespace = false

  private def inserted = line("""\+.*""", Inserted)
  private def deleted = line("""-.*""", Deleted)
  private def subheading = line("""@.*""", Subheading)
  private def headingA = line("""Index.*""", Heading)
  private def headingB = line("""=.*""", Heading)
  private def text = line(""".*""", Text)
  private def diff = (subheading | headingA | headingB | inserted | deleted | text) *

  def parse(value: String): Result = makeResult(parseAll(diff, value))
}

// vim: set ts=2 sw=2 et:
