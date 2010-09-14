package de.downgra.slayon.lexer

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import util.parsing.combinator.{Parsers, RegexParsers}

import de.downgra.slayon.token.{Token, Text, Whitespace, Keyword}
import de.downgra.slayon.token.{Comments, Names, Keywords}

object JavaLexer extends RegexLexer {
  val name = "Java"
  val aliases = List("java")
  val filenames = List("*.java")
  val mimetypes = List("text/x-java")

  override val defaultRegexFlags = "(?m)"


  private def ws = """[\n\r]+""".re
  private def commentSingle = """//.*""".re
  private def commentMulti = """/\*[^\*]*\*/""".re
  private def decorator = """@[\p{Alpha}_][\p{Alnum}_\.]*""".re
  private def keyword = ("""(assert|break|case|catch|continue|default|do|""" +
                         """else|finally|for|if|goto|instanceof|new|return|""" +
                         """switch|this|throw|try|while)\b""").re

  private def java = 
    ( commentSingle %% Comments.Single
    | commentMulti  %% Comments.Multiline
    | decorator %% Names.Decorator
    | keyword %% Keyword
    | ws %% Whitespace
    ) *

  def parse(value: String): Result = makeResult(parseAll(java, value))
}

// vim: set ts=2 sw=2 et:
