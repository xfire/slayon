package de.downgra.slayon.lexer

import scala.util.parsing.combinator.RegexParsers

import de.downgra.slayon.Token

object DiffLexer extends RegexParsers {

  def parse(value: String): List[Token] = List()

}

// vim: set ts=2 sw=2 et:
