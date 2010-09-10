package de.downgra.slayon

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import lexer.DiffLexer

class DiffSpec extends FlatSpec with ShouldMatchers {

  "An empty input" should "result in an empty list of tokens" in {
    DiffLexer.parse("").length should be === 0    
  }

}

// vim: set ts=2 sw=2 et:
