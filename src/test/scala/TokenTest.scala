package de.downgra.slayon

import lexers.Lexer
import tokens.Token

import org.scalatest.matchers.ShouldMatchers

trait TokenTesters {
  this: ShouldMatchers =>
  
  val lexer: Lexer

  def testPositiv(value: Token) {
    testPositiv(value.content, List(value))
  }

  def testPositiv(value: String, result: List[Token]) {
    val res = lexer.parse(value)
    res match {
      case Left(s) => fail(s)
      case Right(r) =>
        r should be === result
        r map (_.content) mkString("") should be === value
    }
  }

}

// vim: set ts=2 sw=2 et:
