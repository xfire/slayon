package de.downgra.slayon

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import lexer.JavaLexer
import token.{Token, Text, Whitespace, Keyword}
import token.{Comments, Names, Keywords}

class JavaSpec extends FunSuite with ShouldMatchers with TokenTesters {

  val lexer = JavaLexer

  test("empty input") {
    val res = lexer.parse("")
    res.isRight should be === true
    res.right.get.length should be === 0
  }

  test("single decorator") {
    testPositiv(Names.Decorator("@foo"))
    testPositiv(Names.Decorator("@foo.bar"))
    testPositiv(Names.Decorator("@foo.b_ar"))
    testPositiv(Names.Decorator("@_foo"))
    testPositiv(Names.Decorator("@_foo.bar"))
    testPositiv(Names.Decorator("@f_oo"))
  }

  test("single singleline comment") {
    testPositiv(Comments.Single("//foo bar spam eggs"))
    testPositiv(Comments.Single("// foo bar spam eggs"))
    testPositiv(Comments.Single("//    foo bar spam eggs"))
    testPositiv(Comments.Single("// foo bar // spam eggs"))
  }

  test("single multiline comment") {
    testPositiv(Comments.Multiline("/**/"))
    testPositiv(Comments.Multiline("/* */"))
    testPositiv(Comments.Multiline("/*foo*/"))
    testPositiv(Comments.Multiline("/* foo */"))
    testPositiv(Comments.Multiline("/* foo\nbar */"))
  }

  test("nested multiline comments") {
    // testPositiv(Comments.Multiline("/* foo\n/*bar*/\nfoo */"))
  }

  test("single keywords") {
    testPositiv(Keyword("case"))
    testPositiv(Keyword("switch"))
    testPositiv(Keyword("throw"))
    testPositiv(Keyword("instanceof"))
    testPositiv(Keyword("new"))
    testPositiv(Keyword("return"))
    testPositiv(Keyword("if"))
  }

}

// vim: set ts=2 sw=2 et:
