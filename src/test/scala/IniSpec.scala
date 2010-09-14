package de.downgra.slayon

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import lexer.IniLexer
import token.{Token, Whitespace, Comment, Keyword, String => StringToken, Operator}
import token.Names.Attribute

class IniSpec extends FunSuite with ShouldMatchers with TokenTesters {

  val lexer = IniLexer

  test("empty input") {
    val res = lexer.parse("")
    res.isRight should be === true
    res.right.get.length should be === 0
  }


  test("a single normal line only with spaces") {
    testPositiv(Whitespace("    "))
    testPositiv(Whitespace("  \t\t  \t"))
  }

  test("a single line specifing a comment") {
    testPositiv(Comment(";foo bar"))
    testPositiv(Comment("#test abc"))
  }

  test("a single line specifing a section") {
    testPositiv(Keyword("[]"))
    testPositiv(Keyword("[foo]"))
    testPositiv(Keyword("[foo bar]"))
  }

  test("a single line specifing a key/value pair") {
    testPositiv("key = value",
                List(Attribute("key"),
                     Whitespace(" "),
                     Operator("="),
                     Whitespace(" "),
                     StringToken("value")))
  }

  test("a single line specifing a key/value pair without spaces") {
    testPositiv("key=value",
                List(Attribute("key"),
                     Operator("="),
                     StringToken("value")))
  }

  test("a simple input with all possible types") {
    testPositiv("""|# this is a test
                   |
                   |[section 1]
                   |k1=v 1
                   |k2  =   v2
                   |[section 2]
                   |k3= v3
                   |
                   |[section 3]
                   |
                   |k4 =v4
                   |[section 4]
                   |[section 5]""".stripMargin,
                List(Comment("# this is a test"), Whitespace("\n\n"),
                     Keyword("[section 1]"), Whitespace("\n"),
                     Attribute("k1"), Operator("="), StringToken("v 1"), Whitespace("\n"),
                     Attribute("k2"), Whitespace("  "), Operator("="), Whitespace("   "), StringToken("v2"),
                     Whitespace("\n"),
                     Keyword("[section 2]"), Whitespace("\n"),
                     Attribute("k3"), Operator("="), Whitespace(" "), StringToken("v3"),
                     Whitespace("\n\n"),
                     Keyword("[section 3]"),
                     Whitespace("\n\n"),
                     Attribute("k4"), Whitespace(" "), Operator("="), StringToken("v4"), Whitespace("\n"),
                     Keyword("[section 4]"), Whitespace("\n"),
                     Keyword("[section 5]")))
  }

}

// vim: set ts=2 sw=2 et:
