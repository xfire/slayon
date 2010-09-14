package de.downgra.slayon

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import lexer.DiffLexer
import token.{Token, Text, Whitespace}
import token.Generics.{Inserted, Deleted, Subheading, Heading}

class DiffSpec extends FunSuite with ShouldMatchers {

  test("empty input") {
    val res = DiffLexer.parse("")
    res.isRight should be === true
    res.right.get.length should be === 0
  }


  test("A single normal line") {
    testPositiv(Text("abcde"))
    testPositiv(Text("test 123"))
    testPositiv(Text(" foo bar"))
  }

  test("a single line with an insertion") {
    testPositiv(Inserted("+abcde"))
    testPositiv(Inserted("+++test"))
  }

  test("a single line with an deletion") {
    testPositiv(Deleted("-ab cd e"))
    testPositiv(Deleted("---test"))
  }

  test("a single line with an heading") {
    testPositiv(Heading("Index: foo"))
    testPositiv(Heading("=foo"))
  }

  test("a single line with an subheading") {
    testPositiv(Subheading("@bar"))
    testPositiv(Subheading("@spam eggs"))
  }

  test("a simple input with all possible types") {
    testPositiv("-ab\n+cd\n=foo\ntesttext line\nIndex:bar\n@spam\n",
                List(Deleted("-ab"), Whitespace("\n"),
                     Inserted("+cd"), Whitespace("\n"),
                     Heading("=foo"), Whitespace("\n"),
                     Text("testtext line"), Whitespace("\n"),
                     Heading("Index:bar"), Whitespace("\n"),
                     Subheading("@spam"), Whitespace("\n")))
  }

  private def testPositiv(value: Token) {
    testPositiv(value.content, List(value))
  }

  private def testPositiv(value: String, result: List[Token]) {
    val res = DiffLexer.parse(value)
    res match {
      case Left(s) => fail(s)
      case Right(r) =>
        r should be === result
        r map (_.content) mkString("") should be === value
    }
  }

}

// vim: set ts=2 sw=2 et:
