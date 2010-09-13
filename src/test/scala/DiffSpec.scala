package de.downgra.slayon

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import lexer.DiffLexer
import token.{Token, Text, Whitespace}
import token.Generics.{Inserted, Deleted, Subheading, Heading}

class DiffSpec extends FlatSpec with ShouldMatchers {

  "Empty input" should "result in an empty list of tokens" in {
    val res = DiffLexer.parse("")
    res.isRight should be === true
    res.right.get.length should be === 0
  }


  "A single normal line" should "produce a single text token" in {
    testPositiv(Text("abcde"))
    testPositiv(Text("test 123"))
    testPositiv(Text(" foo bar"))
  }

  "A single line with an insertion" should "produce a single insertion token" in {
    testPositiv(Inserted("+abcde"))
    testPositiv(Inserted("+++test"))
  }

  "A single line with an deletion" should "produce a single deletion token" in {
    testPositiv(Deleted("-ab cd e"))
    testPositiv(Deleted("---test"))
  }

  "A single line with an heading" should "produce a single heading token" in {
    testPositiv(Heading("Index: foo"))
    testPositiv(Heading("=foo"))
  }

  "A single line with an subheading" should "produce a single subheading token" in {
    testPositiv(Subheading("@bar"))
    testPositiv(Subheading("@spam eggs"))
  }

  "A simple input with all possible types" should "produce the correct resultlist" in {
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
