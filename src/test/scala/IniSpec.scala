package de.downgra.slayon

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import lexer.IniLexer
import token.{Token, Text, Comment, Keyword, String => StringToken, Operator}
import token.Names.Attribute


class IniSpec extends FlatSpec with ShouldMatchers {

  "Empty input" should "result in an empty list of tokens" in {
    val res = IniLexer.parse("")
    res.isRight should be === true
    res.right.get.length should be === 0
  }


  "A single normal line only with spaces" should "produce a single text token" in {
    testPositiv(Text("    "))
    testPositiv(Text("  \t\t  \t"))
  }

  "A single line starting with an ; or a #" should "produce a single comment token" in {
    testPositiv(Comment(";foo bar"))
    testPositiv(Comment("#test abc"))
  }

  "A single line specifing a section" should "produce a single keyword token" in {
    testPositiv(Keyword("[]"))
    testPositiv(Keyword("[foo]"))
    testPositiv(Keyword("[foo bar]"))
  }

  "A single line specifing a key/value pair" should "produce the correct token sequence" in {
    testPositiv("key = value",
                List(Attribute("key"),
                     Text(" "),
                     Operator("="),
                     Text(" "),
                     StringToken("value")))
  }

  "A single line specifing a key/value pair without spaces" should "produce a pruned token sequence" in {
    testPositiv("key=value",
                List(Attribute("key"),
                     Operator("="),
                     StringToken("value")))
  }

  "A simple input with all possible types" should "produce the correct resultlist" in {
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
                List(Comment("# this is a test\n"),
                     Text("\n"),
                     Keyword("[section 1]\n"),
                     Attribute("k1"), Operator("="), StringToken("v 1"), Text("\n"),
                     Attribute("k2"), Text("  "), Operator("="), Text("   "), StringToken("v2"), Text("\n"),
                     Keyword("[section 2]\n"),
                     Attribute("k3"), Operator("="), Text(" "), StringToken("v3"), Text("\n"),
                     Text("\n"),
                     Keyword("[section 3]\n"),
                     Text("\n"),
                     Attribute("k4"), Text(" "), Operator("="), StringToken("v4"), Text("\n"),
                     Keyword("[section 4]\n"),
                     Keyword("[section 5]")))
  }

  private def testPositiv(value: Token) {
    testPositiv(value.content, List(value))
  }

  private def testPositiv(value: String, result: Seq[Token]) {
    val res = IniLexer.parse(value)
    res.isRight should be === true
    // res.right.get.length should be === result.length
    res.right.get should be === result
    res.right.get map (_.content) mkString("") should be === value
  }
}

// vim: set ts=2 sw=2 et:
