package de.downgra.slayon

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import lexer.JavaLexer
import token.{Token, Text, Whitespace, Keyword, Operator}
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

  test("single keywords") {
    val words = "assert|break|case|catch|continue|default|do|" +
                "else|finally|for|if|goto|instanceof|new|" +
                "return|switch|this|throw|try|while"

    words.split('|').foreach(s => testPositiv(Keyword(s)))
  }

  test("single declaration keywords") {
    val words = "abstract|const|enum|extends|final|implements|native|" +
                "private|protected|public|static|strictfp|super|" +
                "synchronized|throws|transient|volatile"

    words.split('|').foreach(s => testPositiv(Keywords.Declaration(s)))
  }

  test("single type keywords") {
    val words = "boolean|byte|char|double|float|int|long|short|void"

    words.split('|').foreach(s => testPositiv(Keywords.Type(s)))
  }

  test("single method definition") {
    testPositiv("void foo (", List(Keywords.Type("void"), Text(" "),
                                   Names.Function("foo"), Text(" "),
                                   Operator("(")))
    testPositiv("public Person getPerson(", List(Keywords.Declaration("public"), Text(" "),
                                                 Keywords.Type("Person"), Text(" "),
                                                 Names.Function("getPerson"), Operator("(")))

    testPositiv("public static int getX(", List(Keywords.Declaration("public"), Text(" "),
                                                Keywords.Declaration("static"), Text(" "),
                                                Keywords.Type("int"), Text(" "),
                                                Names.Function("getX"), Operator("(")))
  }
}

// vim: set ts=2 sw=2 et:
