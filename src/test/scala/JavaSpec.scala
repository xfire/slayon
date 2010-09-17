package de.downgra.slayon

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import lexer.JavaLexer
import token.{Token, Whitespace, Keyword, Operator, String => LString, Name}
import token.{Comments, Names, Keywords, Strings, Numbers}

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
    testPositiv("void foo (", List(Keywords.Type("void"), Whitespace(" "),
                                   Names.Function("foo"), Whitespace(" "),
                                   Operator("(")))
    testPositiv("public Person getPerson(", List(Keywords.Declaration("public"), Whitespace(" "),
                                                 Keywords.Type("Person"), Whitespace(" "),
                                                 Names.Function("getPerson"), Operator("(")))

    testPositiv("public static int getX(", List(Keywords.Declaration("public"), Whitespace(" "),
                                                Keywords.Declaration("static"), Whitespace(" "),
                                                Keywords.Type("int"), Whitespace(" "),
                                                Names.Function("getX"), Operator("(")))
  }

  test("single package definition") {
    testPositiv("package ", List(Keywords.Namespace("package"), Whitespace(" ")))
    testPositiv("package   ", List(Keywords.Namespace("package"), Whitespace("   ")))
  }

  test("single constants") {
    "true|false|null".split('|').foreach(s => testPositiv(Keywords.Constant(s)))
  }

  test("single class/interface definition") {
    for(what <- List("class", "interface")) {
      testPositiv(what + " FooBar", List(Keywords.Declaration(what), Whitespace(" "), Names.Class("FooBar")))
      testPositiv(what + "  FooBar", List(Keywords.Declaration(what), Whitespace("  "), Names.Class("FooBar")))
      testPositiv(what + "\nFooBar", List(Keywords.Declaration(what), Whitespace("\n"), Names.Class("FooBar")))
    }
  }

  test("single import statement") {
    testPositiv("import foo", List(Keywords.Namespace("import"), Whitespace(" "), Names.Namespace("foo")))
    testPositiv("import\nfoo.bar", List(Keywords.Namespace("import"),
                                        Whitespace("\n"),
                                        Names.Namespace("foo.bar")))
    testPositiv("import  foo.bar.*", List(Keywords.Namespace("import"),
                                          Whitespace("  "),
                                          Names.Namespace("foo.bar.*")))
  }

  test("single string") {
    testPositiv(LString("\"foo\""))
    testPositiv(LString("\"foo bar\""))
    testPositiv(LString("\"foo\\nbar\""))
    testPositiv(LString("\"foo \\t  bar\\\"spam\\\"\""))
  }

  test("single char") {
    testPositiv(Strings.Char("'\\n'"))
    testPositiv(Strings.Char("'.'"))
    testPositiv(Strings.Char("' '"))
    testPositiv(Strings.Char("'a'"))
    testPositiv(Strings.Char("'Z'"))
    testPositiv(Strings.Char("'9'"))
    testPositiv(Strings.Char("'\\0'"))
    testPositiv(Strings.Char("'\\30'"))
    testPositiv(Strings.Char("'\\377'"))
    testPositiv(Strings.Char("'\\u0000'"))
    testPositiv(Strings.Char("'\\uffff'"))
  }

  test("single attribute") {
    testPositiv("._foo", List(Operator("."), Names.Attribute("_foo")))
    testPositiv(".foo", List(Operator("."), Names.Attribute("foo")))
    testPositiv("._foo23", List(Operator("."), Names.Attribute("_foo23")))
    testPositiv(".foo23", List(Operator("."), Names.Attribute("foo23")))
    testPositiv("._foo_23", List(Operator("."), Names.Attribute("_foo_23")))
    testPositiv(".foo_23", List(Operator("."), Names.Attribute("foo_23")))
  }

  test("single label") {
    testPositiv(Names.Label("foo:"))
    testPositiv(Names.Label("_f_oo42:"))
  }

  test("single name") {
    testPositiv(Name("foo"))
    testPositiv(Name("_foo_"))
    testPositiv(Name("$foo_"))
  }

  test("single operator") {
    "~^*!%&[](){}<>|+=:;,.?-".foreach(s => testPositiv(Operator(s.toString)))
  }

  test("single float") {
    testPositiv(Numbers.Float("1.41421"))
    testPositiv(Numbers.Float("2e6"))
    testPositiv(Numbers.Float("3.77f"))
    testPositiv(Numbers.Float("3.7E-4f"))
  }

  test("single hex") {
   testPositiv(Numbers.Hex("0xCAFE"))
   testPositiv(Numbers.Hex("0xCaFeBaBe"))
  }

  test("single integer") {
   testPositiv(Numbers.Integer("0"))
   testPositiv(Numbers.Integer("23l"))
   testPositiv(Numbers.Integer("42L"))
  }

  test("simple example") {
    testPositiv("""|public class HalloWelt {
                   |  public static void main(String[] args) {
                   |    System.out.println("Hallo Welt!");
                   |  }
                   |}
                   |""".stripMargin,
                List(Keywords.Declaration("public"), Whitespace(" "), Keywords.Declaration("class"),
                     Whitespace(" "), Names.Class("HalloWelt"), Whitespace(" "), Operator("{"),
                     Whitespace("\n"), Whitespace("  "), Keywords.Declaration("public"),
                     Whitespace(" "), Keywords.Declaration("static"), Whitespace(" "),
                     Keywords.Type("void"), Whitespace(" "), Names.Function("main"), Operator("("),
                     Name("String"), Operator("["), Operator("]"), Whitespace(" "), Name("args"),
                     Operator(")"), Whitespace(" "), Operator("{"), Whitespace("\n"),
                     Whitespace("    "), Name("System"), Operator("."), Names.Attribute("out"),
                     Operator("."), Names.Attribute("println"), Operator("("), LString("\"Hallo Welt!\""),
                     Operator(")"), Operator(";"), Whitespace("\n"), Whitespace("  "), Operator("}"),
                     Whitespace("\n"), Operator("}"), Whitespace("\n")))

  }
}

// vim: set ts=2 sw=2 et:
