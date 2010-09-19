package de.downgra.slayon

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import formatters.RawFormatter
import tokens.{Text, Number, Operator}

import java.io.StringWriter

class RawFormatterSpec extends FunSuite with ShouldMatchers {

  test("empty token list") {
    val w = new StringWriter
    RawFormatter.format(List(),w)

    w.toString should be === ""
  }

  test("token list with 1 element") {
    val w = new StringWriter
    RawFormatter.format(List(Text("foo")), w)
    w.toString should be === "Text(foo)\n"
  }

  test("token list with >1 elements") {
    val w = new StringWriter
    RawFormatter.format(List(Text("foo"),
                             Number("21"),
                             Operator("+")
                            ),w)
    w.toString should be === "Text(foo)\nNumber(21)\nOperator(+)\n"
  }

}

// vim: set ts=2 sw=2 et:
