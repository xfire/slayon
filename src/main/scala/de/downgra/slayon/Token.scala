package de.downgra.slayon
package token

import java.lang.{String => JString}

sealed trait Token {
  val content: JString
}

case class Text(content: JString) extends Token
case class Whitespace(content: JString) extends Token
case class Error(content: JString) extends Token

case class Literal(content: JString) extends Token
case class Punctuation(content: JString) extends Token
case class Operator(content: JString) extends Token

case class Generic(content: JString) extends Token
object Generic {
  case class Deleted(content: JString) extends Token
  case class Emph(content: JString) extends Token
  case class Error(content: JString) extends Token
  case class Heading(content: JString) extends Token
  case class Inserted(content: JString) extends Token
  case class Output(content: JString) extends Token
  case class Prompt(content: JString) extends Token
  case class Strong(content: JString) extends Token
  case class Subheading(content: JString) extends Token
  case class Traceback(content: JString) extends Token
}

case class Comment(content: JString) extends Token
object Comment {
  case class Multiline(content: JString) extends Token
  case class Preproc(content: JString) extends Token
  case class Single(content: JString) extends Token
  case class Special(content: JString) extends Token
}

case class Number(content: JString) extends Token
object Number {
  case class Float(content: JString) extends Token
  case class Hex(content: JString) extends Token
  case class Integer(content: JString) extends Token
  case class Long(content: JString) extends Token
  case class Oct(content: JString) extends Token
}

case class String(content: JString) extends Token
object String {
  case class Backtick(content: JString) extends Token
  case class Char(content: JString) extends Token
  case class Doc(content: JString) extends Token
  case class Double(content: JString) extends Token
  case class Escape(content: JString) extends Token
  case class Heredoc(content: JString) extends Token
  case class Interpol(content: JString) extends Token
  case class Other(content: JString) extends Token
  case class Regex(content: JString) extends Token
  case class Single(content: JString) extends Token
  case class Symbol(content: JString) extends Token
}

case class Name(content: JString) extends Token
object Name {
  case class Attribute(content: JString) extends Token
  case class Builtin(content: JString) extends Token
  case class Class(content: JString) extends Token
  case class Constant(content: JString) extends Token
  case class Decorator(content: JString) extends Token
  case class Entity(content: JString) extends Token
  case class Exception(content: JString) extends Token
  case class Function(content: JString) extends Token
  case class Property(content: JString) extends Token
  case class Label(content: JString) extends Token
  case class Namespace(content: JString) extends Token
  case class Other(content: JString) extends Token
  case class Tag(content: JString) extends Token
  case class Variable(content: JString) extends Token
  object Variable {
    case class Class(content: JString) extends Token
    case class Global(content: JString) extends Token
    case class Instance(content: JString) extends Token
  }
}

case class Keyword(content: JString) extends Token
object Keyword {
  case class Constant(content: JString) extends Token
  case class Declaration(content: JString) extends Token
  case class Namespace(content: JString) extends Token
  case class Pseudo(content: JString) extends Token
  case class Reserved(content: JString) extends Token
  case class Type(content: JString) extends Token
}




// vim: set ts=2 sw=2 et:
