package de.downgra.slayon.lexer

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import util.parsing.combinator.{Parsers, RegexParsers}

import de.downgra.slayon.token.{Token, Text, Whitespace, Keyword, Operator}
import de.downgra.slayon.token.{Comments, Names, Keywords}

object JavaLexer extends RegexLexer {
  val name = "Java"
  val aliases = List("java")
  val filenames = List("*.java")
  val mimetypes = List("text/x-java")

  override val defaultRegexFlags = "(?m)"

  private def ws = """\s*""".re

  private def lineBreak = """[\n\r]+""".re
  private def commentSingle = """//.*""".re
  private def commentMulti = """/\*[^\*]*\*/""".re // no nestes comments
  private def decorator = """@[\p{Alpha}_][\p{Alnum}_\.]*""".re
  private def keyword = ("""(assert|break|case|catch|continue|default|do|""" +
                         """else|finally|for|if|goto|instanceof|new|return|""" +
                         """switch|this|throw|try|while)\b""").re
  private def declKeyword = ("""(abstract|const|enum|extends|final|implements|""" +
                             """native|private|protected|public|static|strictfp|""" +
                             """super|synchronized|throws|transient|volatile)\b""").re
  private def typeKeyword = """(boolean|byte|char|double|float|int|long|short|void)\b""".re


  private def methodModifier = ("""(public|protected|private|static|strictfp|native|""" +
                                """abstract|final|synchronized)\b""").re
  private def methodReturnType = """[a-zA-Z_][a-zA-Z0-9_\.\[\]]*""".re
  private def methodName = """[a-zA-Z_][a-zA-Z0-9_]*""".re
  private def methodDefinition = ((methodModifier ~ """\s+""".re)*) ~ ws ~ methodReturnType ~ ws ~
                                 methodName ~ ws ~ """\(""".re


  private def java = 
    ( commentSingle %% Comments.Single
    | commentMulti  %% Comments.Multiline
    | decorator %% Names.Decorator
    | methodDefinition *% (List(Keywords.Declaration, Text),
                           List(Text, Keywords.Type, Text, Names.Function, Text, Operator))
    | keyword %% Keyword
    | declKeyword %% Keywords.Declaration
    | typeKeyword %% Keywords.Type
    | lineBreak %% Whitespace
    ) *

  def parse(value: String): Result = makeResult(parseAll(java, value))
}

// vim: set ts=2 sw=2 et:
