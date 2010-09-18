package de.downgra.slayon.lexers

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import util.parsing.combinator.{Parsers, RegexParsers}

import de.downgra.slayon.tokens.{Token, Whitespace, Keyword, Operator,
                                 String => LString, Name}
import de.downgra.slayon.tokens.{Comments, Names, Keywords, Strings, Numbers}

object JavaLexer extends RegexLexer {
  val name = "Java"
  val aliases = List("java")
  val filenames = List("*.java")
  val mimetypes = List("text/x-java")

  override val defaultRegexFlags = "(?m)"

  private def ws = """\s+""".re
  private def wso = """\s*""".re

  private def jlineBreak = """[\n\r]+""".re
  private def jcommentSingle = """//.*""".re
  private def jcommentMulti = """/\*[^\*]*\*/""".re // no nestes comments
  private def jdecorator = """@[\p{Alpha}_][\p{Alnum}_\.]*""".re
  private def jkeyword = ("""(assert|break|case|catch|continue|default|do|""" +
                         """else|finally|for|if|goto|instanceof|new|return|""" +
                         """switch|this|throw|try|while)\b""").re
  private def jdeclKeyword = ("""(abstract|const|enum|extends|final|implements|""" +
                             """native|private|protected|public|static|strictfp|""" +
                             """super|synchronized|throws|transient|volatile)\b""").re
  private def jtypeKeyword = """(boolean|byte|char|double|float|int|long|short|void)\b""".re
  private def jpackage = "package" ~ ws
  private def jconstant = """(true|false|null)\b""".re
  private def jclassInterface = """class|interface""".re ~ ws ~ """[a-zA-Z_][a-zA-Z0-9_]+""".re
  private def jimport = "import" ~ ws ~ """[a-zA-Z0-9_.]+\*?""".re
  // the vim scala syntax highlighter don't like the following line ;)
  private def jstring = """\"(\\\\|\\"|[^"])*\"""".re
  private def jchar = """'(\\.|[^\\]|\\u[0-9a-fA-F]{4}|\\[0-3][0-7]{0,2})'""".re
  private def jattribute = """(\.)""".re ~ """([a-zA-Z_][a-zA-Z0-9_]*)""".re
  private def jlabel = """[a-zA-Z_][a-zA-Z0-9_]*:""".re
  private def jname = """[a-zA-Z_\$][a-zA-Z0-9_]*""".re
  private def joperator = """[~\^\*!%&\[\]\(\)\{\}<>\|+=:;,./?-]""".re

  private def jfloat = """(([1-9][0-9]+\.[0-9]*)|([1-9]*\.[0-9]+)|([1-9]+(?=[eE])))([eE][-+]?[0-9]+)?[fd]?""".re
  private def jhex = """0x[0-9a-fA-F]+""".re
  private def jinteger = """[0-9]+[lL]?""".re

  // method
  private def methodModifier = ("""(public|protected|private|static|strictfp|native|""" +
                                """abstract|final|synchronized)\b""").re
  private def methodReturnType = """[a-zA-Z_][a-zA-Z0-9_\.\[\]]+""".re
  private def methodName = """[a-zA-Z_][a-zA-Z0-9_]+""".re
  private def jmethodDefinition = ((methodModifier ~ """\s+""".re)*) ~ wso ~
                                 methodReturnType ~ wso ~
                                 methodName ~ wso ~ """\(""".re

  private def java = 
    ( jcommentSingle %% Comments.Single
    | jcommentMulti  %% Comments.Multiline
    | jdecorator %% Names.Decorator
    | jmethodDefinition *% (List(Keywords.Declaration, Whitespace),
                            List(Whitespace, Keywords.Type, Whitespace, Names.Function, Whitespace, Operator))
    | jkeyword %% Keyword
    | jdeclKeyword %% Keywords.Declaration
    | jtypeKeyword %% Keywords.Type
    | jconstant %% Keywords.Constant
    | jpackage %%% (Keywords.Namespace, Whitespace)
    | jclassInterface %%% (Keywords.Declaration, Whitespace, Names.Class)
    | jimport %%% (Keywords.Namespace, Whitespace, Names.Namespace)
    | jstring %% LString
    | jchar %% Strings.Char
    | jattribute %%% (Operator, Names.Attribute)
    | jlabel %% Names.Label
    | jname %% Name
    | joperator %% Operator
    | jhex %% Numbers.Hex
    | jfloat %% Numbers.Float
    | jinteger %% Numbers.Integer
    | jlineBreak %% Whitespace
    | ws %% Whitespace
    ) *

  def parse(value: String): Result = makeResult(parseAll(java, value))
}

// vim: set ts=2 sw=2 et:
