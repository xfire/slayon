package de.downgra.slayon

import tokens.Token

package object lexers {
  trait Lexer {
    /** result type of the lexing process */
    type Result = Either[String, Seq[Token]]

    /** informations about the lexer */
    val name: String
    val aliases: List[String]
    val filenames: List[String]
    val mimetypes: List[String]

    /** the parse method */
    def parse(value: String): Result
  }
}

// vim: set ts=2 sw=2 et:
