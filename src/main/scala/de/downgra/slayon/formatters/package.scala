package de.downgra.slayon

import java.io.Writer
import tokens.Token

package object formatters {

  trait Formatter {
    def format(tokens: Seq[Token], writer: Writer)
  }

}

// vim: set ts=2 sw=2 et:
