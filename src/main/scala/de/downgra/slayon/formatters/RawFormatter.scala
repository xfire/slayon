package de.downgra.slayon.formatters

import java.io.{Writer, OutputStreamWriter}
import de.downgra.slayon.tokens.Token

object RawFormatter extends Formatter {

  override def format(tokens: Seq[Token], writer: Writer) = tokens.foreach { token =>
    writer.write(token.toString + "\n")
  }

}

// vim: set ts=2 sw=2 et:
