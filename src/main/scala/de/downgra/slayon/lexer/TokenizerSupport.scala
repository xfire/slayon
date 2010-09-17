package de.downgra.slayon.lexer

import util.matching.Regex
import util.parsing.combinator.RegexParsers

import de.downgra.slayon.token.Token

trait TokenizerSupport {
  self: RegexParsers =>

  type FT = (String) => Token     // FunctionType
  type RT = Parser[Seq[Token]]    // ReturnType


  implicit def toTokenList(value: Parser[String]): TokenToList = new TokenToList(value)
  implicit def toTokenList(value: Regex): TokenToList = new TokenToList(value)
  protected class TokenToList(p: Parser[String]) {
    def %%(f: FT): RT = p ^^ {value => List(f(value))}
  }
 
  /** provide syntactic sugar to instances of `Parser[~[_,_]]`. this should help
   *  creating regex based tokenizers. these sugar does actually the same as the
   * `Parser.^^` function, only optimized to produce a list of tokens.
   */
  implicit def parserToTokenizerSupport[T, U](p: Parser[~[T,U]])(implicit m1: Manifest[T], m2: Manifest[U]) = new {
    def %%%(fs: FT*): RT = p ^^ { case pc => (fs, flattenParsers(pc)).zipped.map(_(_))}

    def %*%(pre: FT, rep: Seq[FT], post: FT): RT = %*%(List(pre), rep, List(post))
    def %*%(pre: FT, rep: Seq[FT], post: Seq[FT]): RT = %*%(List(pre), rep, post)
    def %*%(pre: Seq[FT], rep: Seq[FT], post: FT): RT = %*%(pre, rep, List(post))

    def *%(rep: Seq[FT], other: FT) = %*%(List(), rep, List(other))
    def *%(rep: Seq[FT], others: Seq[FT]) = %*%(List(), rep, others)

    def %*(other: FT, rep: Seq[FT]) = %*%(List(other), rep, List())
    def %*(others: Seq[FT], rep: Seq[FT]) = %*%(others, rep, List())

    def %*%(pre: Seq[FT], rep: Seq[FT], post: Seq[FT]): RT = p ^^ { case pc =>
      val flatParsers = flattenParsers(pc)
      assume (flatParsers.length >= (pre.length + post.length), "[%s, %s, %s] %s".format(pre.length, rep.length, post.length, flatParsers))
      val a = flatParsers.slice(0, pre.length)
      val b = flatParsers.slice(pre.length, flatParsers.length - post.length)
      val c = flatParsers.slice(flatParsers.length - post.length, flatParsers.length)
      
      (  (pre, a).zip
      ++ Stream.continually(rep).flatten.zip(b).toList
      ++ (post, c).zip
      ).map(t => t._1(t._2))
    }
  }

  /** helper to flatten nestes Lists, Strings and ~[_, _]
   * @param value a instance of a List, String or ~[_, _]
   * @return the flatted seq of strings
   */
   protected def flattenParsers(value: Any): Seq[String] = value match {
     case Nil        => Nil
     case l: String  => List(l)
     case l@(_ :: _) => l.flatMap(flattenParsers)
     case l: ~[_, _] => flattenParsers(l._1) ++ flattenParsers(l._2)
   }
}

// vim: set ts=2 sw=2 et:
