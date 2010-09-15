package de.downgra.slayon.lexer

import util.matching.Regex
import util.parsing.combinator.Parsers

import de.downgra.slayon.token.Token

trait Chaining extends Parsers {
 
    type SQ1 = ~[String, String]
    type SQ2 = ~[SQ1,    String]
    type SQ3 = ~[SQ2,    String]
    type SQ4 = ~[SQ3,    String]
    type SQ5 = ~[SQ4,    String]
    type SQ6 = ~[SQ5,    String]
    type SQ7 = ~[SQ6,    String]
 
    implicit def sequentialComposition1[T <: Parser[SQ1]](p: T) = new {
      def %%%(f0: (String) => Token,
              f1: (String) => Token
             ): Parser[List[Token]] = p ^^ { case a ~ b => List(f0(a), f1(b)) }
    }

    implicit def sequentialComposition2[T <: Parser[SQ2]](p: T) = new {
      def %%%(f0: (String) => Token,
              f1: (String) => Token,
              f2: (String) => Token
             ): Parser[List[Token]] = p ^^ { case a ~ b ~ c => List(f0(a), f1(b), f2(c)) }
    }

    implicit def sequentialComposition3[T <: Parser[SQ3]](p: T) = new {
      def %%%(f0: (String) => Token, f1: (String) => Token,
              f2: (String) => Token, f3: (String) => Token
             ): Parser[List[Token]] = p ^^ { case a ~ b ~ c ~ d => List(f0(a), f1(b), f2(c), f3(d)) }
    }

    implicit def sequentialComposition4[T <: Parser[SQ4]](p: T) = new {
      def %%%(f0: (String) => Token, f1: (String) => Token,
              f2: (String) => Token, f3: (String) => Token,
              f4: (String) => Token
             ): Parser[List[Token]] = p ^^
         { case a ~ b ~ c ~ d ~ e => List(f0(a), f1(b), f2(c), f3(d), f4(e)) }
    }

    implicit def sequentialComposition5[T <: Parser[SQ5]](p: T) = new {
      def %%%(f0: (String) => Token, f1: (String) => Token,
              f2: (String) => Token, f3: (String) => Token,
              f4: (String) => Token, f5: (String) => Token
             ): Parser[List[Token]] = p ^^
         { case a ~ b ~ c ~ d ~ e ~ f => List(f0(a), f1(b), f2(c), f3(d), f4(e), f5(f)) }
    }

    implicit def sequentialComposition6[T <: Parser[SQ6]](p: T) = new {
      def %%%(f0: (String) => Token, f1: (String) => Token,
              f2: (String) => Token, f3: (String) => Token,
              f4: (String) => Token, f5: (String) => Token,
              f6: (String) => Token
             ): Parser[List[Token]] = p ^^
         { case a ~ b ~ c ~ d ~ e ~ f ~ g =>
            List(f0(a), f1(b), f2(c), f3(d), f4(e), f4(f), f6(g)) }
    }

    implicit def sequentialComposition7[T <: Parser[SQ7]](p: T) = new {
      def %%%(f0: (String) => Token, f1: (String) => Token,
              f2: (String) => Token, f3: (String) => Token,
              f4: (String) => Token, f5: (String) => Token,
              f6: (String) => Token, f7: (String) => Token
             ): Parser[List[Token]] = p ^^
         { case a ~ b ~ c ~ d ~ e ~ f ~ g ~ h =>
            List(f0(a), f1(b), f2(c), f3(d), f4(e), f4(f), f6(g), f7(h)) }
    }

}

// vim: set ts=2 sw=2 et:
