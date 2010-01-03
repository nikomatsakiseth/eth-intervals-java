package ch.ethz.intervals

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Reader
import scala.util.parsing.syntax.Tokens
import scala.util.parsing.syntax.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical

class BaseParser extends StandardTokenParsers {
    
    // Extend parser so that "`.*`" is considered an identifier:
    override val lexical = new StdLexical {
        override def token: Parser[Token] = {
            val superToken = super.token
            new Parser[Token]() {
                def untilTick(prefix: String, in: Reader[Char]): (String, Reader[Char]) =
                    if(in.first == '`') (prefix, in.rest)
                    else untilTick(prefix + in.first, in.rest)
                def apply(in: Reader[Char]) = {
                    if(in.first == '`') {                        
                        val (str, out) = untilTick("", in.rest)
                        Success(Identifier(str), out)
                    } else {
                        superToken.apply(in)
                    }
                }
            }
        }
    }
    
    lexical.delimiters += (
        "{", "}", "[", "]", "(", ")", "<", ">",
        ",", "@", "?", ":", ".", ";", "=", "->", 
        "#", "<=", "&&", "=="
    )
    
    def parse[A](p: Parser[A])(text: String) = {
      val tokens = new lexical.Scanner(text)
      phrase(p)(tokens)
    }    
    
    def comma[A](p: Parser[A]): Parser[List[A]] = repsep(p, ",")
                                        
}