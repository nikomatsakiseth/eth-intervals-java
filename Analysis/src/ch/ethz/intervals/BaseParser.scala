package ch.ethz.intervals

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Reader
import scala.util.parsing.syntax.Tokens
import scala.util.parsing.syntax.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical

// Common code shared by both the IrParser (used in unit testing)
// and the AnnotParser (used in comp. plugin).
abstract class BaseParser extends StandardTokenParsers {
    
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
    lexical.reserved += (
        "hb", "subintervalOf", "readableBy", "writableBy", "locks"
    )
    
    def parse[A](p: Parser[A])(text: String) = {
      val tokens = new lexical.Scanner(text)
      phrase(p)(tokens)
    }
    
    def parseToResult[A](p: Parser[A])(text: String) = 
        parse(p)(text) match {
            case Success(v, _) => v
            case n: NoSuccess => throw new ir.IrError("intervals.parse.error", n.toString)            
        }            
    
    def comma[A](p: Parser[A]): Parser[List[A]] = repsep(p, ",")
    
    def p: Parser[ir.Path]
    
    def wp = (
        p
    |   "?"                                     ^^ { case _ => ir.WcHb(List(), List()) }
    |   "readableBy"~comma(p)                   ^^ { case _~ps => ir.WcReadableBy(ps) }
    |   "writableBy"~comma(p)                   ^^ { case _~ps => ir.WcWritableBy(ps) }
    |   comma(p)~"hb"~comma(p)                  ^^ { case ps~_~qs => ir.WcHb(ps, qs) }
    |   "locks"~comma(p)                        ^^ { case _~ps => ir.WcLocks(ps) }
    |   comma(p)~"locks"                        ^^ { case ps~_ => ir.WcLockedBy(ps) }
    )

                                        
}