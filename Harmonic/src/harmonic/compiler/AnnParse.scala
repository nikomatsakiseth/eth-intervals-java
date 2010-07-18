package harmonic.compiler

import scala.util.parsing.input.Reader
import scala.util.parsing.input.PagedSeqReader
import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.OffsetPosition
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.syntax.Tokens
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers
import scala.collection.immutable.PagedSeq

import com.smallcultfollowing.lathos.Context

import java.io.File

import Ast.{Parse => out}
import Error.CanFail
import Util._

/** A parser just for Java annotations.  The syntax for these is
  * much less forgiving and user-friendly than what we expect in
  * Harmonic source. */
object AnnParse extends StdTokenParsers with PackratParsers {
    type Tokens = StdLexical
    val lexical = new StdLexical()
    
    lexical.delimiters ++= List(
        "{{", "}}", "{", "}", "[", "]", "(", ")", "#", 
        "$", "=", "->", ",", ":", ".", ";", "*", "@"
    )
    
    lexical.reserved ++= List(
        "permitsWr", "permitsRd", "ensuresFinal", "locks",
        "subOf", "inlineSubOf", "static", "true", "false",
        "null"
    )
    
    def comma[A](p: PackratParser[A]) = repsep(p, ",")<~opt(",")
    def comma1[A](p: PackratParser[A]) = rep1sep(p, ",")<~opt(",")
    def optl[A](p: PackratParser[List[A]]) = opt(p) ^^ { _.getOrElse(List()) }
    
    lazy val varIdent = ident

    lazy val packageName = repsep(ident, ".") ^^ { idents =>
        idents.foldLeft[Name.Package](Name.Root)(Name.Subpackage(_, _))
    }
    
    lazy val topLevelClassName = rep1sep(ident, ".") ^^ { idents => 
        val pkg = idents.dropRight(1).foldLeft[Name.Package](Name.Root)(Name.Subpackage(_, _))
        Name.Class(pkg, idents.last)
    }
    
    lazy val innerClassName = className~"$"~ident ^^ { 
        case c~"$"~d => Name.Class(c, d) 
    }
    
    lazy val className: PackratParser[Name.Class] = topLevelClassName | innerClassName
    
    lazy val memberName = "("~className~"#"~ident~")" ^^ {
        case "("~c~"#"~m~")" => Name.Member(c, m)
    }
    
    lazy val localName = ident ^^ {
        case i => Name.LocalVar(i)
    }
    
    lazy val anonVar = ty                           ^^ Pattern.AnonVar
    lazy val anonTuple = "("~>comma(anonPat)<~")"     ^^ Pattern.AnonTuple
    lazy val anonPat: PackratParser[Pattern.Anon] = anonVar | anonTuple
    
    lazy val methodPart = ident~anonTuple ^^ {
        case i~pat => (i, pat)
    }
    
    lazy val staticMethodId = "("~>className~"#"~rep1(methodPart)<~")" ^^ {
        case className~"#"~parts => {
            val methodName = Name.Method(parts.map(_._1))
            val patterns = parts.map(_._2)
            MethodId.Static(className, methodName, patterns)
        }
    }
    
    lazy val virtualMethodId = "("~>className~"#"~rep1(methodPart)<~")" ^^ {
        case className~"#"~parts => {
            val methodName = Name.Method(parts.map(_._1))
            val patterns = parts.map(_._2)
            MethodId.Virtual(className, methodName, patterns)
        }
    }
    
    lazy val static = "static" ^^^ Path.Static
    
    lazy val owner: PackratParser[Path.Owner] = static | path
    
    lazy val staticCall = staticMethodId~"("~comma(path)~")" ^^ {
        case m~_~args~_ => Path.StaticCall(m, args)
    }
    
    lazy val virtualCall = path~"."~virtualMethodId~"("~comma(path)~")" ^^ {
        case r~_~m~_~args~_ => Path.Call(r, m, args)
    }
    
    lazy val path: PackratParser[Path] = (
        localName               ^^ { case i => Path.Local(i) }
    |   numericLit              ^^ { case l => Path.Constant(Integer.valueOf(l)) }
    |   stringLit               ^^ { case l => Path.Constant(l) }
    |   "true"                  ^^ { case _ => Path.Constant(java.lang.Boolean.TRUE) }
    |   "false"                 ^^ { case _ => Path.Constant(java.lang.Boolean.FALSE) }
    |   memberName              ^^ { case f => Path.Field(Path.Static, f) }
    |   path~"."~memberName     ^^ { case o~_~f => Path.Field(o, f) }
    |   path~"["~path~"]"       ^^ { case a~_~i~_ => Path.Index(a, i) }
    |   "("~comma1(path)~")"    ^^ { case _~t~_ => Path.Tuple(t) }
    |   virtualCall
    |   staticCall
    )
    
    lazy val ty: PackratParser[Type] = (
        path~":"~memberName             ^^ { case p~":"~m => Type.Member(p, m) }
    |   className~"["~comma(arg)~"]"    ^^ { case c~_~a~_ => Type.Class(c, a) }
    |   className                       ^^ { case c => Type.Class(c, Nil) }
    |   "("~comma(ty)~")"               ^^ { case _~t~_ => Type.Tuple(t) }
    |   "null"                          ^^^ Type.Null
    )
    
    lazy val arg = (
        memberName~pcRel~path           ^^ { case m~rel~r => Type.PathArg(m, rel, r) }
    |   memberName~tcRel~ty             ^^ { case m~rel~r => Type.TypeArg(m, rel, r) }
    )
    
    lazy val pcRel: PackratParser[PcRel] = (
        "locks"         ^^^ PcLocks.asInstanceOf[PcRel]
    |   "inlineSubOf"   ^^^ PcInlineSubOf.asInstanceOf[PcRel]
    |   "subOf"         ^^^ PcSubOf.asInstanceOf[PcRel]
    |   "->"            ^^^ PcHb.asInstanceOf[PcRel]
    |   "="             ^^^ PcEq.asInstanceOf[PcRel]
    |   "permitsWr"     ^^^ PcPermitsWr.asInstanceOf[PcRel]
    |   "permitsRd"     ^^^ PcPermitsRd.asInstanceOf[PcRel]
    |   "ensuresFinal"  ^^^ PcEnsuresFinal.asInstanceOf[PcRel]
    )
    
    lazy val tcRel: PackratParser[TcRel] = (
        ":"             ^^^ TcEq
    |   "<:"            ^^^ TcSub
    |   ":>"            ^^^ TcSup
    )
    
    lazy val fact = (
        path~pcRel~path         ^^ { case l~rel~r => rel.toFact(l, r) }
    |   ty~tcRel~ty             ^^ { case l~rel~r => rel.toFact(l, r) }
    )

    private[this] def parse[R](prod: PackratParser[R])(text: String): CanFail[R] = {
        val tokens = new lexical.Scanner(text)
        phrase(prod)(tokens) match {
            case n: NoSuccess => Left(harmonic.compiler.Error.ParseError(n.msg))
            case Success(res, _) => Right(res)
        }
    }
    
    def parsePath = parse(path) _    
    def parseFact = parse(fact) _    
}

