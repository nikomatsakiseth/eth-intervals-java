package ch.ethz.intervals

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.NoPosition
import scala.util.parsing.syntax.Tokens
import scala.util.parsing.syntax.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical

import Util._

class IrParser extends BaseParser {
    
    lexical.reserved += (
        "class", "new", "constructor", "null", 
        "return", "Rd", "Wr", "Free", "extends", 
        "requires", "super", 
        "subinterval", "push", "pop", 
        "interface", "goto",
        "switch", "loop", "try", "catch"
    )

    def optd[A](p: Parser[A], d: => A) = 
        opt(p)                                  ^^ { case None => d
                                                     case Some(l) => l }
    
    def optl2[A](k: String, p: Parser[List[A]]) = opt(
        k~p                                     ^^ { case _~r => r }
    ) ^^ { case None => List(); case Some(l) => l }

    def optl3[A](a: String, p: Parser[List[A]], b: String) =
        opt(a~p~b)                              ^^ { case Some(_~l~_) => l; case None => List() }
    
    def integer = numericLit                    ^^ { case s => s.toInt }
    
    // Not all attributes are suitable in all contexts, but as this is an
    // internal IR we don't prevent irrelevant attributes from being added:
    def attr = (
        "constructor"                           ^^ { case _ => ir.AttrCtor }    
    |   "interface"                             ^^ { case _ => ir.AttrInterface }
    )
    def attrs = rep(attr)                       ^^ { case la => ir.Attrs(Set(la: _*)) }
    
    // For method, variable, field, and class names, we just use identifiers,
    // but sometimes we apply some conversions to internal names:
    def m = ident                               ^^ { case i => ir.MethodName(i) }
    def cm = opt(m)                             ^^ { case Some(m) => m; case None => ir.m_init }
    def lv = ident                              ^^ { case i => ir.VarName(i) }
    def f = (
        "constructor"                           ^^ { case _ => ir.f_ctor }
    |   ident                                   ^^ { 
        case "creator" => ir.f_creator
        case i => ir.FieldName(i) }
    )
    def c = repsep(ident, ".")                  ^^ { 
        case List("Object") => ir.c_object
        case List("Interval") => ir.c_interval
        case List("Guard") => ir.c_guard
        case List("Point") => ir.c_point
        case List("Lock") => ir.c_lock
        case List("String") => ir.c_string
        case List("scalar") => ir.c_scalar
        case List("void") => ir.c_void
        case List("array") => ir.c_array
        case idents => ir.ClassName(".".join(idents)) 
    }
    
    def dotf = "."~f                            ^^ { case _~f => f }
    def p = lv~rep(dotf)                        ^^ { case lv~fs => lv ++ fs }
    def g = "<"~f~":"~p~">"                     ^^ { case _~f~_~p~_ => ir.Ghost(f, p) }    
    def t = c~rep(g)                            ^^ { case c~lg => ir.TypeRef(c, lg, ir.noAttrs) }

    override def wp = super.wp                  // Defined in BaseParser
    def wg = "<"~f~":"~wp~">"                   ^^ { case _~f~_~wp~_ => ir.WcGhost(f, wp) }    
    def wt = c~rep(wg)~attrs                    ^^ { case c~lwg~la => ir.WcTypeRef(c, lwg, la) }
    
    def lvdecl = (
        wt~lv                                   ^^ { case wt~lv => ir.LvDecl(lv, wt) }
    )
    
    private var counter = 0
    def anonLv = {
        val ctr = counter
        counter = counter + 1
        ir.VarName("parser[%s]".format(ctr))
    }
    
    def optLv = optd(
        lv~"="                                      ^^ { case x~_ => x },
        anonLv
    )
    
    def locks = optl2("locks", comma(p))
    def loopArg = lvdecl~"="~p                      ^^ { case lv~_~p => (lv, p) }
    def ckind: Parser[ir.CompoundKind] = (
        "{"~rep(stmt)~"}"                           ^^ { case _~ss~_ => ir.Seq(ss) }
    |   "switch"~"{"~rep(stmt)~"}"                  ^^ { case _~_~ss~_ => ir.Switch(ss) }
    |   "loop"~"("~comma(loopArg)~")"~stmt          ^^ { case _~_~args~_~b => ir.Loop(args.map(_._1), args.map(_._2), b) }
    |   "subinterval"~lv~locks~stmt                 ^^ { case _~x~ps~s => ir.Subinterval(x, ps, s) }
    |   "try"~stmt~"catch"~stmt                     ^^ { case _~t~_~c => ir.TryCatch(t, c) }
    )
    
    def stmt_compound: Parser[ir.StmtCompound] = positioned(
        ckind~optl3("(",comma(lvdecl),")")          ^^ { case ckind~defs => ir.StmtCompound(ckind, defs) }
    )
    
    def stmt: Parser[ir.Stmt] = positioned(
       "super"~cm~"("~comma(p)~")"~";"              ^^ { case _~m~_~ps~_~_ => ir.StmtSuperCtor(m,ps) }
    |   optLv~p~"->"~m~"("~comma(p)~")"~";"         ^^ { case x~p~"->"~m~"("~qs~")"~_ => ir.StmtCall(x, p, m, qs) }
    |   optLv~"super"~"->"~m~"("~comma(p)~")"~";"   ^^ { case x~_~"->"~m~"("~qs~")"~_ => ir.StmtSuperCall(x, m, qs) }
    |   lv~"="~p~"->"~f~";"                         ^^ { case x~"="~p~"->"~f~_ => ir.StmtGetField(x, p, f) }
    |   lv~"="~"new"~t~cm~"("~comma(p)~")"~";"      ^^ { case x~"="~"new"~t~m~"("~qs~")"~_ => ir.StmtNew(x, t, m, qs) }
    |   lv~"="~"("~wt~")"~p~";"                     ^^ { case x~"="~"("~wt~")"~p~";" => ir.StmtCast(x, wt, p) }
    |   lv~"="~"("~wt~")"~"null"~";"                ^^ { case x~"="~"("~wt~")"~"null"~";" => ir.StmtNull(x, wt) }
    |   p~"->"~f~"="~p~";"                          ^^ { case p~_~f~_~q~_ => ir.StmtSetField(p, f, q) }
    |   p~"hb"~p~";"                                ^^ { case p~_~q~_ => ir.StmtHb(p, q) }
    |   p~"locks"~p~";"                             ^^ { case p~_~q~_ => ir.StmtLocks(p, q) }
    |   "break"~integer~"("~comma(p)~")"~";"        ^^ { case _~i~"("~ps~")"~";" => ir.StmtBreak(i, ps) }
    |   "continue"~integer~"("~comma(p)~")"~";"     ^^ { case _~i~"("~ps~")"~";" => ir.StmtContinue(i, ps) }
    |   "return"~p~";"                              ^^ { case _~p~_ => ir.StmtReturn(p) }
    |   stmt_compound
    )
    
    def req = positioned(
        "requires"~comma(p)~"readableBy"~comma(p)   ^^ { case _~lp~_~lq => ir.ReqReadableBy(lp, lq) }
    |   "requires"~comma(p)~"writableBy"~comma(p)   ^^ { case _~lp~_~lq => ir.ReqWritableBy(lp, lq) }
    |   "requires"~comma(p)~"subinterval"~comma(p)  ^^ { case _~lp~_~lq => ir.ReqSubintervalOf(lp, lq) }
    |   "requires"~comma(p)~"hb"~comma(p)           ^^ { case _~lp~_~lq => ir.ReqHb(lp, lq) }
    )    
    def reqs = rep(req)
    
    def methodDecl = positioned(
        attrs~wt~m~"("~comma(lvdecl)~")"~reqs~stmt        
    ^^ {
        case attrs~wt_ret~name~"("~args~")"~reqs~blocks =>
            ir.MethodDecl(attrs, wt_ret, name, args, reqs, blocks)
    })
    
    def constructor = positioned(
        "constructor"~cm~"("~comma(lvdecl)~")"~reqs~stmt
    ^^ {
        case "constructor"~m~"("~args~")"~reqs~stmt =>
            ir.MethodDecl(ir.ctorAttrs, ir.t_void, m, args, reqs, stmt)
    })
    
    def ghostFieldDecl = positioned(
        "<"~wt~f~">"                            ^^ { case _~wt~f~_ => ir.GhostFieldDecl(wt, f) }
    )
    
    def reifiedFieldDecl = positioned(
        attrs~wt~f~"requires"~p~";"             ^^ { case as~wt~f~_~p~_ => ir.ReifiedFieldDecl(as, wt, f, p) }
    |   attrs~wt~f~";"                          ^^ { case as~wt~f~_ => ir.ReifiedFieldDecl(as, wt, f, ir.p_this_creator) }
    )
    
    def classDecl = positioned(
        attrs~"class"~c~rep(ghostFieldDecl)~
        optl2("extends", comma(c))~
        rep(g)~
        reqs~
        "{"~
            rep(reifiedFieldDecl)~
            rep(constructor)~
            rep(methodDecl)~
        "}"
    ^^ {
        case attrs~"class"~name~gfds~superClasses~guards~reqs~"{"~rfds~ctors~methods~"}" =>
            ir.ClassDecl(attrs, name, superClasses, guards, reqs, ctors, gfds ++ rfds, methods)
    })
    
    def classDecls = rep(classDecl)
    
}
