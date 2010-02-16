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
        "class", "new", ir.ctor, "null", 
        "return", "Rd", "Wr", "Free", "extends", 
        "requires", "super", 
        "subinterval", "push", "pop", 
        "interface", "break", "continue", "cbreak",
        "switch", "loop", "try", "catch"
    )

    def optd[A](d: => A, p: Parser[A]) = (
        opt(p)                                  ^^ { case None => d
                                                     case Some(l) => l }
    )
    
    def optl[A](p: Parser[List[A]]) = (
        opt(p) ^^ { case None => List(); case Some(l) => l }
    )
                                                     
    def integer = numericLit                    ^^ { case s => s.toInt }
    
    // Not all attributes are suitable in all contexts, but as this is an
    // internal IR we don't prevent irrelevant attributes from being added:
    def attr = (
        "interface"                             ^^ { case _ => ir.AttrInterface }
    )
    def attrs = rep(attr)                       ^^ { case la => ir.Attrs(Set(la: _*)) }
    
    // For method, variable, field, and class names, we just use identifiers,
    // but sometimes we apply some conversions to internal names:
    def m = ident                               ^^ { case i => ir.MethodName(i) }
    def cm = opt(m)                             ^^ { case Some(m) => m; case None => ir.m_init }
    def lv = id                                 ^^ ir.VarName
    def tv = id                                 ^^ ir.TypeVarName
    
    def uncons = optd(ir.FullyConstructed,
        "{"~"}"                                 ^^ { case _ => ir.FullyUnconstructed }
    |   "{"~>c<~"}"                             ^^ ir.PartiallyConstructed
    )
    
    def p = lv~rep("."~>f)                      ^^ { case lv~fs => lv ++ fs }
    def g = "@"~f~"("~p~")"                     ^^ { case _~f~_~p~_ => ir.Ghost(f, p) }    
    def ta = "<"~tv~":"~wt~">"                  ^^ { case _~tv~_~wt~_ => ir.TypeArg(tv, wt) }
    def pt = p~"."~tv                           ^^ { case p~_~tv => ir.PathType(p, tv) }
    def ct = c~rep(g)~rep(ta)~uncons            ^^ { case c~gs~tas~u => ir.ClassType(c, gs, tas, u) }
    override def wp = super.wp                  // Defined in BaseParser
    def wg = "@"~f~"("~wp~")"                   ^^ { case _~f~_~wp~_ => ir.WcGhost(f, wp) }    
    def typeBounds = (
        opt(comma(wt)<~"<:")~"?"~"<:"~comma(wt) ^^ { case oubs~_~_~lbs => ir.TypeBounds(lbs, oubs) }
    )
    def wta = (
        "<"~tv~":"~typeBounds~">"               ^^ { case _~tv~_~bounds~_ => ir.BoundedTypeArg(tv, bounds) }
    |   ta
    )
    def wct = c~rep(wg)~rep(wta)~uncons         ^^ { case c~lwg~la~u => ir.WcClassType(c, lwg, la, u) }
    def wt: Parser[ir.WcTypeRef] = (wct | pt)
    
    def lvdecl = (
        wt~lv                                   ^^ { case wt~lv => ir.LvDecl(lv, wt) }
    )
    
    private var counter = 0
    def anonLv = {
        val ctr = counter
        counter = counter + 1
        ir.VarName("parser[%s]".format(ctr))
    }
    
    def optLv = optd(anonLv,
        lv~"="                                      ^^ { case x~_ => x }
    )
    
    def seq = "{"~>rep(stmt)<~"}"                   ^^ ir.StmtSeq
    
    def locks = optl("locks"~>comma(p))
    def loopArg = lvdecl~"="~p                      ^^ { case lv~_~p => (lv, p) }
    def ckind: Parser[ir.CompoundKind] = (
        "{"~>seq<~"}"                               ^^ ir.Block
    |   "switch"~"{"~rep(seq)~"}"                   ^^ { case _~_~ss~_ => ir.Switch(ss) }
    |   "loop"~"("~comma(loopArg)~")"~seq           ^^ { case _~_~args~_~b => ir.Loop(args.map(_._1), args.map(_._2), b) }
    |   "subinterval"~lv~locks~seq                  ^^ { case _~x~ps~s => ir.Subinterval(x, ps, s) }
    |   "try"~seq~"catch"~seq                       ^^ { case _~t~_~c => ir.TryCatch(t, c) }
    )
    
    def stmt_defines = optl(("=>"~"(")~>comma(lvdecl)<~(")"~";"))
    
    def stmt_compound: Parser[ir.StmtCompound] = positioned(
        ckind~stmt_defines                          ^^ { case ckind~defs => ir.StmtCompound(ckind, defs) }
    )
    
    def stmt: Parser[ir.Stmt] = positioned(
       "super"~cm~"("~comma(p)~")"~";"              ^^ { case _~m~_~ps~_~_ => ir.StmtSuperCtor(m,ps) }
    |   optLv~p~"->"~m~"("~comma(p)~")"~";"         ^^ { case x~p~"->"~m~"("~qs~")"~_ => ir.StmtCall(x, p, m, qs) }
    |   optLv~"super"~"->"~m~"("~comma(p)~")"~";"   ^^ { case x~_~"->"~m~"("~qs~")"~_ => ir.StmtSuperCall(x, m, qs) }
    |   lv~"="~p~"->"~f~";"                         ^^ { case x~"="~p~"->"~f~_ => ir.StmtGetField(x, p, f) }
    |   lv~"="~"new"~ct~cm~"("~comma(p)~")"~";"     ^^ { case x~"="~"new"~ct~m~"("~qs~")"~_ => ir.StmtNew(x, ct, m, qs) }
    |   lv~"="~"("~wt~")"~p~";"                     ^^ { case x~"="~"("~wt~")"~p~";" => ir.StmtCast(x, wt, p) }
    |   lv~"="~"("~wt~")"~"null"~";"                ^^ { case x~"="~"("~wt~")"~"null"~";" => ir.StmtNull(x, wt) }
    |   p~"->"~f~"="~p~";"                          ^^ { case p~_~f~_~q~_ => ir.StmtSetField(p, f, q) }
    |   p~"<:"~wt~";"                               ^^ { case p~_~wt~_ => ir.StmtCheckType(p, wt) }
    |   p~"hb"~p~";"                                ^^ { case p~_~q~_ => ir.StmtHb(p, q) }
    |   p~"locks"~p~";"                             ^^ { case p~_~q~_ => ir.StmtLocks(p, q) }
    |   "break"~integer~"("~comma(p)~")"~";"        ^^ { case _~i~"("~ps~")"~";" => ir.StmtBreak(i, ps) }
    |   "cbreak"~integer~"("~comma(p)~")"~";"       ^^ { case _~i~"("~ps~")"~";" => ir.StmtCondBreak(i, ps) }
    |   "continue"~integer~"("~comma(p)~")"~";"     ^^ { case _~i~"("~ps~")"~";" => ir.StmtContinue(i, ps) }
    |   "return"~opt(p)~";"                         ^^ { case _~p~_ => ir.StmtReturn(p) }
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
        wt~m~"("~comma(lvdecl)~")"~uncons~reqs~seq        
    ^^ {
        case wt_ret~name~"("~args~")"~uncons~reqs~seq =>
            ir.MethodDecl(wt_ret, name, args, uncons, reqs, seq)
    })
    
    def constructor = positioned(
        "constructor"~cm~"("~comma(lvdecl)~")"~reqs~seq
    ^^ {
        case "constructor"~m~"("~args~")"~reqs~seq =>
            ir.MethodDecl(ir.t_void, m, args, ir.FullyUnconstructed, reqs, seq)
    })
    
    def reifiedFieldDecl = positioned(
        attrs~wt~f~"requires"~p~";"             ^^ { case as~wt~f~_~p~_ => ir.ReifiedFieldDecl(as, wt, f, p) }
    |   attrs~wt~f~";"                          ^^ { case as~wt~f~_ => ir.ReifiedFieldDecl(as, wt, f, ir.p_this_creator) }
    )
    
    def constructors = rep(constructor)         ^^ {
        case List() => List(ir.md_emptyCtor)
        case ctors => ctors
    }
    
    def typeVarDecl = positioned(
        "<"~tv~"<:"~comma(wt)~">"               ^^ { case _~tv~_~wts~_ => ir.TypeVarDecl(tv, wts) }
    )
    
    def ghostFieldDecl = positioned(
        "@"~f~"("~wt~")"                        ^^ { case _~f~_~wt~_ => ir.GhostFieldDecl(wt, f) }
    )
    
    def classDecl = positioned(
        attrs~"class"~c~
        rep(ghostFieldDecl)~
        rep(typeVarDecl)~
        optl("extends"~>comma(c))~
        rep(g)~
        rep(ta)~
        reqs~
        "{"~
            rep(reifiedFieldDecl)~
            constructors~
            rep(methodDecl)~
        "}"
    ^^ {
        case attrs~"class"~name~gfds~tvds~superClasses~guards~targs~reqs~"{"~rfds~ctors~methods~"}" =>
            ir.ClassDecl(
                attrs, name, tvds, targs, superClasses, guards, reqs, 
                ctors, gfds ++ rfds, methods)
    })
    
    def classDecls = rep(classDecl)
    
}
