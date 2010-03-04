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
        "class", "new", "null", 
        "return", "Rd", "Wr", "Free", "extends", 
        "requires", "super", 
        "inlineInterval", "push", "pop", 
        "interface", "break", "continue", "cbreak",
        "switch", "loop", "try", "catch", "assert", "Is"
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
    
    def p = (
        c~"#"~f                                 ^^ { case c~_~f => ir.PathStatic(c) / f }
    |   lv~rep("."~>f)                          ^^ { case lv~fs => lv /+ fs }
    )
    def g = "@"~f~"("~p~")"                     ^^ { case _~f~_~p~_ => ir.Ghost(f, p) }    
    def ta = "<"~tv~":"~wt~">"                  ^^ { case _~tv~_~wt~_ => ir.TypeArg(tv, wt) }
    def pt = p~":"~tv                           ^^ { case p~_~tv => ir.PathType(p, tv) }
    def ct = c~rep(g)~rep(ta)                   ^^ { case c~gs~tas => ir.ClassType(c, gs, tas) }
    override def wp = super.wp                  // Defined in BaseParser
    def wg = "@"~f~"("~wp~")"                   ^^ { case _~f~_~wp~_ => ir.WcGhost(f, wp) }    
    def typeBounds = (
        optl(comma(wt)<~"<:")~"?"~"<:"~comma(wt)^^ { case oubs~_~_~lbs => ir.TypeBounds(lbs, oubs) }
    )
    def wta = (
        "<"~tv~":"~typeBounds~">"               ^^ { case _~tv~_~bounds~_ => ir.BoundedTypeArg(tv, bounds) }
    |   ta
    )
    def wct = c~rep(wg)~rep(wta)                ^^ { case c~lwg~la => ir.WcClassType(c, lwg, la) }
    def wt: Parser[ir.WcTypeRef] = (pt | wct)
    
    // Wildcard type with defaults for @Constructor
    def wt_dflt = wt                            ^^ { case wt => wt.withDefaultWghosts(ir.wgs_constructed) }
    
    def isdecl = optl("@"~>"Is"~>"("~>comma(wp)<~")")
    
    def lvdecl = isdecl~wt_dflt~lv              ^^ { case is~wt~lv => ir.LvDecl(lv, wt, is) }
    
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
    
    def locks = optl("locks"~>comma(lv))
    def loopArg = lvdecl~"="~lv                     ^^ { case lv~_~p => (lv, p) }
    def ckind: Parser[ir.CompoundKind] = (
        "{"~>seq<~"}"                               ^^ ir.Block
    |   "switch"~"{"~comma(seq)~"}"                 ^^ { case _~_~ss~_ => ir.Switch(ss) }
    |   "loop"~"("~comma(loopArg)~")"~seq           ^^ { case _~_~args~_~b => ir.Loop(args.map(_._1), args.map(_._2), b) }
    |   "inlineInterval"~lv~seq~","~seq             ^^ { case _~x~s1~_~s2 => ir.InlineInterval(x, s1, s2) }
    |   "try"~seq~"catch"~seq                       ^^ { case _~t~_~c => ir.TryCatch(t, c) }
    )
    
    def stmt_defines = optl(("=>"~"(")~>comma(lvdecl)<~(")"~";"))
    
    def stmt_compound: Parser[ir.StmtCompound] = positioned(
        ckind~stmt_defines                          ^^ { case ckind~defs => ir.StmtCompound(ckind, defs) }
    )
    
    def stmt: Parser[ir.Stmt] = positioned(
       "super"~cm~"("~comma(lv)~")"~";"             ^^ { case _~m~_~ps~_~_ => ir.StmtSuperCtor(m,ps) }
    |   optLv~lv~"->"~m~"("~comma(lv)~")"~";"       ^^ { case x~p~"->"~m~"("~qs~")"~_ => ir.StmtCall(x, p, m, qs) }
    |   optLv~"super"~"->"~m~"("~comma(lv)~")"~";"  ^^ { case x~_~"->"~m~"("~qs~")"~_ => ir.StmtSuperCall(x, m, qs) }
    |   lv~"="~"static"~"["~c~"]"~";"               ^^ { case x~_~_~_~c~_~_ => ir.StmtGetStatic(x, c) }
    |   lv~"="~lv~"->"~f~";"                        ^^ { case x~"="~p~"->"~f~_ => ir.StmtGetField(x, p, f) }
    |   lv~"="~"new"~ct~cm~"("~comma(lv)~")"~";"    ^^ { case x~"="~"new"~ct~m~"("~qs~")"~_ => ir.StmtNew(x, ct, m, qs) }
    |   lv~"="~"("~wt_dflt~")"~lv~";"               ^^ { case x~"="~"("~wt~")"~p~";" => ir.StmtCast(x, wt, p) }
    |   lv~"="~"("~wt_dflt~")"~"null"~";"           ^^ { case x~"="~"("~wt~")"~"null"~";" => ir.StmtNull(x, wt) }
    |   lv~"->"~f~"="~lv~";"                        ^^ { case p~_~f~_~q~_ => ir.StmtSetField(p, f, q) }
    |   "assert"~lv~"<:"~wt_dflt~";"                ^^ { case _~p~_~wt~_ => ir.StmtCheckType(p, wt) }
    |   lv~"hb"~lv~";"                              ^^ { case p~_~q~_ => ir.StmtHb(p, q) }
    |   lv~"locks"~lv~";"                           ^^ { case p~_~q~_ => ir.StmtLocks(p, q) }
    |   "break"~integer~"("~comma(lv)~")"~";"       ^^ { case _~i~"("~ps~")"~";" => ir.StmtBreak(i, ps) }
    |   "cbreak"~integer~"("~comma(lv)~")"~";"      ^^ { case _~i~"("~ps~")"~";" => ir.StmtCondBreak(i, ps) }
    |   "continue"~integer~"("~comma(lv)~")"~";"    ^^ { case _~i~"("~ps~")"~";" => ir.StmtContinue(i, ps) }
    |   "return"~opt(lv)~";"                        ^^ { case _~p~_ => ir.StmtReturn(p) }
    |   stmt_compound
    )
    
    def req = positioned("requires"~>reqBase)
    def reqs = rep(req)
    
    def methodDecl = positioned(
        isdecl~wt_dflt~m~"("~comma(lvdecl)~")"~reqs~seq        
    ^^ {
        case is~wt_ret~name~"("~args~")"~reqs~seq =>
            ir.MethodDecl(name, args, reqs, wt_ret, is, seq)
    })
    
    def constructor = positioned(
        ir.ctor~cm~"("~comma(lvdecl)~")"~reqs~seq
    ^^ {
        case _~m~"("~args~")"~reqs~seq =>
            ir.MethodDecl(m, args, reqs, ir.t_void, List(), seq)
    })
    
    def createReifiedFieldDecl(wt: ir.WcTypeRef, f: ir.FieldName, p_guard: ir.Path, is: List[ir.WcPath]) = {
        ir.ReifiedFieldDecl(wt.withDefaultWghosts(ir.wgs_fieldsDefault), f, p_guard, is)
    }
    def reifiedFieldDecl = positioned(
        isdecl~wt~f~"requires"~p~";"                ^^ { case is~wt~f~_~p~_ => createReifiedFieldDecl(wt, f, p, is) }
    |   isdecl~wt~f~";"                             ^^ { case is~wt~f~_ => createReifiedFieldDecl(wt, f, ir.p_this_creator, is) }
    )
    
    def constructors = rep(constructor)             ^^ {
        case List() => List(ir.md_emptyCtor)
        case ctors => ctors
    }
    
    def typeVarDecl = positioned(
        "<"~tv~"<:"~comma(wt_dflt)~">"              ^^ { case _~tv~_~wts~_ => ir.TypeVarDecl(tv, wts) }
    )
    
    def ghostFieldDecl = positioned(
        "@"~f~"("~c~")"                             ^^ { case "@"~f~"("~c~")" => ir.GhostFieldDecl(f, c) }
    )
    
    def classDecl = positioned(
        attrs~"class"~anyC~
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
        case attrs~"class"~name~gfds~gs~tvds~tas~superClasses~reqs~"{"~rfds~ctors~methods~"}" =>
            ir.ClassDecl(
                attrs, name, gfds, gs, tvds, tas, superClasses, reqs, ctors, rfds, methods)
    })
    
    def classDecls = rep(classDecl)
    
}
