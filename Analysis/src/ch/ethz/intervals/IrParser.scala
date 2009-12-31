package ch.ethz.intervals

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

import Util._

class IrParser extends StandardTokenParsers {
    lexical.delimiters += (
        "{", "}", "[", "]", "(", ")", "<", ">",
        ",", "@", "?", ":", ".", ";", "=", "->", 
        "#", "<=", "&&", "`", "=="
    )
    lexical.reserved += (
        "class", "new", "constructor", "null", 
        "return", "Rd", "Wr", "Free", "extends", 
        "hb", "requires", "super", "locks",
        "subinterval", "readableBy", "writableBy",
        "interface", "goto"
    )
    
    def parse[A](p: Parser[A])(text: String) = {
      val tokens = new lexical.Scanner(text)
      phrase(p)(tokens)
    }

    def optd[A](p: Parser[A], d: => A) = 
        opt(p)                                  ^^ { case None => d
                                                     case Some(l) => l }
    
    def optl2[A](k: String, p: Parser[List[A]]) = opt(
        k~p                                     ^^ { case _~r => r }
    ) ^^ { case None => List(); case Some(l) => l }

    def optl3[A](a: String, p: Parser[List[A]], b: String) =
        opt(a~p~b)                              ^^ { case Some(_~l~_) => l; case None => List() }
    
    def comma[A](p: Parser[A]): Parser[List[A]] = repsep(p, ",")
                                    
    def id = (
        "`"~repsep(ident, ".")~"`"              ^^ { case "`"~is~"`" => is.mkString(".") }
    |   ident
    )
    
    def integer = numericLit                    ^^ { case s => s.toInt }
    
    // Not all attributes are suitable in all contexts, but as this is an
    // internal IR we don't prevent irrelevant attributes from being added:
    def attr = (
        "constructor"                           ^^ { case _ => ir.AttrCtor }    
    |   "interface"                             ^^ { case _ => ir.AttrInterface }
    )
    def attrs = rep(attr)                       ^^ { case la => ir.Attrs(Set(la: _*)) }
    
    def m = id                                  ^^ { case i => ir.MethodName(i) }
    def c = id                                  ^^ { case i => ir.ClassName(i) }
    def lv = id                                 ^^ { case i => ir.VarName(i) }
    def f = (
        "constructor"                           ^^ { case _ => ir.f_ctor }
    |   id                                      ^^ { case i => ir.FieldName(i) }
    )
    
    def dotf = "."~f                            ^^ { case _~f => f }
    def p = lv~rep(dotf)                        ^^ { case lv~fs => lv ++ fs }
    
    def g = "<"~f~":"~p~">"                     ^^ { case _~f~_~p~_ => ir.Ghost(f, p) }    
    def t = c~rep(g)                            ^^ { case c~lg => ir.TypeRef(c, lg, ir.noAttrs) }
    
    def wp = (
        p
    |   "?"                                     ^^ { case _ => ir.WcHb(List(), List()) }
    |   "readableBy"~comma(p)                   ^^ { case _~ps => ir.WcReadableBy(ps) }
    |   "writableBy"~comma(p)                   ^^ { case _~ps => ir.WcWritableBy(ps) }
    |   comma(p)~"hb"~comma(p)                  ^^ { case ps~_~qs => ir.WcHb(ps, qs) }
    |   "locks"~comma(p)                        ^^ { case _~ps => ir.WcLocks(ps) }
    |   comma(p)~"locks"                        ^^ { case ps~_ => ir.WcLockedBy(ps) }
    )
    
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
    
    def optLocks = optl2("locks", comma(p))
    
    def stmt: Parser[ir.Stmt] = positioned(
       "super"~"("~comma(p)~")"~";"                 ^^ { case _~_~ps~_~_ => ir.StmtSuperCtor(ps) }
    |   optLv~p~"->"~m~"("~comma(p)~")"~";"         ^^ { case x~p~"->"~m~"("~qs~")"~_ => ir.StmtCall(x, p, m, qs) }
    |   optLv~"super"~"->"~m~"("~comma(p)~")"~";"   ^^ { case x~_~"->"~m~"("~qs~")"~_ => ir.StmtSuperCall(x, m, qs) }
    |   lv~"="~p~"->"~f~";"                         ^^ { case x~"="~p~"->"~f~_ => ir.StmtGetField(x, p, f) }
    |   lv~"="~"new"~t~"("~comma(p)~")"~";"         ^^ { case x~"="~"new"~t~"("~qs~")"~_ => ir.StmtNew(x, t, qs) }
    |   lv~"="~"("~wt~")"~p~";"                     ^^ { case x~"="~"("~wt~")"~p~";" => ir.StmtCast(x, wt, p) }
    |   lv~"="~"("~wt~")"~"null"~";"                ^^ { case x~"="~"("~wt~")"~"null"~";" => ir.StmtNull(x, wt) }
    |   p~"->"~f~"="~p~";"                          ^^ { case p~_~f~_~q~_ => ir.StmtSetField(p, f, q) }
    |   p~"hb"~p~";"                                ^^ { case p~_~q~_ => ir.StmtHb(p, q) }
    |   p~"locks"~p~";"                             ^^ { case p~_~q~_ => ir.StmtLocks(p, q) }
    |   "subinterval"~lv~optLocks~"{"~rep(stmt)~"}" ^^ { case _~x~ps~"{"~stmts~"}" => ir.StmtSubinterval(x, ps, stmts) }
    |   "return"~p~";"                              ^^ { case _~p~_ => ir.StmtReturn(p) }
    )
    
    def req = positioned(
        "requires"~comma(p)~"readableBy"~comma(p)   ^^ { case _~lp~_~lq => ir.ReqReadableBy(lp, lq) }
    |   "requires"~comma(p)~"writableBy"~comma(p)   ^^ { case _~lp~_~lq => ir.ReqWritableBy(lp, lq) }
    |   "requires"~comma(p)~"subinterval"~comma(p)  ^^ { case _~lp~_~lq => ir.ReqSubintervalOf(lp, lq) }
    |   "requires"~comma(p)~"hb"~comma(p)           ^^ { case _~lp~_~lq => ir.ReqHb(lp, lq) }
    )    
    def reqs = rep(req)
    
    def goto = positioned(
        "goto"~integer~"("~comma(p)~")"~";"         ^^ { case _~b~"("~ps~")"~";" => ir.Goto(b, ps) }
    )
    
    def block = (
        "("~comma(lvdecl)~")"~"{"~rep(stmt)~rep(goto)~"}" 
    ) ^^ { 
        case "("~args~")"~"{"~stmts~gotos~"}" => ir.Block(args, stmts, gotos)
    }
    
    def methodBody = (
        "{"~rep(stmt)~rep(goto)~"}"~rep(block)
    ) ^^ {
        case _~stmts~gotos~_~blocks =>
            Array((ir.Block(List(), stmts, gotos) :: blocks): _*)
    }
    
    def methodDecl = positioned(
        attrs~wt~m~"("~comma(lvdecl)~")"~reqs~methodBody        
    ^^ {
        case attrs~wt_ret~name~"("~args~")"~reqs~blocks =>
            ir.MethodDecl(attrs, wt_ret, name, args, reqs, blocks)
    })
    
    def constructor = positioned(
        "constructor"~"("~comma(lvdecl)~")"~reqs~methodBody
    ^^ {
        case "constructor"~"("~args~")"~reqs~blocks =>
            ir.MethodDecl(ir.ctorAttrs, ir.t_void, ir.m_ctor, args, reqs, blocks)
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
        optl2("extends", comma(t))~
        reqs~
        "{"~
            rep(reifiedFieldDecl)~
            constructor~
            rep(methodDecl)~
        "}"
    ^^ {
        case attrs~"class"~name~gfds~superTypes~reqs~"{"~rfds~ctor~methods~"}" =>
            ir.ClassDecl(attrs, name, superTypes, reqs, ctor, gfds ++ rfds, methods)
    })
    
    def classDecls = rep(classDecl)
    
}
