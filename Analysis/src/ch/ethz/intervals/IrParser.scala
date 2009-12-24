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
        "interface", "succ"
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
    
    def wp = (
        p
    |   "?"                                     ^^ { case _ => ir.WcHb(List(), List()) }
    |   "readableBy"~comma(p)                   ^^ { case _~ps => ir.WcReadableBy(ps) }
    |   "writableBy"~comma(p)                   ^^ { case _~ps => ir.WcWritableBy(ps) }
    |   comma(p)~"hb"~comma(p)                  ^^ { case ps~_~qs => ir.WcHb(ps, qs) }
    |   "locks"~comma(p)                        ^^ { case _~ps => ir.WcLocks(ps) }
    |   comma(p)~"locks"                        ^^ { case ps~_ => ir.WcLockedBy(ps) }
    )
    
    def wt =
        c~optl3("<",comma(wp),">")~attrs        ^^ { case c~wps~la => ir.WcTypeRef(c, wps, la) }
    
    def t =
        c~optl3("<",comma(p),">")               ^^ { case c~ps => ir.TypeRef(c, ps, ir.noAttrs) }
    
    def lvdecl = (
        wt~lv                                   ^^ { case wt~lv => ir.LvDecl(lv, wt) }
    )
    
    private var counter = 0
    def anonLvDecl = {
        val ctr = counter
        counter = counter + 1
        ir.LvDecl(ir.VarName("parser[%s]".format(ctr)), ir.t_void)
    }
    
    def optLvDecl = optd(
        lvdecl~"="                                      ^^ { case vd~_ => vd },
        anonLvDecl
    )
    
    def optLocks = optl2("locks", comma(p))
    
    def stmt: Parser[ir.Stmt] = positioned(
       "super"~"("~comma(p)~")"~";"                     ^^ { case _~_~ps~_~_ => ir.StmtSuperCtor(ps) }
    |   optLvDecl~p~"->"~m~"("~comma(p)~")"~";"         ^^ { case vd~p~"->"~m~"("~qs~")"~_ => ir.StmtCall(vd, p, m, qs) }
    |   optLvDecl~"super"~"->"~m~"("~comma(p)~")"~";"   ^^ { case vd~_~"->"~m~"("~qs~")"~_ => ir.StmtSuperCall(vd, m, qs) }
    |   lvdecl~"="~p~"->"~f~";"                         ^^ { case vd~"="~p~"->"~f~_ => ir.StmtGetField(vd, p, f) }
    |   t~lv~"="~"new"~"("~comma(p)~")"~";"             ^^ { case t~x~"="~"new"~"("~qs~")"~_ => ir.StmtNew(x, t, qs) }
    |   lvdecl~"="~"null"~";"                           ^^ { case vd~"="~"null"~";" => ir.StmtNull(vd) }
    |   p~"->"~f~"="~p~";"                              ^^ { case p~_~f~_~q~_ => ir.StmtSetField(p, f, q) }
    |   p~"hb"~p~";"                                    ^^ { case p~_~q~_ => ir.StmtHb(p, q) }
    |   p~"locks"~p~";"                                 ^^ { case p~_~q~_ => ir.StmtLocks(p, q) }
    |   "subinterval"~lv~optLocks~"{"~rep(stmt)~"}"     ^^ { case _~x~ps~"{"~stmts~"}" => ir.StmtSubinterval(x, ps, stmts) }
    |   "return"~p~";"                                  ^^ { case _~p~_ => ir.StmtReturn(p) }
    )
    
    def req = positioned(
        "requires"~comma(p)~"readableBy"~comma(p)       ^^ { case _~lp~_~lq => ir.ReqReadableBy(lp, lq) }
    |   "requires"~comma(p)~"writableBy"~comma(p)       ^^ { case _~lp~_~lq => ir.ReqWritableBy(lp, lq) }
    |   "requires"~comma(p)~"subinterval"~comma(p)      ^^ { case _~lp~_~lq => ir.ReqSubintervalOf(lp, lq) }
    |   "requires"~comma(p)~"hb"~comma(p)               ^^ { case _~lp~_~lq => ir.ReqHb(lp, lq) }
    )    
    def reqs = rep(req)
    
    def succ = "succ"~integer~"("~comma(p)~")"~";"      ^^ { case _~b~"("~ps~")"~";" => ir.Succ(b, ps) }
    
    def block = (
        "("~comma(lvdecl)~")"~"{"~rep(stmt)~rep(succ)~"}" 
    ) ^^ { 
        case "("~args~")"~"{"~stmts~succs~"}" => ir.Block(args, stmts, succs)
    }
    
    def methodBody = (
        "{"~rep(stmt)~rep(succ)~"}"~rep(block)
    ) ^^ {
        case _~stmts~succs~_~blocks =>
            Array((ir.Block(List(), stmts, succs) :: blocks): _*)
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
        wt~f                                    ^^ { case wt~f => ir.GhostDecl(wt, f) }
    )

    def realFieldDecl = positioned(
        attrs~wt~f~"requires"~p~";"             ^^ { case as~wt~f~_~p~_ => ir.FieldDecl(as, wt, f, p) }
    )
    
    def classDecl = positioned(
        attrs~"class"~c~optl3("<",
            comma(ghostFieldDecl),
        ">")~
        optl2("extends", comma(t))~
        reqs~
        "{"~
            rep(realFieldDecl)~
            constructor~
            rep(methodDecl)~
        "}"
    ^^ {
        case attrs~"class"~name~ghosts~superTypes~reqs~"{"~fields~ctor~methods~"}" =>
            ir.ClassDecl(attrs, name, ghosts, superTypes, reqs, ctor, fields, methods)
    })
    
    def classDecls = rep(classDecl)
    
}
