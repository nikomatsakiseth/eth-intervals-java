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
        "hb", "hbeq", "requires"
    )

    def parse[A](p: Parser[A])(text: String) = {
      val tokens = new lexical.Scanner(text)
      phrase(p)(tokens)
    }

    def optd[A](p: Parser[A], d: A) = 
        opt(p)                                  ^^ { case None => d
                                                     case Some(l) => l }
    def optl[A](p: Parser[List[A]]) = optd(p, List())

    def optl3[A](a: String, p: Parser[List[A]], b: String) =
        opt(a~p~b)                              ^^ { case Some(_~l~_) => l; case None => List() }
    
    def comma[A](p: Parser[A]): Parser[List[A]] = repsep(p, ",")
                                    
    def id = (
        "`"~repsep(ident, ".")~"`"              ^^ { case "`"~is~"`" => is.mkString(".") }
    |   ident
    )
                                                
    def attr = "constructor"                    ^^ { case _ => ir.AttrCtor }
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
    |   comma(p)~"hb"~comma(p)                  ^^ { case ps~_~qs => ir.WcHb(ps, qs) }
    |   comma(p)~"hbeq"~comma(p)                ^^ { case ps~_~qs => ir.WcHbEq(ps, qs) }
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
    
    def stmt = (
        lvdecl~"="~p~"->"~m~"("~comma(p)~")"~";"^^ { case vd~"="~p~"->"~m~"("~qs~")"~_ => ir.StmtCall(vd, p, m, qs) }
    |   lvdecl~"="~p~"->"~f~";"                 ^^ { case vd~"="~p~"->"~f~_ => ir.StmtGetField(vd, p, f) }
    |   lvdecl~"="~"new"~t~"("~comma(p)~")"~";" ^^ { case vd~"="~"new"~t~"("~qs~")"~_ => ir.StmtNew(vd, t, qs) }
    |   lvdecl~"="~"null"~";"                   ^^ { case vd~"="~"null"~";" => ir.StmtNull(vd) }
    |   p~"->"~f~"="~p~";"                      ^^ { case p~_~f~_~q~_ => ir.StmtSetField(p, f, q) }
    |   p~"hb"~p~";"                            ^^ { case p~_~q~_ => ir.StmtHb(p, q) }
    |   p~"locks"~p~";"                         ^^ { case p~_~q~_ => ir.StmtLocks(p, q) }
    )
    
    def req = (
        "requires"~comma(p)                     ^^ { case _~ps => ir.ReqOwned(ps) }
    |   "requires"~p~"hb"~comma(p)              ^^ { case _~p~_~qs => ir.ReqHb(p, qs) }
    |   "requires"~p~"hbeq"~comma(p)            ^^ { case _~p~_~qs => ir.ReqHbEq(p, qs) }
    |   "requires"~p~"=="~p                     ^^ { case _~p~_~q => ir.ReqEq(p, q) }
    |   "requires"~p~"locks"~comma(p)           ^^ { case _~p~_~qs => ir.ReqLocks(p, qs) }
    )    
    def reqs = rep(req)
    
    def ret = opt(
        "return"~p~";"                          ^^ { case _~p~_ => p }
    )
    
    def methodDecl = (
        attrs~wt~m~"("~comma(lvdecl)~")"~reqs~"{"~
            rep(stmt)~
            ret~
        "}"
    ) ^^ {
        case attrs~wt_ret~name~"("~args~")"~reqs~"{"~stmts~op_ret~"}" =>
            ir.MethodDecl(attrs, wt_ret, name, args, reqs, stmts, op_ret)
    }
    
    def constructor = (
        "constructor"~"("~comma(lvdecl)~")"~reqs~"{"~
            rep(stmt)~
        "}"
    ) ^^ {
        case "constructor"~"("~args~")"~reqs~"{"~stmts~"}" =>
            ir.MethodDecl(ir.ctorAttrs, ir.t_void, ir.m_ctor, args, reqs, stmts, None)
    }
    
    def ghostFieldDecl = (
        wt~f                                    ^^ { case wt~f => ir.GhostDecl(wt, f) }
    )

    def realFieldDecl = (
        attrs~wt~f~"requires"~p~";"             ^^ { case as~wt~f~_~p~_ => ir.FieldDecl(as, wt, f, p) }
    )
    
    def classDecl = (
        "class"~c~optl3("<",
            comma(ghostFieldDecl),
        ">")~"extends"~t~reqs~"{"~
            rep(realFieldDecl)~
            constructor~
            rep(methodDecl)~
        "}"
    ) ^^ {
        case "class"~name~ghosts~"extends"~superType~reqs~"{"~fields~ctor~methods~"}" =>
            ir.ClassDecl(name, ghosts, Some(superType), reqs, ctor, fields, methods)
    }
    
    def classDecls = rep(classDecl)
    
}
