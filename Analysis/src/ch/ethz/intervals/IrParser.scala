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
        "return", "Rd", "Wr", "Free", "extends", "guardedBy", 
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
        "`"~repsep(ident, ".")~"`"              ^^ { case "`"~is~"`" => ".".join(is) }
    |   ident
    )
                                                
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
        comma(p)~"hb"~comma(p)                  ^^ { case ps~_~qs => ir.WcHb(ps, qs) }
    |   comma(p)~"hbeq"~comma(p)                ^^ { case ps~_~qs => ir.WcHbEq(ps, qs) }
    |   p
    )
    
    def wt =
        c~optl3("<",comma(wp),">")              ^^ { case c~wps => ir.WcTypeRef(c, wps) }
    
    def t =
        c~optl3("<",comma(p),">")               ^^ { case c~ps => ir.TypeRef(c, ps) }
    
    def expr = (
        p~"->"~m~"("~comma(p)~")"               ^^ { case p~"->"~m~"("~qs~")" => ir.ExprCall(p, m, qs) }
    |   p~"->"~f                                ^^ { case p~"->"~f => ir.ExprField(p, f) }
    |   "new"~t~"("~comma(p)~")"                ^^ { case _~t~"("~qs~")" => ir.ExprNew(t, qs) }
    |   "null"                                  ^^ { case _ => ir.ExprNull }
    )
    
    def lvdecl = (
        wt~lv                                   ^^ { case wt~lv => ir.LvDecl(lv, wt) }
    )
    
    def stmt = (
        lvdecl~"="~expr~";"                     ^^ { case vd~_~ex~_ => ir.StmtVarDecl(vd, ex) }
    |   p~"->"~f~"="~p~";"                      ^^ { case p~_~f~_~q~_ => ir.StmtAssignField(p, f, q) }
    |   p~"hb"~p~";"                            ^^ { case p~_~q~_ => ir.StmtHb(p, q) }
    |   p~"locks"~p~";"                         ^^ { case p~_~q~_ => ir.StmtLocks(p, q) }
    )
    
    def req = (
        "requires"~p~"hb"~comma(p)              ^^ { case _~p~_~qs => ir.ReqHb(p, qs) }
    |   "requires"~p~"hbeq"~comma(p)            ^^ { case _~p~_~qs => ir.ReqHbEq(p, qs) }
    |   "requires"~p~"=="~p                     ^^ { case _~p~_~q => ir.ReqEq(p, q) }
    |   "requires"~p~"locks"~comma(p)           ^^ { case _~p~_~qs => ir.ReqLocks(p, qs) }
    )
    
    def reqs = rep(req)
    
    def methodDecl = (
        wt~m~"("~comma(lvdecl)~")"~reqs~"{"~
            rep(stmt)~
            "return"~expr~";"~
        "}"
    ) ^^ {
        case wt_ret~name~"("~args~")"~reqs~"{"~stmts~"return"~ex_ret~";"~"}" =>
            ir.MethodDecl(wt_ret, name, args, reqs, stmts, ex_ret)
    }
    
    def constructor = (
        "constructor"~"("~comma(lvdecl)~")"~reqs~"{"~
            rep(stmt)~
        "}"
    ) ^^ {
        case "constructor"~"("~args~")"~reqs~"{"~stmts~"}" =>
            ir.MethodDecl(ir.t_void, ir.m_ctor, args, reqs, stmts, ir.ExprNull)        
    }
    
    def ghostFieldDecl = (
        wt~f                                    ^^ { case wt~f => ir.GhostFieldDecl(wt, f) }
    )

    def realFieldDecl = (
        wt~f~"guardedBy"~p~";"                  ^^ { case wt~f~_~p~_ => ir.RealFieldDecl(wt, f, p) }
    )
    
    def classDecl = (
        "class"~c~optl3("<",
            comma(ghostFieldDecl),
        ">")~"extends"~t~"{"~
            rep(realFieldDecl)~
            constructor~
            rep(methodDecl)~
        "}"
    ) ^^ {
        case "class"~name~ghosts~"extends"~superType~"{"~fields~ctor~methods~"}" =>
            ir.ClassDecl(name, ghosts, Some(superType), ctor, fields, methods)
    }
    
    def classDecls = rep(classDecl)
    
}
