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
    
    def comma[A](p: Parser[A]): Parser[List[A]] = repsep(p, ",")
                                    
    def id = (
        ident
    | "`"~repsep(ident, ".")~"`"                ^^ { case "`"~is~"`" => ".".join(is) }
    )
                                                
    def m = id                                  ^^ { case i => ir.MethodName(i) }
    def f = id                                  ^^ { case i => ir.FieldName(i) }
    def c = id                                  ^^ { case i => ir.ClassName(i) }
    def lv = id                                 ^^ { case i => ir.VarName(i) }
    
    def dotf = "."~f                            ^^ { case _~f => f }
    def p = lv~rep(dotf)                        ^^ { case lv~fs => lv ++ fs }
    
    def wp = (
        comma(p)~"hb"~comma(p)                  ^^ { case ps~_~qs => ir.WcHb(ps, qs) }
    |   comma(p)~"hbeq"~comma(p)                ^^ { case ps~_~qs => ir.WcHbEq(ps, qs) }
    |   p
    )
    
    def wt = (
        c~"<"~comma(wp)~">"                     ^^ { case c~_~wps~_ => ir.WcTypeRef(c, wps) }
    |   c                                       ^^ { case c => ir.WcTypeRef(c, List()) }
    )
    
    def t = (
        c~"<"~comma(p)~">"                      ^^ { case c~_~ps~_ => ir.TypeRef(c, ps) }
    |   c                                       ^^ { case c => ir.TypeRef(c, List()) }
    )
    
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
        "requires"~comma(p)~"hb"~comma(p)~";"   ^^ { case _~ps~_~qs~_ => ir.ReqHb(ps, qs) }
    |   "requires"~comma(p)~"hbeq"~comma(p)~";" ^^ { case _~ps~_~qs~_ => ir.ReqHbEq(ps, qs) }
    |   "requires"~p~"=="~p~";"                 ^^ { case _~p~_~q~_ => ir.ReqEq(p, q) }
    |   "requires"~p~"locks"~comma(p)~";"       ^^ { case _~ps~_~qs~_ => ir.ReqLocks(ps, qs) }
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
        wt~f                                    ^^ { case wt~f => ir.GhostFieldDecl(f, wt) }
    )

    def realFieldDecl = (
        wt~f~"guardedBy"~p~";"                  ^^ { case wt~f~_~p~_ => ir.RealFieldDecl(wt, f, p) }
    )
    
    def classDecl = (
        "class"~c~"<"~
            comma(ghostFieldDecl)~
        ">"~"extends"~t~"{"~
            rep(realFieldDecl)~
            constructor~
            rep(methodDecl)~
        "}"
    ) ^^ {
        case _~name~"<"~ghosts~">"~"extends"~superType~"{"~fields~ctor~methods~"}" =>
            ir.ClassDecl(name, ghosts, Some(superType), ctor, fields, methods)
    }
    
    def classDecls = rep(classDecl)
    
}
