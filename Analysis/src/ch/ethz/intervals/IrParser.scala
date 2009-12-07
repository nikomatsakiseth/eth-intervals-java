package ch.ethz.intervals

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class IrParser extends StandardTokenParsers {
    lexical.delimiters += ("{", "}", "[", "]", "(", ")", ",", "@", "?", ":", ".", ";", "=", "->", "#", "<=", "&&", "`")
    lexical.reserved += ("start", "end", "Sh", "Ex", "Free", "Lock")

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
    def p = lv~rep(dotf)                        ^^ { case lv~fs => }
    
    def wp = (
        "?"                                     ^^ { case _ => ir.WcUnkPath }
    |   p
    )
    
    def over = (
        "["~m~"("~lv~")"~"="~eff~"]"            ^^ { case _~m~_~lv~_~_~e~_ => ir.Over(m, lv, e) }
    )
    
    def wt = (
        c~"<"~comma(wp)~">"~rep(over)           ^^ { case c~_~wps~_~overs => ir.WcTypeRef(c, wps, overs) }
    |   c~rep(over)                             ^^ { case c~_~overs => ir.WcTypeRef(c, List(), overs) }
    )
    
    def t = (
        c~"<"~comma(p)~">"~rep(over)            ^^ { case c~_~ps~_~overs => ir.TypeRef(c, ps, overs) }
    |   c~rep(over)                             ^^ { case c~_~overs => ir.TypeRef(c, List(), overs) }
    )
    
    def expr = (
        p~"->"~m~"("~p~")"                      ^^ { case p~"->"~m~"("~q~")" => ir.ExprCall(p, m, q) }
    |   p~"->"~f                                ^^ { case p~"->"~f => ir.ExprField(p, f) }
    |   "new"~t                                 ^^ { case _~t => ir.ExprNew(t) }
    |   "interval"~p~p~"("~comma(p)~")"         ^^ { case _~b~t~_~gs~_ => ir.ExprNewInterval(b, t, gs) }
    |   "guard"~p                               ^^ { case _~g => ir.ExprNewGuard(g) }    
    )
    
    def lvdecl = (
        wt~lv                                   ^^ { case wt~lv => ir.LvDecl(wt, lv) }
    )
    
    def stmt = (
        lvdecl~"="~expr~";"                     ^^ { case vd~_~ex~_ => ir.StmtVarDecl(vd, ex) }
    |   p~"->"~f~"="~p~";"                      ^^ { case p~_~f~_~q~_ => ir.StmtAssignField(p, f, q) }
    |   "add"~p~"->"~q~";"                      ^^ { case _~p~_~q~_ => ir.StmtAddHb(p, q) }
    )
    
    def interval = (
        p                                       ^^ { case i => ir.Interval(List(i+ir.f_start), List(i+ir.f_end)) }
    |   "("~rep(p)~"-"~rep(p)~")"               ^^ { case ps~_~qs => ir.Interval(ps, qs) }
    )
    
    def guards = (
        p                                       ^^ { case p => List(p) }
    |   "("~rep(p)~")"                          ^^ { case ps => ps }
    )
    
    def eff0 = (
        "("~eff~")"                             ^^ { case _~e~_ => e }
    |   interval~":"~eff0                       ^^ { case i~_~e => ir.EffectShift(i, e) }
    |   guards~"/"~eff0                         ^^ { case gs~_~e => ir.EffectLock(gs, e) }
    |   p~"->"~m~"("~p~")"                      ^^ { case p~"->"~m~"("~q~")" => ir.EffectMethod(p, m, q) }
    |   "Rd"~"("~p~")"                          ^^ { case _~_~p~_ => ir.EffectFixed(ir.Rd, p) }
    |   "Wr"~"("~p~")"                          ^^ { case _~_~p~_ => ir.EffectFixed(ir.Wr, p) }
    |   "?"                                     ^^ { case _ => ir.EffectAny }
    )
    
    def eff = (
        rep(eff0)                               ^^ { case List(e) => e
                                                     case List() => ir.EffectNone
                                                     case es => ir.EffectUnion(es) }
    )
    
    def methodDecl = (
        wt~m~"("~lvdecl~")"~eff~"{"~
            rep(stmt)~
            "return"~expr~";"~
        "}"
    ) ^^ {
        case wt_ret~name~_~arg~_~e~_~stmts~ex_ret~_ =>
            ir.MethodDecl(wt_ret, name, arg, e, stmts, ex_ret)
    }
    
    def mod = (
        "final"                                 ^^ { case _ => ir.Final }
    )
    
    def ghostFieldDecl = (
        wt~f                                    ^^ { case wt~f => ir.GhostFieldDecl(wt, f) }
    )

    def realFieldDecl = (
        rep(mod)~wt~f~"guarded_by"~p            ^^ { case mods~wt~f~_~p => ir.RealFieldDecl(mods, wt, f, p) }
    )

    def classDecl = (
        "class"~c~"<"~
            comma(ghostFieldDecl)~
        ">"~"extends"~t~"{"~
            rep(realFieldDecl)~
            rep(methodDecl)~
        "}"
    ) ^^ {
        case _~name~_~ghosts~_~_~superType~_~fields~methods~_ =>
            ir.ClassDecl(name, ghosts, superType, fields, methods)
    }
    
}
