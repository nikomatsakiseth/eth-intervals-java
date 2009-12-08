package ch.ethz.intervals

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

import Util._

class IrParser extends StandardTokenParsers {
    lexical.delimiters += (
        "{", "}", "[", "]", "(", ")", "<", ">",
        ",", "@", "?", ":", ".", ";", "=", "->", 
        "#", "<=", "&&", "`"
    )
    lexical.reserved += (
        "class", "new", "interval", "guard", "null", "add", 
        "return", "Rd", "Wr", "Free", "extends", "final", "guardedBy"
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
        "?"                                     ^^ { case _ => ir.WcUnkPath }
    |   p
    )
    
    def over = (
        "["~m~"("~comma(lv)~")"~"="~eff~"]"     ^^ { case _~m~_~lvs~_~_~e~_ => ir.Over(m, lvs, e) }
    )
    
    def wt = (
        c~"<"~comma(wp)~">"~rep(over)           ^^ { case c~_~wps~_~overs => ir.WcTypeRef(c, wps, overs) }
    |   c~rep(over)                             ^^ { case c~overs => ir.WcTypeRef(c, List(), overs) }
    )
    
    def t = (
        c~"<"~comma(p)~">"~rep(over)            ^^ { case c~_~ps~_~overs => ir.TypeRef(c, ps, overs) }
    |   c~rep(over)                             ^^ { case c~overs => ir.TypeRef(c, List(), overs) }
    )
    
    def expr = (
        p~"->"~m~"("~comma(p)~")"               ^^ { case p~"->"~m~"("~qs~")" => ir.ExprCall(p, m, qs) }
    |   p~"->"~f                                ^^ { case p~"->"~f => ir.ExprField(p, f) }
    |   "new"~t~"("~comma(p)~")"                ^^ { case _~t~"("~qs~")" => ir.ExprNew(t, qs) }
    |   "interval"~p~p~"("~comma(p)~")"         ^^ { case _~b~t~_~gs~_ => ir.ExprNewInterval(b, t, gs) }
    |   "guard"~p                               ^^ { case _~g => ir.ExprNewGuard(g) }    
    |   "null"                                  ^^ { case _ => ir.ExprNull }
    )
    
    def lvdecl = (
        wt~lv                                   ^^ { case wt~lv => ir.LvDecl(lv, wt) }
    )
    
    def stmt = (
        lvdecl~"="~expr~";"                     ^^ { case vd~_~ex~_ => ir.StmtVarDecl(vd, ex) }
    |   p~"->"~f~"="~p~";"                      ^^ { case p~_~f~_~q~_ => ir.StmtAssignField(p, f, q) }
    |   "add"~p~"->"~p~";"                      ^^ { case _~p~_~q~_ => ir.StmtAddHb(p, q) }
    |   "schedule"~"("~")"~";"                  ^^ { case _ => ir.StmtSchedule() }
    )
    
    def interval = (
        p                                       ^^ { case i => ir.startEnd(i) }
    |   "("~rep(p)~"->"~rep(p)~")"              ^^ { case _~ps~_~qs~_ => ir.Interval(ps, qs) }
    )
    
    def guards = (
        p                                       ^^ { case p => List(p) }
    |   "("~comma(p)~")"                        ^^ { case _~ps~_ => ps }
    )
    
    def eff0: Parser[ir.Effect] = (
        "("~eff~")"                             ^^ { case _~e~_ => e }
    |   interval~":"~eff0                       ^^ { case i~_~e => ir.EffectInterval(i, e) }
    |   guards~"/"~eff0                         ^^ { case gs~_~e => ir.EffectLock(gs, e) }
    |   p~"->"~m~"("~comma(p)~")"               ^^ { case p~"->"~m~"("~qs~")" => ir.EffectMethod(p, m, qs) }
    |   "Rd"~"("~p~")"                          ^^ { case _~_~p~_ => ir.EffectFixed(ir.Rd, p) }
    |   "Wr"~"("~p~")"                          ^^ { case _~_~p~_ => ir.EffectFixed(ir.Wr, p) }
    |   "0"                                     ^^ { case _ => ir.EffectNone }
    |   "?"                                     ^^ { case _ => ir.EffectAny }
    )
    
    def eff = (
        comma(eff0)                             ^^ { case List(e) => e
                                                     case List() => ir.EffectNone
                                                     case es => ir.EffectUnion(es) }
    )
    
    def disjoint = repsep(p, "#")               ^^ { case ps => ir.DisjointDecl(ps) }

    def methodDecl = (
        wt~m~"("~comma(lvdecl)~")"~comma(disjoint)~eff~"{"~
            rep(stmt)~
            "return"~expr~";"~
        "}"
    ) ^^ {
        case wt_ret~name~_~args~_~disj~e~_~stmts~_~ex_ret~_~_ =>
            ir.MethodDecl(wt_ret, name, args, disj, e, stmts, ex_ret)
    }
    
    def mod = (
        "final"                                 ^^ { case _ => ir.Final }
    )
    
    def ghostFieldDecl = (
        wt~f                                    ^^ { case wt~f => ir.GhostFieldDecl(f, wt) }
    )

    def realFieldDecl = (
        rep(mod)~wt~f~"guardedBy"~p~";"         ^^ { case mods~wt~f~_~p~_ => ir.RealFieldDecl(mods, wt, f, p) }
    )
    
    def classDecl = (
        "class"~c~"<"~
            comma(ghostFieldDecl)~
        ">"~comma(disjoint)~"extends"~t~"{"~
            rep(realFieldDecl)~
            rep(methodDecl)~
        "}"
    ) ^^ {
        case _~name~"<"~ghosts~">"~disjoints~"extends"~superType~_~fields~methods~_ =>
            ir.ClassDecl(name, ghosts, disjoints, Some(superType), fields, methods)
    }
    
    def classDecls = rep(classDecl)
    
}
