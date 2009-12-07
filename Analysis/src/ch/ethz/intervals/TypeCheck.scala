package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import Util.forallzip

class TypeCheck(prog: Prog) {    
    import prog.classDecl
    import prog.fresh

    /// environment: mutable field is easier than tracing it through
    var env: ir.TcEnv
    
    def savingEnv[R](g: => R): R = {
        val oldEnv = env
        try { g } finally { env = oldEnv }
    }
    
    def at(l: ir.Locatable, default: R)(g: => R): R = 
        try {
            g
        } catch {
            case err: ir.IrError =>
                prog.report(l, err)
                default           
        }
    
    /// capture an object ref.
    def capObj(wo: ir.WcObjPath) = wo match {
        case o: ir.ObjPath => o
        case ir.WcUnkObj => ir.ObjPath(fresh("Cap"))
    }
    
    def canon(x: ir.VarName): ir.Obj = x.o
    
    /// capture wt into a TypeRef
    def cap(wt: ir.WcTypeRef): ir.TypeRef = 
        ir.TypeRef(wt.c, wt.objs.map(capObj), wt.mthds)

    /// subst. for obj. param. of t
    def PathSubst(t: ir.TypeRef) =
        PathSubst(classDecl(t.c).objs.map(_.x), t.objs)

    /// supertype of t
    def sup(t: ir.TypeRef): Option[ir.TypeRef] = {
        val cd = classDecl(t.c)
        cd.superType match {
            case None => None
            case Some(t_1) => PathSubst(t).tref(t_1)
        }
    }
    
    def fieldDecl(ot: ir.TypeRef, f: ir.FieldName) = {
        def search(t: ir.TypeRef): ir.FieldDecl = {
            val cd = classDecl(t.c)
            cd.fields.find(_.name == f) match {
                case Some(fd) => PathSubst(t).varDecl(fd)
                case None => sup(t) match {
                    case Some(t_1) => search(t_1)
                    case None => throw IrError("intervals.no.such.field", ot, f)
                }
            }
        }
        search(ot)        
    }
    
    def methodSig(ot: ir.TypeRef, m: ir.MethodName): ir.MethodSig = {
        def search(t: ir.TypeRef): ir.MethodSig = {
            val cd = classDecl(t.c)
            cd.methods.find(_.name == m) match {
                case Some(md) => PathSubst(t).msig(md.msig)
                case None => sup(t) match {
                    case Some(t_1) => search(t_1)
                    case None => throw IrError("intervals.no.such.method", ot, m)
                }
            }
        }
        search(ot)
    }
    
    /// type of a local variable ref.
    def wt_lv(lv: ir.VarName) =
        env.lvs(lv)
    
    /// type of an object reference.
    def wt_path(p: ir.Obj): ir.WcTypeRef = o match {
        case ir.Obj(lv, List()) => wt_lv(x)
        case ir.Obj(lv, f :: fs) => 
            val q = lv ++ fs
            val t = cap(wt_path(q))
            val fd = fieldDecl(t, f)
            if(fd.isFinal) fd.wt
            else throw new ir.IrError("intervals.not.final", q, t, f)
    }
    
    def substdFieldDecl(p: ir.Path, f: ir.FieldName) = {
        val fd = fieldDecl(cap(wt_path(p)), f)
        PathSubst(ir.p_this, p).fieldDecl(fd)        
    }

    /// method sig. for o.m(p) with subst. performed
    def substdMethodSig(p: ir.Path, m: ir.MethodName, q: ir.Path) = {
        val t_p = cap(wt_path(p))
        val t_q = cap(wt_path(q))
        val msig = methodSig(t_p, m)
        checkIsSubtype(t_q, msig.arg.wt)
        PathSubst(
            List(ir.p_this, msig.arg.name.path), 
            List(p, q)
        ).msig(msig)        
    }
    
    /// effect of invoking p.m(q) where p has type wt_p
    def effectwt(p: ir.Path, wt_p: ir.WcTypeRef, m: ir.MethodName, q: ir.Obj) = {
        wt.overs.find(_.m == m) match {
            case Some(ov) => 
                PathSubst(
                    List(ir.p_this, ov.lv.path), 
                    List(p, q)
                ).effect(ov.e)
            case None =>
                val t = cap(wt_p)
                val msig = methodSig(t, m)
                PathSubst(
                    List(ir.p_this, msig.arg.name.path),
                    List(p, q)
                ).effect(msig.e)
        }
    }
    
    /// effect of invoking p.m(q)
    def effect(p: ir.Path, m: ir.MethodName, q: ir.Path) =
        effectwt(p, wt_path(p), m, q)
    
    /// wp <: wq
    def isSubpath(wp: ir.WcObj, wq: ir.WcObj) = (wp, wq) match {
        case (_, ir.WcObj) => true
        case (ir.WcObj, _) => false
        case (p: ir.Obj, q: ir.Obj) => p == q
    }

    /// p→q in the minimal interpretation
    def hb(p: ir.Path, q: ir.Path): Boolean =
        env.min.contains(p, q)
        
    /// p either is q or p→q
    def hbeq(p: ir.Path, q: ir.Path): Boolean =
        p == q || hb(p, q)
    
    /// i is a smaller-or-equal span of time than j
    def isSubinterval(i: ir.Interval, j: ir.Interval) =
        forallcross(j.ps, i.ps, hbeq)
        && forallcross(i.qs, j.qs, hbeq)
    
    /// e0 <: f0
    def isSubeffect(e0: ir.Effect, f0: ir.Effect): Boolean = (e0, f0) match {
        case (ir.EffectNone, _) => true
        case (_, ir.EffectNone) => false
        
        case (_, ir.EffectAny) => true
        case (ir.EffectAny, _) => false
        
        case (ir.EffectUnion(es), f) => es.forall(isSubeffect(_, f))
        case (ir.EffectMethod(p, m, q), f) =>
            isSubeffect(effect(p, m, q), f)
            
        case (e, ir.EffectUnion(fs)) => fs.exists(isSubeffect(e, _))
        case (e, ir.EffectMethod(p, m, q)) =>
            isSubeffect(e, effect(p, m, q))
            
        case (ir.EffectInterval(i, e), ir.EffectInterval(j, f)) =>
            isSubinterval(i, j) && isSubeffect(e, f)
            
        case (ir.EffectLock(ps, e), ir.EffectLock(qs, f)) =>
            ps.forall(p =>
                qs.exists(q =>
                    isSubobj(p, q)))
            && isSubeffect(e, f)

        case (ir.EffectFixed(_, p), ir.EffectFixed(ir.Wr, q)) =>
            isSubobj(p, q)
        case (ir.EffectFixed(ir.Rd, p), ir.EffectFixed(ir.Rd, q)) =>
            isSubobj(p, q)
            
        case _ =>
            false
    }
    
    /// are effects of ov
    def isSubover(ov_sub: ir.Over, ov_sup: ir.Over): Boolean = {
        if(ov_sub.m == ov_sup.m) {
            val f = fresh("Sub")
            val es_sub = LvSubst(ov_sub.lv, f).effect(ov_sub.e)
            val es_sup = LvSubst(ov_sub.lv, f).effect(ov_sup.e)
            isSubeffect(es_sub, es_sup)
        } else
            false
    }
    
    def isSubtype(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef): Boolean = {
        if(wt_sub.c != wt_sub.c) {
            sup(cap(wt_sub)) match {
                case None => false
                case Some(wt) => isSubtype(wt, wt_sup)
            }
        } else {
            forallzip(wt_sub.objs, wt_sup.objs, isSubobj) &&
            t_sub.overs.forall(ov_sub => 
                isSubeffect(ov_sub.e, effectwt(ir.p_this, wt_sup, ov_sub.m, ov_sub.lv.x)))
        }
    }
    
    def checkIsSubtype(t_sub: ir.TypeRef, wt_sup: ir.WcTypeRef) {
        if(!isSubover(t_sub, wt_sup))
            throw new ir.IrError("intervals.expected.subtype", t_sub, wt_sup)
    }
    
    def ordered(oi_0: Option[ir.Interval], oi_1: Option[ir.Interval]) =
        (oi_0, oi_1) match {
            case (None, None) => true
            case (Some(i_0), Some(i_1)) =>
                forallcross(i_0.ms, i_1.ns, hbeq)
                || forallcross(i_1.ms, i_0.ns, hbeq)
            case _ => false
        }
        
    def canFollow(
        oi_pre: Option[ir.Interval],
        l_pre: Set[ir.Path],
        e_pre: ir.Effect,        
        oi_post: Option[ir.Interval],
        l_post: Set[ir.Path],
        e_post: ir.Effect
    ): Boolean =
        (e_pre, e_post) match {
            case (_, ir.EffectNone) => true
            case (ir.EffectNone, _) => true
        
            case (_, ir.EffectAny) => false
            case (ir.EffectAny, _) => false
        
            case (ir.EffectUnion(es_pre), e_post) => 
                es_pre.forall(canFollow(_, e_post))
            case (e_pre, ir.EffectUnion(es_post)) => 
                es_post.forall(canFollow(e_pre, _))
        
            case (ir.EffectShift(i_pre, e_pre), e_post) => 
                canFollow(Some(i_pre), Set(), e_pre, pi_post, l_post, e_post)
            case (e_pre, ir.EffectShift(i_post, e_post)) => 
                canFollow(pi_pre, l_pre, e_pre, Some(i_post), Set(), e_post)
            
            case (ir.EffectLock(os_pre, e_pre), e_post) => 
                canFollow(i_pre, l_pre ++ os_pre, e_pre, i_post, l_post, e_post)
            case (e_pre, ir.EffectLock(os_post, e_post)) => 
                canFollow(i_pre, l_pre, e_pre, i_post, l_post ++ os_post, e_post)
                
            case (ir.EffectMethod(o, m, p), e_post) => 
                canFollow(i_pre, l_pre, effect(o, m, p), i_post, l_post, e_post)
            case (e_pre, ir.EffectMethod(o, m, p)) => 
                canFollow(i_pre, l_pre, e_pre, i_post, l_post, effect(o, m, p))                
            
            case (ir.EffectFixed(k_pre, o_pre), ir.EffectFixed(k_post, o_post)) => 
                (k_pre == ir.Rd && k_post == ir.Rd)
                || (l_pre(o_pre) && l_post(o_post))
                || ordered(oi_pre, oi_post)
        }
        
    def checkAllows(e_pre: ir.Effect, e_post: ir.Effect) =
        if(!canFollow(None, Set(), e_pre, None, Set(), e_post))
            throw new ir.IrError("intervals.cannot.follow", e_pre, e_post)
    
    def expr(ex: ir.Expr): (ir.WcTypeRef, ir.Effect) = ex match {
        
        case ir.ExprCall(p, m, q) =>
            val wt_p = wt_path(p), wt_q = wt_path(q)
        
            // Check argument type matches:
            val msig = methodSig(wt_p, m)
            checkIsSubtype(wt_q, msig.arg.wt)
        
            // Compute return type:
            val wt_ret = 
                PathSubst(
                    List(ir.p_this, msig.arg.name.path), 
                    List(p, q)
                ).wtref(msig.wt_ret)
                    
            (wt_ret, effect(x, m, y))
                   
        case ir.ExprField(p, f) =>
            val fd = substdFieldDecl(p, f)
            (fd.wt, ir.EffectFixed(ir.Rd, fd.guard))
            
        case ir.ExprNew(t: ir.TypeRef) =>
            checkTref(t)
            (t, ir.EffectNone)
            
        case ir.ExprNewInterval(p_b, p_t, ps_g) =>
            checkIsSubtype(wt_path(p_b), ir.wt_point)
            checkIsSubtype(wt_path(p_t), ir.wt_task)
            ps_g.map(wt_path).foreach(checkIsSubtype(_, ir.wt_guard))
            val e_m = effect(p_t, ir.m_run, ir.p_new)
            val e_l = ir.EffectLock(ps_g, e_m)
            val e_i = ir.EffectShift(ir.p_new, e_l)
            (ir.TypeRef(ir.c_interval, List(p_b), List()), e_i)
            
        case ir.ExprNewGuard(p) =>
            checkIsSubtype(wt_path(p), ir.wt_guard)
            (ir.TypeRef(ir.c_guard, List(p), List()), ir.EffectNone)
            
    }
    
    def statement(stmt: ir.Stmt): ir.Effect = 
        at(stmt, ir.EffectNone) {
            stmt match {        
                case ir.StmtVarDecl(ir.LvDecl(x, wt_x), ex) =>
                    checkWf(wt_x)
                    val (wt_e, e) = expr(ex)
                    checkIsSubtype(wt_e, wt_x)
                    e
        
                case ir.StmtAssignField(p, f, q) =>
                    val fd = substdFieldDecl(p, f)
                    checkIsSubtype(wt_path(q), fd.wt)
                    ir.EffectFixed(ir.Wr, fd.guard)
        
                case ir.StmtAddHb(p, q) =>
                    checkIsSubtype(wt_path(p), ir.wt_point)
                    checkIsSubtype(wt_path(q), ir.wt_point)
                    env = env.addHb(p, q)
                    ir.EffectNone            
            }
        } 
        
    def statements(e0: ir.Effect, stmts0: List[ir.Stmt]): ir.Effect = stmts0 match {
        case List() => e0
        case stmt :: stmts =>
            val e = statement(stmt)
            checkAllows(e0, e)
            statements(ir.union(e0, e), stmts)
    }
    
    def methodDecl(md: ir.MethodDecl) = savingEnv {
        at(md, ()) {
            checkWf(md.arg.wt)
            env = env.addLv(md.arg.name, md.arg.wt)
            val e_stmts = statements(md.stmts)
            val (wt_ret, e_ret) = expr(md.ex_ret)
            checkIsSubtype(wt_ret, md.wt_ret)
            checkAllows(e_stmts, e_ret)
            checkIsSubeffect(ir.union(e_stmts, e_ret), md.e)
        }         
    }
    
    def classDecl(cd: ir.ClassDecl) = savingEnv {
        at(cd, ()) {
            
        }
    }
}