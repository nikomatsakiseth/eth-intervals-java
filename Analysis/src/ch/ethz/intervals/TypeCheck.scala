package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable.ListBuffer
import Util._

class TypeCheck(log: Log, prog: Prog) {    
    import prog.classDecl
    import prog.fresh

    // ______________________________________________________________________
    // Environment
    
    var env = ir.TcEnv(
        Map(
            (ir.lv_readOnly, ir.wt_guard),
            (ir.lv_locked, ir.wt_guard)
        ),
        ir.Graph.empty,
        MultiMap.empty
    )
    
    def savingEnv[R](g: => R): R = {
        val oldEnv = env
        try { g } finally { env = oldEnv }
    }
    
    def addLv(lv: ir.VarName, wt: ir.WcTypeRef) {
        env = ir.TcEnv(env.lvs + Pair(lv, wt), env.min, env.disjoints)
    }

    def addHb(p: ir.Path, q: ir.Path) {
        env = ir.TcEnv(env.lvs, env.min + (p, q), env.disjoints)
    }

    def addDisjoint(ps: List[ir.Path]) {
        pairs(ps).foreach { case (p, q) =>
            env = ir.TcEnv(env.lvs, env.min, env.disjoints + (p, q))
        }
    }

    // ______________________________________________________________________
    // Errors

    def at[R](loc: ir.Locatable, default: R)(g: => R): R = 
        try {
            g
        } catch {
            case err: ir.IrError =>
                prog.reportError(loc, err)
                log("Error: %s", err)
                default           
        }
    
    // ______________________________________________________________________
    // Captures 
    
    /// capture an object ref.
    def capObj(wo: ir.WcPath) = wo match {
        case o: ir.Path => o
        case ir.WcUnkPath => ir.Path(ir.VarName(fresh("Cap")), List())
    }
    
    /// capture wt into a TypeRef
    def cap(wt: ir.WcTypeRef): ir.TypeRef = 
        ir.TypeRef(wt.c, wt.wpaths.map(capObj), wt.overs)
        
    /// subst. for obj. param. of t
    def pathSubst(t: ir.TypeRef) = {
        val ghostPaths = classDecl(t.c).ghosts.map(_.thisPath)
        PathSubst.pp(ghostPaths, t.paths)
    }    

    /// supertype of t
    def sup(t: ir.TypeRef): Option[ir.TypeRef] = {
        val cd = classDecl(t.c)
        cd.superType match {
            case None => None
            case Some(t_1) => Some(pathSubst(t).tref(t_1))
        }
    }
    
    /// lookup field decl for ot.f 
    def fieldDecl(ot: ir.TypeRef, f: ir.FieldName) = {
        def search(t: ir.TypeRef): ir.FieldDecl = {
            val cd = classDecl(t.c)
            cd.allFields.find(_.name == f) match {
                case Some(fd) => pathSubst(t).fieldDecl(fd)
                case None => sup(t) match {
                    case Some(t_1) => search(t_1)
                    case None => throw ir.IrError("intervals.no.such.field", ot, f)
                }
            }
        }
        search(ot)        
    }
    
    /// lookup method decl for ot.m()
    def methodSig(ot: ir.TypeRef, m: ir.MethodName): ir.MethodSig = {
        def search(t: ir.TypeRef): ir.MethodSig = {
            val cd = classDecl(t.c)
            cd.methods.find(_.name == m) match {
                case Some(md) => pathSubst(t).methodSig(md.msig)
                case None => sup(t) match {
                    case Some(t_1) => search(t_1)
                    case None => throw ir.IrError("intervals.no.such.method", ot, m)
                }
            }
        }
        search(ot)
    }
        
    /// type of a local variable ref.
    def wt_lv(lv: ir.VarName) =
        env.lvs.get(lv) match {
            case Some(wt) => wt
            case None => throw ir.IrError("intervals.no.such.variable", lv)
        }
    
    /// type of an object reference.
    def wt_path(p: ir.Path): ir.WcTypeRef = p match {
        case ir.Path(lv, List()) => wt_lv(lv)
        case ir.Path(lv, f :: fs) => 
            val q = lv ++ fs
            val t = cap(wt_path(q))
            val fd = fieldDecl(t, f)
            if(fd.isFinal) fd.wt
            else throw new ir.IrError("intervals.not.final", q, t, f)
    }
    
    // ______________________________________________________________________
    // Path Canonicalization
    
    class Canon(excl: List[ir.VarName]) extends BaseSubst {
        
        def without(lvs: List[ir.VarName]) =
            new Canon(excl ++ lvs)
        
        def path(p: ir.Path): ir.Path = {
            p match {
                case ir.Path(lv, List()) => p // ref. to a lv is canonical
                case ir.Path(lv, f :: fs) =>
                    val p1 = path(ir.Path(lv, fs))
                    val wt = wt_path(p1)
                    val cd = classDecl(wt.c)
                    cd.ghosts.zip(wt.wpaths).find(_._1.name == f) match {
                        case Some((_, p: ir.Path)) => p
                        case _ => p1 + f
                    }
            }
        }        
        
    }
    val canon = new Canon(List())
    
    // ______________________________________________________________________
    
    /// field decl of p.f, with ref to this subst'd
    def substdFieldDecl(p: ir.Path, f: ir.FieldName) = {
        val fd0 = fieldDecl(cap(wt_path(p)), f)
        val fd1 = PathSubst.vp(ir.lv_this, p).fieldDecl(fd0)
        canon.fieldDecl(fd1)
    }

    /// method sig. for o.m(p) with subst. performed
    def substdMethodSig(p: ir.Path, m: ir.MethodName, qs: List[ir.Path]) = {
        val t_p = cap(wt_path(p))
        val ts_q = qs.map(wt_path).map(cap)
        val msig = methodSig(t_p, m)
        foreachzip(ts_q, msig.args.map(_.wt))(checkIsSubtype)
        val msig1 = PathSubst.vp(
            ir.lv_this :: msig.args.map(_.name),
            p :: qs
        ).methodSig(msig)
        canon.methodSig(msig1)
    }
    
    /// effect of invoking p.m(q) where p has type wt_p
    def effectwt(p: ir.Path, wt_p: ir.WcTypeRef, m: ir.MethodName, qs: List[ir.Path]) = {
        val t_p = cap(wt_p)
        val e = t_p.overs.find(_.m == m) match {
            case Some(ov) => 
                PathSubst.vp(
                    ir.lv_this :: ov.args,
                    p :: qs
                ).effect(ov.e)
            case None =>
                val msig = methodSig(t_p, m)
                PathSubst.vp(
                    ir.lv_this :: msig.args.map(_.name),
                    p :: qs
                ).effect(msig.e)
        }
        canon.effect(e)
    }
    
    /// effect of invoking p.m(q)
    def effect(p: ir.Path, m: ir.MethodName, qs: List[ir.Path]) =
        effectwt(p, wt_path(p), m, qs)
    
    /// wp <: wq
    def isSubpath(wp: ir.WcPath, wq: ir.WcPath) = (wp, wq) match {
        case (_, ir.WcUnkPath) => true
        case (ir.WcUnkPath, _) => false
        case (p: ir.Path, q: ir.Path) => p == q
    }
    
    /// wp <:g wq
    def isSubguard(wp: ir.WcPath, wq: ir.WcPath) =  
        isSubpath(wp, wq) // XXX Take type into account.

    /// wp # wq
    val builtIn = Set[ir.WcPath](ir.p_readOnly, ir.p_locked)
    def isDisjointGuard(wp: ir.WcPath, wq: ir.WcPath) =  
        if(wp != wq && (builtIn(wp) || builtIn(wq)))
            true
        else
            env.disjoints(wp).exists(isSubguard(wq, _))

    /// p→q in the minimal interpretation
    def hb(p: ir.Path, q: ir.Path): Boolean =
        env.min.contains(p, q)
        
    /// p either is q or p→q
    def hbeq(p: ir.Path, q: ir.Path): Boolean =
        p == q || hb(p, q)
    
    /// i is a smaller-or-equal span of time than j
    def isSubinterval(i: ir.Interval, j: ir.Interval) =
        forallcross(j.ps, i.ps, hbeq) && forallcross(i.qs, j.qs, hbeq)
    
    /// e0 <: f0
    def isSubeffect(e0: ir.Effect, f0: ir.Effect): Boolean = (e0, f0) match {
        case (ir.EffectNone, _) => true
        case (_, ir.EffectNone) => false
        
        case (_, ir.EffectAny) => true
        case (ir.EffectAny, _) => false
        
        case (ir.EffectUnion(es), f) => es.forall(isSubeffect(_, f))
        case (ir.EffectMethod(p, m, qs), f) =>
            isSubeffect(effect(p, m, qs), f)
            
        case (e, ir.EffectUnion(fs)) => fs.exists(isSubeffect(e, _))
        case (e, ir.EffectMethod(p, m, qs)) =>
            isSubeffect(e, effect(p, m, qs))
            
        case (ir.EffectInterval(i, e), ir.EffectInterval(j, f)) =>
            isSubinterval(i, j) && isSubeffect(e, f)
            
        case (ir.EffectLock(ps, e), ir.EffectLock(qs, f)) =>
            ps.forall(p =>
                qs.exists(q =>
                    isSubguard(p, q))) &&
            isSubeffect(e, f)

        case (ir.EffectFixed(_, p), ir.EffectFixed(ir.Wr, q)) =>
            isSubpath(p, q)
        case (ir.EffectFixed(ir.Rd, p), ir.EffectFixed(ir.Rd, q)) =>
            isSubpath(p, q)
            
        case _ =>
            false
    }
    
    def checkIsSubeffect(e: ir.Effect, f: ir.Effect) {
        if(!isSubeffect(e, f))
            throw new ir.IrError("intervals.expected.subeffect", e, f)
    }
    
    /// are effects of ov
    def isSubover(ov_sub: ir.Over, ov_sup: ir.Over): Boolean = {
        if(ov_sub.m == ov_sup.m) {
            val lvs = ov_sub.args.map { case _ => ir.VarName(fresh("Sub")).path }
            val es_sub = PathSubst.vp(ov_sub.args, lvs).effect(ov_sub.e)
            val es_sup = PathSubst.vp(ov_sup.args, lvs).effect(ov_sup.e)
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
            forallzip(wt_sub.wpaths, wt_sup.wpaths, isSubpath) &&
            wt_sub.overs.forall(ov_sub => 
                isSubeffect(ov_sub.e, 
                    effectwt(ir.p_this, wt_sup, ov_sub.m, ov_sub.args.map(_.path))))
        }
    }
    
    def checkIsSubtype(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef) {
        if(!isSubtype(wt_sub, wt_sup))
            throw new ir.IrError("intervals.expected.subtype", wt_sub, wt_sup)
    }
    
    def ordered(oi_0: Option[ir.Interval], oi_1: Option[ir.Interval]) =
        (oi_0, oi_1) match {
            case (None, None) => true
            case (Some(i_0), Some(i_1)) =>
                forallcross(i_0.qs, i_1.ps, hbeq) ||
                forallcross(i_1.qs, i_0.ps, hbeq)
            case _ => false
        }
        
    def canFollow(
        oi_pre: Option[ir.Interval],
        l_pre: Set[ir.Path],
        e_pre: ir.Effect,        
        oi_post: Option[ir.Interval],
        l_post: Set[ir.Path],
        e_post: ir.Effect
    ): Boolean = (e_pre, e_post) match {
        case (_, ir.EffectNone) => true
        case (ir.EffectNone, _) => true
    
        case (_, ir.EffectAny) => false
        case (ir.EffectAny, _) => false
    
        case (ir.EffectUnion(es_pre), _) => 
            es_pre.forall(canFollow(oi_pre, l_pre, _, oi_post, l_post, e_post))
        case (_, ir.EffectUnion(es_post)) => 
            es_post.forall(canFollow(oi_pre, l_pre, e_pre, oi_post, l_post, _))
    
        case (ir.EffectInterval(i_pre, e_pre), _) => 
            canFollow(Some(i_pre), Set(), e_pre, oi_post, l_post, e_post)
        case (_, ir.EffectInterval(i_post, e_post)) => 
            canFollow(oi_pre, l_pre, e_pre, Some(i_post), Set(), e_post)
        
        case (ir.EffectLock(os_pre, e_pre_lck), _) => 
            canFollow(oi_pre, l_pre ++ os_pre, e_pre_lck, oi_post, l_post, e_post)
        case (_, ir.EffectLock(os_post, e_post_lck)) => 
            canFollow(oi_pre, l_pre, e_pre, oi_post, l_post ++ os_post, e_post_lck)
            
        case (ir.EffectMethod(o, m, p), _) => 
            canFollow(oi_pre, l_pre, effect(o, m, p), oi_post, l_post, e_post)
        case (_, ir.EffectMethod(o, m, p)) => 
            canFollow(oi_pre, l_pre, e_pre, oi_post, l_post, effect(o, m, p))                
        
        case (ir.EffectFixed(k_pre, p_pre), ir.EffectFixed(k_post, q_post)) => 
            isDisjointGuard(p_pre, q_post) ||
            (k_pre == ir.Rd && k_post == ir.Rd) ||
            (l_pre(p_pre) && l_post(q_post)) || // XXX Or a super guard is locked?
            ordered(oi_pre, oi_post)
    }
        
    def checkAllows(e_pre: ir.Effect, e_post: ir.Effect) =
        if(!canFollow(None, Set(), e_pre, None, Set(), e_post))
            throw new ir.IrError("intervals.cannot.follow", e_pre, e_post)
            
    def checkWfEffect(e: ir.Effect) = {
        // XXX
    }
            
    def checkWfWt(wt: ir.WcTypeRef) = {
        val cd = classDecl(wt.c)
        
        if(cd.ghosts.length != wt.wpaths.length)
            throw new ir.IrError("intervals.wrong.number.of.ghosts", cd.ghosts.length, wt.wpaths.length)
        
        wt.overs.foreach { case ov =>
            cd.methods.find(_.name == ov.m) match {
                case Some(_) => checkWfEffect(ov.e)
                case None => throw new ir.IrError("intervals.no.such.method", wt, ov.m)
            }
        }
    }

    def realFieldDecl(p: ir.Path, f: ir.FieldName) = {
        val fd = substdFieldDecl(p, f)
        if(fd.isGhost)
            throw new ir.IrError("intervals.not.with.ghost", p, f)
        fd.asInstanceOf[ir.RealFieldDecl]        
    }
    
    def checkReified(p: ir.Path) {
        // XXX
    }
    
    def expr(ex: ir.Expr): (ir.WcTypeRef, ir.Effect) = ex match {
        
        case ir.ExprCall(p, m, qs) =>
            checkReified(p)
            qs.foreach(checkReified)
            
            val t_p = cap(wt_path(p))
            val wts_q = qs.map(wt_path)
        
            // Check argument types match:
            val msig = methodSig(t_p, m)
            foreachzip(wts_q, msig.args.map(_.wt))(checkIsSubtype)
        
            // Compute return type:
            val wt_ret0 = 
                PathSubst.vp(
                    ir.lv_this :: msig.args.map(_.name), 
                    p :: qs
                ).wtref(msig.wt_ret)
            val wt_ret = canon.wtref(wt_ret0)
                    
            (wt_ret, effectwt(p, t_p, m, qs))
                   
        case ir.ExprField(p, f) =>
            checkReified(p)
            val fd = realFieldDecl(p, f)
            (fd.wt, ir.EffectFixed(ir.Rd, fd.guard))
            
        case ir.ExprNew(t, qs) =>
            checkWfWt(t)            
            qs.foreach(checkReified)
            
            val cd = classDecl(t.c)
            
            // Check types of ghosts:
            if(cd.ghosts.length != t.paths.length)
                throw new ir.IrError(
                    "intervals.wrong.number.ghost.arguments", 
                    cd.ghosts.length, t.paths.length)
            foreachzip(cd.ghosts, t.paths) { case (gfd, p) =>
                val wt_p = wt_path(p)
                checkIsSubtype(wt_p, gfd.wt)
            }
            
            // Check types of final arguments:
            val fds_final = cd.fields.filter(_.isFinal)
            if(fds_final.length != qs.length)
                throw new ir.IrError(
                    "intervals.wrong.number.final.arguments", 
                    fds_final.length, qs.length)
            foreachzip(fds_final, qs) { case (fd, q) =>
                val wt_q = wt_path(q)
                checkIsSubtype(wt_q, fd.wt)
            }
            
            (t, ir.EffectNone)
            
        case ir.ExprNewInterval(p_b, p_t, ps_g) =>
            checkReified(p_b)
            checkReified(p_t)
            ps_g.foreach(checkReified)
            checkIsSubtype(wt_path(p_b), ir.wt_point)
            checkIsSubtype(wt_path(p_t), ir.wt_task)
            ps_g.map(wt_path).foreach(checkIsSubtype(_, ir.wt_guard))
            val e_m = effect(p_t, ir.m_run, List(ir.p_new))
            val e_l = ir.EffectLock(ps_g, e_m)
            val e_i = ir.EffectInterval(ir.startEnd(ir.p_new), e_l)
            (ir.TypeRef(ir.c_interval, List(p_b), List()), e_i)
            
        case ir.ExprNewGuard(p) =>
            checkReified(p)
            checkIsSubtype(wt_path(p), ir.wt_guard)
            (ir.TypeRef(ir.c_guard, List(p), List()), ir.EffectNone)
            
        case ir.ExprNull =>
            (ir.t_void, ir.EffectNone)
            
    }
    
    def statement(stmt: ir.Stmt): ir.Effect = 
        at[ir.Effect](stmt, ir.EffectNone) {
            stmt match {        
                case ir.StmtVarDecl(ir.LvDecl(x, wt_x), ex) =>
                    checkWfWt(wt_x)
                    val (wt_e, e) = expr(ex)
                    val subst = PathSubst.vp(ir.lv_new, x.path)
                    if(ex != ir.ExprNull)
                        checkIsSubtype(wt_e, wt_x)
                    addLv(x, wt_x)
                    canon.effect(subst.effect(e))
        
                case ir.StmtAssignField(p, f, q) =>
                    checkReified(p)
                    checkReified(q)                    
                    val fd = realFieldDecl(p, f)
                    checkIsSubtype(wt_path(q), fd.wt)
                    if(fd.isFinal)
                        ir.EffectNone // kind of a hack...
                    else
                        ir.EffectFixed(ir.Wr, fd.guard)
        
                case ir.StmtAddHb(p, q) =>
                    checkReified(p)
                    checkReified(q)                    
                    checkIsSubtype(wt_path(p), ir.wt_point)
                    checkIsSubtype(wt_path(q), ir.wt_point)
                    addHb(p, q)
                    ir.EffectNone            
            }
        } 
        
    def statements(e0: ir.Effect, stmts0: List[ir.Stmt]): ir.Effect = stmts0 match {
        case List() => e0
        case stmt :: stmts =>
            val e1 = log.indented(stmt) {
                val e = statement(stmt)
                log("Effects Before: %s", e0)
                log("Effects: %s", e)
                checkAllows(e0, e)
                ir.union(e0, e)
            }
            statements(e1, stmts)
    }
    
    def checkRealFieldDecl(rfd: ir.RealFieldDecl) = savingEnv {
        log.indented(rfd) {
            at(rfd, ()) {
                checkWfWt(rfd.wt)
                val wt_guard = wt_path(rfd.guard)
                checkIsSubtype(wt_guard, ir.wt_guard)
            }
        }
    }
    
    def checkMethodDecl(md: ir.MethodDecl) = savingEnv {
        log.indented(md) {
            at(md, ()) {
                md.args.foreach { case arg => 
                    checkWfWt(arg.wt)
                    addLv(arg.name, arg.wt) 
                }
                val e_stmts = statements(ir.EffectNone, md.stmts)
                val (wt_ret, e_ret) = expr(md.ex_ret)
                if(md.ex_ret != ir.ExprNull)
                    checkIsSubtype(wt_ret, md.wt_ret)
                checkAllows(e_stmts, e_ret)
                checkIsSubeffect(ir.union(e_stmts, e_ret), md.e)
            }         
        }
    }
    
    def checkClassDecl(cd: ir.ClassDecl) = savingEnv {
        log.indented(cd) {
            at(cd, ()) {
                val ps_ghosts = cd.ghosts.map(_.thisPath)
                val t_this = ir.TypeRef(cd.name, ps_ghosts, List())
                addLv(ir.lv_this, t_this)
                cd.fields.foreach(checkRealFieldDecl)
                cd.methods.foreach(checkMethodDecl)
            }
        }
    }
    
    def check = prog.cds_user.foreach(checkClassDecl)
}