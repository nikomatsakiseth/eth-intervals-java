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
            (ir.lv_readOnly, ir.TeePee(ir.t_interval, ir.p_readOnly, false)),
            (ir.lv_cur, ir.TeePee(ir.t_interval, ir.p_cur, false))
        ),
        ir.Relation.emptyTrans,
        ir.Relation.emptyTransRefl,
        ir.Relation.empty
    )
    
    def savingEnv[R](g: => R): R = {
        val oldEnv = env
        try { g } finally { env = oldEnv }
    }
    
    def addLv(lv: ir.VarName, tp: ir.TeePee): Unit = 
        log.indented("addLv(%s,%s)", lv, tp) {
            env.lvs.get(lv) match {
                case None =>
                    env = ir.TcEnv(
                        env.lvs + Pair(lv, tp),
                        env.hb,
                        env.hbeq,
                        env.locks)
                case Some(_) =>
                    throw new ir.IrError("intervals.shadowed.variable", lv)
            }
        }

    def tp_lv(lv: ir.VarName) =
        env.lvs.get(lv) match {
            case Some(tp) => tp
            case None => throw ir.IrError("intervals.no.such.variable", lv)
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
    // Basic Type Operations: Finding fields, methods, supertypes
        
    /// subst. ghosts of t
    def ghostSubst(t: ir.TypeRef) = {
        val ghostPaths = classDecl(t.c).ghosts.map(_.thisPath)
        PathSubst.pp(ghostPaths, t.paths)
    }

    /// supertype of t
    def sup(t: ir.TypeRef): Option[ir.TypeRef] = {
        val cd = classDecl(t.c)
        cd.superType.map { case t_1 => ghostSubst(t).tref(t_1) }
    }

    /// Field decl for t0::f 
    def realFieldDecl(c: ir.ClassName, f: ir.FieldName): ir.RealFieldDecl = {
        log.indentedRes("realFieldDecl(%s,%s)", c, f) {
            def search(t: ir.TypeRef): ir.RealFieldDecl = 
                log.indentedRes("search(%s)", t) {
                    val cd = classDecl(t.c)
                    cd.fields.find(_.name == f) match {
                        case Some(fd) => fd
                        case None => sup(t) match {
                            case Some(t_1) => ghostSubst(t_1).realFieldDecl(search(t_1))
                            case None => throw ir.IrError("intervals.no.such.field", c, f)
                        }
                    }
                }
            val cd = classDecl(c)
            search(cd.thisTref)
        }            
    }

    /// Method sig for t0::m()
    def methodSig(c: ir.ClassName, m: ir.MethodName): ir.MethodSig = {
        log.indentedRes("methodSig(%s,%s)", c, m) {
            def search(t: ir.TypeRef): ir.MethodSig = {
                val cd = classDecl(t.c)
                cd.methods.find(_.name == m) match {
                    case Some(md) => md.msig
                    case None => sup(t) match {
                        case Some(t_1) => ghostSubst(t_1).methodSig(search(t_1))
                        case None => throw ir.IrError("intervals.no.such.method", c, m)
                    }
                }
            }
            val cd = classDecl(c)
            search(cd.thisTref)
        }
    }
    
    // ______________________________________________________________________
    // TeePees
    //
    // A TeePee is a typed, canonical path.  The value of a TeePees is 
    // generally constant during the current method, with one exception:
    // a TeePee may reference fields of the this object guarded by the
    // current interval or held under lock, which could then be mutated.  
    
    def ghostPaths(tp: ir.TeePee): List[ir.Path] = {
        val cd = classDecl(tp.wt.c)
        cd.ghosts.zip(tp.wt.wpaths).map {
            case (_, s: ir.Path) => s
            case (gfd, _) => tp.p + gfd.name
        }        
    }
    
    def ghostSubst(tp: ir.TeePee): PathSubst = {
        val cd = classDecl(tp.wt.c)
        PathSubst.pp(
            ir.p_this :: cd.ghosts.map(_.thisPath),
            tp.p :: ghostPaths(tp)
        )     
    }
    
    def cap(tp: ir.TeePee): ir.TypeRef =
        ir.TypeRef(tp.wt.c, ghostPaths(tp))        
    
    /// Field decl for tp.f
    def substdRealFieldDecl(tp: ir.TeePee, f: ir.FieldName) = {
        val rfd = realFieldDecl(tp.wt.c, f)
        ghostSubst(tp).realFieldDecl(rfd)
    }
    
    /// Method sig for tp.m(tqs)
    def substdMethodSig(tp: ir.TeePee, m: ir.MethodName, tqs: List[ir.TeePee]) = {
        val msig = methodSig(tp.wt.c, m)
        val subst = ghostSubst(tp) + PathSubst.vp(msig.args.map(_.name), tqs.map(_.p))
        subst.methodSig(msig)        
    }
    
    def constructTeePee(p0: ir.Path): ir.TeePee = log.indentedRes("constructTeePee(%s)", p0) {
        p0 match {
            case ir.Path(lv, List()) => 
                tp_lv(lv)
            case ir.Path(lv, f :: rev_fs) =>
                val tp = constructTeePee(ir.Path(lv, rev_fs))
                
                // Invoked when we have a path like a.b.c.f 
                // where a.b.c yields a type Foo<f: ps hb/eq qs>.
                // In other words, the field f is a ghost field
                // of a.b.c, and the value on a.b.c is a wildcard.
                // In that case, the final path is a.b.c.p, but
                // we add whatever relations we can to it.
                def ghost(
                    gfd: ir.GhostFieldDecl,
                    add: Function2[ir.TeePee, ir.TeePee, Unit], 
                    ps: List[ir.Path], 
                    qs: List[ir.Path]) = 
                {
                    // Compute final path a.b.c.f:
                    val wt_cap = ghostSubst(tp).wtref(gfd.wt)
                    val tp_cap = ir.TeePee(wt_cap, tp.p + f, false)
                    
                    // Add relations from the wildcard:                    
                    ps.foreach { case p => add(teePee(p), tp_cap) }
                    qs.foreach { case q => add(tp_cap, teePee(q)) }
                    
                    tp_cap
                }
                   
                // Look for a ghost field with the name "f".  If it maps
                // to a fixed path p, start again from p.  If it
                // maps to a wildcard, then keep path but use ghost to add
                // appropriate restrictions.
                val cd = classDecl(tp.wt.c)
                cd.ghosts.zip(tp.wt.wpaths).find(_._1.name == f) match {
                    case Some((gfd, p: ir.Path)) => 
                        if(tp.p + f == p) // This is a captured version of the ghost:
                            ir.TeePee(gfd.wt, p, tp.as.withGhost)
                        else
                            constructTeePee(p)
                    case Some((gfd, ir.WcHb(ps, qs))) => ghost(gfd, addHb, ps, qs)
                    case Some((gfd, ir.WcHbEq(ps, qs))) => ghost(gfd, addHbEq, ps, qs)
                    case None =>                     
                        if(f == ir.f_ctor) {
                            // Hokey special case constructor interval:
                            //   We assume that x.constructor has always finished
                            //   if x≠this, because this does not escape the ctor.
                            val tq = ir.TeePee(ir.t_interval, tp.p + f, tp.as.withGhost)
                            addHbEq(tq, tp_mthd)
                            tq
                        } else {
                            // Look for a "reliable" real field (not quite the same as "constant").
                            val rfd = substdRealFieldDecl(tp, f)
                            checkReqFulfilled(
                                // XXX Permit locks as well
                                if(tp.p == ir.p_this) ir.ReqHbEq(rfd.p_guard, List(ir.p_cur))
                                else ir.ReqHb(rfd.p_guard, List(ir.p_cur))
                            )
                            ir.TeePee(rfd.wt, tp.p + f, tp.reified)                            
                        }                    
                }
        }
    }

    /// Constructs a 'tp' for 'p' and checks that it has at most the attributes 'as'
    def teePee(as: ir.Attrs, p: ir.Path): ir.TeePee = {
        val tp = teePeeAny(as)
        val unwanted = tp.as.diff(as)
        if(!unwanted.isEmpty)
            throw new ir.IrError("intervals.illegal.path.attr", p, unwanted.mkEnglishString)
    }
        
    /// Constructs tps for 'ps' and checks that they have at most the attributes 'as'
    def teePee(as: ir.Attrs, ps: List[ir.Path]): List[ir.TeePee] = 
        ps.map(teePee)
        
    // Note: these are not vals but defs!  This is important
    // because the outcome of teePee() depends on the env.
    def tp_cur = teePee(ir.p_cur)
    def tp_mthd = teePee(ir.p_mthd)
    def tp_this = teePee(ir.p_this)
    
    // ______________________________________________________________________
    // Well-formedness
    //
    // Very basic sanity checks.
    
    def checkWfReq(req: ir.Req) = req match {
        case ir.ReqHb(p, qs) => teePee(p); teePee(qs)
        case ir.ReqHbEq(p, qs) => teePee(p); teePee(qs)
        case ir.ReqEq(lv, p) => teePee(lv.path); teePee(p)
        case ir.ReqEqPath(p, q) => teePee(p); teePee(q)
        case ir.ReqLocks(p, qs) => teePee(p); teePee(qs)
    }
    
    def checkWfWt(wt: ir.WcTypeRef) = {
        val cd = classDecl(wt.c)
        
        if(cd.ghosts.length != wt.wpaths.length)
            throw new ir.IrError(
                "intervals.wrong.number.of.ghosts", 
                cd.ghosts.length, wt.wpaths.length)
                
        // Note: we don't check that the arguments
        // match the various ghost bounds etc.  We just
        // check that when the type is constructed.
    }

    // ______________________________________________________________________
    // Env Relations
    
    def declLv(lv: ir.VarName, tq: ir.TeePee) {
        log.indented("declLv(%s,%s)", lv, tq) {
            env.canon.get(lv.path) match {
                case Some(_) =>
                    throw new ir.IrError("intervals.variable.in.scope", lv)
                
                case None =>
                    env = ir.TcEnv(
                        env.canon + Pair(lv.path, tq),
                        env.fs_invalidated,
                        env.hb,
                        env.hbeq,
                        env.locks,
                        env.mutable)
            }
        }
    }
    
    def addCanon(tp: ir.TeePee, tq: ir.TeePee) {
        log.indented("addCanon(%s,%s)", tp, tq) {
            env = ir.TcEnv(
                env.canon + Pair(tp, tq),
                env.fs_invalidated,
                env.hb,
                env.hbeq,
                env.locks,
                env.mutable)
        }
    }
        
    def addHb(tp: ir.TeePee, tq: ir.TeePee): Unit = log.indented("addHb(%s,%s)", tp, tq) {
        assert(isSubtype(cap(tp), ir.t_interval))
        assert(isSubtype(cap(tq), ir.t_interval))
        env = ir.TcEnv(
            env.lvs,
            env.hb + (tp.p, tq.p),
            env.hbeq + (tp.p, tq.p),
            env.locks)
    }
    
    def addHbEq(tp: ir.TeePee, tq: ir.TeePee): Unit = log.indented("addHbEq(%s,%s)", tp, tq) {
        assert(isSubtype(cap(tp), ir.t_interval))
        assert(isSubtype(cap(tq), ir.t_interval))
        env = ir.TcEnv(
            env.lvs,
            env.hb,
            env.hbeq + (tp.p, tq.p),
            env.locks)
    }
    
    def addLocks(tp: ir.TeePee, tq: ir.TeePee): Unit = log.indented("addLocks(%s,%s)", tp, tq) {
        assert(isSubtype(cap(tp), ir.t_interval))
        assert(isSubtype(cap(tq), ir.t_lock))
        env = ir.TcEnv(
            env.lvs,
            env.hb,
            env.hbeq,
            env.locks + (tp.p, tq.p))
    }
    
    def hb(p: ir.TeePee, q: ir.TeePee): Boolean = log.indentedRes("%s hb %s?", p, q) {
        env.hb.contains(p.p, q.p)
    }
    
    def hbeq(p: ir.TeePee, q: ir.TeePee): Boolean = log.indentedRes("%s hbeq %s?", p, q) {
        env.hbeq.contains(p.p, q.p)
    }
    
    def equiv(p: ir.TeePee, q: ir.TeePee): Boolean = log.indentedRes("%s eq %s?", p, q) {
        p.p == q.p
    }
    
    def locks(p: ir.TeePee, q: ir.TeePee): Boolean = log.indentedRes("%s locks %s?", p, q) {
        env.locks.contains(p.p, q.p)
    }
    
    def addReq(req: ir.Req) = {
        def teePeeAdd(add: Function2[ir.TeePee, ir.TeePee, Unit], p: ir.Path, qs: List[ir.Path]) = {
            val tp = teePee(p)
            val tqs = teePee(qs)
            tqs.foreach(add(tp, _))
        }
        req match {
            case ir.ReqHb(p, qs) => teePeeAdd(addHb, p, qs)
            case ir.ReqHbEq(p, qs) => teePeeAdd(addHbEq, p, qs)
            case ir.ReqEqPath(p, q) => addCanon(teePee(p), teePee(q))
            case ir.ReqLocks(p, qs) => teePeeAdd(addLocks, p, qs)
        }   
    } 
    
    // ______________________________________________________________________
    // Subtyping    
    
    /// wp <= wq
    def isSubpath(wp: ir.WcPath, wq: ir.WcPath) = log.indentedRes("%s <= %s?", wp, wq) {
        (wp, wq) match {
            case (p: ir.Path, q: ir.Path) => 
                eq(p, q)
            case (_, q: ir.Path) =>
                false
            case (r: ir.Path, ir.WcHb(ps, qs)) => 
                ps.forall { p => hb(p, r) } &&
                qs.forall { q => hb(r, q) }
            case (r: ir.Path, ir.WcHbEq(ps, qs)) => 
                ps.forall { p => hbeq(p, r) } &&
                qs.forall { q => hbeq(r, q) }
            case (ir.WcHb(rs, ss), ir.WcHb(ps, qs)) => 
                forallcross(ps, rs)(hbeq) &&
                forallcross(ss, qs)(hbeq)
            case (ir.WcHbEq(rs, ss), ir.WcHbEq(ps, qs)) => 
                forallcross(ps, rs)(hbeq) &&
                forallcross(ss, qs)(hbeq)
            case (ir.WcHb(rs, ss), ir.WcHbEq(ps, qs)) => 
                forallcross(ps, rs)(hb) &&
                forallcross(ss, qs)(hb)
            case (ir.WcHbEq(rs, ss), ir.WcHb(ps, qs)) => 
                forallcross(ps, rs)(hb) &&
                forallcross(ss, qs)(hb)
        }
    }
    
    /// t_sub <: wt_sup
    def isSubtype(t_sub: ir.TypeRef, wt_sup: ir.WcTypeRef): Boolean =
        log.indentedRes("%s <: %s?", t_sub, wt_sup) {
            
            // (t <: t constructor) but (t constructor not <: t)
            if(wt_sup.as.ctor && !t.as.ctor) 
                false
            
            if(t_sub.c == wt_sup.c)
                forallzip(t_sub.wpaths, wt_sup.wpaths)(isSubpath) 
            else
                sup(t_sub) match {
                    case None => false
                    case Some(t) => isSubtype(t, wt_sup)
                }                
            
        }
    
    def isSubtype(tp_sub: ir.TeePee, wt_sup: ir.WcTypeRef): Boolean =
        isSubtype(cap(tp_sub), wt_sup)
        
    /// wt_sub <: wt_sup
    def checkIsSubtype(tp_sub: ir.TeePee, wt_sup: ir.WcTypeRef) {
        if(!isSubtype(tp_sub, wt_sup))
            throw new ir.IrError("intervals.expected.subtype", tp_sub.p, tp_sub.wt, wt_sup)
    }
        
    def isReqFulfilled(req: ir.Req) = req match {
        case ir.ReqHb(p, qs) => qs.forall(hb(p, _))
        case ir.ReqHbEq(p, qs) => qs.forall(hbeq(p, _))
        case ir.ReqEq(lv, p) => eq(lv.path, p)
        case ir.ReqEqPath(p, q) => eq(p, q)
        case ir.ReqLocks(p, qs) => qs.forall(locks(p, _))
    }
    
    def checkReqFulfilled(req: ir.Req) {
        if(!isReqFulfilled(req))
            throw new ir.IrError("intervals.requirement.not.met", req)
    }
    
    def checkLengths(l1: List[_], l2: List[_], msg: String) {
        if(l1.length != l2.length)
            throw new ir.IrError(msg, l1.length, l2.length)
    }
    
    def checkArgumentTypes(msig: ir.MethodSig, tqs: List[ir.TeePee]) {
        checkLengths(msig.args, tqs, "intervals.wrong.number.method.arguments")
        foreachzip(tqs, msig.args.map(_.wt))(checkIsSubtype)
    }
    
    def checkLvDecl(vd: ir.LvDecl, owt: Option[ir.WcTypeRef], op: Option[ir.Path]) = {
        
        // XXX make sure vd not already in scope
        
        checkWfWt(vd.wt)
        
        owt.foreach (savingEnv { case wt =>
            val lv = ir.VarName(fresh("val"))
            val tp = ir.TeePee(wt, lv.path, true)
            addLv(lv, tp)
            checkIsSubtype(tp, vd.wt)
        })
        
        addLv(vd.name, ir.TeePee(
            vd.wt, 
            op.getOrElse(vd.name.path),
            true))
    }
    
    def checkCurrentLocks(tp_guard: ir.TeePee) {
        if(!locks(tp_cur, tp_guard))
            throw new ir.IrError("intervals.no.lock", tp_guard.p)
    }
    
    /// Returns true if 'wt' is linked to 'p': that is,
    /// if it depends on 'p' in some way.  'p' must be
    /// a canonical path.
    def isLinkedWt(p: ir.Path, wt: ir.WcTypeRef) =
        wt.wpaths.exists(_.dependentOn(p))
    
    /// A linked field f_l to p.f is one whose type is invalidated
    /// if p.f changes.  In general this is because p.f appears
    /// in the type of f_l.
    def linkedFields(tp: ir.TeePee, f: ir.FieldName) = {
        val cd = classDecl(tp.wt.c)
        val p_f = f.thisPath
        
        cd.fields.filter { rfd => isLinkedWt(p_f, rfd.wt) }
    }
    
    def linkedVariables(tp: ir.TeePee, f: ir.FieldName): List[ir.VarName] = {
        val p_f = f.thisPath
        
        env.lvs.values.
        List()
    }
                    
    def checkStatement(stmt: ir.Stmt) = log.indented(stmt) {
        at(stmt, ()) {
            stmt match {  
                case ir.StmtCall(vd, p, m, qs) =>
                    val tp = teePee(p)
                    val tqs = teePee(qs)
                    val msig = substdMethodSig(tp, m, tqs)
                    checkArgumentTypes(msig, tqs)
                    checkLvDecl(vd, Some(msig.wt_ret), None)
                    msig.reqs.foreach(checkReqFulfilled)
        
                case ir.StmtGetField(vd, p, f) =>
                    val tp = teePee(p)
                    val fd = substdRealFieldDecl(tp, f)
                    val tp_guard = teePee(fd.p_guard)

                    val op = 
                        if(isSubtype(tp_guard, ir.t_interval)) {
                            if(hb(tp_guard, tp_mthd))
                                Some(p + f) // safe, constant
                            else if(eq(tp_guard, tp_cur))
                                None // safe, non-constant
                            else if(eq(tp_guard, tp_mthd))
                                None // safe, non-constant
                            else 
                                throw new ir.IrError("intervals.no.hb", tp_guard.p)
                        } else {
                            checkCurrentLocks(tp_guard)
                            None // safe, non-constant
                        }
            
                    checkLvDecl(vd, Some(fd.wt), op)
                    
                case ir.StmtNew(vd, t, qs) =>
                    checkWfWt(t)
                    val cd = classDecl(t.c)

                    // Check Ghost Types:
                    checkLengths(cd.ghosts, t.paths, "intervals.wrong.number.ghost.arguments")
                    val substGhosts = PathSubst.pp(cd.ghosts.map(_.thisPath), t.paths)
                    foreachzip(cd.ghosts, t.paths) { case (gfd, p) =>
                        val tp = teePee(p)
                        checkIsSubtype(tp, substGhosts.wtref(gfd.wt))
                    }

                    // Check Argument Types:
                    val ctor = cd.ctor
                    val tqs = teePee(qs)
                    val subst = PathSubst.vp(
                        ir.lv_this :: ctor.args.map(_.name),
                        vd.name.path :: tqs.map(_.p)
                    )
                    val msig = subst.methodSig(ctor.msig)
                    checkArgumentTypes(msig, tqs)

                    // Check Constructor Requirements:
                    msig.reqs.foreach(checkReqFulfilled)
                    
                    checkLvDecl(vd, Some(t), None)
                    
                case ir.StmtNull(vd) =>
                    checkLvDecl(vd, None, None)

                case ir.StmtSetField(p, f, q) =>
                    // XXX Here is where we might incur obligations
                    // XXX to re-assign other fields that use f!
                    val tp = teePee(p)
                    val tq = teePee(q)
                    val fd = substdRealFieldDecl(tp, f)
                    checkIsSubtype(tq, fd.wt)
                    
                    val tp_guard = teePee(fd.p_guard)
                    if(isSubtype(cap(tp_guard), ir.t_interval)) {
                        if(!eq(tp_cur, tp_guard))
                            throw new ir.IrError("intervals.not.current", tp_guard.p)                        
                    } else
                        checkCurrentLocks(tp_guard)
                        
                case ir.StmtHb(p, q) =>
                    val tp = teePee(p)
                    val tq = teePee(q)
                    
                    // XXX Rejigger HB to be based on points:
                    //checkIsSubtype(wt_path(p), ir.t_point)
                    //checkIsSubtype(wt_path(q), ir.t_point)
                    
                    checkIsSubtype(tp, ir.t_interval)
                    checkIsSubtype(tq, ir.t_interval)
                    addHb(tp, tq)
                    
                case ir.StmtLocks(p, q) =>
                    val tp = teePee(p)
                    val tq = teePee(q)
                    checkIsSubtype(tp, ir.t_interval)
                    checkIsSubtype(tq, ir.t_lock)
                    addLocks(tp, tq)
            }
        }
    }
    
    def checkRealFieldDecl(rfd: ir.RealFieldDecl) = savingEnv {
        log.indented(rfd) {
            at(rfd, ()) {
                checkWfWt(rfd.wt)
                val tp_guard = teePee(rfd.p_guard)
                if(!isSubtype(tp_guard, ir.t_interval) &&
                    !isSubtype(tp_guard, ir.t_lock))
                    throw new ir.IrError("intervals.invalid.guard", tp_guard.p)
            }
        }
    }
    
    def checkMethodDecl(md: ir.MethodDecl) = savingEnv {
        log.indented(md) {
            at(md, ()) {
                md.args.foreach { case arg => 
                    checkWfWt(arg.wt)
                    addLv(arg.name, ir.TeePee(arg.wt, arg.name.path, true))
                }
                
                md.reqs.foreach(checkWfReq)
                md.reqs.foreach(addReq)
                
                md.stmts.foreach(checkStatement)
                
                md.p_ret.foreach { p_ret =>
                    val tp_ret = teePee(p_ret)
                    checkReified(tp_ret)
                    checkIsSubtype(tp_ret, md.wt_ret)
                }
            }         
        }
    }
    
    def checkClassDecl(cd: ir.ClassDecl) = savingEnv {
        log.indented(cd) {
            at(cd, ()) {
                addLv(ir.lv_this, ir.TeePee(cd.thisTref, ir.p_this, true)) 
                cd.fields.foreach(checkRealFieldDecl)
                
                addHb(ir.p_readOnly, ir.p_cur) // HACK
                
                // XXX Need to extract visible effects of ctor
                // XXX and save them in environment for other
                // XXX methods.
                savingEnv {
                    addLv(ir.lv_mthd, ir.TeePee(ir.t_interval, ir.gfd_ctor.thisPath, false))
                    checkMethodDecl(cd.ctor)                    
                }
                
                savingEnv {
                    addLv(ir.lv_mthd, ir.TeePee(ir.t_interval, ir.p_mthd, false))
                    cd.ctor.reqs.foreach(addReq)
                    addHb(ir.gfd_ctor.thisPath, ir.p_mthd)
                    cd.methods.foreach(checkMethodDecl)                    
                }
            }
        }
    }
    
    def check = prog.cds_user.foreach(checkClassDecl)
}