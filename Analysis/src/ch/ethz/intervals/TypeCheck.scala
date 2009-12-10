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
            (ir.lv_readOnly, ir.TeePee(ir.t_interval, ir.p_readOnly)),
            (ir.lv_cur, ir.TeePee(ir.t_interval, ir.p_cur))
        ),
        ir.Relation.emptyTrans,
        ir.Relation.emptyTransRefl,
        ir.Relation.empty
    )
    
    val nonreifiedVars = Set(ir.lv_readOnly, ir.lv_cur, ir.lv_mthd)
    
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
    def realFieldDecl(t0: ir.TypeRef, f: ir.FieldName): ir.RealFieldDecl = 
        log.indentedRes("realFieldDecl(%s,%s)", t0, f) {
            def search(t: ir.TypeRef): ir.RealFieldDecl = 
                log.indentedRes("search(%s)", t) {
                    val cd = classDecl(t.c)
                    cd.fields.find(_.name == f) match {
                        case Some(fd) => fd
                        case None => sup(t) match {
                            case Some(t_1) => ghostSubst(t_1).realFieldDecl(search(t_1))
                            case None => throw ir.IrError("intervals.no.such.field", t0, f)
                        }
                    }
                }
            search(t0)        
        }
    
    /// Method sig for t0::m()
    def methodSig(t0: ir.TypeRef, m: ir.MethodName): ir.MethodSig = {
        def search(t: ir.TypeRef): ir.MethodSig = {
            val cd = classDecl(t.c)
            cd.methods.find(_.name == m) match {
                case Some(md) => md.msig
                case None => sup(t) match {
                    case Some(t_1) => ghostSubst(t_1).methodSig(search(t_1))
                    case None => throw ir.IrError("intervals.no.such.method", t0, m)
                }
            }
        }
        search(t0)
    }
    
    /// Field decl for tp.f
    def substdRealFieldDecl(tp: ir.TeePee, f: ir.FieldName) = {
        val rfd = realFieldDecl(tp.t, f)
        tp.thisSubst.fieldDecl(rfd)
    }
    
    /// Method sig for tp.m(tqs)
    def substdMethodSig(tp: ir.TeePee, m: ir.MethodName, tqs: List[ir.TeePee]) = {
        val msig = methodSig(tp.t, m)
        PathSubst.vp(
            ir.lv_this :: msig.args.map(_.name),
            tp.p :: tqs.map(_.p)
        ).methodSig(msig)        
    }
    
    // ______________________________________________________________________
    // TeePees
    //
    // A TeePee is a typed, canonical path.  The value of a TeePees is 
    // always constant during the current method.
    
    def teePee(p0: ir.Path): ir.TeePee = log.indentedRes("teePee(%s)", p0) {
        p0 match {
            case ir.Path(lv, List()) => 
                tp_lv(lv)
            case ir.Path(lv, f :: rev_fs) =>
                val tp = teePee(ir.Path(lv, rev_fs))
                
                // Captures a WcTypeRef to a TypeRef by
                // converting any wildcard paths to r.f
                // where f is the name of the ghost field.
                //
                // In other words, given Foo<x: ?> as wt
                // and a.b.c as r, would yield Foo<a.b.c.x>
                def capWt(wt: ir.WcTypeRef, r: ir.Path) = {
                    val cd = classDecl(wt.c)
                    ir.TypeRef(
                        wt.c,
                        cd.ghosts.zip(wt.wpaths).map { 
                            case (_, s: ir.Path) => s
                            case (gfd, _) => r + gfd.name
                        }
                    )
                }
                
                // Constructs a TeePee whose (wildcard) type is
                // wt and whose path is r.
                def capWteePee(wt: ir.WcTypeRef, r: ir.Path) =
                    ir.TeePee(capWt(wt, r), r)
            
                // Invoked when we have a path like a.b.c.f 
                // where a.b.c yields a type Foo<f: ps hb/eq qs>.
                // In other words, the field f is a ghost field
                // of a.b.c, and the value on a.b.c is a wildcard.
                // In that case, the final path is a.b.c.p, but
                // we add whatever relations we can to it.
                def ghost(
                    gfd: ir.GhostFieldDecl,
                    add: Function2[ir.Path, ir.Path, Unit], 
                    ps: List[ir.Path], 
                    qs: List[ir.Path]) = 
                {
                    // Compute final path a.b.c.f:
                    val p_cap = tp.p + f
                    
                    // Add relations from the wildcard:
                    ps.foreach(add(_, p_cap))
                    qs.foreach(add(p_cap, _))
                
                    val wt_cap = tp.thisSubst.wtref(gfd.wt)
                    capWteePee(wt_cap, p_cap)
                }
                   
                // Look for a ghost field with the name "f".  If it maps
                // to a fixed path p, start again from p.  If it
                // maps to a wildcard, then keep path but use ghost to add
                // appropriate restrictions.
                val cd = classDecl(tp.t.c)
                cd.ghosts.zip(tp.wt.wpaths).find(_._1.name == f) match {
                    case Some((_, p: ir.Path)) => teePee(p)
                    case Some((gfd, ir.WcHb(ps, qs))) => ghost(gfd, addHb, ps, qs)
                    case Some((gfd, ir.WcHbEq(ps, qs))) => ghost(gfd, addHbEq, ps, qs)
                    case None => 
                    
                        // Look for a constant real field.
                        val rfd = substdRealFieldDecl(tp, f)
                        checkReqFulfilled(ir.ReqHb(rfd.p_guard, List(ir.p_cur)))
                        capWteePee(rfd.wt, tp.p + f)
                }
        }
    }    
    
    def teePee(ps: List[ir.Path]): List[ir.TeePee] = 
        qs.map(teePee)
        
    def tp_cur = teePee(ir.p_cur)
        
    // ______________________________________________________________________
    // Path checking
    
    /// Checks whether path p is reified and/or constant.
    def checkPath(p: ir.Path, chkReified: Boolean, chkConstant: Boolean, chkReliable: Boolean) {
        p match {
            case ir.Path(lv, List()) => 
                if(chkReified && nonreifiedVars(lv))
                    throw new ir.IrError("intervals.reified.path.required", p)            
                    
            case ir.Path(lv, f :: rev_fs) => 
                val p1 = ir.Path(lv, rev_fs)
                checkPath(p1, chkReified, chkConstant, chkReliable)
                val wt_p1 = wt_path(p1)
                val t_p1 = cap(wt_p1)
                fieldDecl(t_p1, f) match {
                    case ir.RealFieldDecl(wt, _, p_guard) =>
                        // A field is constant if its guard has completed.
                        //
                        // A field is "reliable" if (a) it is constant or                        
                        // (b) the field is relative to "this"
                        if(chkConstant || (chkReliable && (p != ir.p_this + f)))
                            checkReqFulfilled(ir.ReqHb(p_guard, List(ir.p_cur)))

                    case ir.GhostFieldDecl(wt, _) =>
                        if(chkReified)
                            throw new ir.IrError("intervals.reified.path.required", p)
                }            
        }
    }
    
    def checkReifiedAndConstant(p: ir.Path) {
        checkPath(p, true, true, false)
    }
    
    def checkConstant(p: ir.Path) {
        checkPath(p, false, true, false)
    }
    
    def checkReliable(p: ir.Path) {
        checkPath(p, false, false, true)
    }
    
    // ______________________________________________________________________
    // Well-formedness
    //
    // Very basic sanity checks.
    
    def checkWfReq(req: ir.Req) = req match {
        case ir.ReqHb(p, qs) => checkWfPath(p); qs.foreach(checkWfPath)
        case ir.ReqHbEq(p, qs) => checkWfPath(p); qs.foreach(checkWfPath)
        case ir.ReqEq(lv, p) => checkWfPath(lv.path); checkWfPath(p)
        case ir.ReqEqPath(p, q) => checkWfPath(p); checkWfPath(q)
        case ir.ReqLocks(p, qs) => checkWfPath(p); qs.foreach(checkWfPath)
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
        
    def addEq(lv: ir.VarName, tp: ir.TeePee) =
        log.indented("addEq(%s,%s)", lv, tp) {
            env.lvs.get(lv) match {
                case None =>
                    throw new ir.IrError("intervals.no.such.variable", lv)
                case Some(tq) =>
                    // Check old type?
                    env = ir.TcEnv(
                        env.lvs + Pair(lv, tp),
                        env.hb,
                        env.hbeq,
                        env.locks)
            }
        }
        
    def addEq(lv: ir.VarName, p: ir.Path): Unit = addLv(lv, teePee(p))
    
    def addHb(tp: ir.TeePee, tq: ir.TeePee): Unit = log.indented("addHb(%s,%s)", p, q) {
        assert(isSubtype(tp.t, ir.t_interval))
        assert(isSubtype(tq.t, ir.t_interval))
        env = ir.TcEnv(
            env.lvs,
            env.hb + (tp.p, tq.p),
            env.hbeq + (tp.p, tq.p),
            env.locks)
    }
    
    def addHb(p: ir.Path, q: ir.Path): Unit = addHb(teePee(p), teePee(q))

    def addHbEq(tp: ir.TeePee, tq: ir.TeePee): Unit = log.indented("addHbEq(%s,%s)", p, q) {
        assert(isSubtype(tp.t, ir.t_interval))
        assert(isSubtype(tq.t, ir.t_interval))
        env = ir.TcEnv(
            env.lvs,
            env.hb,
            env.hbeq + (tp.p, tq.p),
            env.locks)
    }
    
    def addHbEq(p: ir.Path, q: ir.Path): Unit = addHbEq(teePee(p), teePee(q))
    
    def addLocks(tp: ir.TeePee, tq: ir.TeePee): Unit = log.indented("addLocks(%s,%s)", p, q) {
        assert(isSubtype(tp.t, ir.t_interval))
        assert(isSubtype(tq.t, ir.t_lock))
        env = ir.TcEnv(
            env.lvs,
            env.hb,
            env.hbeq,
            env.locks + (tp.p, tq.p))
    }
    
    def addLocks(p: ir.Path, q: ir.Path): Unit = addLocks(teePee(p), teePee(q))
        
    def hb(p: ir.TeePee, q: ir.TeePee): Boolean = log.indentedRes("%s hb %s?", p, q) {
        env.hb.contains(p.p, q.p)
    }
    
    def hb(p: ir.Path, q: ir.Path): Boolean = hb(teePee(p), teePee(q))
    
    def hbeq(p: ir.TeePee, q: ir.TeePee): Boolean = log.indentedRes("%s hbeq %s?", p, q) {
        env.hbeq.contains(p.p, q.p)
    }
    
    def hbeq(p: ir.Path, q: ir.Path): Boolean = hbeq(teePee(p), teePee(q))
    
    def eq(p: ir.TeePee, q: ir.TeePee): Boolean = log.indentedRes("%s eq %s?", p, q) {
        p.p == q.p
    }
    
    def eq(p: ir.Path, q: ir.Path): Boolean = eq(teePee(p), teePee(q))
    
    def locks(p: ir.TeePee, q: ir.TeePee): Boolean = log.indentedRes("%s locks %s?", p, q) {
        env.locks.contains(p.p, q.p)
    }
    
    def locks(p: ir.Path, q: ir.Path): Boolean = locks(teePee(p), teePee(q))
    
    def addReq(req: ir.Req) = {
        def teePeeAdd(add: Function2[ir.TeePee, ir.TeePee, Unit], p: ir.Path, qs: List[ir.Path]) = {
            val tp = teePee(tp)
            val tqs = qs.map(teePee)
            tqs.foreach(add(tp, _))
        }
        req match {
            case ir.ReqHb(p, qs) => teePeeAdd(addHb, p, qs)
            case ir.ReqHbEq(p, qs) => teePeeAdd(addHbEq, p, qs)
            case ir.ReqEq(lv, p) => addEq(lv, teePee(p))
            case ir.ReqEqPath(lv, p) => /* users can't enter these directly, can't add to env */
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
    
    /// wt_sub <: wt_sup
    def isSubtype(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef): Boolean =
        log.indentedRes("%s <: %s?", wt_sub, wt_sup) {
            if(wt_sub.c == wt_sub.c)
                forallzip(wt_sub.wpaths, wt_sup.wpaths)(isSubpath) 
            else
                sup(cap(wt_sub)) match {
                    case None => false
                    case Some(wt) => isSubtype(wt, wt_sup)
                }            
        }
    
    /// wt_sub <: wt_sup
    def checkIsSubtype(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef) {
        if(!isSubtype(wt_sub, wt_sup))
            throw new ir.IrError("intervals.expected.subtype", wt_sub, wt_sup)
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
        foreachzip(tqs.map(_.t), msig.args.map(_.wt))(checkIsSubtype)
    }
    
    def checkLvDecl(vd: ir.LvDecl, owt: Option[ir.WcTypeRef], op: Option[ir.Path]) = {
        checkWfWt(vd.wt)
        owt.foreach(checkIsSubtype(_, vd.wt))
        addLv(vd.name, ir.TeePee(vd.wt, op.map(canon.path).getOrElse(vd.name.path)))
    }
    
    def checkCurrentLocks(tp_guard: ir.TeePee) {
        if(!locks(tp_cur, tp_guard))
            throw new ir.IrError("intervals.no.lock", canon.path(p_guard))
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
                    checkReifiedAndConstant(p)
                    val fd = substdRealFieldDecl(p, f)
                    val tp_guard = teePee(fd.guard)

                    val op = 
                        if(isSubtype(tp_guard.t, ir.t_interval)) {
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
                    t.paths.foreach(checkConstant) // maybe weaken to reliable?
                    val tqs = ir.TeePee(qs)

                    val cd = classDecl(t.c)
                    val ctor = cd.ctor

                    // Check Ghost Types:
                    checkLengths(cd.ghosts, t.paths, "intervals.wrong.number.ghost.arguments")
                    val substGhosts = PathSubst.pp(cd.ghosts.map(_.thisPath), t.paths)
                    foreachzip(cd.ghosts, t.paths) { case (gfd, p) =>
                        val wt_p = wt_path(p)
                        checkIsSubtype(wt_p, substGhosts.wtref(gfd.wt))
                    }

                    // Check Argument Types:
                    val subst = PathSubst.vp(
                        ir.lv_this :: ctor.args.map(_.name),
                        vd.name.path :: tqs
                    )
                    val msig = subst.methodSig(ctor.msig)
                    checkArgumentTypes(msig, qs)

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
                    checkIsSubtype(q.t, fd.wt)
                    
                    val tp_guard = teePee(fd.guard)
                    if(isSubtype(wt_guard, ir.t_interval)) {
                        if(!eq(ir.p_cur, fd.guard))
                            throw new ir.IrError("intervals.not.current", fd.guard)                        
                    } else
                        checkCurrentLocks(fd.guard)
                        
                case ir.StmtHb(p, q) =>
                    checkReifiedAndConstant(p)
                    checkReifiedAndConstant(q)
                    
                    // XXX Rejigger HB to be based on points:
                    //checkIsSubtype(wt_path(p), ir.t_point)
                    //checkIsSubtype(wt_path(q), ir.t_point)
                    
                    checkIsSubtype(wt_path(p), ir.t_interval)
                    checkIsSubtype(wt_path(q), ir.t_interval)
                    addHb(p, q)
                    
                case ir.StmtLocks(p, q) =>
                    checkReifiedAndConstant(p)
                    checkReifiedAndConstant(q)
                    checkIsSubtype(wt_path(p), ir.t_interval)
                    checkIsSubtype(wt_path(q), ir.t_lock)
                    addLocks(p, q)
            }
        }
    }
    
    def checkRealFieldDecl(rfd: ir.RealFieldDecl) = savingEnv {
        log.indented(rfd) {
            at(rfd, ()) {
                checkWfWt(rfd.wt)
                val wt_guard = wt_path(rfd.guard)
                if(!isSubtype(wt_guard, ir.t_interval))
                    checkIsSubtype(wt_guard, ir.t_lock)
            }
        }
    }
    
    def checkMethodDecl(md: ir.MethodDecl) = savingEnv {
        log.indented(md) {
            at(md, ()) {
                md.args.foreach { case arg => 
                    checkWfWt(arg.wt)
                    addLv(arg.name, arg.wt, arg.name.path) 
                }
                
                md.reqs.foreach(checkWfReq)
                md.reqs.foreach(addReq)
                
                md.stmts.foreach(checkStatement)
                
                md.p_ret.foreach { p_ret =>
                    checkReifiedAndConstant(p_ret)
                    val wt_ret = wt_path(p_ret)
                    checkIsSubtype(wt_ret, md.wt_ret)
                }
            }         
        }
    }
    
    def checkClassDecl(cd: ir.ClassDecl) = savingEnv {
        log.indented(cd) {
            at(cd, ()) {
                val ps_ghosts = cd.ghosts.map(_.thisPath)
                val t_this = ir.TypeRef(cd.name, ps_ghosts)
                addLv(ir.lv_this, t_this, ir.p_this)                
                cd.fields.foreach(checkRealFieldDecl)
                
                addHb(ir.p_readOnly, ir.p_cur) // HACK
                
                // XXX Need to extract visible effects of ctor
                // XXX and save them in environment for other
                // XXX methods.
                savingEnv {
                    addLv(ir.lv_mthd, ir.t_interval, ir.gfd_ctor.thisPath)
                    checkMethodDecl(cd.ctor)                    
                }
                
                savingEnv {
                    addLv(ir.lv_mthd, ir.t_interval, ir.p_mthd)
                    cd.ctor.reqs.foreach(addReq)
                    addHb(ir.gfd_ctor.thisPath, ir.p_mthd)
                    cd.methods.foreach(checkMethodDecl)                    
                }
            }
        }
    }
    
    def check = prog.cds_user.foreach(checkClassDecl)
}