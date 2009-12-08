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
            (ir.lv_cur, ir.t_interval)
        ),
        ir.Relation.emptyTrans,
        ir.Relation.emptyTransRefl,
        ir.Relation.emptyTransRefl,
        ir.Relation.empty
    )
    
    val nonreifiedVars = env.lvs.keySet
    
    def savingEnv[R](g: => R): R = {
        val oldEnv = env
        try { g } finally { env = oldEnv }
    }
    
    def equivPaths(p: ir.Path): Set[ir.Path] = p match {
        case ir.Path(lv, List()) => Set(p) ++ env.equiv(p)
        case ir.Path(lv, f :: rev_fs) =>
            equivPaths(ir.Path(lv, rev_fs)).map(_ + f) ++ env.equiv(p)
    }
    
    def addLv(lv: ir.VarName, wt: ir.WcTypeRef) {
        env.lvs.get(lv) match {
            case None =>
                env = ir.TcEnv(
                    env.lvs + Pair(lv, wt), 
                    env.hb,
                    env.hbeq,
                    env.equiv,
                    env.locks)
            case Some(_) =>
                throw new ir.IrError("intervals.shadowed.variable", lv)
        }
    }

    def addHb(p: ir.Path, q: ir.Path) = log.indented("addHb(%s,%s)", p, q) {
        env = ir.TcEnv(
            env.lvs,
            env.hb + (p, q),
            env.hbeq + (p, q),
            env.equiv,
            env.locks)
    }

    def addHbEq(p: ir.Path, q: ir.Path) = log.indented("addHbEq(%s,%s)", p, q) {
        env = ir.TcEnv(
            env.lvs,
            env.hb,
            env.hbeq + (p, q),
            env.equiv,
            env.locks)
    }
    
    def addEq(p: ir.Path, q: ir.Path) = log.indented("addEq(%s,%s)", p, q) {
        env = ir.TcEnv(
            env.lvs,
            env.hb,
            env.hbeq + (p, q) + (q, p),
            env.equiv + (p, q) + (q, p),
            env.locks)
    }

    /// Adds equivalencies for 'p' that result from ghost arguments.
    /// For example, if p is obj.creator and obj has type Object<q>,
    /// then we add an equivalence between obj.creator and q.
    def addGhostEq(p: ir.Path): ir.WcTypeRef = log.indentedRes("addGhostEq(%s)", p) {
        p match {
            case ir.Path(lv, List()) => wt_lv(lv)
            case ir.Path(lv, f :: rev_fs) =>
                val p1 = ir.Path(lv, rev_fs)
                val wt_p1 = addGhostEq(p1)
                val cd = classDecl(wt_p1.c)
                cd.ghosts.zip(wt_p1.wpaths).find(_._1.name == f) match {
                    case Some((wt, wp_ghost)) =>
                        val p_ghost = capPath(wp_ghost)
                        addEq(p, p_ghost)
                        log("Ghost field: %s maps to path: %s cap: %s", p, wp_ghost, p_ghost)
                        wt
                    case _ => 
                        log("Not ghost field: %s", f)
                }
        }
    }
    
    def addLocks(p: ir.Path, q: ir.Path) = log.indented("addLocks(%s,%s)", p, q) {
        log("addLocks(%s,%s)", p, q)
        env = ir.TcEnv(
            env.lvs,
            env.hb,
            env.hbeq,
            env.equiv,
            env.locks + (p, q))
    }
    
    def addReq(req: ir.Req) = req match {
        case ir.ReqHb(p, qs) => qs.foreach(addHb(p, _))
        case ir.ReqHbEq(p, qs) => qs.foreach(addHbEq(p, _))
        case ir.ReqEq(p, q) => addEq(p, q)
        case ir.ReqLocks(p, qs) => qs.foreach(addLocks(p, _))
    }
    
    def hb(p: ir.Path, q: ir.Path) = log.indentedRes("%s hb %s?", p, q) {
        existscross(equivPaths(p), equivPaths(q))(env.hb.contains)
    }
    
    def hbeq(p: ir.Path, q: ir.Path) = log.indentedRes("%s hbeq %s?", p, q) {
        existscross(equivPaths(p), equivPaths(q))(env.hbeq.contains)
    }
    
    def eq(p: ir.Path, q: ir.Path) = log.indentedRes("%s eq %s?", p, q) {
        existscross(equivPaths(p), equivPaths(q)) { _ == _ }
    }
    
    def locks(p: ir.Path, q: ir.Path) = log.indentedRes("%s locks %s?", p, q) {
        existscross(equivPaths(p), equivPaths(q))(env.locks.contains)
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
    
    /// capture an object ref. (affects the environment)
    def capPath(wo: ir.WcPath) = {
        def cap(add: Function2[ir.Path, ir.Path, Unit], ps: List[ir.Path], qs: List[ir.Path]) = {
            val lv_cap = ir.VarName(fresh("Cap"))
            addLv(lv_cap, ir.t_interval)
            val p_cap = lv_cap.path
            ps.foreach(add(_, p_cap))
            qs.foreach(add(p_cap, _))
            p_cap
        }
        wo match {
            case o: ir.Path => o
            case ir.WcHb(ps, qs) => cap(addHb, ps, qs)
            case ir.WcHbEq(ps, qs) => cap(addHbEq, ps, qs)
        }
    }
    
    /// capture wt into a TypeRef
    def cap(wt: ir.WcTypeRef): ir.TypeRef = log.indentedRes("cap(%s)", wt) {        
        ir.TypeRef(wt.c, wt.wpaths.map(capPath))
    }
        
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
    def fieldDecl(ot: ir.TypeRef, f: ir.FieldName) = log.indentedRes("fieldDecl(%s,%s)", ot, f) {
        def search(t: ir.TypeRef): ir.FieldDecl = log.indentedRes("search(%s)", t) {
            val cd = classDecl(t.c)
            cd.allFields.find(_.name == f) match {
                case Some(fd) => fd
                case None => sup(t) match {
                    case Some(t_1) => pathSubst(t_1).fieldDecl(search(t_1))
                    case None => throw ir.IrError("intervals.no.such.field", ot, f)
                }
            }
        }
        if(f == ir.f_ctor)
            ir.gfd_ctor        
        else
            search(ot)        
    }
    
    /// lookup method decl for ot.m(), no substitutions performed!
    def methodSig(ot: ir.TypeRef, m: ir.MethodName): ir.MethodSig = {
        def search(t: ir.TypeRef): ir.MethodSig = {
            val cd = classDecl(t.c)
            cd.methods.find(_.name == m) match {
                case Some(md) => md.msig
                case None => sup(t) match {
                    case Some(t_1) => pathSubst(t_1).methodSig(search(t_1))
                    case None => throw ir.IrError("intervals.no.such.method", ot, m)
                }
            }
        }
        search(ot)
    }
    
    def substdMethodSig(p: ir.Path, m: ir.MethodName, qs: List[ir.Path]) = {
        val t_p = cap(wt_path(p))
        val msig = methodSig(t_p, m)
        PathSubst.vp(
            ir.lv_this :: msig.args.map(_.name),
            p :: qs
        ).methodSig(msig)        
    }
        
    /// type of a local variable ref.
    def wt_lv(lv: ir.VarName) =
        env.lvs.get(lv) match {
            case Some(wt) => wt
            case None => throw ir.IrError("intervals.no.such.variable", lv)
        }
        
    /// type of an object reference.
    def wt_path(p: ir.Path): ir.WcTypeRef = 
        log.indentedRes("wt_path(%s)", p) {
            p match {
                case ir.Path(lv, List()) => 
                    wt_lv(lv)
                case ir.Path(lv, f :: rev_fs) => 
                    val t = cap(wt_path(ir.Path(lv, rev_fs)))
                    fieldDecl(t, f).wt
            }    
        }
    
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
    
    // ______________________________________________________________________
    // Well-formedness
    //
    // Very basic sanity checks.
    
    def checkWfPath(p: ir.Path) = {
        wt_path(p) // Computing the type of a path also checks that it's WF.
    }
    
    def checkWfReq(req: ir.Req) = req match {
        case ir.ReqHb(p, qs) => checkWfPath(p); qs.foreach(checkWfPath)
        case ir.ReqHbEq(p, qs) => checkWfPath(p); qs.foreach(checkWfPath)
        case ir.ReqEq(p, q) => checkWfPath(p); checkWfPath(q)
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
    
    /// field decl of p.f, with ref to this subst'd
    def substdFieldDecl(p: ir.Path, f: ir.FieldName) = {
        val fd0 = fieldDecl(cap(wt_path(p)), f)
        val fd1 = PathSubst.vp(ir.lv_this, p).fieldDecl(fd0)
        canon.fieldDecl(fd1)
    }
    
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
        
    def substdRealFieldDecl(p: ir.Path, f: ir.FieldName) = {
        val fd = substdFieldDecl(p, f)
        if(fd.isGhost)
            throw new ir.IrError("intervals.not.with.ghost", p, f)
        fd.asInstanceOf[ir.RealFieldDecl]        
    }    
    
    def isReqFulfilled(req: ir.Req) = req match {
        case ir.ReqHb(p, qs) => qs.forall(hb(p, _))
        case ir.ReqHbEq(p, qs) => qs.forall(hbeq(p, _))
        case ir.ReqEq(p, q) => eq(p, q)
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
    
    def checkArgumentTypes(msig: ir.MethodSig, qs: List[ir.Path]) {
        checkLengths(msig.args, qs, "intervals.wrong.number.method.arguments")
        val wts_q = qs.map(wt_path)
        foreachzip(wts_q, msig.args.map(_.wt))(checkIsSubtype)
    }
    
    def checkReifiedAndConstant(p: ir.Path) {
        checkPath(p, true, true, false)
    }
    
    def expr(ex: ir.Expr): (ir.WcTypeRef, List[ir.Req]) = ex match {
        
        case ir.ExprCall(p, m, qs) =>
            checkReifiedAndConstant(p)
            qs.foreach(checkReifiedAndConstant)
            val msig = substdMethodSig(p, m, qs)
            checkArgumentTypes(msig, qs)
            (msig.wt_ret, msig.reqs)

        case ir.ExprField(p, f) =>
            checkReifiedAndConstant(p)
            val fd = substdRealFieldDecl(p, f)
            
            val wt_guard = wt_path(fd.guard)
            addGhostEq(fd.guard)
            val req = 
                if(isSubtype(wt_guard, ir.t_interval))
                    ir.ReqHbEq(fd.guard, List(ir.p_cur))
                else
                    ir.ReqLocks(ir.p_cur, List(fd.guard))
            
            (fd.wt, List(req))
            
        case ir.ExprNew(t, qs) =>
            checkWfWt(t)
            qs.foreach(checkReifiedAndConstant)
            
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
                ir.p_new :: qs
            )
            val msig = subst.methodSig(ctor.msig)
            savingEnv {
                addLv(ir.lv_new, t)
                checkArgumentTypes(msig, qs)
            }
            
            (t, msig.reqs)
            
        case ir.ExprNull =>
            (ir.t_void, List())
            
    }
                    
    def statement(stmt: ir.Stmt): List[ir.Req] = 
        at(stmt, List[ir.Req]()) {
            stmt match {        
                case ir.StmtVarDecl(ir.LvDecl(x, wt_x), ex) =>
                    checkWfWt(wt_x)
                    val (wt_e, reqs) = expr(ex)
                    val subst = PathSubst.vp(ir.lv_new, x.path)
                    if(ex != ir.ExprNull)
                        checkIsSubtype(wt_e, wt_x)
                    addLv(x, wt_x)
                    reqs.map(subst.req)
        
                case ir.StmtAssignField(p, f, q) =>
                    // XXX Here is where we might incur obligations
                    // XXX to re-assign other fields that use f!
                    checkReifiedAndConstant(p)
                    checkReifiedAndConstant(q)                    
                    val fd = substdRealFieldDecl(p, f)
                    checkIsSubtype(wt_path(q), fd.wt)
                    
                    val wt_guard = wt_path(fd.guard)
                    addGhostEq(fd.guard)
                    if(isSubtype(wt_guard, ir.t_interval))
                        List(ir.ReqEq(ir.p_cur, fd.guard))
                    else
                        List(ir.ReqLocks(ir.p_cur, List(fd.guard)))
        
                case ir.StmtHb(p, q) =>
                    checkReifiedAndConstant(p)
                    checkReifiedAndConstant(q)
                    
                    // XXX Rejigger HB to be based on points:
                    //checkIsSubtype(wt_path(p), ir.t_point)
                    //checkIsSubtype(wt_path(q), ir.t_point)
                    
                    checkIsSubtype(wt_path(p), ir.t_interval)
                    checkIsSubtype(wt_path(q), ir.t_interval)
                    addHb(p, q)
                    
                    List()
                    
                case ir.StmtLocks(p, q) =>
                    checkReifiedAndConstant(p)
                    checkReifiedAndConstant(q)
                    checkIsSubtype(wt_path(p), ir.t_interval)
                    checkIsSubtype(wt_path(q), ir.t_lock)
                    addLocks(p, q)
                    List()
            }
        }
        
    def checkStatement(stmt: ir.Stmt) {
        log.indented(stmt) {
            val reqs = statement(stmt)
            log("Requirements: %s", reqs)
            reqs.foreach(checkReqFulfilled)
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
                    addLv(arg.name, arg.wt) 
                }
                
                md.reqs.foreach(checkWfReq)
                md.reqs.foreach(addReq)
                
                md.stmts.foreach(checkStatement)
                
                val (wt_ret, reqs_ret) = expr(md.ex_ret)
                if(md.ex_ret != ir.ExprNull)
                    checkIsSubtype(wt_ret, md.wt_ret)
                reqs_ret.foreach(checkReqFulfilled)                
            }         
        }
    }
    
    def checkClassDecl(cd: ir.ClassDecl) = savingEnv {
        log.indented(cd) {
            at(cd, ()) {
                val ps_ghosts = cd.ghosts.map(_.thisPath)
                val t_this = ir.TypeRef(cd.name, ps_ghosts)
                addLv(ir.lv_this, t_this)                
                cd.fields.foreach(checkRealFieldDecl)
                
                addHb(ir.p_readOnly, ir.p_cur) // HACK
                
                // XXX Need to extract visible effects of ctor
                // XXX and save them in environment for other
                // XXX methods.
                savingEnv {
                    addEq(ir.p_cur, ir.gfd_ctor.thisPath)
                    checkMethodDecl(cd.ctor)                    
                }
                
                savingEnv {
                    cd.ctor.reqs.foreach(addReq)
                    addHb(ir.gfd_ctor.thisPath, ir.p_cur)
                    cd.methods.foreach(checkMethodDecl)                    
                }
            }
        }
    }
    
    def check = prog.cds_user.foreach(checkClassDecl)
}