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
            (ir.lv_cur, (ir.t_interval, ir.lv_cur.path))
        ),
        ir.Relation.emptyTrans,
        ir.Relation.emptyTransRefl,
        ir.Relation.empty
    )
    
    val nonreifiedVars = env.lvs.keySet
    
    def savingEnv[R](g: => R): R = {
        val oldEnv = env
        try { g } finally { env = oldEnv }
    }
    
    def addLv(lv: ir.VarName, wt: ir.WcTypeRef, p: ir.Path) = 
        log.indented("addLv(%s,%s,%s)", lv, wt, p) {
            val p1 = canon.path(p)
            env.lvs.get(lv) match {
                case None =>
                    env = ir.TcEnv(
                        env.lvs + Pair(lv, (wt, p1)), 
                        env.hb,
                        env.hbeq,
                        env.locks)
                case Some(_) =>
                    throw new ir.IrError("intervals.shadowed.variable", lv)
            }
        }
        
    def addEq(lv: ir.VarName, p: ir.Path) =
        log.indented("addEq(%s,%s)", lv, p) {
            val p1 = canon.path(p)
            env.lvs.get(lv) match {
                case None =>
                    throw new ir.IrError("intervals.no.such.variable", lv)
                case Some((wt, _)) =>
                    env = ir.TcEnv(
                        env.lvs + Pair(lv, (wt, p1)),
                        env.hb,
                        env.hbeq,
                        env.locks)
            }
        }
        
    def addHb(p: ir.Path, q: ir.Path) = log.indented("addHb(%s,%s)", p, q) {
        val p1 = canon.path(p)
        val q1 = canon.path(p)
        env = ir.TcEnv(
            env.lvs,
            env.hb + (p1, q1),
            env.hbeq + (p1, q1),
            env.locks)
    }

    def addHbEq(p: ir.Path, q: ir.Path) = log.indented("addHbEq(%s,%s)", p, q) {
        val p1 = canon.path(p)
        val q1 = canon.path(p)
        env = ir.TcEnv(
            env.lvs,
            env.hb,
            env.hbeq + (p1, q1),
            env.locks)
    }
    
    def addLocks(p: ir.Path, q: ir.Path) = log.indented("addLocks(%s,%s)", p, q) {
        val p1 = canon.path(p)
        val q1 = canon.path(p)
        env = ir.TcEnv(
            env.lvs,
            env.hb,
            env.hbeq,
            env.locks + (p1, q1))
    }
    
    def addReq(req: ir.Req) = req match {
        case ir.ReqHb(p, qs) => qs.foreach(addHb(p, _))
        case ir.ReqHbEq(p, qs) => qs.foreach(addHbEq(p, _))
        case ir.ReqEq(lv, p) => addEq(lv, p)
        case ir.ReqEqPath(lv, p) => /* users can't enter these directly, can't add to env */
        case ir.ReqLocks(p, qs) => qs.foreach(addLocks(p, _))
    }
    
    /// type of a local variable ref.
    def wt_lv(lv: ir.VarName) =
        env.lvs.get(lv) match {
            case Some((wt, _)) => wt
            case None => throw ir.IrError("intervals.no.such.variable", lv)
        }
    
    /// equivalent path for a lv.
    def p_lv(lv: ir.VarName) =
        env.lvs.get(lv) match {
            case Some((_, p)) => p
            case None => throw ir.IrError("intervals.no.such.variable", lv)
        }    
        
    def hb(p: ir.Path, q: ir.Path) = log.indentedRes("%s hb %s?", p, q) {
        val p1 = canon.path(p)
        val q1 = canon.path(p)
        env.hb.contains(p1, q1)
    }
    
    def hbeq(p: ir.Path, q: ir.Path) = log.indentedRes("%s hbeq %s?", p, q) {
        val p1 = canon.path(p)
        val q1 = canon.path(p)
        env.hbeq.contains(p1, q1)
    }
    
    def eq(p: ir.Path, q: ir.Path) = log.indentedRes("%s eq %s?", p, q) {
        val p1 = canon.path(p)
        val q1 = canon.path(p)
        p1 == q1
    }
    
    def locks(p: ir.Path, q: ir.Path) = log.indentedRes("%s locks %s?", p, q) {
        val p1 = canon.path(p)
        val q1 = canon.path(p)
        env.locks.contains(p1, q1)
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
            addLv(lv_cap, ir.t_interval, lv_cap.path)
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
    // Path Canonicalization
    
    object canon extends BaseSubst {
        
        def path(p: ir.Path): ir.Path = log.indentedRes("canon.path(%s)", p) {
            p match {
                case ir.Path(lv, List()) => 
                    p_lv(lv) // Find canonical path for lv
                case ir.Path(lv, f :: rev_fs) =>
                    val p1 = path(ir.Path(lv, rev_fs))
                    val wt = wt_path(p1) // could be made more efficient...but so what.
                    val cd = classDecl(wt.c)
                    cd.ghosts.zip(wt.wpaths).find(_._1.name == f) match {
                        case Some((_, p: ir.Path)) => p // subst value for ghost path if known
                        case _ => p1 + f // but not if it's a wildcard or not a ghost
                    }
            }
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
    
    def checkWfPath(p: ir.Path) = {
        wt_path(p) // Computing the type of a path also checks that it's WF.
    }
    
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
    
    def checkArgumentTypes(msig: ir.MethodSig, qs: List[ir.Path]) {
        checkLengths(msig.args, qs, "intervals.wrong.number.method.arguments")
        val wts_q = qs.map(wt_path)
        foreachzip(wts_q, msig.args.map(_.wt))(checkIsSubtype)
    }
    
    def checkLvDecl(vd: ir.LvDecl, owt: Option[ir.WcTypeRef], op: Option[ir.Path]) = {
        checkWfWt(vd.wt)
        owt.foreach(checkIsSubtype(_, vd.wt))
        addLv(vd.name, vd.wt, op.getOrElse(vd.name.path))
    }
    
    def checkCurrentLocks(p_guard: ir.Path) {
        if(!locks(ir.p_cur, p_guard))
            throw new ir.IrError("intervals.no.lock", canon.path(p_guard))
    }
                    
    def checkStatement(stmt: ir.Stmt) = log.indented(stmt) {
        at(stmt, ()) {
            stmt match {  
                case ir.StmtCall(vd, p, m, qs) =>
                    checkReifiedAndConstant(p)
                    qs.foreach(checkReifiedAndConstant)
                    val msig = substdMethodSig(p, m, qs)
                    checkArgumentTypes(msig, qs)
                    checkLvDecl(vd, Some(msig.wt_ret), None)
                    msig.reqs.foreach(checkReqFulfilled)
        
                case ir.StmtGetField(vd, p, f) =>
                    checkReifiedAndConstant(p)
                    val fd = substdRealFieldDecl(p, f)
            
                    val wt_guard = wt_path(fd.guard)

                    val op = 
                        if(isSubtype(wt_guard, ir.t_interval)) {
                            if(hb(fd.guard, ir.p_mthd))
                                Some(p + f) // safe, constant
                            else if(eq(fd.guard, ir.p_cur))
                                None // safe, non-constant
                            else if(eq(fd.guard, ir.p_mthd))
                                None // safe, non-constant
                            else 
                                throw new ir.IrError("intervals.no.hb", wt_guard)
                        }
                        else {
                            checkCurrentLocks(fd.guard)
                            None // safe, non-constant
                        }
            
                    checkLvDecl(vd, Some(fd.wt), op)
                    
                case ir.StmtNew(vd, t, qs) =>
                    checkWfWt(t)
                    qs.foreach(checkReifiedAndConstant)
                    t.paths.foreach(checkConstant) // maybe weaken to reliable?

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
                        vd.name.path :: qs
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
                    checkReifiedAndConstant(p)
                    checkReifiedAndConstant(q)                    
                    val fd = substdRealFieldDecl(p, f)
                    checkIsSubtype(wt_path(q), fd.wt)
                    
                    val wt_guard = wt_path(fd.guard)
                    
                    if(isSubtype(wt_guard, ir.t_interval))
                        if(!eq(ir.p_cur, fd.guard))
                            throw new ir.IrError("intervals.not.current", fd.guard)
                    else
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
                    addEq(ir.lv_mthd, ir.gfd_ctor.thisPath)
                    checkMethodDecl(cd.ctor)                    
                }
                
                savingEnv {
                    cd.ctor.reqs.foreach(addReq)
                    addHb(ir.gfd_ctor.thisPath, ir.p_mthd)
                    cd.methods.foreach(checkMethodDecl)                    
                }
            }
        }
    }
    
    def check = prog.cds_user.foreach(checkClassDecl)
}