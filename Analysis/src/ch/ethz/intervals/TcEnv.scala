package ch.ethz.intervals

sealed case class TcEnv(
    prog: Prog,
    op_cur: Option[ir.Path],                // current interval
    wt_ret: ir.WcTypeRef,                   // return type of current method
    perm: Map[ir.VarName, ir.CanonPath], // permanent equivalences, hold for duration of method
    flow: FlowEnv
) {
    import prog.logStack.log
    import prog.classDecl
    import prog.isSubclass
    import prog.unboundGhostFieldsOnClassAndSuperclasses
    
    // ___ Adjusting Individual Values ______________________________________
    
    def withCurrent(op_cur: Option[ir.Path]) = TcEnv(prog, op_cur, wt_ret, perm, flow)
    def withRet(wt_ret: ir.WcTypeRef) = TcEnv(prog, op_cur, wt_ret, perm, flow)
    def withPerm(perm: Map[ir.VarName, ir.CanonPath]) = TcEnv(prog, op_cur, wt_ret, perm, flow)
    def withFlow(flow: FlowEnv) = TcEnv(prog, op_cur, wt_ret, perm, flow)
    
    // ___ Merging Flows ____________________________________________________
    
    def addFlow(flow1: FlowEnv) = withFlow(flow + flow1)        
    def intersectFlow(flow1: FlowEnv) = withFlow(flow ** flow1)
        
    // ___ Local Variables __________________________________________________
    
    /// Add a local variable whose value is the canon path 'cp'
    def addPerm(x: ir.VarName, cp: ir.CanonPath) = {
        assert(!mutable(cp))
        perm.get(x) match {
            case Some(_) => throw new CheckFailure("intervals.shadowed", x)
            case None => withPerm(perm + Pair(x, cp))
        }
    }
    
    /// Define a local variable according to the given decl
    def addArg(arg: ir.LvDecl) = {
        addPerm(arg.name, ir.CpLv(arg.name, arg.wt, false))
    }
    
    /// Define local variables according to the given decls
    def addArgs(args: List[ir.LvDecl]) = {
        args.foldLeft(this) { case (e, a) => e.addArg(a) }        
    }
    
    /// Define a reified local variable 'x' with type 'wt'
    def addReifiedLocal(x: ir.VarName, wt: ir.WcTypeRef) = {
        addPerm(x, ir.CpLv(x, wt, false))
    }
    
    /// Define a ghost local variable 'x' with type 'wt'
    def addGhostLocal(x: ir.VarName, wt: ir.WcTypeRef) = {
        addPerm(x, ir.CpLv(x, wt, true))        
    }
    
    /// Temporarily redirect from 'p' to 'q'
    def addTemp(p: ir.Path, q: ir.Path) = {
        log("addTemp(%s,%s)", p, q)
        withFlow(flow.withTemp(flow.temp + Pair(p, q)))
    }
    
    /// Clear all temporary redirects
    def clearTemp = {
        log("clearTemp")
        withFlow(flow.withTemp(Map()))
    }
    
    def addInvalidated(p: ir.Path) = {
        log("addInvalidated(%s)", p)
        withFlow(flow.withInvalidated(flow.ps_invalidated + p))        
    }
        
    def removeInvalidated(p: ir.Path) = {
        log("removeInvalidated(%s)", p)
        withFlow(flow.withInvalidated(flow.ps_invalidated - p))        
    }
    
    def addHbPnt(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addHbPnt(%s,%s)", cp, cq)
        assert(!mutable(cp) && !mutable(cq))
        assert(isSubclass(cp.wt, ir.c_point))
        assert(isSubclass(cq.wt, ir.c_point))        
        withFlow(flow.withHbRel(flow.hbRel + (cp.p, cq.p)))        
    }

    def addHbInter(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addHbInter(%s,%s)", cp, cq)
        assert(!mutable(cp) && !mutable(cq))
        assert(isSubclass(cp.wt, ir.c_interval))
        assert(isSubclass(cq.wt, ir.c_interval))
        withFlow(flow.withHbRel(flow.hbRel + (cp.p.end, cq.p.start)))
    }
    
    def addDeclaredReadableBy(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addDeclaredReadableBy(%s, %s)", cp, cq)
        assert(!mutable(cp) && !mutable(cq))
        assert(isSubclass(cp.wt, ir.c_guard))
        assert(isSubclass(cq.wt, ir.c_interval))
        withFlow(flow.withReadableRel(flow.readableRel + (cp.p, cq.p)))
    }
    
    def addDeclaredWritableBy(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addDeclaredWritableBy(%s, %s)", cp, cq)
        assert(!mutable(cp) && !mutable(cq))
        assert(isSubclass(cp.wt, ir.c_guard))
        assert(isSubclass(cq.wt, ir.c_interval))
        withFlow(flow.withWritableRel(flow.writableRel + (cp.p, cq.p)))
    }
    
    def addSubintervalOf(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented("addSubintervalOf(%s, %s)", cp, cq) {
            // This assertion is not valid during checkReifiedFieldDecl():
            //assert(!mutable(cp) && !mutable(cq))
            assert(isSubclass(cp.wt, ir.c_interval))
            assert(isSubclass(cq.wt, ir.c_interval))
            withFlow(flow
                .withHbRel(flow.hbRel + (cq.p.start, cp.p.start) + (cp.p.end, cq.p.end))
                .withSubintervalRel(flow.subintervalRel + (cp.p, cq.p)))
        }        
    }

    def addLocks(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented("addLocks(%s, %s)", cp, cq) {
            // This assertion is not valid during checkReifiedFieldDecl:
            //assert(!mutable(cp) && !mutable(cq))
            assert(isSubclass(cp.wt, ir.c_interval))
            assert(isSubclass(cq.wt, ir.c_lock))
            withFlow(flow.withLocksRel(flow.locksRel + (cp.p, cq.p)))
        }        
    }

    /// Converts cp0 and cq0 to points that can happen before one another.
    /// - If cp0 is an interval, use cp0.end.
    /// - If cq0 is an interval, use cq0.start.
    private def userHbPair(cp0: ir.CanonPath, cq0: ir.CanonPath): Option[(ir.Path, ir.Path)] = {
        log.indented("userHbPair(%s,%s)", cp0, cq0) {
            def makePoint(cp: ir.CanonPath, f: ir.FieldName) = {
                if(isSubclass(cp.wt, ir.c_point)) 
                    Some(cp.p)
                else if(isSubclass(cp.wt, ir.c_interval)) 
                    Some(cp.p + f)
                else 
                    None                
            }
                
            (makePoint(cp0, ir.f_end), makePoint(cq0, ir.f_start)) match {
                case (Some(p1), Some(q1)) => Some((p1, q1))
                case _ => None
            }
        }        
    }
        
    def addUserHb(cp0: ir.CanonPath, cq0: ir.CanonPath) = {
        log.indented("addUserHb(%s,%s)", cp0, cq0) {
            userHbPair(cp0, cq0) match {
                case Some((p1, q1)) => 
                    log("adding (%s,%s)", p1, q1)
                    withFlow(flow.withHbRel(flow.hbRel + (p1, q1)))
                case None => this
            }
        }        
    }
        
    def addNonNull(cp: ir.CanonPath) = {
        log.indented("addNonNull(%s)", cp) {
            withFlow(flow.withNonnull(flow.nonnull + cp.p))            
        }
    }
        
    // ___ Constructing Canonical Paths _____________________________________
    
    def canon(p1: ir.Path): ir.CanonPath = 
        log.indented(false, "canon(%s)", p1) {
            canonicalize(Set(), p1)
        }
        
    def extendCanonWithReifiedField(cp: ir.CanonPath, rfd: ir.ReifiedFieldDecl) = {
        ir.CpField(cp, rfd)
    }
    
    private def canonicalize(vis: Set[ir.Path], p1: ir.Path): ir.CanonPath = log.indented("canonicalize(%s)", p1) {
        assert(!vis(p1))
        if(flow.temp.contains(p1)) {
            val p1_redirect = flow.temp(p1)
            log("temp redirect")
            canonicalize(vis + p1, p1_redirect)                
        } else p1.rev_fs match {
            case List() =>
                perm.get(p1.lv) match {
                    case Some(cp) => cp
                    case None => throw new CheckFailure("intervals.no.such.variable", p1.lv)
                }
                
            case f :: rev_fs =>
                val p0 = ir.Path(p1.lv, rev_fs)      // p1 = p0.f
                val cp0 = canonicalize(Set(), p0)
                
                if(f == ir.f_ctor) {                    
                    ir.CpCtor(cp0) // Handle p0.ctor specially
                } else if (f == ir.f_super) {
                    ir.CpSuper(cp0) // Handle p0.super specially
                } else substdFieldDecl(cp0, f) match {
                    case gfd: ir.GhostFieldDecl =>
                        canonicalGhostField(vis, cp0, gfd)
                    case rfd: ir.ReifiedFieldDecl =>
                        log("ReifiedFieldDecl: %s", rfd)
                        extendCanonWithReifiedField(cp0, rfd)
                }
        }
    }

    // Helper for ir.CanonPath(): Computes the ir.CanonPath for a path cp0.f where f is 
    // a ghost field of (post-substitution) type wt_f.
    private def canonicalGhostField(
        vis_in: Set[ir.Path],
        cp0: ir.CanonPath, 
        gfd: ir.GhostFieldDecl
    ) = {
        log.indented("canonicalGhostField(%s, %s, %s)", vis_in, cp0, gfd) {
            val cp1 = ir.CpField(cp0, gfd)
            val vis1 = vis_in + cp1.p
            
            // Does cp0's type specify a value for ghost field f?
            cp0.wt.owghost(gfd.name) match {
                // cp0 had a type with a precise path like Foo<f: q>
                // where q has not yet been visited.  Redirect.
                case Some(q: ir.Path) if !vis1(q) =>
                    log.indented("Redirect: %s", q) {
                        canonicalize(vis1, q)
                    }
                    
                case _ => cp1
            }            
        }        
    }    
    
    // ___ Queries of the environment _______________________________________
    //
    // Queries are generally defined over canonical paths.
    
    /// Current interval (must be defined)
    def p_cur = op_cur.get
    def cp_cur = canon(p_cur)
    
    /// Field decl for p.f
    def substdFieldDecl(cp: ir.CanonPath, f: ir.FieldName) = {
        val rfd = prog.fieldDecl(cp.wt.c, f)
        cGhostSubst(cp).fieldDecl(rfd)
    }

    /// Method sig for constructor p(qs)
    def substdCtorSig(cp: ir.CanonPath, m: ir.MethodName, cqs: List[ir.CanonPath]) = {
        classDecl(cp.wt.c).ctors.find(_.name == m) match {
            case Some(md) =>
                val msig = md.msig(cap(cp))
                val subst = cGhostSubst(cp) + PathSubst.vp(msig.args.map(_.name), cqs.map(_.p))
                subst.methodSig(msig)
            case None =>
                throw new CheckFailure("intervals.no.such.ctor", cp.wt.c, m)
        }
    }

    /// Method sig for p.m(qs)
    def substdMethodSig(cp: ir.CanonPath, m: ir.MethodName, cqs: List[ir.CanonPath]) = {
        prog.methodSig(cp.wt.c, m) match {
            case Some(msig) =>
                val subst = cGhostSubst(cp) + PathSubst.vp(msig.args.map(_.name), cqs.map(_.p))
                subst.methodSig(msig)        
            case None =>
                throw new CheckFailure("intervals.no.such.method", cp.wt.c, m)
        }
    }

    /// True if the path p is known to be nonnull
    def isNonnull(cp: ir.CanonPath) = log.indented(false, "isNonnull(%s)?", cp) {
        log.env(false, "Environment", this)
        flow.nonnull(cp.p)
    }

    /// Does cp0 hb cq0?  
    def userHb(cp0: ir.CanonPath, cq0: ir.CanonPath) = {
        log.indented("userHb(%s,%s)", cp0, cq0) {
            userHbPair(cp0, cq0) match {
                case Some((p1, q1)) => flow.hb((p1, q1))
                case None => false
            }
        }        
    }
    
    /// Does the point cp hb the point cq?
    def hbPnt(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = {
        log.indented("%s hb[point] %s?", cp, cq) {
            flow.hb((cp.p, cq.p))
        }        
    }
        
    /// Does the interval cp hb the interval cq?
    def hbInter(cp: ir.CanonPath, cq: ir.CanonPath) = {
        // Note: cp, cq won't be in flow.hb if not intervals:
        val pEnd = cp.p + ir.f_end
        val qStart = cq.p + ir.f_start
        flow.hb((pEnd, qStart))
    }
    
    private def superintervals(cp: ir.CanonPath) = {
        flow.subintervalRel.values(cp.p)
    }        
    
    private def superintervalsOrSelf(cp: ir.CanonPath) = {
        flow.subintervalRel.values(cp.p) + cp.p
    }        
    
    def isSubintervalOf(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented(false, "isSubintervalOf(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)
            flow.subinterval((cp.p, cq.p))
        }
    }
    
    def locks(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented(false, "locks(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)
            superintervalsOrSelf(cp).exists { p => flow.locks((p, cq.p)) }
        }
    }
    
    /// Is data guarded by 'p' immutable in the interval 'q'?
    def isImmutableIn(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = {
        log.indented(false, "isImmutableIn(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)
            hbInter(cp, cq)
        }    
    }
    
    /// Is data guarded by 'p' writable by the interval 'q'?
    def guardsDataWritableBy(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = {
        log.indented(false, "guardsDataWritableBy(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)
            (
                cp match {
                    case ir.CpField(cp0, ir.GhostFieldDecl(_, f)) =>
                        cp0.wt.owghost(f) match {
                            // Is cp a ghost declared writable by q?
                            case Some(ir.WcWritableBy(qs)) => among(cq, qs)
                            case _ => false
                        }
                    case _ => false
                }
            ) || {
                flow.writable((cp.p, cq.p)) ||
                equiv(cp, cq) ||
                locks(cp, cq) ||
                isSubintervalOf(cq, cp)
            }
        }
    }    
    
    /// Is data guarded by 'p' readable by the interval 'q'?
    def guardsDataReadableBy(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = {
        log.indented(false, "guardsDataReadableBy(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)
            (
                cp match {
                    case ir.CpField(cp0, ir.GhostFieldDecl(_, f)) =>
                        cp0.wt.owghost(f) match {
                            // Is cp a ghost declared readable (or writable) by q?
                            case Some(ir.WcReadableBy(qs)) => among(cq, qs)
                            case Some(ir.WcWritableBy(qs)) => among(cq, qs)
                            case _ => false
                        }
                    case ir.CpCtor(cp0) => !cp0.wt.as.ctor // ctor type == ctor may not be completed
                    case ir.CpSuper(cp0) => true
                    case _ => false
                } 
            ) || {
                superintervalsOrSelf(cq).exists(q => flow.readable((cp.p, q))) ||
                flow.readable((cp.p, cq.p)) ||
                hbInter(cp, cq) ||
                guardsDataWritableBy(cp, cq)            
            }
        }
    }
    
    /// Did the interval cp happen before the current interval?
    def hbNow(cp: ir.CanonPath): Boolean = {
        log.indented(false, "hbNow(%s)", cp) {
            hbInter(cp, canon(op_cur.get))
        }
    }
    
    /// Could the value of 'cp1' change as the method executes?
    def mutable(cp1: ir.CanonPath): Boolean = cp1 match {
        // Local variables never change value once assigned:
        case ir.CpLv(_, _, _) => false
        
        // Fields guarded by past intervals cannot change but others can:
        case ir.CpField(cp0, ir.ReifiedFieldDecl(_, _, _, p_guard)) =>
            mutable(cp0) || { 
                val cp_guard = canon(p_guard)
                mutable(cp_guard) || !hbNow(cp_guard)
            }
            
        // Ghosts never change value but the path they extend might:
        case ir.CpField(cp0, _: ir.GhostFieldDecl) => mutable(cp0)
        case ir.CpCtor(cp0) => mutable(cp0)
        case ir.CpSuper(cp0) => mutable(cp0)
    }
    
    /// Does 'cp1' represent a ghost value (i.e., an erased or non-reified value)?
    def ghost(cp1: ir.CanonPath): Boolean = cp1 match {
        case ir.CpLv(_, _, isGhost) => isGhost
        case ir.CpField(cp0, _: ir.GhostFieldDecl) => true
        case ir.CpField(cp0, _: ir.ReifiedFieldDecl) => ghost(cp0)
        case ir.CpCtor(cp0) => true
        case ir.CpSuper(cp0) => true
    }

    /// A field f_l is linked to cp_o.f if its type is invalidated
    /// when p.f changes.  This occurs when p.f appears in f_l's type.
    /// The rules we enforce when checking field decls. guarantee that
    /// all linked fields either (a) occur in the same class defn as f
    /// or (b) are guarded by some interval which has not yet happened.
    def linkedPaths(cp_o: ir.CanonPath, f: ir.FieldName) = {
        /// Returns true if 'wt' is linked to 'p': that is,
        /// if it depends on 'p' in some way.  'p' must be
        /// a canonical path.
        def isLinkedWt(p: ir.Path, wt: ir.WcTypeRef) = {
            wt.wghosts.exists(_.wp.dependentOn(p))        
        }
    
        val cd = classDecl(cp_o.wt.c)
        val p_f = f.thisPath
        
        // only reified fields can be linked to another field
        val lrfd = cd.fields.foldLeft(List[ir.ReifiedFieldDecl]()) {
            case (l, rfd: ir.ReifiedFieldDecl) => rfd :: l
            case (l, _) => l
        }
        
        // find fields where cp_o.f appears in the type
        val fds_maybe_linked = lrfd.filter { rfd => isLinkedWt(p_f, rfd.wt) }
        
        // screen out those which cannot have been written yet (and whose 
        // value is therefore null, which is valid for any type)
        lazy val subst = cGhostSubst(cp_o)
        val ocp_cur = op_cur.map(canon)
        val fds_linked = fds_maybe_linked.filter { rfd =>
            !ocp_cur.exists(cp_cur =>
                hbInter(cp_cur, canon(subst.path(rfd.p_guard))))
        }
        
        // map to the canonical path for the field
        fds_linked.map { fd => subst.path(fd.thisPath) }
    }
    
    // ___ Other operations on canonical paths ______________________________
    
    /// Returns a list of ghosts for cp.wt.
    /// Any wildcards or missing ghosts in cp.wt are replaced with a path based on 'cp'.
    /// So if cp.wt was Foo<f: ?><g: q>, the result would be List(<f: p.f>, <g: q>) where p = cp.p.
    private def cGhosts(cp: ir.CanonPath): List[ir.Ghost] = {
        val gfds_unbound = unboundGhostFieldsOnClassAndSuperclasses(cp.wt.c).toList
        gfds_unbound.map { gfd =>
            cp.wt.owghost(gfd.name) match {
                case Some(p: ir.Path) => ir.Ghost(gfd.name, p) // Defined explicitly.
                case _ => ir.Ghost(gfd.name, cp.p + gfd.name)  // Wildcard or undefined.
            }
        }
    }

    /// Creates a subst from 'cp' that includes 'this→cp.p' and also
    /// substitutes all ghost fields 'this.F' with their correct values.
    private def cGhostSubst(cp: ir.CanonPath): PathSubst = {
        val cd = classDecl(cp.wt.c)
        val gs_tp = cGhosts(cp)
        PathSubst.pp(
            ir.p_this :: gs_tp.map(g => ir.p_this + g.f),
            cp.p      :: gs_tp.map(g => g.p)
        )     
    }

    /// Captures cp.wt, using ghostPaths(cp) for the type args.
    def cap(cp: ir.CanonPath): ir.TypeRef = {
        ir.TypeRef(cp.wt.c, cGhosts(cp), cp.wt.as)
    }

    private def equiv(cp: ir.CanonPath, cq: ir.CanonPath) = (cp.p == cq.p)
    
    private def among(cp: ir.CanonPath, qs: List[ir.Path]) = {
        qs.exists(q => equiv(cp, canon(q)))
    }

    // ___ Subtyping ________________________________________________________
    
    /// p <= wq
    def isSubpath(p: ir.Path, wq: ir.WcPath) = {
        log.indented("%s <= %s?", p, wq) {                
            val cp = canon(p)
            wq match {
                case q: ir.Path =>
                    equiv(cp, canon(q))
                case ir.WcReadableBy(lq) =>
                    lq.forall { q => guardsDataReadableBy(cp, canon(q)) }
                case ir.WcWritableBy(lq) =>
                    lq.forall { q => guardsDataWritableBy(cp, canon(q)) }
            }
        }        
    }
    
    /// ∃ g∈t.ghosts . (g.f == f, g.p <= wp)?
    def isSubghost(t_sub: ir.TypeRef, wt_ghost: ir.WcGhost) = {
        t_sub.oghost(wt_ghost.f) match {
            case Some(p) => isSubpath(p, wt_ghost.wp)
            case None => false
        }
    }
    
    /// t_sub <: wt_sup
    def isSubtype(t_sub: ir.TypeRef, wt_sup: ir.WcTypeRef): Boolean = {
        log.indented("%s <: %s?", t_sub, wt_sup) {            
            log("t_sub.ctor=%s wt_sup.as.ctor=%s", t_sub.as.ctor, wt_sup.as.ctor)
            if(t_sub.c == wt_sup.c) {
                (!t_sub.as.ctor || wt_sup.as.ctor) && // (t <: t ctor) but (t ctor not <: t)
                wt_sup.wghosts.forall(isSubghost(t_sub, _)) // c<F: P> <: c<F: WP> iff F:P <= F:WP
            } else // else walk to supertype(s) of t_sub
                prog.sups(t_sub).exists(isSubtype(_, wt_sup))
        }                
    }
    
    def isSubtype(cp_sub: ir.CanonPath, wt_sup: ir.WcTypeRef): Boolean = 
        isSubtype(cap(cp_sub), wt_sup)
    
}