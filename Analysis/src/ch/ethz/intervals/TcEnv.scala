package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.collection.immutable.Map
import scala.collection.immutable.Queue

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
    import prog.ghostsOnClassAndSuperclasses
    import prog.typeArgsOnClassAndSuperclasses
    import prog.typeVarsDeclaredOnClassAndSuperclasses
    
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
    
    /// Indicates that point cp hb point cq.
    def addHbPnt(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addHbPnt(%s,%s)", cp, cq)
        assert(!mutable(cp) && !mutable(cq))
        assert(isSubclass(cp.wt, ir.c_point))
        assert(isSubclass(cq.wt, ir.c_point))
        withFlow(flow.withHbRel(flow.hbRel + (cp.p, cq.p)))
    }
    
    /// Indicates that interval cp hb interval cq.
    def addHbInter(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addHbInter(%s,%s)", cp, cq)
        assert(!mutable(cp) && !mutable(cq))
        assert(isSubclass(cp.wt, ir.c_interval))
        assert(isSubclass(cq.wt, ir.c_interval))
        addHbPnt(fld(cp, ir.f_end), fld(cq, ir.f_start))
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

    /// Converts cp_from and cp_to to points that can happen before one another.
    private def userHbPair(cp_from: ir.CanonPath, cp_to: ir.CanonPath): Option[(ir.CanonPath, ir.CanonPath)] = {
        log.indented("userHbPair(%s,%s)", cp_from, cp_to) {
            def makePoint(cp: ir.CanonPath, f: ir.FieldName) = {
                if(isSubclass(cp.wt, ir.c_point)) Some(cp)
                else if(isSubclass(cp.wt, ir.c_interval)) Some(fld(cp, f))
                else None                
            }

            (makePoint(cp_from, ir.f_end), makePoint(cp_to, ir.f_start)) match {
                case (Some(cp_fromi), Some(cp_toi)) => Some((cp_fromi, cp_toi))
                case _ => None
            }
        }        
    }
        
    def addUserHb(cp0: ir.CanonPath, cq0: ir.CanonPath) = {
        log.indented("addUserHb(%s,%s)", cp0, cq0) {
            userHbPair(cp0, cq0) match {
                case Some((cp1, cq1)) => addHbPnt(cp1, cq1)
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
                extendCanonWithFieldNamed(vis, cp0, f)
        }
    }

    def extendCanonWithFieldNamed(
        vis: Set[ir.Path],
        cp0: ir.CanonPath, 
        f: ir.FieldName
    ) = {
        if(f == ir.f_ctor) {                    
            ir.CpCtor(cp0) // Handle p0.ctor specially
        } else if (f == ir.f_super) {
            ir.CpSuper(cp0) // Handle p0.super specially
        } else substdFieldDecl(cp0, f) match {
            case gfd: ir.GhostFieldDecl =>
                extendCanonWithGhostField(vis, cp0, gfd)
            case rfd: ir.ReifiedFieldDecl =>
                log("ReifiedFieldDecl: %s", rfd)
                extendCanonWithReifiedField(cp0, rfd)
        }
    }
    
    private def fld(cp: ir.CanonPath, f: ir.FieldName) = {
        log.indented(false, "fld(%s, %s)", cp, f) {
            extendCanonWithFieldNamed(Set(), cp, f)            
        }
    }
        
    def extendCanonWithReifiedField(cp: ir.CanonPath, rfd: ir.ReifiedFieldDecl) = {
        ir.CpField(cp, rfd)
    }
    
    // Helper for ir.CanonPath(): Computes the ir.CanonPath for a path cp0.f where f is 
    // a ghost field of (post-substitution) type wt_f.
    private def extendCanonWithGhostField(
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
                case Some((cp1, cq1)) => hbPnt(cp1, cq1)
                case None => false
            }
        }        
    }
    
    /// Does the point cp_from hb the point cp_to?
    def hbPnt(cp_from: ir.CanonPath, cp_to: ir.CanonPath) = {
        log.indented("hbPnt(%s, %s)", cp_from, cp_to) {
            log.env(false, "Environment", this)
            assert(!mutable(cp_from))
            assert(!mutable(cp_to))
            assert(isSubclass(cp_from.wt, ir.c_point))
            assert(isSubclass(cp_to.wt, ir.c_point))
            bfs(cp_from, cp_to)
        }
    }
    
    /// Does the interval cp hb the interval cq?
    def hbInter(cp_from: ir.CanonPath, cp_to: ir.CanonPath) = {
        log.indented("hbInter(%s, %s)?", cp_from, cp_to) {
            log.env(false, "Environment", this)
            assert(!mutable(cp_from))
            assert(!mutable(cp_to))
            ( // Sometimes we're sloppy and invoke with wrong types:
                isSubclass(cp_from.wt, ir.c_interval) && 
                isSubclass(cp_to.wt, ir.c_interval) &&
                bfs(fld(cp_from, ir.f_end), fld(cp_to, ir.f_start))
            )
        }
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
                owghost(cp) match {
                    // Is cp a ghost declared writable by q?
                    case Some(ir.WcGhost(_, ir.WcWritableBy(qs))) => among(cq, qs)
                    case _ => false
                }
            ) || {
                flow.writable((cp.p, cq.p)) ||
                equiv(cp, cq) ||
                locks(cq, cp) ||
                isSubintervalOf(cq, cp)
            }
        }
    }    
    
    /// Is data guarded by 'p' readable by the interval 'q'?
    def guardsDataReadableBy(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = {
        log.indented(false, "guardsDataReadableBy(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)
            (
                owghost(cp) match {
                    // Is cp a ghost declared readable by q or immutable in q?
                    case Some(ir.WcGhost(_, ir.WcReadableBy(qs))) => among(cq, qs)
                    case Some(ir.WcGhost(_, ir.WcImmutableIn(qs))) => among(cq, qs)
                    case _ => false
                } 
            ) || {
                superintervalsOrSelf(cq).exists(q => flow.readable((cp.p, q))) ||
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
    
    /// Could the value of 'cp1' change during current interval?
    def mutable(cp_outer: ir.CanonPath) = {
        def m(cp1: ir.CanonPath): Boolean = log.indented("m(%s)", cp1) {
            cp1 match {
                // Local variables never change value once assigned:
                case ir.CpLv(_, _, _) => false

                // Fields guarded by past intervals cannot change but others can:
                case ir.CpField(cp0, ir.ReifiedFieldDecl(_, _, _, p_guard)) =>
                    m(cp0) || { 
                        val cp_guard = canon(p_guard)
                        m(cp_guard) || !hbNow(cp_guard)
                    }

                // Ghosts never change value but the path they extend might:
                case ir.CpField(cp0, _: ir.GhostFieldDecl) => m(cp0)
                case ir.CpCtor(cp0) => m(cp0)
                case ir.CpSuper(cp0) => m(cp0)
            }            
        }
            
        log.indented(false, "mutable(%s)", cp_outer) {
            log.env(false, "Environment", this)
            m(cp_outer)
        }
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
    
    // ___ Happens-Before Searches __________________________________________
    
    private def bfs(cp_from: ir.CanonPath, cp_to: ir.CanonPath) = {
        def depoint(cp: ir.CanonPath, f: ir.FieldName) =
            cp match {
                case ir.CpField(cp_inter, fd) if fd.name == f => Some(cp_inter)
                case _ => None
            }

        def ifInterval(cp0: ir.CanonPath, f: ir.FieldName) =
            if(isSubclass(cp0.wt, ir.c_interval)) Some(fld(cp0, f))
            else None
            
        def succ(cp: ir.CanonPath): Set[ir.CanonPath] = log.indented("succ(%s)", cp) {
            flow.hb.values(cp.p).map(canon) ++ {
                log.indented("x.start -> x.end") {                    
                    depoint(cp, ir.f_start) match {
                        case Some(cp_i) => ifInterval(cp_i, ir.f_end)
                        case _ => None
                    }
                }
            } ++ {
                // If x is an interval, then x.ctor.end hb x.start:
                log.indented("x.ctor -> x.start") {                    
                    depoint(cp, ir.f_end) match {
                        case Some(ir.CpCtor(cp0)) => ifInterval(cp0, ir.f_start)
                        case Some(ir.CpSuper(cp0)) => ifInterval(cp0, ir.f_start)
                        case _ => None
                    }
                }
            } ++ {
                // x.super.end hb current & x.ctor.end hb current.start unless x has ctor type:
                log.indented("x.ctor.end -> cur (%s)", op_cur) {                    
                    depoint(cp, ir.f_end) match {
                        case Some(ir.CpSuper(cp0)) => 
                            op_cur.map(p => canon(p + ir.f_start))
                        case Some(ir.CpCtor(cp0)) if !cp0.wt.as.ctor => 
                            op_cur.map(p => canon(p + ir.f_start))
                        case ocp => 
                            log("Incomplete ctor: %s", ocp)
                            None
                    }                
                }
            } ++ {
                // If x is an interval, then x.ctor.end hb x.start:
                log.indented("hbNow.end -> cur.start") {                    
                    depoint(cp, ir.f_end) match {
                        case Some(ir.CpField(cp0)) => ifInterval(cp0, ir.f_start)
                        case Some(ir.CpSuper(cp0)) => ifInterval(cp0, ir.f_start)
                        case _ => None
                    }
                }
            }             
        }
        
        def search(vis: Set[ir.CanonPath], queue0: Queue[ir.CanonPath]): Boolean = {
            if(queue0.isEmpty)
                false
            else {
                val (cp_cur, queue1) = queue0.dequeue
                log("search(%s)", cp_cur)
                equiv(cp_cur, cp_to) || {
                    val cp_unvisited_succ = succ(cp_cur).filter(cp => !vis(cp))
                    search(vis ++ cp_unvisited_succ, queue1.enqueue(cp_unvisited_succ))
                }                
            }
        }
        
        log.indented("bfs(%s,%s)", cp_from, cp_to) {
            assert(isSubclass(cp_from.wt, ir.c_point))
            assert(isSubclass(cp_to.wt, ir.c_point))
            search(Set(cp_from), Queue.Empty.enqueue(cp_from))            
        }
    }
    
    // ___ Other operations on canonical paths ______________________________
    
    // If cp represents a ghost field, returns the WcGhost defining
    // its value (if any).
    private def owghost(cp: ir.CanonPath): Option[ir.WcGhost] = {
        cp match {
            case ir.CpField(cp0, ir.GhostFieldDecl(_, f)) =>
                ghostsOnType(cp0.wt).find(_.name == f)
            case ir.CpCtor(cp0) =>
                ghostsOnType(cp0.wt).find(_.name == ir.f_ctor)
            case ir.CpSuper(cp0) =>
                ghostsOnType(cp0.wt).find(_.name == ir.f_super)
            case _ =>
                None
        }
    }
    
    def cap(cp: ir.CanonPath): ir.TypeRef = {
        cp.wt match {
            case t: ir.TypeRef => t
            
            case ir.WcClassType(c, wghosts, wtargs, as) =>
                val ghosts = wghosts.flatMap(_.toOptionGhost)
                val targs = wtargs.flatMap(_.toOptionTypeArg)
                ir.ClassType(c, ghosts, targs, as)
        }
    }

    private def equiv(cp: ir.CanonPath, cq: ir.CanonPath) = (cp.p == cq.p)
    
    private def among(cp: ir.CanonPath, qs: List[ir.Path]) = {
        qs.exists { q =>
            val cq = canon(q)
            equiv(cp, cq) || isSubintervalOf(cp, cq)
        }
    }

    // ___ Operations on Types ______________________________________________
    
    private def addFromLowerBounds[X](
        func: (ir.WcClassType => List[X])
    )(
        wt: ir.WcType
    ): List[X] = {
        wt match {
            case pt: ir.PathType => 
                resolvePathType(pt).wts_lb.flatMap(addFromLowerBounds(func))
            case wct: ir.WcClassType => 
                func(wct)            
        }
    }
    
    /// Returns a list of class types that are lower-bounds of wt
    def boundingClassTypes(wt: ir.WcType): List[ir.WcClassType] = {
        addFromLowerBounds(wct => 
            List(wct)
        )(wt)
    }
    
    /// Given a type, returns all ghosts bound on instances of this type.
    /// This includes ghosts in the type and also in the class the type refers to.
    def ghostsOnType(wt: ir.WcType): List[ir.WcGhost] = {
        addFromLowerBounds(wct =>
            wct.wghosts ++ ghostsOnClassAndSuperclasses(wct.c)
        )(wt)
    }
    
    /// Given a type, returns all ghosts bound on instances of this type.
    /// This includes ghosts in the type and also in the class the type refers to.
    def typeArgsOnType(wt: ir.WcType): List[ir.WcTypeArg] = {
        addFromLowerBounds(wct =>
            wct.wtargs ++ typeArgsOnClassAndSuperclasses(wct.c)
        )(wt)
    }
    
    /// Returns all type variables declared on the class(es) this type refers to.
    def typeVarsOnType(wt: ir.WcType): List[ir.TypeVarDecl] = {
        addFromLowerBounds(wct =>
            typeVarsDeclaredOnClassAndSuperclasses(wct.c)
        )(wt)
    }
    
    /// Returns the upper- and lower-bounds for this path type.
    def resolvePathType(pt: ir.PathType): ir.WcTypeArg = {
        val cp = canon(pt.p)
        val wtargs = typeArgsOnType(cp.wt)
        wtargs.find(_.name == pt.tv) match {
            case Some(wtarg) => 
                wtarg
            case None => 
                typeVarsOnType(cp.wt).find(_.name == pt.tv) match {
                    case Some(tvdecl) => 
                        ir.BoundedTypeArg(tvdecl.name, tvdecl.wts_lb, List())
                    case None =>
                        throw new CheckFailure("intervals.no.such.type.var", cp.wt, pt.tv)
                }
        }
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