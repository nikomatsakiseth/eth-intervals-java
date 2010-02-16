package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.collection.immutable.Map
import scala.collection.immutable.Queue

import Util._

sealed case class TcEnv(
    prog: Prog,
    op_cur: Option[ir.Path],                // current interval
    wt_ret: ir.WcTypeRef,                   // return type of current method
    perm: Map[ir.VarName, ir.CanonPath], // permanent equivalences, hold for duration of method
    flow: FlowEnv
) {
    import prog.logStack.log
    import prog.classDecl
    import prog.ghostsOnClassAndSuperclasses
    import prog.typeArgsOnClassAndSuperclasses
    import prog.typeVarsDeclaredOnClassAndSuperclasses
    import prog.unboundGhostFieldsOnClassAndSuperclasses
    import prog.unboundTypeVarsDeclaredOnClassAndSuperclasses
    
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
        assert(!isMutable(cp))
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
        assert(!isMutable(cp) && !isMutable(cq))
        assert(isSubclass(cp.wt, ir.c_point))
        assert(isSubclass(cq.wt, ir.c_point))
        withFlow(flow.withHbRel(flow.hbRel + (cp.p, cq.p)))
    }
    
    /// Indicates that interval cp hb interval cq.
    def addHbInter(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addHbInter(%s,%s)", cp, cq)
        assert(!isMutable(cp) && !isMutable(cq))
        assert(isSubclass(cp.wt, ir.c_interval))
        assert(isSubclass(cq.wt, ir.c_interval))
        addHbPnt(fld(cp, ir.f_end), fld(cq, ir.f_start))
    }
    
    def addDeclaredReadableBy(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addDeclaredReadableBy(%s, %s)", cp, cq)
        assert(!isMutable(cp) && !isMutable(cq))
        assert(isSubclass(cp.wt, ir.c_guard))
        assert(isSubclass(cq.wt, ir.c_interval))
        withFlow(flow.withReadableRel(flow.readableRel + (cp.p, cq.p)))
    }
    
    def addDeclaredWritableBy(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addDeclaredWritableBy(%s, %s)", cp, cq)
        assert(!isMutable(cp) && !isMutable(cq))
        assert(isSubclass(cp.wt, ir.c_guard))
        assert(isSubclass(cq.wt, ir.c_interval))
        withFlow(flow.withWritableRel(flow.writableRel + (cp.p, cq.p)))
    }
    
    def addSubintervalOf(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented("addSubintervalOf(%s, %s)", cp, cq) {
            // This assertion is not valid during checkReifiedFieldDecl():
            //assert(!isMutable(cp) && !isMutable(cq))
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
            //assert(!isMutable(cp) && !isMutable(cq))
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
        f match {
            case ir.CtorFieldName(oc) => 
                ir.CpCtor(cp0, oc)
                
            case _ => 
                substdFieldDecl(cp0.toTcp, f) match {
                    case (_, gfd: ir.GhostFieldDecl) =>
                        extendCanonWithGhostField(vis, cp0, gfd)
                    case (_, rfd: ir.ReifiedFieldDecl) =>
                        log("ReifiedFieldDecl: %s", rfd)
                        extendCanonWithReifiedField(cp0, rfd)
                }
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
            ghost(cp0.wt, gfd.name) match {
                // cp0 had a type with a precise path like Foo<f: q>
                // where q has not yet been visited.  Redirect.
                case Some(ir.WcGhost(_, q: ir.Path)) if !vis1(q) =>
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
    def substdFieldDecl(tcp: ir.TeeCeePee[ir.WcTypeRef], f: ir.FieldName) = {
        boundingClassTypes(tcp.ty).firstSomeReturned(prog.fieldDecl(_, f)) match {
            case Some((wct, fd)) => 
                (
                    ir.TeeCeePee(tcp.cp, wct), 
                    PathSubst.vp(ir.lv_this, tcp.p).fieldDecl(fd)
                )
            case None => throw new CheckFailure("intervals.no.such.field", tcp.ty, f)
        }
    }

    /// Method sig for constructor p(qs)
    def substdCtorSig(tcp: ir.TeeCeePee[ir.WcClassType], m: ir.MethodName, cqs: List[ir.CanonPath]) = {
        classDecl(tcp.ty.c).ctors.find(_.isNamed(m)) match {
            case Some(md) =>            
                val msig = md.msig(tcp.ty)
                PathSubst.vp(
                    ir.lv_this :: msig.args.map(_.name),
                    tcp.p      :: cqs.map(_.p)
                ).methodSig(msig)
            case None =>
                throw new CheckFailure("intervals.no.such.ctor", tcp.ty, m)
        }
    }

    /// Method sig for p.m(qs)
    def substdMethodSig(tcp: ir.TeeCeePee[ir.WcTypeRef], m: ir.MethodName, cqs: List[ir.CanonPath]) = {
        boundingClassTypes(tcp.ty).firstSomeReturned(prog.methodDecl(_, m)) match {
            case Some((wct, md)) =>
                val msig = md.msig(wct)
                PathSubst.vp(
                    ir.lv_this :: msig.args.map(_.name),
                    tcp.p      :: cqs.map(_.p)
                ).methodSig(msig)
            case None =>
                throw new CheckFailure("intervals.no.such.method", tcp.ty, m)
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
            assert(!isMutable(cp_from))
            assert(!isMutable(cp_to))
            assert(isSubclass(cp_from.wt, ir.c_point))
            assert(isSubclass(cp_to.wt, ir.c_point))
            bfs(cp_from, cp_to)
        }
    }
    
    /// Does the interval cp hb the interval cq?
    def hbInter(cp_from: ir.CanonPath, cp_to: ir.CanonPath) = {
        log.indented("hbInter(%s, %s)?", cp_from, cp_to) {
            log.env(false, "Environment", this)
            assert(!isMutable(cp_from))
            assert(!isMutable(cp_to))
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
                is(cp) match {
                    // Is cp a ghost declared writable by q?
                    case ir.WcWritableBy(qs) => among(cq, qs)
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
                is(cp) match {
                    // Is cp a ghost declared readable by q or immutable in q?
                    case ir.WcReadableBy(qs) => among(cq, qs)
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
    def isMutable(cp_outer: ir.CanonPath) = {
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
                case ir.CpCtor(cp0, _) => m(cp0)
            }            
        }
            
        log.indented(false, "isMutable(%s)", cp_outer) {
            log.env(false, "Environment", this)
            m(cp_outer)
        }
    }
    
    /// Does 'cp1' represent a ghost value (i.e., an erased or non-reified value)?
    def isGhost(cp1: ir.CanonPath): Boolean = cp1 match {
        case ir.CpLv(_, _, isGhost) => isGhost
        case ir.CpField(cp0, _: ir.GhostFieldDecl) => true
        case ir.CpField(cp0, _: ir.ReifiedFieldDecl) => isGhost(cp0)
        case ir.CpCtor(cp0, _) => true
    }

    def dependentPaths(wt: ir.WcTypeRef): Set[ir.Path] = {
        boundingClassTypes(wt).foldLeft(Set.empty[ir.Path]) { (s0, wct) =>
            wct.wghosts.foldLeft(s0) { (s, g) => 
                g.wp.addDependentPaths(s) }
        }
    }
        

    /// A field f_l is linked to cp_o.f if its type is invalidated
    /// when p.f changes.  This occurs when p.f appears in f_l's type.
    /// The rules we enforce when checking field decls. guarantee that
    /// all linked fields either (a) occur in the same class defn as f
    /// or (b) are guarded by some interval which has not yet happened.
    def linkedPaths(tcp_o: ir.TeeCeePee[ir.WcClassType], f: ir.FieldName) = {
        def isDependentOn(wt: ir.WcTypeRef, p: ir.Path) = {
            boundingClassTypes(wt).exists { wct =>
                wct.wghosts.exists(_.wp.isDependentOn(p))
            }
        }
        
        val cd = classDecl(tcp_o.ty.c)
        val p_f = f.thisPath
        
        // only reified fields can be linked to another field
        val rfds = cd.fields.foldLeft(List[ir.ReifiedFieldDecl]()) {
            case (l, rfd: ir.ReifiedFieldDecl) => rfd :: l
            case (l, _) => l
        }
        
        // find fields where cp_o.f appears in the type
        val fds_maybe_linked = rfds.filter(rfd => isDependentOn(rfd.wt, p_f))
        
        // screen out those which cannot have been written yet (and whose 
        // value is therefore null, which is valid for any type)
        val subst = tcp_o.cp.thisSubst
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
                        case Some(ir.CpCtor(cp0, _)) => ifInterval(cp0, ir.f_start)
                        case _ => None
                    }
                }
            } ++ {
                // x.super.end hb current & x.ctor.end hb current.start unless x has ctor type:
                log.indented("x.ctor.end -> cur (%s)", op_cur) {                    
                    depoint(cp, ir.f_end) match {
                        case Some(ir.CpCtor(cp0, oc)) if wtImpliesConstructed(cp0.wt, oc) =>
                            op_cur.map(p => canon(p + ir.f_start))
                        case ocp => 
                            log("Incomplete ctor: %s", ocp)
                            None
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

    // Returns an existential WcPath for 'cp' if one can be found.  
    // For example, if p has type @Owner(? readableBy inter), then is(p.Owner) would 
    // yield "? readableBy inter".  Returns cp.p if no existential version is found.
    private def is(cp: ir.CanonPath): ir.WcPath = {
        val owp = cp match {
            case ir.CpField(cp0, ir.GhostFieldDecl(_, f)) =>
                ghost(cp0.wt, f).map(_.wp)
            case _ =>
                None
        }
        owp.getOrElse(cp.p)
    }
    
    private def equiv(cp: ir.CanonPath, cq: ir.CanonPath) = (cp.p == cq.p)
    
    private def among(cp: ir.CanonPath, qs: List[ir.Path]) = {
        qs.exists { q =>
            val cq = canon(q)
            equiv(cp, cq) || isSubintervalOf(cp, cq)
        }
    }

    // ___ Unconstructed Objects ____________________________________________
    
    def uImpliesConstructed(unconstructed: ir.Unconstructed, oc: Option[ir.ClassName]) = {
        (unconstructed, oc) match {
            case (ir.FullyConstructed, _) => true
            case (ir.PartiallyConstructed(c_lb, cs_uncons), Some(c)) if prog.isSubclass(c_lb, c) =>
                !cs_uncons.exists(prog.isSubclass(c, _))
            case _ => false
        }        
    }
    
    // Does an object of the type 'wt' implies that the constructor for the class 'oc'
    // has completed?  If oc is None, then the object as a whole must have been constructed.
    def wtImpliesConstructed(wt: ir.WcTypeRef, oc: Option[ir.ClassName]) = {
        boundingClassTypes(wt).exists { wct =>
            uImpliesConstructed(wct.unconstructed, oc)
        }
    }
    
    // ___ Operations on Types ______________________________________________
    
    private def addFromLowerBounds[X](
        func: (ir.WcClassType => Iterable[X])
    )(
        wt: ir.WcTypeRef
    ): List[X] = {
        wt match {
            case pt: ir.PathType => 
                boundPathType(pt).wts_lb.flatMap(addFromLowerBounds(func))
            case wct: ir.WcClassType => 
                func(wct).toList
        }
    }
    
    /// Returns a list of class types that are lower-bounds of wt
    def boundingClassTypes(wt: ir.WcTypeRef): List[ir.WcClassType] = {
        addFromLowerBounds(wct => 
            List(wct)
        )(wt)
    }
    
    /// Given a type, returns all ghosts bound on instances of this type.
    /// This includes ghosts in the type and also in the class the type refers to.
    def ghosts(wt: ir.WcTypeRef): List[ir.WcGhost] = {
        addFromLowerBounds(wct =>
            wct.wghosts ++ ghostsOnClassAndSuperclasses(wct.c)
        )(wt)
    }
    
    /// Returns the binding for the ghost field 'f' on type 'cp0', if any.
    def ghost(wt: ir.WcTypeRef, f: ir.FieldName): Option[ir.WcGhost] = {
        ghosts(wt).find(_.isNamed(f))
    }
    
    /// Given a type, returns all ghosts bound on instances of this type.
    /// This includes ghosts in the type and also in the class the type refers to.
    def typeArgs(wt: ir.WcTypeRef): List[ir.WcTypeArg] = {
        addFromLowerBounds(wct =>
            wct.wtargs ++ typeArgsOnClassAndSuperclasses(wct.c)
        )(wt)
    }
    
    /// Returns the binding for the ghost field 'f' on type 'cp0', if any.
    private def typeArg(wt: ir.WcTypeRef, tv: ir.TypeVarName): Option[ir.WcTypeArg] = {
        typeArgs(wt).find(_.isNamed(tv))
    }
    
    /// Returns all type variables declared on the class(es) this type refers to.
    def typeVarsDeclaredOnType(wt: ir.WcTypeRef): List[ir.TypeVarDecl] = {
        addFromLowerBounds(wct =>
            typeVarsDeclaredOnClassAndSuperclasses(wct.c)
        )(wt)
    }
    
    /// Returns the upper- and lower-bounds for a type.
    def boundPathType(pt: ir.PathType): ir.TypeBounds = {
        val cp = canon(pt.p)
        typeArg(cp.wt, pt.tv) match {
            case Some(wtarg) => 
                wtarg.bounds
            case None => 
                typeVarsDeclaredOnType(cp.wt).find(_.isNamed(pt.tv)) match {
                    case Some(tvdecl) =>
                        ir.TypeBounds(tvdecl.wts_lb, None)
                    case None =>
                        throw new CheckFailure("intervals.no.such.type.var", cp.wt, pt.tv)
                }
        }
    }
    
    // Is wt an erased subtype of class c?
    def isSubclass(wt: ir.WcTypeRef, c: ir.ClassName): Boolean = {
        boundingClassTypes(wt).exists(wct => prog.isSubclass(wct.c, c))
    }
        
    // ___ Subtyping ________________________________________________________
    
    private def isLtUnconstructed(u_sub: ir.Unconstructed, u_sup: ir.Unconstructed) = {
        (u_sub, u_sup) match {
            case (ir.FullyConstructed, _) => true
            case (_, ir.FullyConstructed) => false
            case (ir.PartiallyConstructed(c_lb, cs_incomplete), _) =>
                !uImpliesConstructed(u_sup, Some(c_lb)) &&
                cs_incomplete.forall(c_incomplete => 
                    !uImpliesConstructed(u_sup, Some(c_incomplete)))
        }
    }
    
    private def isLtWpath(wp: ir.WcPath, wq: ir.WcPath) = {
        (wp, wq) match {
            case (p: ir.Path, q: ir.Path) =>
                equiv(canon(p), canon(q))
            case (p: ir.Path, ir.WcReadableBy(qs)) =>
                val cp = canon(p)
                qs.forall { q => guardsDataReadableBy(cp, canon(q)) }
            case (p: ir.Path, ir.WcWritableBy(qs)) =>
                val cp = canon(p)
                qs.forall { q => guardsDataWritableBy(cp, canon(q)) }
            case (ir.WcWritableBy(ps), ir.WcWritableBy(qs)) => 
                qs.forall { q => among(canon(q), ps) }
            case (ir.WcWritableBy(ps), ir.WcReadableBy(qs)) => 
                qs.forall { q => among(canon(q), ps) }
            case (ir.WcReadableBy(ps), ir.WcReadableBy(qs)) => 
                qs.forall { q => among(canon(q), ps) }
        }
    }
        
    private def isLtGhost(wct_sub: ir.WcClassType, wg_sup: ir.WcGhost) = {
        log.indented("%s <= %s?", wct_sub, wg_sup) {
            ghost(wct_sub, wg_sup.f).exists(wg_sub => isLtWpath(wg_sub.wp, wg_sup.wp))
        }   
    }
    
    private def isLtBounds(bounds_sub: ir.TypeBounds, bounds_sup: ir.TypeBounds) = {
        def compareLowerBounds(lbs_sub: List[ir.WcTypeRef], lbs_sup: List[ir.WcTypeRef]) = {
            lbs_sup.forall(lb_sup =>
                lbs_sub.exists(lb_sub =>
                    isSubtype(lb_sub, lb_sup)))            
        }
        
        (bounds_sub, bounds_sup) match {
            case (ir.TypeBounds(lbs_sub, _), ir.TypeBounds(lbs_sup, None)) =>
                compareLowerBounds(lbs_sub, lbs_sup)
                        
            case (ir.TypeBounds(lbs_sub, Some(ubs_sub)), ir.TypeBounds(lbs_sup, Some(ubs_sup))) =>
                compareLowerBounds(lbs_sub, lbs_sup) &&
                ubs_sup.forall(ub_sup =>
                    ubs_sub.exists(ub_sub =>
                        isSubtype(ub_sup, ub_sub)))
        }
    }
    
    private def isLtTarg(wct_sub: ir.WcClassType, wtarg_sup: ir.WcTypeArg) = {
        log.indented("%s <= %s?", wct_sub, wtarg_sup) {
            val tv = wtarg_sup.tv
            typeArg(wct_sub, tv).exists(wtarg_sub => 
                isLtBounds(wtarg_sub.bounds, wtarg_sup.bounds))
        }            
    }
    
    private def isSubClassType(wct_sub: ir.WcClassType, wct_sup: ir.WcClassType): Boolean = {
        log.indented("isSubClassType(%s, %s)?", wct_sub, wct_sup) {
            if(wct_sub.c != wct_sup.c) {
                prog.sups(wct_sub).exists(isSubClassType(_, wct_sup))
            } else {
                isLtUnconstructed(wct_sub.unconstructed, wct_sup.unconstructed) &&
                wct_sup.wghosts.forall(isLtGhost(wct_sub, _)) &&
                wct_sup.wtargs.forall(isLtTarg(wct_sub, _))                
            }
        }
    }
    
    /// t_sub <: wt_sup
    private def isSubtype(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef): Boolean = {
        log.indented("%s <: %s?", wt_sub, wt_sup) {
            (wt_sub, wt_sup) match {
                case (pt_sub: ir.PathType, pt_sup: ir.PathType) if pt_sub == pt_sup =>
                    true
                
                case (wct_sub: ir.WcClassType, wct_sup: ir.WcClassType) =>
                    isSubClassType(wct_sub, wct_sup)
                    
                /* 
                    XXX Be wary of cyclic recursive types.
                */
                
                // If super type is a path type, promote to upper-bound if any:
                case (_, pt_sup: ir.PathType) =>
                    boundPathType(pt_sup).owts_ub.exists(wts_ub =>
                        wts_ub.exists(isSubtype(wt_sub, _)))
                    
                // If subtype is a path type, promote to lower-bound:
                case (pt_sub: ir.PathType, _) =>
                    boundPathType(pt_sub).wts_lb.exists(isSubtype(_, wt_sup))
            }            
        }                
    }
    
    private def capture(cp: ir.CanonPath): ir.WcTypeRef = {
        cp.wt match {
            case pt: ir.PathType => pt
            case wct: ir.WcClassType =>
                val ghosts = unboundGhostFieldsOnClassAndSuperclasses(wct.c).toList
                val typeVarDecls = unboundTypeVarsDeclaredOnClassAndSuperclasses(wct.c).toList
                ir.WcClassType(
                    wct.c,
                    ghosts.map(_.ghostOf(cp.p)),
                    typeVarDecls.map(_.typeArgOf(cp.p)),
                    wct.unconstructed
                )
        }
    }
    
    def pathHasType(cp_sub: ir.CanonPath, wt_sup: ir.WcTypeRef): Boolean = {
        isSubtype(capture(cp_sub), wt_sup)
    }
    
}