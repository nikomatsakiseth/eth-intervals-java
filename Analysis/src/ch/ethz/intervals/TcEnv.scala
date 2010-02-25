package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.collection.immutable.Map
import scala.collection.immutable.Queue

import Util._

sealed case class TcEnv(
    prog: Prog,
    c_this: ir.ClassName,
    ocp_cur: Option[ir.CanonPath],
    wt_ret: ir.WcTypeRef,                   // return type of current method
    perm: Map[ir.VarName, ir.CanonPath],    // permanent equivalences, hold for duration of method
    flow: FlowEnv
) {
    import prog.logStack.log
    import prog.logStack.at    
    import prog.classDecl
    import prog.ghostFieldsDeclaredOnClassAndSuperclasses
    import prog.ghostsOnClassAndSuperclasses
    import prog.typeArgsOnClassAndSuperclasses
    import prog.typeVarsDeclaredOnClassAndSuperclasses
    import prog.unboundGhostFieldsOnClassAndSuperclasses
    import prog.unboundTypeVarsDeclaredOnClassAndSuperclasses
    
    // ___ Merging Flows ____________________________________________________
    
    def addFlow(flow1: FlowEnv) = copy(flow = flow + flow1)        
    def intersectFlow(flow1: FlowEnv) = copy(flow = flow & flow1)
        
    // ___ Modifying the Environment ________________________________________
    
    /** Set return type to `wt_ret` */
    def withReturnType(wt_ret: ir.WcTypeRef) = copy(wt_ret = wt_ret)
    
    /** Set current interval to `cp_cur` */
    def withCurrent(cp_cur: ir.CanonPath) = copy(ocp_cur = Some(cp_cur))

    /** Set `c_this`, adding `this` as a local variable. */
    def withThisClass(c_this: ir.ClassName) = addReifiedLocal(ir.lv_this, c_this.ct).copy(c_this = c_this)
    
    /** Add a local variable whose value is the canon path `cp` */
    def addPerm(x: ir.VarName, cp: ir.CanonPath): TcEnv = {
        assert(isImmutable(cp))
        perm.get(x) match {
            case Some(_) => throw new CheckFailure("intervals.shadowed", x)
            case None => copy(perm = perm + (x -> cp))
        }
    }
    
    /** Define a reified local variable `x` with type `wt` */
    def addReifiedLocal(x: ir.VarName, wt: ir.WcTypeRef) = addPerm(x, ir.CpReifiedLv(x, wt))
    
    /** Define a local variable according to the given decl */
    def addArg(arg: ir.LvDecl) = addReifiedLocal(arg.name, arg.wt)
    
    /** Define local variables according to the given decls */
    def addArgs(args: List[ir.LvDecl]) = args.foldLeft(this)(_ addArg _)
    
    /** Define a reified local variable `x` with type `wt` */
    def redefineReifiedLocal(x: ir.VarName, wt: ir.WcTypeRef) = {
        copy(perm = perm + Pair(x, ir.CpReifiedLv(x, wt)))
    }
    
    /** Define a ghost local variable `x` with type `wt` */
    def addGhostLocal(lv: ir.VarName, c: ir.ClassName) = addPerm(lv, ir.CpGhostLv(lv, c))
    
    /** Defines a fresh ghost variable of type `wt` */
    def freshCp(c: ir.ClassName) = {
        val lv = prog.freshVarName
        val env1 = addGhostLocal(lv, c)
        (env1.perm(lv), env1)
    }
        
    /** Temporarily redirect from `p` to `q` */
    def addTemp(p: ir.Path, q: ir.Path): TcEnv = {
        log("addTemp(%s,%s)", p, q)
        copy(flow = flow.withTemp(flow.temp + (p -> q)))
    }
    
    /** Clear all temporary redirects */
    def clearTemp(): TcEnv = {
        log("clearTemp")
        copy(flow = flow.withTemp(Map()))
    }
    
    def addInvalidated(p: ir.Path) = {
        log("addInvalidated(%s)", p)
        copy(flow = flow.withInvalidated(flow.ps_invalidated + p))        
    }
        
    def removeInvalidated(p: ir.Path) = {
        log("removeInvalidated(%s)", p)
        copy(flow = flow.withInvalidated(flow.ps_invalidated - p))        
    }
    
    /// Indicates that point cp hb point cq.
    def addHbPnt(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addHbPnt(%s,%s)", cp, cq)
        assert(isImmutable(cp) && isImmutable(cq))
        assert(pathCouldHaveClass(cp, ir.c_point))
        assert(pathCouldHaveClass(cq, ir.c_point))
        copy(flow = flow.withHbRel(flow.hbRel + (cp.p, cq.p)))
    }
    
    /// Indicates that interval cp hb interval cq.
    def addHbInter(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addHbInter(%s,%s)", cp, cq)
        assert(isImmutable(cp) && isImmutable(cq))
        assert(pathCouldHaveClass(cp, ir.c_interval))
        assert(pathCouldHaveClass(cq, ir.c_interval))
        copy(flow = flow.withHbRel(flow.hbRel + (cp.p.end, cq.p.start)))
    }
    
    def addDeclaredReadableBy(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addDeclaredReadableBy(%s, %s)", cp, cq)
        assert(isImmutable(cp) && isImmutable(cq))
        assert(pathCouldHaveClass(cp, ir.c_guard))
        assert(pathCouldHaveClass(cq, ir.c_interval))
        copy(flow = flow.withReadableRel(flow.readableRel + (cp.p, cq.p)))
    }
    
    def addDeclaredWritableBy(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addDeclaredWritableBy(%s, %s)", cp, cq)
        assert(isImmutable(cp) && isImmutable(cq))
        assert(pathCouldHaveClass(cp, ir.c_guard))
        assert(pathCouldHaveClass(cq, ir.c_interval))
        copy(flow = flow.withWritableRel(flow.writableRel + (cp.p, cq.p)))
    }
    
    def addSuspends(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented(false, "addSuspends(%s, %s)", cp, cq) {
            // This assertion is not valid during checkReifiedFieldDecl():
            //assert(isImmutable(cp) && isImmutable(cq))
            assert(pathCouldHaveClass(cp, ir.c_interval))
            assert(pathCouldHaveClass(cq, ir.c_interval))
            copy(flow = flow
                .withHbRel(flow.hbRel + (cq.p.start, cp.p.start) + (cp.p.end, cq.p.end))
                .withInlineIntervalRel(flow.inlineIntervalRel + (cp.p, cq.p)))
        }        
    }

    def addLocks(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented(false, "addLocks(%s, %s)", cp, cq) {
            // This assertion is not valid during checkReifiedFieldDecl:
            //assert(isImmutable(cp) && isImmutable(cq))
            assert(pathCouldHaveClass(cp, ir.c_interval))
            assert(pathCouldHaveClass(cq, ir.c_lock))
            copy(flow = flow.withLocksRel(flow.locksRel + (cp.p, cq.p)))
        }        
    }

    /** Converts cp_from and cp_to to points that can happen before one another. */
    private def userHbPair(cp_from: ir.CanonPath, cp_to: ir.CanonPath): Option[(ir.Path, ir.Path)] = {
        log.indented(false, "userHbPair(%s,%s)", cp_from, cp_to) {
            def makePoint(cp: ir.CanonPath, f: ir.FieldName) = {
                if(pathHasClass(cp, ir.c_point)) Some(cp.p)
                else if(pathHasClass(cp, ir.c_interval)) Some(cp.p + f)
                else None                
            }

            (makePoint(cp_from, ir.f_end), makePoint(cp_to, ir.f_start)) match {
                case (Some(p_fromi), Some(p_toi)) => Some((p_fromi, p_toi))
                case _ => None
            }
        }        
    }
        
    def addUserHb(cp0: ir.CanonPath, cq0: ir.CanonPath) = {
        log.indented(false, "addUserHb(%s,%s)", cp0, cq0) {
            assert(isImmutable(cp0))
            assert(isImmutable(cq0))
            userHbPair(cp0, cq0) match {
                case Some((p1, q1)) => 
                    copy(flow = flow.withHbRel(flow.hbRel + (p1, q1)))
                case None => 
                    this
            }
        }        
    }
        
    def addNonNull(cp: ir.CanonPath) = {
        log.indented(false, "addNonNull(%s)", cp) {
            copy(flow = flow.withNonnull(flow.nonnull + cp.p))            
        }
    }
        
    def addReq(req: ir.Req) = {
        log.indented("addReq(%s)", req) {
            at(req, this) {
                def cross(
                    add: ((TcEnv, ir.CanonPath, ir.CanonPath) => TcEnv), 
                    ps: List[ir.Path], 
                    qs: List[ir.Path]
                ) = {
                    val cps = immutableCanonPaths(ps)
                    val cqs = immutableCanonPaths(qs)
                    cps.foldLeft(this) { case (env0, cp) =>
                        cqs.foldLeft(env0) { case (env1, cq) =>
                            add(env1, cp, cq)
                        }
                    }                    
                }
                
                req match {
                    case ir.ReqWritableBy(ps, qs) => cross(_.addDeclaredWritableBy(_, _), ps, qs)
                    case ir.ReqReadableBy(ps, qs) => cross(_.addDeclaredReadableBy(_, _), ps, qs)
                    case ir.ReqHb(ps, qs) => cross(_.addUserHb(_, _), ps, qs)
                    case ir.ReqSuspends(ps, qs) => cross(_.addSuspends(_, _), ps, qs)
                }   
            }
        }        
    }
    
    def addReqs(reqs: List[ir.Req]) = reqs.foldLeft(this)(_ addReq _)
        
    // ___ Constructing Canonical Paths _____________________________________
    
    def reifiedPath(p: ir.Path): ir.CanonReifiedPath = {
        canonPath(p) match {
            case crp: ir.CanonReifiedPath => crp
            case _ => throw new CheckFailure("intervals.must.be.reified", p)
        }
    }
    def reifiedPaths(ps: List[ir.Path]) = ps.map(reifiedPath)
    def reifiedLv(lv: ir.VarName) = reifiedPath(lv.path)
    def reifiedLvs(lvs: List[ir.VarName]) = lvs.map(reifiedLv)
    
    def immutableReifiedPath(p: ir.Path): ir.CanonReifiedPath = {
        val crp = reifiedPath(p)
        if(!isImmutable(crp))
            throw new CheckFailure("intervals.must.be.immutable", p)
        crp
    }    
    def immutableReifiedPaths(ps: List[ir.Path]) = ps.map(immutableReifiedPath)
    def immutableReifiedLv(lv: ir.VarName) = immutableReifiedPath(lv.path)
    def immutableReifiedLvs(lvs: List[ir.VarName]) = lvs.map(immutableReifiedLv)
    
    def immutableCanonPath(p: ir.Path): ir.CanonPath = {
        val cp = canonPath(p)
        if(!isImmutable(cp))
            throw new CheckFailure("intervals.must.be.immutable", p)
        cp        
    }
    def immutableCanonPaths(ps: List[ir.Path]) = ps.map(immutableCanonPath)
    def immutableCanonLv(lv: ir.VarName) = immutableCanonPath(lv.path)    
    def immutableCanonLvs(lvs: List[ir.VarName]) = lvs.map(immutableCanonLv)
    
    def canonPath(p1: ir.Path): ir.CanonPath = log.indented(false, "canonPath(%s)", p1) {
        canonicalize(Set(), p1)
    }
    def canonPaths(ps: List[ir.Path]) = ps.map(canonPath)
    def canonLv(lv: ir.VarName) = canonPath(lv.path)
    def canonLvs(lvs: List[ir.VarName]) = lvs.map(canonLv)

    private def canonicalize(vis: Set[ir.Path], p1: ir.Path): ir.CanonPath = log.indented("canonicalize(%s) vis=%s", p1, vis) {
        assert(!vis(p1))
        if(flow.temp.contains(p1)) {
            val p1_redirect = flow.temp(p1)
            log("temp redirect to %s", p1_redirect)
            canonicalize(vis + p1, p1_redirect)                
        } else p1.rev_fs match {
            case List() =>
                perm.get(p1.lv) match {
                    case Some(cp) => cp
                    case None => throw new CheckFailure("intervals.no.such.variable", p1.lv)
                }
                
            case f :: rev_fs =>
                val p0 = ir.Path(p1.lv, rev_fs)      // p1 = p0.f
                val crp0 = reifiedPath(p0)
                extendCanonWithFieldNamed(vis, crp0, f)
        }
    }
    
    def extendCanonWithFieldNamed(
        vis: Set[ir.Path],
        crp0: ir.CanonReifiedPath, 
        f: ir.FieldName
    ) = {
        f match {
            case ir.ClassCtorFieldName(c) => 
                ir.CpClassCtor(crp0, c)
                
            case ir.f_objCtor() =>
                extendCanonWithGhostField(vis, crp0, ir.f_objCtor, ir.c_interval)
                
            case _ => 
                ghostFieldsDeclaredOnType(crp0.wt).find(_.isNamed(f)) match {
                    case Some(ir.GhostFieldDecl(f_gfd, c_gfd)) =>
                        extendCanonWithGhostField(vis, crp0, f_gfd, c_gfd)
                        
                    case None =>
                        val (_, rfd) = substdReifiedFieldDecl(crp0.toTcp, f)
                        extendCanonWithReifiedField(crp0, rfd)
                }
        }
    }
    
    private def fld(crp0: ir.CanonReifiedPath, fs: ir.FieldName*) = {
        log.indented(false, "fld(%s, %s)", crp0, fs) {
            val crp2 = fs.dropRight(1).foldLeft(crp0) { case (crp, f) =>
                extendCanonWithFieldNamed(Set(), crp, f) match {
                    case crp1: ir.CanonReifiedPath => crp1
                    case _ => throw new CheckFailure("intervals.must.be.reified", crp.p + f)
                }
            }
            
            val f = fs.takeRight(1).head
            extendCanonWithFieldNamed(Set(), crp2, f)
        }
    }
        
    def extendCanonWithReifiedField(crp: ir.CanonReifiedPath, rfd: ir.ReifiedFieldDecl) = {
        log("extendCanonWithReifiedField(%s, %s)", crp, rfd)
        ir.CpReifiedField(crp, rfd)
    }
    
    // Helper for ir.CanonPath(): Computes the ir.CanonPath for a path cp0.f where f is 
    // a ghost field of (post-substitution) type wt_f.
    private def extendCanonWithGhostField(
        vis_in: Set[ir.Path],
        crp0: ir.CanonReifiedPath, 
        f_gfd: ir.FieldName,
        c_gfd: ir.ClassName
    ) = {
        log.indented("extendCanonWithGhostField(%s, %s, %s, %s)", vis_in, crp0, f_gfd, c_gfd) {
            val cp1 = ir.CpGhostField(crp0, f_gfd, c_gfd)
            val vis1 = vis_in + cp1.p
            
            // Does cp0's type specify a value for ghost field f?
            ghost(crp0, f_gfd) match {
                // cp0 had a type with a precise path like Foo<f: q>
                // where q has not yet been visited.  Redirect.
                case ir.WcGhost(_, q: ir.Path) if !vis1(q) =>
                    log.indented("Redirect: %s", q) {
                        canonicalize(vis1, q)
                    }
                    
                case _ => cp1
            }            
        }        
    }    
    
    // ___ Locating Members _________________________________________________

    /** Given a type, returns all GhostFieldDecls applied on it or its supertypes. */
    def ghostFieldsDeclaredOnType(wt: ir.WcTypeRef): Set[ir.GhostFieldDecl] = {
        lowerBoundingCts(wt).flatMap(wct => ghostFieldsDeclaredOnClassAndSuperclasses(wct.c))
    }
    
    /** Returns all type variables declared on the class(es) this type refers to. */
    def typeVarsDeclaredOnType(wt: ir.WcTypeRef): Set[ir.TypeVarDecl] = {
        lowerBoundingCts(wt).flatMap(wct => typeVarsDeclaredOnClassAndSuperclasses(wct.c))
    }
    
    ///
    private[this] def searchClassAndSuperclasses[X](
        func: (ir.ClassDecl => Option[X])
    )(
        c: ir.ClassName
    ): Option[(ir.ClassName, X)] = {
        val cd = classDecl(c)
        func(cd) match {
            case Some(x) =>
                Some((c, x))
                
            case None =>
                cd.superClasses.firstSomeReturned(searchClassAndSuperclasses(func) _)
        }
    }
    
    /** Reified field decl for `tcp.f` */
    def substdReifiedFieldDecl(tcp: ir.TeeCeePee[ir.WcTypeRef], f: ir.FieldName) = {
        def search(wct: ir.WcClassType) = 
            searchClassAndSuperclasses(_.reifiedFieldDecls.find(_.isNamed(f)))(wct.c)
        log.indented(false, "substdReifiedFieldDecl(%s::%s)", tcp, f) {
            upperBoundingCts(tcp.ty).firstSomeReturned(search) match {
                case Some((c_fd, fd)) => 
                    log.reifiedFieldDecl("fd: ", fd)
                    (c_fd, PathSubst.vp(ir.lv_this, tcp.p).reifiedFieldDecl(fd))
                    
                case None => 
                    throw new CheckFailure("intervals.no.such.field", tcp.ty, f)
            }
        }
    }

    /** Method sig for `tcp.m(tqs)`.  Substitutes `tcp` for the receiver, 
      * `tqs` for the arguments, and the current interval for `method`. */
    def substdMethodSig(tcp: ir.TeeCeePee[ir.WcTypeRef], m: ir.MethodName, cqs: List[ir.CanonPath]) = {
        def search(wct: ir.WcClassType) = 
            searchClassAndSuperclasses(_.methods.find(_.isNamed(m)))(wct.c)
        log.indented(false, "substdMethodSig(%s::%s)", tcp.ty, m) {
            lowerBoundingCts(tcp.ty).firstSomeReturned(search) match {
                case Some((_, md)) =>
                    log.methodDecl("md: ", md)
                    PathSubst.vp(
                        ir.lv_mthd  :: ir.lv_this :: md.args.map(_.name),
                        p_cur       :: tcp.p      :: cqs.map(_.p)
                    ).methodSig(md.msig)
                case None =>
                    throw new CheckFailure("intervals.no.such.method", tcp.ty, m)
            }            
        }
    }
    
    /** Method sig for constructor `m` of `tcp` invoked with args `tqs`.
      * Substitutes `tcp` for the receiver, `tqs` for the arguments, 
      * and the current interval for `method`. */
    def substdCtorSig(tcp: ir.TeeCeePee[ir.WcClassType], m: ir.MethodName, cqs: List[ir.CanonPath]) = {
        classDecl(tcp.ty.c).ctors.find(_.isNamed(m)) match {
            case Some(md) =>            
                PathSubst.vp(
                    ir.lv_mthd  :: ir.lv_this :: md.args.map(_.name),
                    p_cur       :: tcp.p      :: cqs.map(_.p)
                ).methodSig(md.msig)
            case None =>
                throw new CheckFailure("intervals.no.such.ctor", tcp.ty, m)
        }
    }

    /** Method signatures of all methods overridden by `md_sub` declared on
      * the class `ct`. */
    def substdOverriddenMethodSigs(ct: ir.ClassType, md_sub: ir.MethodDecl) = {
        val cd = classDecl(ct.c)        
        val mds_overridden = cd.superClasses.foldLeft(List[ir.MethodDecl]()) { case (l, c) =>
            val cd_sup = classDecl(c)
            cd_sup.methods.find(_.isNamed(md_sub.name)) match {
                case None => l
                case Some(md) => md :: l
            }
        }
        val lvs_sub = md_sub.args.map(_.name)
        mds_overridden.map { md_overridden =>
            val msig_overridden = md_overridden.msig
            val lvs_overridden = md_overridden.args.map(_.name)
            val subst = PathSubst.vv(lvs_overridden, lvs_sub)
            subst.methodSig(msig_overridden)
        }
    }
    
    // ___ Queries of the environment _______________________________________
    //
    // Queries are generally defined over canonical paths.
    
    /** Current interval (must be defined) */
    def cp_cur = ocp_cur.get
    
    /** Current interval (must be defined) */
    def p_cur = ocp_cur.get.p
    
    /** Canonical path to `this` */
    def crp_this = reifiedLv(ir.lv_this)
    
    /** TeeCeePee of `this` */
    def tcp_this = ir.TeeCeePee(crp_this, ct_this)

    /** ClassType of `this` */
    def ct_this = c_this.ct
    
    /** Canonical path to `method` */
    def cp_mthd = canonLv(ir.lv_mthd)
    
    /** Upcast version of this to the first superclass */
    def tcp_super = superTcps(tcp_this) match {
        case List() => throw new CheckFailure("intervals.no.supertype", crp_this.wt)
        case tcp :: _ => tcp
    }
    
    /** True if the path `cp` is known to be nonnull */
    def isNonnull(cp: ir.CanonPath) = log.indented(false, "isNonnull(%s)?", cp) {
        log.env(false, "Environment", this)
        flow.nonnull(cp.p)
    }

    /** Does `cp0` hb `cq0`? */
    def userHb(cp0: ir.CanonPath, cq0: ir.CanonPath) = {
        log.indented("userHb(%s,%s)", cp0, cq0) {
            assert(isImmutable(cp0))
            assert(isImmutable(cq0))
            userHbPair(cp0, cq0) match {
                case Some((p1, q1)) => bfs(p1, q1)
                case None => false
            }
        }        
    }
    
    /// Does the point cp_from hb the point cp_to?
    def hbPnt(cp_from: ir.CanonPath, cp_to: ir.CanonPath) = {
        log.indented("hbPnt(%s, %s)", cp_from, cp_to) {
            log.env(false, "Environment", this)
            assert(isImmutable(cp_from))
            assert(isImmutable(cp_to))
            assert(pathHasClass(cp_from, ir.c_point))
            assert(pathHasClass(cp_to, ir.c_point))
            bfs(cp_from.p, cp_to.p)
        }
    }
    
    /// Does the interval cp hb the interval cq?
    def hbInter(cp_from: ir.CanonPath, cp_to: ir.CanonPath) = {
        log.indented("hbInter(%s, %s)?", cp_from, cp_to) {
            log.env(false, "Environment", this)
            ( // Sometimes we're sloppy and invoke with wrong types:
                pathHasClass(cp_from, ir.c_interval) && 
                pathHasClass(cp_to, ir.c_interval) &&
                bfs(cp_from.p.end, cp_to.p.start)
            )
        }
    }
    
    def suspends(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented(false, "suspends(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)

            superintervalsOrSelf(cp).contains(cq)
        }
    }
    
    def locks(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented(false, "locks(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)
            
            superintervalsOrSelf(cp).exists { cp_sup => flow.locks((cp_sup.p, cq.p)) }
        }
    }
    
    /// Is data guarded by 'p' writable by the interval 'q'?
    def guardsDataWritableBy(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = {
        log.indented(false, "guardsDataWritableBy(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)
            (
                is(cp) match {
                    // Is cp a ghost declared writable by q?
                    case ir.WcWritableBy(qs) => among(cq, canonPaths(qs))
                    case _ => false
                }
            ) || {
                superintervalsOrSelf(cq).exists(cq_sup => flow.writable((cp.p, cq_sup.p))) ||
                equiv(cp, cq) ||
                locks(cq, cp) ||
                suspends(cq, cp)
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
                    case ir.WcReadableBy(qs) => among(cq, canonPaths(qs))
                    case _ => false
                } 
            ) || {
                superintervalsOrSelf(cq).exists(cq_sup => flow.readable((cp.p, cq_sup.p))) ||
                hbInter(cp, cq) ||
                guardsDataWritableBy(cp, cq)            
            }
        }
    }
    
    private[this] def interHappened(cp: ir.CanonPath): Boolean = {
        log.indented(true, "interHappened(%s)", cp) {
            ocp_cur.exists(cp_cur => bfs(cp.p.end, cp_cur.p.start))
        }
    }
            
    /// cp hbNow cqs if either (1) cp hb cur; or (2) ∃i. cp.end hbeq cqs(i).end.
    def hbNow(cp: ir.CanonPath, cqs: List[ir.CanonPath]): Boolean = {
        log.indented(false, "hbNow(%s, %s)", cp, cqs) {
            log.env(false, "Environment", this)
            
            (
                is(cp) match {
                    case ir.WcHbNow(ps) => isLtCanonPaths(canonPaths(ps), cqs)
                    case _ => false
                }
            ) || {
                interHappened(cp) ||
                among(cp, cqs) ||
                cqs.exists(cq => bfs(cp.p.end, cq.p.end))
            }            
        }
    }
    
    /** Could the value of `cp_test` change during `cp_inter`? */
    def isImmutableIn(cp_test: ir.CanonPath, cp_inter: ir.CanonPath) = {
        def imm(cp1: ir.CanonPath): Boolean = log.indented("imm(%s)", cp1) {
            cp1 match {
                // Local variables never change value once assigned:
                case ir.CpReifiedLv(_, _) => true
                case ir.CpGhostLv(_, _) => true

                // Ghosts never change value but the path they extend might:
                case ir.CpGhostField(cp0, _, _) => imm(cp0)
                case ir.CpClassCtor(cp0, _) => imm(cp0)
                
                // Fields guarded by past intervals cannot change but others can:
                case ir.CpReifiedField(cp0, ir.ReifiedFieldDecl(_, _, _, p_guard)) =>
                    imm(cp0) && { 
                        val cp_guard = canonPath(p_guard)
                        imm(cp_guard) && bfs(cp_guard.p.end, cp_inter.p.start)
                    }
            }            
        }
            
        log.indented(false, "isImmutableDuring(%s, %s)", cp_test, cp_inter) {
            log.env(false, "Environment", this)
            imm(cp_test)
        }
    }
    
    def isImmutable(cp_test: ir.CanonPath): Boolean = cp_test match {
        // Hackli: addPerm() sometimes asserts immutability before cp_cur is
        // defined.  That's ok so long as the canonical path is just a LV and ghosts.
        case ir.CpReifiedLv(_, _) => true
        case ir.CpGhostLv(_, _) => true
        case ir.CpGhostField(cp0, _, _) => isImmutable(cp0)
        case ir.CpClassCtor(cp0, _) => isImmutable(cp0)
        case _ => isImmutableIn(cp_test, cp_cur)
    }

    def dependentPaths(wt: ir.WcTypeRef): Set[ir.Path] = {
        lowerBoundingCts(wt).foldLeft(Set.empty[ir.Path]) { (s0, wct) =>
            wct.wghosts.foldLeft(s0) { (s, g) => 
                g.wp.addDependentPaths(s) 
            }
        }
    }
        

    /// A field f_l is linked to cp_o.f if its type is invalidated
    /// when p.f changes.  This occurs when p.f appears in f_l's type.
    /// The rules we enforce when checking field decls. guarantee that
    /// all linked fields either (a) occur in the same class defn as f
    /// or (b) are guarded by some interval which has not yet happened.
    def linkedPaths(cp_o: ir.CanonPath, c_o: ir.ClassName, f: ir.FieldName) = {
        assert(pathHasClass(cp_o, c_o))
        
        def isDependentOn(wt: ir.WcTypeRef, p: ir.Path) = {
            lowerBoundingCts(wt).exists { wct =>
                wct.wghosts.exists(_.wp.isDependentOn(p))
            }
        }
        
        val cd = classDecl(c_o)
        val p_f = f.thisPath        
        assert(cd.reifiedFieldDecls.exists(_.isNamed(f)))
        
        // only reified fields can be linked to another field
        val rfds = cd.reifiedFieldDecls
        
        // find fields where cp_o.f appears in the type
        val rfds_maybe_linked = rfds.filter(rfd => isDependentOn(rfd.wt, p_f))
        
        // screen out those which cannot have been written yet (and whose 
        // value is therefore null, which is valid for any type)
        val subst = cp_o.thisSubst
        val rfds_linked = rfds_maybe_linked.filter { rfd =>
            !ocp_cur.exists(cp_cur =>
                hbInter(cp_cur, canonPath(subst.path(rfd.p_guard))))
        }
        
        // map to the canonical path for the field
        rfds_linked.map { rfd => subst.path(rfd.thisPath) }
    }
    
    // ___ Superintervals ___________________________________________________
    
    private def superintervalsOrSelf(cp0: ir.CanonPath) = {
        def immediateSuperintervalsOf(cp: ir.CanonPath): Set[ir.CanonPath] = {
            log.indented("immediateSuperintervalsOf(%s)", cp) {
                flow.inlineIntervalRel.values(cp.p).map(canonPath) ++ {
                    log.indented("cp0.Constructor[_].end < cp0.Constructor.end?") {
                        cp match {
                            case ir.CpClassCtor(cp0, _) => 
                                Some(fld(cp0, ir.f_objCtor))
                            case _ =>                     
                                None
                        }
                    }
                }            
            }
        }

        def iterate(stale: Set[ir.CanonPath], fresh: Set[ir.CanonPath]): Set[ir.CanonPath] = {
            log.indented(true, "iterate(fresh=%s)", fresh) {
                if(fresh.isEmpty) stale
                else {
                    val nextStale = stale ++ fresh
                    val nextFresh = fresh.flatMap(immediateSuperintervalsOf).filter(cp => !nextStale(cp))
                    iterate(nextStale, nextFresh)
                }
            }
        }
        
        log.indented(false, "superintervalsOrSelf(%s)", cp0) {
            iterate(Set(), Set(cp0))
        }
    }
    
    // ___ Happens-Before Searches __________________________________________
    
    class HbWalk(
        didNotHappen: Set[ir.Path],
        p_from: ir.Path,
        p_to: ir.Path
    ) {
        
        def doWalk(): Boolean = {
            log.indented(false, "bfs(%s,%s)", p_from, p_to) {
                visitNext(Set(p_from), Queue.Empty.enqueue(p_from))
            }
        }
        
        def depoint(p: ir.Path, f: ir.FieldName) = p.stripSuffix(f).map(canonPath)

        def isInterval(cp0: ir.CanonPath) = pathHasClass(cp0, ir.c_interval)

        def visitNext(vis: Set[ir.Path], queue0: Queue[ir.Path]): Boolean = {
            if(queue0.isEmpty)
                false
            else {
                val (p_cur, queue1) = queue0.dequeue
                log("search(%s)", p_cur)
                (p_cur == p_to) || {
                    val ps_unvisited_succ = succ(p_cur).filter(p => !vis(p))
                    visitNext(vis ++ ps_unvisited_succ, queue1.enqueue(ps_unvisited_succ))
                }                
            }
        }
        
        def succ(p: ir.Path): Set[ir.Path] = log.indented("succ(%s)", p) {
            flow.hb.values(p) ++ {
                log.indented("p0.start -> p0.end if (p0: Interval)") {                    
                    depoint(p, ir.f_start) match {
                        case Some(cp_interval) if isInterval(cp_interval) => 
                            Some(cp_interval.p.end)
                        case _ => None
                    }
                }
            } ++ {
                log.indented("crp0.Constructor[_].end -> crp0.Constructor.end") {
                    depoint(p, ir.f_end) match {
                        case Some(ir.CpClassCtor(crp0, _)) => 
                            Some(fld(crp0, ir.f_objCtor).p.end)
                        case _ => None
                    }
                }
            } ++ {
                log.indented("crp0.Constructor[L].end -> crp0.Constructor[R].start if R <: L") {
                    depoint(p, ir.f_end) match {
                        case Some(ir.CpClassCtor(crp0, c_left)) if prog.isNotInterface(c_left) => 
                            val cs_left = prog.classAndSuperclasses(c_left)
                            var cs_right = lowerBoundingCts(crp0.wt).map(_.c)
                            cs_right = cs_right -- cs_left
                            cs_right = cs_right.filter(prog.isNotInterface)
                            cs_right.map(c_right =>
                                fld(crp0, ir.ClassCtorFieldName(c_right)).p.start).toList
                                
                        case _ => List()
                    }
                }
            } ++ {
                log.indented("crp0.Constructor[_] -> crp0.start if (crp0: Interval)") {
                    depoint(p, ir.f_end) match {
                        case Some(ir.CpClassCtor(crp0, _)) if isInterval(crp0) => 
                            Some(crp0.p + ir.f_start)                            
                        case Some(ir.CpGhostField(crp0, ir.f_objCtor(), _)) if isInterval(crp0) =>
                            Some(crp0.p + ir.f_start)
                        case _ => None
                    }
                }
            } ++ {
                def happened(q: ir.Path) = {
                    // Be wary of two mutually recursive paths like:
                    // p = x.y.(? hbNow q)
                    // q = x.y.(? hbNow p)                    
                    // In this case, while searching to see if 'q' happened,
                    // we cannot assume that 'p' happened.
                    val cq = canonPath(q)
                    !didNotHappen(cq.p) &&
                    new HbWalk(didNotHappen + p, cq.p.end, p_cur.start).doWalk()
                }
        
                log.indented("cp0.(hbNow qs).end -> cur.start if (qs happened)") {
                    depoint(p, ir.f_end).map(is) match {
                        case Some(ir.WcHbNow(qs)) if qs.forall(happened) =>
                            Some(p_cur.start)
                        case Some(wp) =>                            
                            log("cp0.f == %s", wp); None
                        case None => 
                            log("Not end point"); None
                    }
                }                
            }      
        }

    }
    
    private def bfs(p_from: ir.Path, p_to: ir.Path) = {        
        new HbWalk(Set(), p_from, p_to).doWalk()
    }
    
    // ___ Other operations on canonical paths ______________________________

    // Returns an existential WcPath for 'cp' if one can be found.  
    // For example, if p has type @Owner(? readableBy inter), then is(p.Owner) would 
    // yield "? readableBy inter".  Returns cp.p if no existential version is found.
    private[this] def is(cp: ir.CanonPath): ir.WcPath = {
        log.indented(false, "is(%s)", cp) {
            cp match {
                case ir.CpGhostField(crp_base, f, _) =>
                    log("Ghost field of %s", crp_base)
                    ghost(crp_base, f).wp
                case _ =>
                    cp.p
            }
        }
    }
    
    private[this] def equiv(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = (cp.p == cq.p)
    private[this] def equiv(p: ir.Path, q: ir.Path): Boolean = {
        (p == q) || equiv(canonPath(p), canonPath(q))
    }
    
    private[this] def among(cp: ir.CanonPath, cqs: List[ir.CanonPath]) = {
        cqs.exists { cq => equiv(cp, cq) || suspends(cp, cq) }
    }
    
    /** Returns the binding for ghost field `f` on path `tcp`. This might just be `tcp.f`. */
    def ghost(crp: ir.CanonReifiedPath, f: ir.FieldName): ir.WcGhost = {
        val subst = crp.thisSubst
        val wgs_all = lowerBoundingCts(crp.wt).flatMap { wct =>
            wct.wghosts ++ ghostsOnClassAndSuperclasses(wct.c).map(subst.ghost)
        }
        wgs_all.find(_.isNamed(f)) match {
            case Some(wg) => wg
            case None => ir.Ghost(f, crp.p + f)
        }
    }
    
    /** Returns the binding for type arg `tv` on path `tcp`. This might just be `tcp:tv`. */
    def typeArg(crp: ir.CanonReifiedPath, tv: ir.TypeVarName): ir.WcTypeArg = {
        val subst = crp.thisSubst
        val wtas_all = lowerBoundingCts(crp.wt).flatMap { wct =>
            wct.wtargs ++ typeArgsOnClassAndSuperclasses(wct.c).map(subst.typeArg)
        }
        wtas_all.find(_.isNamed(tv)) match {
            case Some(ta) => ta
            case None => ir.TypeArg(tv, ir.PathType(crp.p, tv))
        }
    }    

    // ___ Operations on PathTypes __________________________________________
    
    /** Returns upper- and lower-bounds for an `ir.TeeCeePee` with path type. 
      * Note that these bounds may either reference pt itself or lead to a cycle,
      * so you should generally prefer the functions `lowerBoundingCts()` or
      * `upperBoundingCts()`. */
    private[this] def boundPathType(pt: ir.PathType): ir.TypeBounds = {
        log.indented("boundPathType(%s)", pt) {
            val crp = reifiedPath(pt.p)

            val tvd = typeVarsDeclaredOnType(crp.wt).find(_.isNamed(pt.tv)) match {
                case Some(tvd) => tvd
                case None => throw new CheckFailure("intervals.no.such.type.var", crp.wt, pt.tv)
            }
            log("tvd=%s", tvd)
            val subst = PathSubst.vp(ir.lv_this, crp.p)
            val tvd_lbs = tvd.wts_lb.map(subst.wcTref)
            log("tvd_lbs=%s", tvd_lbs)
            
            val bounds = typeArg(crp, pt.tv).bounds
            log("bounds=%s", bounds)

            ir.TypeBounds(
                wts_lb = bounds.wts_lb ++ tvd_lbs,
                wts_ub = bounds.wts_ub
            )            
        }
    }
    
    /** Accumulate a set of upper- or lower-bounding class types for `wt0`. */
    private[this] def boundingWts(
        label: String,
        func: (ir.TypeBounds => List[ir.WcTypeRef])
    )(
        wt0: ir.WcTypeRef
    ) = {
        def add(wts: Set[ir.WcTypeRef], wt: ir.WcTypeRef): Set[ir.WcTypeRef] = {
            wt match {
                case wct @ ir.WcClassType(_, _, _) => wts + wct
                case pt @ ir.PathType(_, _) if wts(pt) => wts
                case pt @ ir.PathType(_, _) => func(boundPathType(pt)).foldLeft(wts + pt)(add)
            }
        }
        log.indented("%sBoundingWts(%s)", label, wt0) {
            add(Set(), wt0)
        }
    }    
    def upperBoundingWts(wt: ir.WcTypeRef) = boundingWts("upper", _.wts_ub)(wt)
    def lowerBoundingWts(wt: ir.WcTypeRef) = boundingWts("lower", _.wts_lb)(wt)
    
    private[this] def addWcts(set: Set[ir.WcClassType], wt: ir.WcTypeRef) = wt match {
        case wct @ ir.WcClassType(_, _, _) => set + wct
        case pt @ ir.PathType(_, _) => set
    }
    def upperBoundingCts(wt: ir.WcTypeRef) = upperBoundingWts(wt).foldLeft(Set[ir.WcClassType]())(addWcts)
    def lowerBoundingCts(wt: ir.WcTypeRef) = lowerBoundingWts(wt).foldLeft(Set[ir.WcClassType]())(addWcts)
    
    // ___ Subtyping and Subclassing ________________________________________
    
    /** Is `wt` an erased subtype of class `c`? */
    def isSubclass(wt: ir.WcTypeRef, c: ir.ClassName): Boolean = {
        lowerBoundingCts(wt).exists(wct => prog.isSubclass(wct.c, c))
    }
    
    /** Is class `c` an erased subtype of `wt`? */
    def isSubclass(c: ir.ClassName, wt: ir.WcTypeRef): Boolean = {
        upperBoundingCts(wt).exists(wct => prog.isSubclass(c, wct.c))
    }
    
    /** Does `cp_sub` refer to an object whose type is a subclass of `c_sup`? */
    def pathHasClass(cp_sub: ir.CanonPath, c_sup: ir.ClassName): Boolean = {
        // Ok so the name of this function is an abuse of the english language... sue me...
        log.indented(false, "pathHasClass(%s, %s)?", cp_sub, c_sup) {
            cp_sub match {                
                case cgp: ir.CanonGhostPath => prog.isSubclass(cgp.c_cp, c_sup)
                case crp: ir.CanonReifiedPath => isSubclass(crp.wt, c_sup)
            }
        }
    }
    
    /** Does `cp_sub` refer to an object whose type could be `c_sup`? */
    def pathCouldHaveClass(cp_sub: ir.CanonPath, c_sup: ir.ClassName): Boolean = {
        log.indented(false, "pathCouldHaveClass(%s, %s)?", cp_sub, c_sup) {
            cp_sub match {                
                case cgp: ir.CanonGhostPath => 
                    prog.isSubclass(cgp.c_cp, c_sup) || prog.isSubclass(c_sup, cgp.c_cp)
                case crp: ir.CanonReifiedPath => 
                    isSubclass(crp.wt, c_sup) || isSubclass(c_sup, crp.wt)
            }
        }
    }
    
    /** Upcast `tcp` to its supertypes and return new `ir.TeeCeePee`s for them */
    def superTcps(tcp: ir.TeeCeePee[ir.WcClassType]) = log.indented("sups(%s)", tcp) {
        def ofClass(c: ir.ClassName) = {
            val cd = classDecl(c)
            cd.superClasses.map(ir.ClassType(_, cd.ghosts, cd.typeArgs))
        }

        val subst = PathSubst.vp(ir.lv_this, tcp.p)
        ofClass(tcp.ty.c).map { ct_sup =>
            tcp.withTy(
                ir.WcClassType(
                    ct_sup.c,
                    tcp.ty.wghosts ++ ct_sup.ghosts.map(subst.ghost), 
                    tcp.ty.wtargs ++ ct_sup.targs.map(subst.typeArg)
                )
            )
        }
    }
    
    private[this] def isLtCanonPaths(cps: List[ir.CanonPath], cqs: List[ir.CanonPath]) = {
        cps.forall(cp => among(cp, cqs))
    }
    
    private[this] def isLtPaths(ps: List[ir.Path], qs: List[ir.Path]) = {
        val cps = canonPaths(ps)
        val cqs = canonPaths(qs)
        isLtCanonPaths(cps, cqs)
    }
    
    private[this] def isGtPaths(ps: List[ir.Path], qs: List[ir.Path]) = isLtPaths(qs, ps)    
    
    private[this] def isLtWpath(wp: ir.WcPath, wq: ir.WcPath): Boolean = {
        log.indented("isLtWpath(%s, %s)?", wp, wq) {
            (wp, wq) match {
                case (p: ir.Path, q: ir.Path) =>
                    equiv(canonPath(p), canonPath(q))
                case (p: ir.Path, ir.WcReadableBy(qs)) =>
                    val cp = canonPath(p)
                    qs.forall { q => guardsDataReadableBy(cp, canonPath(q)) }
                case (p: ir.Path, ir.WcWritableBy(qs)) =>
                    val cp = canonPath(p)
                    qs.forall { q => guardsDataWritableBy(cp, canonPath(q)) }
                case (p: ir.Path, ir.WcHbNow(qs)) =>
                    val cp = canonPath(p)
                    hbNow(cp, canonPaths(qs))
            
                // Accessible by more is a subtype of accessible by less:
                case (ir.WcWritableBy(ps), ir.WcWritableBy(qs)) => isGtPaths(ps, qs)                
                case (ir.WcWritableBy(ps), ir.WcReadableBy(qs)) => isGtPaths(ps, qs)
                case (ir.WcReadableBy(ps), ir.WcReadableBy(qs)) => isGtPaths(ps, qs)

                // hbNow with less is a subtype of hbNow of more:
                //  ∀i:Interval. (? hbNow) => (? hbNow i)
                case (ir.WcHbNow(ps), ir.WcHbNow(qs)) => 
                    val cps = ps.map(canonPath).filter(cp => !interHappened(cp))
                    val cqs = qs.map(canonPath)
                    isLtCanonPaths(cps, cqs)
                
                case (_, _) => false
            }
        }
    }
        
    private[this] def pathHasSubWcGhost(
        tcp_sub: ir.TeeCeePee[ir.WcClassType], 
        wg_sup: ir.WcGhost
    ) = {
        log.indented("pathHasSubWcGhost(%s, %s)?", tcp_sub, wg_sup) {
            val wg_sub = ghost(tcp_sub.cp, wg_sup.f)
            isLtWpath(wg_sub.wp, wg_sup.wp)
        }
    }
    
    private[this] def isLtBounds(
        bounds_sub: ir.TypeBounds, 
        bounds_sup: ir.TypeBounds
    ) = {
        log.indented("isLtBounds(%s, %s)", bounds_sub, bounds_sup) {
            (bounds_sub, bounds_sup) match {
                case (ir.TypeBounds(lbs_sub, ubs_sub), ir.TypeBounds(lbs_sup, ubs_sup)) =>
                    lbs_sup.forall(lb_sup =>
                        lbs_sub.exists(lb_sub =>
                            isSubtype(lb_sub, lb_sup))) &&
                    ubs_sup.forall(ub_sup =>
                        ubs_sub.exists(ub_sub =>
                            isSubtype(ub_sup, ub_sub)))
            }            
        }
    }
    
    private[this] def pathHasSubWcTarg(
        tcp_sub: ir.TeeCeePee[ir.WcClassType], 
        wtarg_sup: ir.WcTypeArg
    ) = {
        log.indented("pathHasSubWcTarg(%s, %s)?", tcp_sub, wtarg_sup) {
            val tv = wtarg_sup.tv
            val wtarg_sub = typeArg(tcp_sub.cp, tv)
            isLtBounds(wtarg_sub.bounds, wtarg_sup.bounds)
        }            
    }
    
    private[this] def pathHasSubClassType(
        tcp_sub: ir.TeeCeePee[ir.WcClassType], 
        wct_sup: ir.WcClassType
    ): Boolean = {
        log.indented("pathHasSubClassType(%s, %s)?", tcp_sub, wct_sup) {
            if(tcp_sub.ty.c != wct_sup.c) {
                superTcps(tcp_sub).exists(pathHasSubClassType(_, wct_sup))
            } else {
                wct_sup.wghosts.forall(pathHasSubWcGhost(tcp_sub, _)) &&
                wct_sup.wtargs.forall(pathHasSubWcTarg(tcp_sub, _))
            }
        }
    }
    
    /** True if the (typed) path `tcp_sub` refers to a subtype of `wt_sup` */
    def pathHasType(
        tcp_sub: ir.TeeCeePee[ir.WcTypeRef], 
        wt_sup: ir.WcTypeRef
    ): Boolean = {
        log.indented(false, "pathHasType(%s, %s)?", tcp_sub, wt_sup) {
            val wts_sub = lowerBoundingWts(tcp_sub.ty)
            val wts_sup = upperBoundingWts(wt_sup)

            existscross(wts_sub, wts_sup) { 
                case (pt_sub: ir.PathType, pt_sup: ir.PathType) =>
                    (pt_sub.tv == pt_sup.tv) && equiv(pt_sub.p, pt_sup.p)
                    
                case (wct_sub: ir.WcClassType, wct_sup: ir.WcClassType) =>
                    pathHasSubClassType(tcp_sub.withTy(wct_sub), wct_sup)
                    
                case (_, _) => false
            }                 
        }
    }
    
    /** Is `wt_sub` a subtype of `wt_sup`? */
    def isSubtype(
        wt_sub: ir.WcTypeRef, 
        wt_sup: ir.WcTypeRef
    ): Boolean = {
        log.indented(false, "isSubtype(%s, %s)?", wt_sub, wt_sup) {
            val lv = prog.freshVarName
            val env = addReifiedLocal(lv, wt_sub)
            env.pathHasType(env.reifiedLv(lv).toTcp, wt_sup)
        }
    }
    
}