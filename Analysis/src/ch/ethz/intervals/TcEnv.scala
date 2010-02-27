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
    def addReifiedLocal(x: ir.VarName, wt: ir.WcTypeRef, wps_is: List[ir.WcPath] = Nil) = {
        addPerm(x, ir.CanonPath(x.path, List(ir.CpcReifiedLv(x, wt, wps_is))))
    }
    
    /** Define a local variable according to the given decl */
    def addArg(arg: ir.LvDecl) = addReifiedLocal(arg.name, arg.wt, arg.wps_is)
    
    /** Define local variables according to the given decls */
    def addArgs(args: List[ir.LvDecl]) = args.foldLeft(this)(_ addArg _)
    
    /** Define a ghost local variable `x` with type `wt` */
    def addGhostLocal(lv: ir.VarName, c: ir.ClassName) = {
        addPerm(lv, ir.CanonPath(lv.path, List(ir.CpcGhost(lv.path, List(), c))))
    }
    
    /** Defines a fresh ghost variable of type `wt` */
    def freshCp(c: ir.ClassName): (ir.CanonPath, TcEnv) = {
        val lv = prog.freshVarName
        val env1 = addGhostLocal(lv, c)
        (env1.perm(lv), env1)
    }
        
    /** Defines a fresh reified variable of type `wt` */
    def freshCp(wt: ir.WcTypeRef): (ir.CanonPath, TcEnv) = {
        val lv = prog.freshVarName
        val env1 = addReifiedLocal(lv, wt)
        (env1.reifiedLv(lv), env1)
    }
        
    /** Temporarily redirect from `p` to `q` */
    def addTemp(p: ir.Path, q: ir.Path): TcEnv = {
        log("addTemp(%s,%s)", p, q)
        copy(flow = flow.copy(temp = flow.temp + (p -> q)))
    }
    
    /** Clear all temporary redirects */
    def clearTemp(): TcEnv = {
        log("clearTemp")
        copy(flow = flow.copy(temp = Map()))
    }
    
    def addInvalidated(p: ir.Path) = {
        log("addInvalidated(%s)", p)
        copy(flow = flow.copy(ps_invalidated = flow.ps_invalidated + p))        
    }
        
    def removeInvalidated(p: ir.Path) = {
        log("removeInvalidated(%s)", p)
        copy(flow = flow.copy(ps_invalidated = flow.ps_invalidated - p))        
    }
    
    /// Indicates that point cp hb point cq.
    def addHbPnt(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addHbPnt(%s,%s)", cp, cq)
        assert(isImmutable(cp) && isImmutable(cq))
        assert(pathCouldHaveClass(cp, ir.c_point))
        assert(pathCouldHaveClass(cq, ir.c_point))
        copy(flow = flow.copy(hbRel = 
            (cp.paths cross cq.paths).foldLeft(flow.hbRel) { case (hb, (p, q)) =>
                hb + (p -> q)
            }
        ))
    }
    
    /// Indicates that interval cp hb interval cq.
    def addHbInter(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addHbInter(%s,%s)", cp, cq)
        assert(isImmutable(cp) && isImmutable(cq))
        assert(pathCouldHaveClass(cp, ir.c_interval))
        assert(pathCouldHaveClass(cq, ir.c_interval))
        copy(flow = flow.copy(hbRel = 
            (cp.paths cross cq.paths).foldLeft(flow.hbRel)(_ + _)))
    }
    
    def addDeclaredReadableBy(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addDeclaredReadableBy(%s, %s)", cp, cq)
        assert(isImmutable(cp) && isImmutable(cq))
        assert(pathCouldHaveClass(cp, ir.c_guard))
        assert(pathCouldHaveClass(cq, ir.c_interval))
        copy(flow = flow.copy(readableRel = 
            (cp.paths cross cq.paths).foldLeft(flow.readableRel)(_ + _)))
    }
    
    def addDeclaredWritableBy(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log("addDeclaredWritableBy(%s, %s)", cp, cq)
        assert(isImmutable(cp) && isImmutable(cq))
        assert(pathCouldHaveClass(cp, ir.c_guard))
        assert(pathCouldHaveClass(cq, ir.c_interval))
        copy(flow = flow.copy(writableRel = 
            (cp.paths cross cq.paths).foldLeft(flow.writableRel)(_ + _)))
    }
    
    def addSuspends(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented(false, "addSuspends(%s, %s)", cp, cq) {
            // This assertion is not valid during checkReifiedFieldDecl():
            //assert(isImmutable(cp) && isImmutable(cq))
            assert(pathCouldHaveClass(cp, ir.c_interval))
            assert(pathCouldHaveClass(cq, ir.c_interval))
            copy(flow = flow.copy(inlineRel =
                (cp.paths cross cq.paths).foldLeft(flow.inlineRel)(_ + _)))
        }        
    }

    def addLocks(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented(false, "addLocks(%s, %s)", cp, cq) {
            // This assertion is not valid during checkReifiedFieldDecl:
            //assert(isImmutable(cp) && isImmutable(cq))
            assert(pathCouldHaveClass(cp, ir.c_interval))
            assert(pathCouldHaveClass(cq, ir.c_lock))
            copy(flow = flow.copy(locksRel =
                (cp.paths cross cq.paths).foldLeft(flow.locksRel)(_ + _)))
        }        
    }

    /** Converts cp_from and cp_to to points that can happen before one another. */
    private def userHbPair(cp_from: ir.CanonPath, cp_to: ir.CanonPath): (List[ir.Path], List[ir.Path]) = {
        log.indented(false, "userHbPair(%s,%s)", cp_from, cp_to) {
            def make(cp: ir.CanonPath, f: ir.FieldName) = {
                if(pathHasClass(cp, ir.c_point)) cp.paths
                else if(pathHasClass(cp, ir.c_interval)) cp.paths.map(_ + f)
                else List()
            }

            (make(cp_from, ir.f_end), make(cp_to, ir.f_start))
        }        
    }
        
    def addUserHb(cp0: ir.CanonPath, cq0: ir.CanonPath) = {
        log.indented(false, "addUserHb(%s,%s)", cp0, cq0) {
            assert(isImmutable(cp0))
            assert(isImmutable(cq0))
            val (ps, qs) = userHbPair(cp0, cq0) 
            copy(flow = flow.copy(hbRel = 
                (ps cross qs).foldLeft(flow.hbRel)(_ + _)))
        }        
    }
        
    def addNonNull(cp: ir.CanonPath) = {
        log.indented(false, "addNonNull(%s)", cp) {
            copy(flow = flow.copy(nonnull = flow.nonnull ++ cp.paths))            
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
                    (cps cross cqs).foldLeft(this) { case (env, (cp, cq)) =>
                        add(env, cp, cq)
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
    
    def reifiedPath(p: ir.Path): ir.CanonPath = {
        val cp = canonPath(p) 
        if(!cp.isReified)
            throw new CheckFailure("intervals.must.be.reified", p)
        cp
    }
    def reifiedPaths(ps: List[ir.Path]) = ps.map(reifiedPath)
    def reifiedLv(lv: ir.VarName) = reifiedPath(lv.path)
    def reifiedLvs(lvs: List[ir.VarName]) = lvs.map(reifiedLv)
    
    def immutableReifiedPath(p: ir.Path): ir.CanonPath = {
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
    
    private[this] def canonicalize(
        ps_visited0: Set[ir.Path],
        p_request: ir.Path
    ): ir.CanonPath = log.indented("canonicalize(%s,%s)", ps_visited0, p_request) {
        assert(!ps_visited(p_request))
        val ps_visited = ps_visited0 + p_request
        
        (flow.temp.get(p_request), p_request) match {
            case (Some(p_redirect), _) if !ps_visited(p_redirect) =>
                canonicalize(ps_visited, p_redirect)
                
            case (_, ir.Path(lv, List())) =>
                perm.get(lv) match {
                    case Some(cp) => cp
                    case None => throw new CheckFailure("intervals.no.such.variable", lv)
                }
            
            case (_, ir.Path(lv, f :: rev_fs)) =>
                val p_base = ir.Path(lv, rev_fs)
                val cp = canonicalize(ps_visited, p_base)
                extendCanonWithFieldNamed(p_request, ps_visited, cp, f)
        }
    }
    
    private[this] def fld(cp_base: ir.CanonPath, fs: ir.FieldName*) = {
        log.indented(false, "fld(%s, %s)", cp_base, fs) {
            fs.foldLeft(cp_base) { case (cp, f) =>
                extendCanonWithFieldNamed(cp.p + f, cp.paths.toSet, cp, f)
            }
        }
    }
    
    private[this] def extendCanonWithFieldNamed(
        p_request: ir.Path,
        ps_visited0: Set[ir.Path],
        cp_base: ir.CanonPath, 
        f: ir.FieldName
    ) = {        
        def iterate(comps_in: Set[ir.CanonPathComponent]): Set[ir.CanonPathComponent] = {
            val ps_visited = ps_visited0 ++ comps_in.map(_.p)
            
            val ps_freshRedirects = comps.flatMap(_.wps_is.flatMap {
                case p: ir.Path if !ps_visited(p) => Some(p)
                case _ => None
            })
            
            if(ps_freshRedirects.isEmpty) comps_in
            else iterate(
                ps_freshRedirects.foldLeft(comps_in) { case (comps, p) =>
                    comps ++ canonicalize(ps_visited, p).components                    
                }
            )
        }
        
        val comps = cp_base.components.flatMap { comp_base =>
            f match {
                case ir.ClassCtorFieldName(_) | ir.f_objCtor() =>
                    Some(extendComponentWithGhostField(comp_base, f, ir.c_interval))

                case _ =>
                    ghostFieldsDeclaredOnComponent(comp_base).find(_.isNamed(f)) match {
                        case Some(ir.GhostFieldDecl(f_gfd, c_gfd)) =>
                            Some(extendComponentWithGhostField(comp_base, f_gfd, c_gfd))

                        case None =>
                            optSubstdReifiedFieldDeclOfComp(comp_base, f).map { 
                                case (_, rfd) =>
                                    extendComponentWithReifiedField(comp_base, rfd)
                            }
                    }
            }            
        }
        ir.CanonPath(p_request, iterate(comps.toSet).toList)
    }
    
    private[this] def extendComponentWithGhostField(
        comp_base: ir.CanonPathComponent,
        f_gf: ir.FieldName,
        c_gf: ir.ClassName
    ) = {
        log.indented("extendComponentWithGhostField(%s, %s, %s)", comp_base, f_gf, c_gf) {
            val p_gf = comp_base.p + f_gf
            val wp_gf = compGhost(comp_base, f_gf)
            ir.CpcGhost(p_gf, List(wp_gf), c_gf)
        }
    }
    
    private[this] def extendComponentWithReifiedField(
        comp_base: ir.CanonPathComponent,
        rfd: ir.ReifiedFieldDecl
    ) = {
        log("extendComponentWithReifiedField(%s, %s)", comp_base, rfd)
        ir.CpcReifiedField(comp_base.p + rfd.name, rfd)
    }
    
    // ___ Locating Members _________________________________________________

    /** Given a type, returns all GhostFieldDecls applied on it or its supertypes. */
    def ghostFieldsDeclaredOnType(wt: ir.WcTypeRef): Set[ir.GhostFieldDecl] = {
        lowerBoundingCts(wt).flatMap(wct => ghostFieldsDeclaredOnClassAndSuperclasses(wct.c))
    }
    
    /** Given a path, returns all GhostFieldDecls found on its type(s). */
    def ghostFieldsDeclaredOnComponent(comp: ir.CanonPathComponent): Set[ir.GhostFieldDecl] = {
        comp match {
            case ir.CpcReified(_, wt) => ghostFieldsDeclaredOnType(wt)
            case ir.CpcGhost(_, _, c) => ghostFieldsDeclaredOnClassAndSuperclasses(c)
        }
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
    
    /** Returns the class and field decl for `tcp.f`, or `None` if no such field. */
    def optSubstdReifiedFieldDeclOfComp(comp: ir.CanonPathComponent, f: ir.FieldName) = {
        def search(wct: ir.WcClassType) = 
            searchClassAndSuperclasses(_.reifiedFieldDecls.find(_.isNamed(f)))(wct.c)
        log.indented(false, "substdReifiedFieldDecl(%s::%s)", comp, f) {
            comp match {
                case ir.CpcReified(_, wt) =>
                    log("Reified Component of type: %s", wt)
                    lowerBoundingCts(wt).firstSomeReturned(search).map { 
                        case (c_fd, fd) =>
                            (c_fd, comp.thisSubst.reifiedFieldDecl(fd))
                    }
                    
                case ir.CpcGhost(_, _, _) => 
                    log("Ignoring Ghost Component")
                    None
            }
        }
    }
    
    /** Returns the class and field decl for `tcp.f`, error if no such field. */
    def substdReifiedFieldDecl(cp: ir.CanonPath, f: ir.FieldName) = {
        log.indented(false, "substdReifiedFieldDecl(%s::%s)", cp, f) {
            cp.components.firstSomeReturned(optSubstdReifiedFieldDeclOfComp(_, f)).getOrElse {
                throw new CheckFailure("intervals.no.such.field", cp.p, f)
            }
        }
    }

    /** Method sig for `tcp.m(tqs)`.  Substitutes `tcp` for the receiver, 
      * `tqs` for the arguments, and the current interval for `method`. 
      * Returns `None` if no such method. */
    def optSubstdMethodSigOfTcp(tcp: ir.TeeCeePee[ir.WcTypeRef], m: ir.MethodName, cqs: List[ir.CanonPath]) = {
        def search(wct: ir.WcClassType) = 
            searchClassAndSuperclasses(_.methods.find(_.isNamed(m)))(wct.c)
        log.indented(false, "substdMethodSig(%s::%s)", tcp.ty, m) {
            lowerBoundingCts(tcp.ty).firstSomeReturned(search).map { 
                case (_, md) =>
                    log.methodDecl("md: ", md)
                    PathSubst.vp(
                        ir.lv_mthd  :: ir.lv_this :: md.args.map(_.name),
                        p_cur       :: tcp.p      :: cqs.map(_.p)
                    ).methodSig(md.msig)
            }
        }
    }
    
    /** Method sig for `tcp.m(tqs)`.  Substitutes `tcp` for the receiver, 
      * `tqs` for the arguments, and the current interval for `method`. 
      * Error if no such method. */
    def substdMethodSigOfTcp(tcp: ir.TeeCeePee[ir.WcTypeRef], m: ir.MethodName, cqs: List[ir.CanonPath]) = {
        optSubstdMethodSigOfTcp(tcp, m, cqs).getOrElse {
            throw new CheckFailure("intervals.no.such.method", tcp.ty, m) 
        }
    }

    def substdMethodSigOfCp(cp: ir.CanonPath, m: ir.MethodName, cqs: List[ir.CanonPath]) = {
        val res = cp.reifiedComponents.firstSomeReturned(comp =>
            optSubstdMethodSigOfTcp(ir.TeeCeePee(cp, comp.wt), m, cqs))
        res.getOrElse { 
            throw new CheckFailure("intervals.no.such.method", cp.p, m) 
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
    
    /** Canonical path of current interval (must be defined) */
    def cp_cur = ocp_cur.get
    
    /** Path of current interval (must be defined) */
    def p_cur = cp_cur.p
    
    /** Canonical path to `this` */
    def cp_this = reifiedLv(ir.lv_this)
    
    /** TeeCeePee of `this` */
    def tcp_this = ir.TeeCeePee(cp_this, ct_this)

    /** ClassType of `this` */
    def ct_this = c_this.ct
    
    /** Canonical path to `method` */
    def cp_mthd = canonLv(ir.lv_mthd)
    
    /** Upcast version of this to the first superclass */
    def tcp_super = superTcps(tcp_this) match {
        case List() => throw new CheckFailure("intervals.no.supertype", ct_this)
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
            val (ps, qs) = userHbPair(cp0, cq0)
            (ps cross qs).existsPair(bfs)
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
            (cp_from.paths cross cp_to.paths).existsPair(bfs)
        }
    }
    
    /// Does the interval cp hb the interval cq?
    def hbInter(cp_from: ir.CanonPath, cp_to: ir.CanonPath) = {
        log.indented("hbInter(%s, %s)?", cp_from, cp_to) {
            log.env(false, "Environment", this)
            ( // Sometimes we're sloppy and invoke with wrong types:
                pathHasClass(cp_from, ir.c_interval) && 
                pathHasClass(cp_to, ir.c_interval) &&
                (cp_from.paths cross cp_to.paths).existsPair((p_from, p_to) =>
                    bfs(p_from.end, p_to.start))                
            )
        }
    }
    
    def suspends(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented(false, "suspends(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)

            inlineSuperintervalsOf(cp).contains(cq)
        }
    }
    
    def locks(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented(false, "locks(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)
            
            inlineSuperintervalsOf(cp).exists { cp_sup => flow.locks((cp_sup.p, cq.p)) }
        }
    }
    
    private[this] def immediateIsWritableBy(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = {
        log.indented(false, "immediateIsWritableBy(%s, %s)?", cp, cq) {
            (
                is(cp) match {
                    // Is cp a ghost declared writable by q?
                    case ir.WcWritableBy(qs) => among(cq, canonPaths(qs))
                    case _ => false
                }
            ) || {
                flow.writable((cp.p, cq.p)) ||
                equiv(cp, cq) ||
                locks(cq, cp) ||
                suspends(cq, cp)
            }
        }
    }
    
    private[this] def immediateIsReadableBy(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = {
        log.indented(false, "immediateIsReadableBy(%s, %s)?", cp, cq) {
            (
                is(cp) match {
                    // Is cp a ghost declared readable by q or immutable in q?
                    case ir.WcReadableBy(qs) => among(cq, canonPaths(qs))
                    case _ => false
                } 
            ) || {
                // n.b.: This is safe because when cq is active, all superintervals are
                // suspended.  Therefore, even if a superinterval could write, it is
                // safe for us to read.  We could generalize this more to include 
                // any interval X where cq.start -> X.start, X.end -> cq.end, and
                // cp is immutable in X.
                flow.readable((cp.p, cq.p)) ||
                hbInter(cp, cq) ||
                immediateIsWritableBy(cp, cq)            
            }  
        }  
    }
        
    /** Is data guarded by `cp` writable by the interval `cq`? */
    def isWritableBy(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = {
        log.indented(false, "isWritableBy(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)
            inlineSuperintervalsOf(cq).exists(cq_sup => immediateIsWritableBy(cp, cq_sup))
        }
    }    
    
    /** Is data guarded by `cp` readable by the interval `cq`? */
    def isReadableBy(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = {        
        log.indented(false, "isReadableBy(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)
            superintervalsOf(cq).exists(cq_sup => immediateIsReadableBy(cp, cq_sup))
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
    
    // ___ Superintervals and suspended intervals ___________________________
    
    private[this] def immediateInlineSuperintervalsOf(cp: ir.CanonPath) = {
        log.indented("immediateInlineSuperintervalsOf(%s)", cp) {
            cp.paths.toSet.flatMap(flow.inlineRel).map(canonPath) ++ {
                log.indented("cp0.Constructor[_] < cp0.Constructor?") {
                    cp match {
                        case ir.CpClassCtor(cp0, _) => Some(fld(cp0, ir.f_objCtor))
                        case _ => None
                    }
                }
            }            
        }
    }
    
    private[this] def inlineSuperintervalsOf(cp: ir.CanonPath) = {
        log.indented(false, "inlineSuperintervalsOf(%s)", cp) {
            computeTransitiveClosure(immediateInlineSuperintervalsOf, Set(cp))
        }
    }
    
    private[this] def immediateSuperintervalsOf(cp: ir.CanonPath) = {
        log.indented("immediateSuperintervalsOf(%s)", cp) {
            immediateInlineSuperintervalsOf(cp) ++ (cp match {
                case crp: ir.CanonReifiedPath => Set(fld(crp, ir.f_parent))
                case _: ir.CanonGhostPath => Set()                    
            })
        }
    }

    private[this] def superintervalsOf(cp0: ir.CanonPath) = {
        log.indented(false, "superintervalsOf(%s)", cp) {
            computeTransitiveClosure(immediateSuperintervalsOf, Set(cp0))
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
                log.indented("X.start -> X.end if (X: Interval)") {                    
                    depoint(p, ir.f_start) match {
                        case Some(cp_inter) if isInterval(cp_inter) => 
                            Some(cp_inter.p.end)
                        case _ => None
                    }
                }
            } ++ {
                log.indented("X.end -> X.Parent.end if (X: Interval)") {
                    depoint(p, ir.f_end) match {
                        case Some(crp_inter: ir.CanonReifiedPath) if isInterval(crp_inter) =>
                            Some(fld(crp_inter, ir.f_parent).p.end)
                        case _ => None
                    }
                }
            } ++ {
                log.indented("X.Constructor[_].end -> X.Constructor.end") {
                    depoint(p, ir.f_end) match {
                        case Some(ir.CpClassCtor(crp0, _)) => 
                            Some(fld(crp0, ir.f_objCtor).p.end)
                        case _ => None
                    }
                }
            } ++ {
                log.indented("X.Constructor[L].end -> X.Constructor[R].start if R <: L") {
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
                log.indented("X.Constructor[_] -> X.start if (X: Interval)") {
                    depoint(p, ir.f_end) match {
                        case Some(ir.CpClassCtor(crp0, _)) if isInterval(crp0) => 
                            Some(crp0.p + ir.f_start)                            
                        case Some(ir.CpGhostField(crp0, ir.f_objCtor(), _)) if isInterval(crp0) =>
                            Some(crp0.p + ir.f_start)
                        case _ => None
                    }
                }
            } ++ {
                log.indented("X.(hbNow qs).end -> cur.start if (qs happened)") {
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
    
    private[this] def equiv(cp0: ir.CanonPath, cq0: ir.CanonPath): Boolean = {
        cp0 equiv cq0
    }
    
    private[this] def equiv(p: ir.Path, q: ir.Path): Boolean = {
        (p == q) || equiv(canonPath(p), canonPath(q))
    }
    
    private[this] def among(cp: ir.CanonPath, cqs: List[ir.CanonPath]) = {
        cqs.exists { cq => equiv(cp, cq) || suspends(cp, cq) }
    }
    
    /** Returns the binding for ghost field `f` on `comp`. */
    private[this] def compGhost(
        comp: ir.CanonPathComponent, 
        f: ir.FieldName
    ): ir.WcPath = {
        val subst = crp.thisSubst
        val wgs_all = comp match {
            case ir.CpcReified(_, wt) =>
                lowerBoundingCts(wt).flatMap { wct =>
                    wct.wghosts ++ ghostsOnClassAndSuperclasses(wct.c).map(subst.ghost)
                }
                
            case ir.CpcGhost(_, _, c) =>
                ghostsOnClassAndSuperclasses(c).map(subst.ghost)
        }
        wgs_all.find(_.isNamed(f)) match {
            case Some(wg) => wg.wp
            case None => crp.p + f
        }
    }
    
    /** Returns the binding for type arg `tv` on `comp`. */
    private[this] def compTypeArg(
        comp: ir.CanonPathComponent, 
        tv: ir.TypeVarName
    ): ir.WcTypeArg = {
        comp match {
            case ir.CpcReified(_, wt) =>
                val subst = comp.thisSubst
                val wtas_all = lowerBoundingCts(wt).flatMap { wct =>
                    wct.wtargs ++ typeArgsOnClassAndSuperclasses(wct.c).map(subst.typeArg)
                }
                wtas_all.find(_.isNamed(tv)) match {
                    case Some(ta) => ta
                    case None => ir.TypeArg(tv, ir.PathType(comp.p, tv))
                }
                
            case _ =>
                ir.TypeArg(tv, ir.PathType(comp.p, tv))
        }
    }
    
    private[this] def typeArgBounds(cp: ir.CanonPath, tv: ir.TypeVarName) = {
        val wtas = cp.components.map(compTypeArg(_, tv)).toSet
        ir.TypeBounds(
            wts_lb = wtas.flatMap(_.bounds.wts_lb).toList,
            wts_ub = wtas.flatMap(_.bounds.wts_ub).toList
        )
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
            
            val bounds = typeArgBounds(crp, pt.tv)
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
    
    private[this] def isLtWpath(cp: ir.CanonPath, wq: ir.WcPath): Boolean = {
        log.indented("isLtWpath(%s, %s)?", cp, wq) {
            wq match {
                case q: ir.Path =>
                    equiv(cp, canonPath(q))
                case ir.WcReadableBy(qs) =>
                    qs.forall { q => isReadableBy(cp, canonPath(q)) }
                case ir.WcWritableBy(qs) =>
                    qs.forall { q => isWritableBy(cp, canonPath(q)) }
                case ir.WcHbNow(qs) =>
                    hbNow(cp, canonPaths(qs))
            }
        }
    }
        
    private[this] def pathHasSubWcGhost(
        cp_sub: ir.CanonPath,
        wg_sup: ir.WcGhost
    ) = {
        log.indented("pathHasSubWcGhost(%s, %s)?", tcp_sub, wg_sup) {
            val cp_f = fld(cp_sub, wg_sup.f)
            isLtWpath(cp_f, wg_sup.wp)
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
        cp_sub: ir.CanonPath,
        wtarg_sup: ir.WcTypeArg
    ) = {
        log.indented("pathHasSubWcTarg(%s, %s)?", tcp_sub, wtarg_sup) {
            val tv = wtarg_sup.tv
            val bounds_sub = typeArgBounds(cp_sub, tv)
            isLtBounds(bounds_sub, wtarg_sup.bounds)
        }
    }
    
    /** True if the (typed) path `tcp_sub` refers to a subtype of `wt_sup` */
    def pathHasType(
        cp_sub: ir.CanonPath,
        wt_sup: ir.WcTypeRef
    ): Boolean = {
        log.indented(false, "pathHasType(%s, %s)?", cp_sub, wt_sup) {
            cp_sub.reifiedComponents.exists { comp =>
                log.indented("comp=%s") {
                    val wts_sub = lowerBoundingWts(comp.wt)
                    val wts_sup = upperBoundingWts(wt_sup)

                    (wts_sub cross wts_sup).exists {
                        case (pt_sub: ir.PathType, pt_sup: ir.PathType) =>
                            (pt_sub.tv == pt_sup.tv) && equiv(pt_sub.p, pt_sup.p)

                        case (wct_sub: ir.WcClassType, wct_sup: ir.WcClassType) =>
                            prog.isSubclass(wct_sub.c, wct_sup.c) &&
                            wct_sup.wghosts.forall(pathHasSubWcGhost(tcp_sub, _)) &&
                            wct_sup.wtargs.forall(pathHasSubWcTarg(tcp_sub, _))

                        case (_, _) => false

                    }                    
                }
            }
        }
    }
    
    /** Is `wt_sub` a subtype of `wt_sup`? */
    def isSubtype(
        wt_sub: ir.WcTypeRef, 
        wt_sup: ir.WcTypeRef
    ): Boolean = {
        log.indented(false, "isSubtype(%s, %s)?", wt_sub, wt_sup) {
            val (cp, env) = freshCp(wt_sub)
            env.pathHasType(cp, wt_sup)
        }
    }
    
}