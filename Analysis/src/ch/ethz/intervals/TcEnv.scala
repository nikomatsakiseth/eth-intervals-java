package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.collection.immutable.Map
import scala.collection.immutable.Queue

import Util._
import ir./

sealed case class TcEnv(
    prog: Prog,
    c_this: ir.ClassName,
    o_lv_cur: Option[ir.VarName],                 // current interval (if any)
    wt_ret: ir.WcTypeRef,                         // return type of current method
    identityRet: List[ir.WcPath],                 // identity
    perm: Map[ir.VarName, ir.ImmutableCanonPath], // permanent equivalences, hold for duration of method
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
    
    override def toString = "Environment(...)"
    
    // ___ Merging Flows ____________________________________________________
    
    def addFlow(flow1: FlowEnv) = copy(flow = flow + flow1)        
    def intersectFlow(flow1: FlowEnv) = copy(flow = flow & flow1)
        
    // ___ Modifying the Environment ________________________________________
    
    def withReturn(
        wt_ret: ir.WcTypeRef,
        identityRet: List[ir.WcPath]
    ) = copy(wt_ret = wt_ret, identityRet = identityRet)
    
    def withCurrent(lv_cur: ir.VarName) = copy(o_lv_cur = Some(lv_cur))

    /** Set `c_this`, adding `this` as a local variable. */
    def withThisClass(c_this: ir.ClassName) = addReifiedLocal(ir.lv_this, c_this.ct).copy(c_this = c_this)
    
    /** Add a local variable whose value is the canon path `cp` */
    def addPerm(x: ir.VarName, cp: ir.ImmutableCanonPath): TcEnv = {
        perm.get(x) match {
            case Some(_) => throw new CheckFailure("intervals.shadowed", x)
            case None => copy(perm = perm + (x -> cp))
        }
    }
    
    def addReifiedLocal(x: ir.VarName, wt: ir.WcTypeRef) = {
        addPerm(x, ir.ImmutableCanonPath(List(ir.CpcReifiedLv(x, wt, List()))))
    }
    
    def addReifiedLocal(lv: ir.VarName, wt: ir.WcTypeRef, wps_identity: List[ir.WcPath]) = {
        val comp = ir.CpcReifiedLv(lv, wt, wps_identity)
        val cp = expandComponents(Set(), Set(comp))        
        addPerm(lv, toImmutable(cp))
    }
    
    def addArg(arg: ir.LvDecl) = addReifiedLocal(arg.name, arg.wt, arg.wps_identity)
    
    def addArgs(args: List[ir.LvDecl]) = {
        args.foldLeft(this)(_ addArg _)
    }
    
    def addGhostLocal(lv: ir.VarName, c: ir.ClassName) = {
        addPerm(lv, ir.ImmutableCanonPath(List(ir.CpcGhostLv(lv, c, List()))))
    }
    
    /** Defines a fresh ghost variable of class `c` */
    def freshCp(c: ir.ClassName): (ir.VarName, ir.ImmutableCanonPath, TcEnv) = {
        val lv = prog.freshVarName
        val env1 = addGhostLocal(lv, c)
        (lv, env1.perm(lv), env1)
    }
        
    /** Defines a fresh reified variable of type `wt` */
    def freshCp(wt: ir.WcTypeRef): (ir.VarName, ir.ImmutableCanonPath, TcEnv) = {
        val lv = prog.freshVarName
        val env1 = addReifiedLocal(lv, wt)
        (lv, env1.perm(lv), env1)
    }
        
    /** Temporarily redirect from `cp` to `cq` */
    def addTemp(cp: ir.CanonPath, cq: ir.CanonPath): TcEnv = {
        log("addTemp(%s,%s)", cp, cq)
        copy(flow = flow.copy(temp = 
            flow.temp ++ (cp.paths cross cq.paths)))
    }
    
    /** Clear all temporary redirects */
    def clearTemp(): TcEnv = {
        log("clearTemp")
        copy(flow = flow.copy(temp = Map()))
    }
    
    def addInvalidated(cp: ir.CanonPath) = {
        log("addInvalidated(%s)", cp)
        copy(flow = flow.copy(ps_invalidated = flow.ps_invalidated ++ cp.paths))        
    }
        
    def removeInvalidated(cp: ir.CanonPath) = {
        log("removeInvalidated(%s)", cp)
        copy(flow = flow.copy(ps_invalidated = flow.ps_invalidated -- cp.paths))        
    }
    
    /// Indicates that point cp hb point cq.
    def addHbPnt(cp: ir.ImmutableCanonPath, cq: ir.ImmutableCanonPath) = {
        log("addHbPnt(%s,%s)", cp, cq)
        assert(pathCouldHaveClass(cp, ir.c_point))
        assert(pathCouldHaveClass(cq, ir.c_point))
        copy(flow = flow.copy(hbRel = 
            (cp.paths cross cq.paths).foldLeft(flow.hbRel) { case (hb, (p, q)) =>
                hb + (p -> q)
            }
        ))
    }
    
    /// Indicates that interval cp hb interval cq.
    def addHbInter(cp: ir.ImmutableCanonPath, cq: ir.ImmutableCanonPath) = {
        log("addHbInter(%s,%s)", cp, cq)
        assert(pathCouldHaveClass(cp, ir.c_interval))
        assert(pathCouldHaveClass(cq, ir.c_interval))
        copy(flow = flow.copy(hbRel = 
            (cp.paths cross cq.paths).foldLeft(flow.hbRel) { case (hb, (p, q)) =>
                hb + (p.end -> q.start)
            }
        ))
    }
    
    def addDeclaredReadableBy(cp: ir.ImmutableCanonPath, cq: ir.ImmutableCanonPath) = {
        log("addDeclaredReadableBy(%s, %s)", cp, cq)
        assert(pathCouldHaveClass(cp, ir.c_guard))
        assert(pathCouldHaveClass(cq, ir.c_interval))
        copy(flow = flow.copy(readableRel = 
            (cp.paths cross cq.paths).foldLeft(flow.readableRel)(_ + _)))
    }
    
    def addDeclaredWritableBy(cp: ir.ImmutableCanonPath, cq: ir.ImmutableCanonPath) = {
        log("addDeclaredWritableBy(%s, %s)", cp, cq)
        assert(pathCouldHaveClass(cp, ir.c_guard))
        assert(pathCouldHaveClass(cq, ir.c_interval))
        copy(flow = flow.copy(writableRel = 
            (cp.paths cross cq.paths).foldLeft(flow.writableRel)(_ + _)))
    }
    
    def addSuspends(cp: ir.ImmutableCanonPath, cq: ir.ImmutableCanonPath) = {
        log.indented(false, "addSuspends(%s, %s)", cp, cq) {
            assert(pathCouldHaveClass(cp, ir.c_interval))
            assert(pathCouldHaveClass(cq, ir.c_interval))
            copy(flow = flow.copy(inlineRel =
                (cp.paths cross cq.paths).foldLeft(flow.inlineRel)(_ + _)))
        }        
    }

    def addLocks(cp: ir.ImmutableCanonPath, cq: ir.ImmutableCanonPath) = {
        log.indented(false, "addLocks(%s, %s)", cp, cq) {
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
                else if(pathHasClass(cp, ir.c_interval)) cp.paths.map(_ / f)
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
                    add: ((TcEnv, ir.ImmutableCanonPath, ir.ImmutableCanonPath) => TcEnv), 
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
        
    // ___ Immutability _____________________________________________________
    
    /** Could the value of `cp_test` change during `cp_inter`? */
    private[this] def compIsImmutableIn(
        cpc_test: ir.CanonPathComponent, 
        o_cp_inter: Option[ir.CanonPath]
    ) = {
        def imm(cpc: ir.CanonPathComponent): Boolean = log.indented("imm(%s)", cpc) {
            cpc match {
                // Local variables never change value once assigned:
                case ir.CpcReifiedLv(_, _, _) => true
                case ir.CpcGhostLv(_, _, _) => true

                // Ghosts never change value but the path they extend might:
                case ir.CpcGhostField(cpc_base, _, _, _) => imm(cpc_base)
                
                // Fields guarded by past intervals cannot change but others can:
                case ir.CpcReifiedField(cpc_base, ir.ReifiedFieldDecl(_, _, _, p_guard, _)) =>
                    imm(cpc_base) && o_cp_inter.exists { cp_inter =>
                        val cp_guard = canonPath(p_guard)
                        asImmutableIn(cp_guard, Some(cp_inter)).exists(imm_guard =>
                            hbInter(imm_guard, cp_inter)
                        )
                    }
            }            
        }
            
        log.indented(false, "compIsImmutableDuring(%s, %s)", cpc_test, o_cp_inter) {
            log.env(false, "Environment", this)
            imm(cpc_test)
        }
    }

    /** Returns an `ImmutableCanonPath` from `cp_test` containing only those components
      * that are immutable in `cp_inter`.  If there are no such components, returns `None`. */
    def asImmutableIn(
        cp_test: ir.CanonPath, 
        o_cp_inter: Option[ir.CanonPath]
    ): Option[ir.ImmutableCanonPath] = {
        log.indented(false, "asImmutableIn(%s, %s)", cp_test, o_cp_inter) {
            val immutableComponents = cp_test.components.filter(compIsImmutableIn(_, o_cp_inter))
            if(immutableComponents.isEmpty) None
            else Some(ir.ImmutableCanonPath(immutableComponents))
        }        
    }
    
    def isImmutableIn(cp_test: ir.CanonPath, o_cp_inter: Option[ir.CanonPath]): Boolean = {
        asImmutableIn(cp_test, o_cp_inter).isDefined
    }
    
    def isImmutable(cp_test: ir.CanonPath) = isImmutableIn(cp_test, o_cp_cur)
    def asImmutable(cp_test: ir.CanonPath) = asImmutableIn(cp_test, o_cp_cur)
    
    def toImmutable(cp_test: ir.CanonPath) = {
        asImmutable(cp_test) match {
            case None => throw new CheckFailure("intervals.must.be.immutable", cp_test.reprPath)
            case Some(immcp) => immcp
        }        
    }
    
    def immutableCanonPath(p: ir.Path): ir.ImmutableCanonPath = {
        toImmutable(canonPath(p))
    }
    def immutableCanonPaths(ps: List[ir.Path]) = ps.map(immutableCanonPath)
    def immutableCanonLv(lv: ir.VarName) = immutableCanonPath(lv.path)    
    def immutableCanonLvs(lvs: List[ir.VarName]) = lvs.map(immutableCanonLv)    
    
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
    
    def immutableReifiedPath(p: ir.Path): ir.ImmutableCanonPath = {
        val cp = immutableCanonPath(p)
        if(!cp.isReified)
            throw new CheckFailure("intervals.must.be.reified", p)
        cp
    }    
    def immutableReifiedPaths(ps: List[ir.Path]) = ps.map(immutableReifiedPath)
    def immutableReifiedLv(lv: ir.VarName) = immutableReifiedPath(lv.path)
    def immutableReifiedLvs(lvs: List[ir.VarName]) = lvs.map(immutableReifiedLv)
    
    def canonPath(p1: ir.Path): ir.CanonPath = log.indented(false, "canonPath(%s)", p1) {
        canonicalize(Set(), p1)
    }
    def canonPaths(ps: List[ir.Path]) = ps.map(canonPath)
    def canonLv(lv: ir.VarName) = canonPath(lv.path)
    def canonLvs(lvs: List[ir.VarName]) = lvs.map(canonLv)
    
    private[this] def expandComponents(
        ps_visited_in: Set[ir.Path],
        comps_in: Set[ir.CanonPathComponent]
    ): ir.CanonPath = {
        assert(!comps_in.isEmpty)
        val ps_visited = ps_visited_in ++ comps_in.map(_.p)
        
        val ps_freshRedirects = comps_in.flatMap(_.wps_identity.flatMap {
            case p: ir.Path if !ps_visited(p) => Some(p)
            case _ => None
        })
        
        if(ps_freshRedirects.isEmpty) {
            ir.CanonPath(comps_in.toList)            
        } else {
            val comps_out = ps_freshRedirects.foldLeft(comps_in) { 
                case (comps, p) => comps ++ canonicalize(ps_visited, p).components                    
            }
            expandComponents(ps_visited, comps_out)
        }
    }
    
    private[this] def canonicalize(
        ps_visited0: Set[ir.Path],
        p_request: ir.Path
    ): ir.CanonPath = log.indented("canonicalize(%s,%s)", ps_visited0, p_request) {
        assert(!ps_visited0(p_request))
        val ps_visited = ps_visited0 + p_request
        
        (flow.temp.get(p_request), p_request) match {
            case (Some(p_redirect), _) if !ps_visited(p_redirect) =>
                canonicalize(ps_visited, p_redirect)
                
            case (_, ir.PathLv(lv)) =>
                perm.get(lv) match {
                    case Some(cp) => cp
                    case None => throw new CheckFailure("intervals.no.such.variable", lv)
                }
            
            case (_, ir.PathField(p_base, f)) =>
                val cp = canonicalize(ps_visited, p_base)
                extendCanonWithFieldNamed(p_request, ps_visited, cp, f)
        }
    }
    
    private[this] def fld(cp_base: ir.CanonPath, fs: ir.FieldName*) = {
        log.indented(false, "fld(%s, %s)", cp_base, fs) {
            fs.foldLeft(cp_base) { case (cp, f) =>
                extendCanonWithFieldNamed(cp.reprPath / f, Set(), cp, f)
            }
        }
    }
    
    private[this] def extendCanonWithFieldNamed(
        p_request: ir.Path,
        ps_visited: Set[ir.Path],
        cp_base: ir.CanonPath, 
        f: ir.FieldName
    ) = {        
        val comps = cp_base.components.flatMap { comp_base =>
            f match {
                case ir.ClassCtorFieldName(_) | ir.f_objCtor() =>
                    extendComponentWithGhostField(comp_base, f, ir.c_interval)

                case _ =>
                    ghostFieldsDeclaredOnComponent(comp_base).find(_.isNamed(f)) match {
                        case Some(ir.GhostFieldDecl(f_gfd, c_gfd)) =>
                            extendComponentWithGhostField(comp_base, f_gfd, c_gfd)

                        case None =>
                            optSubstdReifiedFieldDeclOfComp(comp_base, f).map { 
                                case (_, rfd) =>
                                    extendComponentWithReifiedField(comp_base, rfd)
                            }
                    }
            }            
        }
        
        if(comps.isEmpty)
            throw new CheckFailure("intervals.no.such.field", cp_base.reprPath, f)
        
        expandComponents(ps_visited, comps.toSet)
    }
    
    private[this] def extendComponentWithGhostField(
        comp_base: ir.CanonPathComponent,
        f_gf: ir.FieldName,
        c_gf: ir.ClassName
    ) = {
        log.indented("extendComponentWithGhostField(%s, %s, %s)", comp_base, f_gf, c_gf) {
            val subst = comp_base.thisSubst
            comp_base match {
                case ir.CpcReified(_, wt) =>
                    val wgs_all = lowerBoundingCts(wt).flatMap { wct =>
                        wct.wghosts ++ ghostsOnClassAndSuperclasses(wct.c).map(subst.ghost)
                    }
                    Some(ir.CpcGhostField(
                        comp_base,
                        f_gf,
                        c_gf,
                        wgs_all.find(_.isNamed(f_gf)).map(_.wp).toList
                    ))
                    
                case ir.CpcGhost(_, c) =>
                    // Unlike for reified bases, we only build a path if the ghost was bound on
                    // some class.  In other words, don't build a path like g1.g2 where g[12]
                    // are ghost fields.  
                    ghostsOnClassAndSuperclasses(c).find(_.isNamed(f_gf)).map(subst.ghost).map {
                        wg => ir.CpcGhostField(comp_base, f_gf, c_gf, List(wg.wp))
                    }
            }
        }
    }
    
    private[this] def extendComponentWithReifiedField(
        comp_base: ir.CanonPathComponent,
        rfd: ir.ReifiedFieldDecl
    ) = {
        log("extendComponentWithReifiedField(%s, %s)", comp_base, rfd)
        ir.CpcReifiedField(comp_base, rfd)
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
            case ir.CpcGhost(_, c) => ghostFieldsDeclaredOnClassAndSuperclasses(c)
        }
    }
    
    /** Returns all type variables declared on the class(es) this type refers to. */
    def typeVarsDeclaredOnType(wt: ir.WcTypeRef): Set[ir.TypeVarDecl] = {
        lowerBoundingCts(wt).flatMap(wct => typeVarsDeclaredOnClassAndSuperclasses(wct.c))
    }
    
    /** Returns all type variables declared on the class(es) this type refers to. */
    def typeVarsDeclaredOnPath(cp: ir.CanonPath): Set[ir.TypeVarDecl] = {
        cp.wts.toSet.flatMap(typeVarsDeclaredOnType)
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
                    
                case ir.CpcGhost(_, _) => 
                    log("Ignoring Ghost Component")
                    None
            }
        }
    }
    
    /** Returns the class and field decl for `tcp.f`, error if no such field. */
    def substdReifiedFieldDecl(cp: ir.CanonPath, f: ir.FieldName) = {
        log.indented(false, "substdReifiedFieldDecl(%s::%s)", cp, f) {
            cp.components.firstSomeReturned(optSubstdReifiedFieldDeclOfComp(_, f)).getOrElse {
                throw new CheckFailure("intervals.no.such.field", cp.reprPath, f)
            }
        }
    }
    
    def methodDeclOfClass(c: ir.ClassName, m: ir.MethodName): Option[(ir.ClassName, ir.MethodDecl)] = {
        searchClassAndSuperclasses(_.methods.find(_.isNamed(m)))(c)        
    }

    def methodDeclOfCp(cp: ir.CanonPath, m: ir.MethodName) = {
        lowerBoundingClasses(cp).firstSomeReturned(methodDeclOfClass(_, m))
    }
    
    def reqdMethod(o: Option[(ir.ClassName, ir.MethodDecl)], m: ir.MethodName): ir.MethodDecl = {
        o match {
            case Some((_, md)) => md
            case None => throw new CheckFailure("intervals.no.such.method", m)
        }
    }

    def ctorOfClass(c: ir.ClassName, m: ir.MethodName) = {
        classDecl(c).ctors.find(_.isNamed(m)) match {
            case Some(md) => md
            case None => throw new CheckFailure("intervals.no.such.ctor", c, m)
        }
    }

    /** Method signatures of all methods overridden by `md_sub` declared on
      * the class `ct`. */
    def substdOverriddenMethodSigs(ct: ir.ClassType, md_sub: ir.MethodDecl) = {
        val cd = classDecl(ct.c)
        val mds_overridden = cd.superClasses.foldLeft(List[ir.MethodDecl]()) { case (lst, c) =>
            methodDeclOfClass(c, md_sub.name) match {
                case None => lst
                case Some((_, md)) => md :: lst
            }
        }
        val lvs_sub = md_sub.args.map(_.name)
        mds_overridden.map(_.msig(ir.lv_mthd, ir.lv_this, lvs_sub))
    }
    
    // ___ Queries of the environment _______________________________________
    //
    // Queries are generally defined over canonical paths.
    
    /** Current interval (must be defined) */
    def lv_cur = o_lv_cur.get
    
    /** Canonical path of current interval (None if not defined) */
    def o_cp_cur = o_lv_cur.map(perm)
    
    /** Canonical path of current interval (must be defined) */
    def cp_cur = o_cp_cur.get
    
    /** Path of current interval (must be defined) */
    def p_cur = cp_cur.reprPath
    
    /** Class name referred to be `super` */
    def c_super = classDecl(c_this).superClasses(0)
    
    /** Canonical path to `this` */
    def cp_this = reifiedLv(ir.lv_this)
    
    /** TeeCeePee of `this` */
    def tcp_this = ir.TeeCeePee(cp_this, ct_this)

    /** ClassType of `this` */
    def ct_this = c_this.ct
    
    /** Canonical path to `method` */
    def cp_mthd = perm(ir.lv_mthd)    
    
    /** True if the path `cp` is known to be nonnull */
    def isNonnull(cp: ir.CanonPath) = log.indented(false, "isNonnull(%s)?", cp) {
        log.env(false, "Environment", this)
        cp.paths.exists(flow.nonnull)
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
            (cp_from.paths cross cp_to.paths).existsPair((p_from, p_to) =>
                bfs(p_from.end, p_to.start))                
        }
    }
    
    def suspends(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented(false, "suspends(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)

            inlineSuperintervalsOf(cp).exists(equiv(_, cq))
        }
    }
    
    def locks(cp: ir.CanonPath, cq: ir.CanonPath) = {
        log.indented(false, "locks(%s, %s)?", cp, cq) {
            log.env(false, "Environment", this)
            
            inlineSuperintervalsOf(cp).exists { cp_sup => 
                (cp_sup.paths cross cq.paths).exists(flow.locks)
            }
        }
    }
    
    private[this] def immediateIsWritableBy(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = {
        log.indented(false, "immediateIsWritableBy(%s, %s)?", cp, cq) {
            (
                cp.wps_identity.exists {
                    // Is cp a ghost declared writable by q?
                    case ir.WcWritableBy(qs) => among(cq, canonPaths(qs))
                    case _ => false
                }
            ) || {
                equiv(cp, cq) ||
                locks(cq, cp) ||    // lock cp is writable by interval cq if cq locks cp
                suspends(cq, cp) || // interval cp is writable by cq if cp suspends cp
                (cp.paths cross cq.paths).exists(flow.writable)
            }
        }
    }
    
    private[this] def immediateIsReadableBy(cp: ir.CanonPath, cq: ir.CanonPath): Boolean = {
        log.indented(false, "immediateIsReadableBy(%s, %s)?", cp, cq) {
            (
                cp.wps_identity.exists {
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
                (cp.paths cross cq.paths).exists(flow.readable) ||
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
            o_cp_cur.exists(cp_cur => bfs(cp.reprPath.end, p_cur.start))
        }
    }
            
    /// cp hbNow cqs if either (1) cp hb cur; or (2) ∃i. cp.end hbeq cqs(i).end.
    def hbNow(cp: ir.CanonPath, cqs: List[ir.CanonPath]): Boolean = {
        log.indented(false, "hbNow(%s, %s)", cp, cqs) {
            log.env(false, "Environment", this)
            
            (
                cp.wps_identity.exists {
                    case ir.WcHbNow(ps) => isLtCanonPaths(canonPaths(ps), cqs)
                    case _ => false
                }
            ) || {
                interHappened(cp) ||
                among(cp, cqs) ||
                cqs.exists(cq => bfs(cp.reprPath.end, cq.reprPath.end))
            }            
        }
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
    def linkedPaths(
        /** Variable owning the field */
        lv_owner: ir.VarName, 
        /** Class declaring the field */
        c_f: ir.ClassName, 
        /** Field name being accessed */
        f: ir.FieldName
    ) = {
        val cp_owner = perm(lv_owner)
        assert(pathHasClass(cp_owner, c_f))
        
        def isDependentOn(wt: ir.WcTypeRef, p: ir.Path) = {
            lowerBoundingCts(wt).exists { wct =>
                wct.wghosts.exists(_.wp.isDependentOn(p))
            }
        }
        
        val cd = classDecl(c_f)
        val p_f = f.thisPath        
        assert(cd.reifiedFieldDecls.exists(_.isNamed(f)))
        
        // only reified fields can be linked to another field
        val rfds = cd.reifiedFieldDecls
        
        // find fields where cp_o.f appears in the type
        val rfds_maybe_linked = rfds.filter(rfd => isDependentOn(rfd.wt, p_f))
        
        // screen out those which cannot have been written yet (and whose 
        // value is therefore null, which is valid for any type)
        val subst = PathSubst.vv(ir.lv_this, lv_owner)
        val rfds_linked = rfds_maybe_linked.filter { rfd =>
            !o_cp_cur.exists(cp_cur =>
                hbInter(cp_cur, canonPath(subst.path(rfd.p_guard))))
        }
        
        // map to the canonical path for the field
        rfds_linked.map { rfd => canonPath(subst.path(rfd.thisPath)) }
    }
    
    // ___ Superintervals and suspended intervals ___________________________
    
    private[this] def immediateInlineSuperintervalsOf(cp: ir.CanonPath): Set[ir.CanonPath] = {
        log.indented("immediateInlineSuperintervalsOf(%s)", cp) {
            cp.paths.toSet.flatMap(flow.inline.values).map(canonPath) ++ {
                log.indented("cp0.Constructor[_] < cp0.Constructor?") {
                    cp.components.flatMap {
                        case ir.CpcGhostField(cp0, ir.ClassCtorFieldName(_), _, _) =>
                            Some(canonPath(cp0.p / ir.f_objCtor))
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
            immediateInlineSuperintervalsOf(cp) ++ {
                if(cp.isReified) Some(fld(cp, ir.f_parent))
                else None                
            }
        }
    }

    private[this] def superintervalsOf(cp0: ir.CanonPath) = {
        log.indented(false, "superintervalsOf(%s)", cp0) {
            computeTransitiveClosure(immediateSuperintervalsOf, Set(cp0))
        }
    }
    
    // ___ Happens-Before Searches __________________________________________
    
    class HbWalk(
        didNotHappen: Set[ir.Path],
        p_from: ir.Path,
        p_original_to: ir.Path
    ) {        
        def starts(cp: ir.CanonPath) = cp.paths.map(_.start)
        def ends(cp: ir.CanonPath) = cp.paths.map(_.end)
        def isInterval(cp: ir.CanonPath) = pathHasClass(cp, ir.c_interval)
        
        // Find a set of points to which we are trying to find a path.  This
        // set includes p_original_to but also the start point of any superintervals.
        // This is necessary because succ() cannot travel from the start point of
        // a superinterval to the start point of its children.
        val ps_to = {
            def expand(p_base: ir.Path) = {
                val cp_base = canonPath(p_base)
                if(isInterval(cp_base)) superintervalsOf(cp_base).flatMap(starts)
                else Set()
            }

            Set(p_original_to) ++ (p_original_to match {
                case p_base / ir.f_start() => expand(p_base)
                case p_base / ir.f_end() => expand(p_base)
                case _ => Set()
            })            
        }

        def doWalk(): Boolean = {
            log.indented(false, "bfs(%s,%s)", p_from, ps_to) {
                visitNext(Set(p_from), Queue.Empty.enqueue(p_from))
            }
        }
        
        def depoint(p: ir.Path, f: ir.FieldName) = p match {
            case p_base / f() => Some(canonPath(p_base))
            case _ => None
        }
        
        def visitNext(vis: Set[ir.Path], queue0: Queue[ir.Path]): Boolean = {
            if(queue0.isEmpty)
                false
            else {
                val (p_cur, queue1) = queue0.dequeue
                log("search(%s)", p_cur)
                ps_to(p_cur) || {
                    val ps_unvisited_succ = succ(p_cur).filter(p => !vis(p))
                    visitNext(vis ++ ps_unvisited_succ, queue1.enqueue(ps_unvisited_succ))
                }                
            }
        }
        
        def succ(p: ir.Path): Set[ir.Path] = log.indented("succ(%s)", p) {
            flow.hb.values(p) ++ {
                log.indented("X.start -> X.end if (X: Interval)") {                    
                    depoint(p, ir.f_start) match {
                        case Some(cp_inter) if isInterval(cp_inter) => ends(cp_inter)
                        case _ => Nil
                    }
                }
            } ++ {
                log.indented("X.end -> X.Parent.end if (X: Interval)") {
                    depoint(p, ir.f_end) match {
                        case Some(cp_inter: ir.CanonPath) if isInterval(cp_inter) =>
                            immediateSuperintervalsOf(cp_inter).flatMap(ends).toList
                        case _ => Nil
                    }
                }
            } ++ {
                log.indented("X.Constructor[L].end -> X.Constructor[R].start if R <: L") {
                    p match {
                        case p_base / ir.ClassCtorFieldName(c_left) / ir.f_end()
                        if !prog.isInterface(c_left) =>
                            val cp = canonPath(p_base)
                            val cs_left = prog.classAndSuperclasses(c_left)
                            val cs_right = lowerBoundingClasses(cp).flatMap(prog.classAndSuperclasses)
                            cs_right.toList.flatMap {
                                case c_right if !cs_left(c_right) && !prog.isInterface(c_right) =>
                                    starts(fld(cp, ir.ClassCtorFieldName(c_right)))
                                case _ => List()
                            }
                        case _ => Nil
                    }
                }
            } ++ {
                log.indented("X.Constructor.end -> X.start if (X: Interval)") {
                    p match {
                        case p_base / ir.f_objCtor() / ir.f_end() =>
                            val cp = canonPath(p_base)
                            if(isInterval(cp)) starts(cp)
                            else Nil
                        case _ => Nil
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
                        canonPath(q).paths.exists { q1 =>
                            !didNotHappen(q1) &&
                            new HbWalk(didNotHappen + q1, q1.end, p_cur.start).doWalk()
                        }
                    }

                    depoint(p, ir.f_end) match {
                        case Some(cp) => cp.wps_identity.flatMap {
                            case ir.WcHbNow(qs) if qs.forall(happened) =>
                                starts(cp_cur)
                            case wp =>                            
                                log("cp0.f == %s", wp); Nil                            
                        }
                        case None => Nil                        
                    }
                }                
            }      
        }

    }
    
    private def bfs(p_from: ir.Path, p_to: ir.Path) = {        
        new HbWalk(Set(), p_from, p_to).doWalk()
    }
    
    // ___ Other operations on canonical paths ______________________________

    private[this] def equiv(cp0: ir.CanonPath, cq0: ir.CanonPath): Boolean = {
        cp0.paths.exists(cq0.paths.toSet)
    }
    
    private[this] def equiv(p: ir.Path, q: ir.Path): Boolean = {
        (p == q) || equiv(canonPath(p), canonPath(q))
    }
    
    private[this] def among(cp: ir.CanonPath, cqs: List[ir.CanonPath]) = {
        cqs.exists { cq => equiv(cp, cq) || suspends(cp, cq) }
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
            val cp = reifiedPath(pt.p)
            
            // search for a declaration of pt.tv:
            val o_tvd = cp.wts.firstSomeReturned(typeVarsDeclaredOnType(_).find(_.isNamed(pt.tv)))
            val tvd = o_tvd.getOrElse {
                throw new CheckFailure("intervals.no.such.type.var", pt.p, pt.tv)
            }
            
            log("tvd=%s", tvd)
            val subst = PathSubst.vp(ir.lv_this, pt.p)
            val tvd_lbs = tvd.wts_lb.map(subst.wcTref)
            log("tvd_lbs=%s", tvd_lbs)
            
            val bounds = typeArgBounds(cp, pt.tv)
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

    def lowerBoundingClasses(cp: ir.CanonPath) = {
        cp.components.foldLeft(Set[ir.ClassName]()) { 
            case (set, ir.CpcReified(_, wt)) => set ++ lowerBoundingCts(wt).map(_.c)
            case (set, ir.CpcGhost(_, c)) => set + c
        }        
    }

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
            cp_sub.components.exists { 
                case ir.CpcGhost(_, c) => prog.isSubclass(c, c_sup)
                case ir.CpcReified(_, wt) => isSubclass(wt, c_sup)
            }
        }
    }
    
    /** Does `cp_sub` refer to an object whose type could be `c_sup`? */
    def pathCouldHaveClass(cp_sub: ir.CanonPath, c_sup: ir.ClassName): Boolean = {
        log.indented(false, "pathCouldHaveClass(%s, %s)?", cp_sub, c_sup) {
            cp_sub.components.exists { 
                case ir.CpcGhost(_, c) => 
                    prog.isSubclass(c, c_sup) || prog.isSubclass(c_sup, c)
                case ir.CpcReified(_, wt) => 
                    isSubclass(wt, c_sup) || isSubclass(c_sup, wt)
            }
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
    
    def pathMatchesWpath(cp: ir.CanonPath, wq: ir.WcPath): Boolean = {
        log.indented("pathMatchesWpath(%s, %s)?", cp, wq) {
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
        log.indented("pathHasSubWcGhost(%s, %s)?", cp_sub, wg_sup) {
            val cp_f = fld(cp_sub, wg_sup.f)
            pathMatchesWpath(cp_f, wg_sup.wp)
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
        log.indented("pathHasSubWcTarg(%s, %s)?", cp_sub, wtarg_sup) {
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
                log.indented("comp=%s", comp) {
                    val wts_sub = lowerBoundingWts(comp.wt)
                    val wts_sup = upperBoundingWts(wt_sup)

                    (wts_sub cross wts_sup).exists {
                        case (pt_sub: ir.PathType, pt_sup: ir.PathType) =>
                            (pt_sub.tv == pt_sup.tv) && equiv(pt_sub.p, pt_sup.p)

                        case (wct_sub: ir.WcClassType, wct_sup: ir.WcClassType) =>
                            prog.isSubclass(wct_sub.c, wct_sup.c) &&
                            wct_sup.wghosts.forall(pathHasSubWcGhost(cp_sub, _)) &&
                            wct_sup.wtargs.forall(pathHasSubWcTarg(cp_sub, _))

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
            val (_, cp, env) = freshCp(wt_sub)
            env.pathHasType(cp, wt_sup)
        }
    }
    
}