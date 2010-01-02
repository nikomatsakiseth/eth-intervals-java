package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Positional
import Util._

class TypeCheck(val prog: Prog) 
extends ComputeRelations(prog)
{    
    import prog.log
    import prog.classDecl
    import prog.at
    import prog.thisTref
    import prog.typeOriginallyDefiningMethod
    import prog.overriddenMethodSigs
    import prog.isSubclass
    import prog.strictSuperclasses
    
    // ______________________________________________________________________
    // Subtyping    
    
    /// wp <= wq
    def isSubpath(p: ir.Path, wq: ir.WcPath) = savingEnv {
        log.indentedRes("%s <= %s?", p, wq) {                
            // Here we just use teePee() without checking that the resulting
            // paths are immutable.  That's safe because we never ADD to the 
            // relations unless the teePee is immutable. 
            //
            // XXX Is this REALLY true...?
            val tp = teePee(p)
            wq match {
                case q: ir.Path =>
                    equiv(tp, teePee(q))
                case ir.WcHb(addCheckedArg1, addCheckedArg2) =>
                    addCheckedArg1.forall { q_1 => userHb(teePee(q_1), tp) } &&
                    addCheckedArg2.forall { q_2 => userHb(tp, teePee(q_2)) }
                case ir.WcReadableBy(lq) =>
                    lq.forall { q => isReadableBy(tp, teePee(q)) }
                case ir.WcWritableBy(lq) =>
                    lq.forall { q => isWritableBy(tp, teePee(q)) }
                case ir.WcLocks(lq) =>
                    lq.forall { q => locks(tp, teePee(q)) }
                case ir.WcLockedBy(lq) =>
                    lq.forall { q => locks(teePee(q), tp) }
            }
        }        
    }
    
    /// ∃ g∈t.ghosts . (g.f == f, g.p <= wp)?
    def isSubghost(t_sub: ir.TypeRef, wt_ghost: ir.WcGhost) = preservesEnv {
        t_sub.oghost(wt_ghost.f) match {
            case Some(p) => isSubpath(p, wt_ghost.wp)
            case None => false
        }
    }
    
    /// t_sub <: wt_sup
    def isSubtype(t_sub: ir.TypeRef, wt_sup: ir.WcTypeRef): Boolean = preservesEnv {
        log.indentedRes("%s <: %s?", t_sub, wt_sup) {            
            log("t_sub.ctor=%s wt_sup.as.ctor=%s", t_sub.as.ctor, wt_sup.as.ctor)
            if(t_sub.c == wt_sup.c) {
                (!t_sub.as.ctor || wt_sup.as.ctor) && // (t <: t ctor) but (t ctor not <: t)
                wt_sup.wghosts.forall(isSubghost(t_sub, _)) // c<F: P> <: c<F: WP> iff F:P <= F:WP
            } else // else walk to supertype(s) of t_sub
                prog.sups(t_sub).exists(isSubtype(_, wt_sup))
        }                
    }
    
    def isSubtype(tp_sub: ir.TeePee, wt_sup: ir.WcTypeRef): Boolean = 
        isSubtype(cap(tp_sub), wt_sup)
        
    def freshTp(wt: ir.WcTypeRef) = {
        val lv = prog.freshVarName
        val tp = ir.TeePee(wt, lv.path, ir.ghostAttrs)
        addPerm(lv, tp)
        tp
    }
        
    /// wt(tp_sub) <: wt_sup
    def checkIsSubtype(tp_sub: ir.TeePee, wt_sup: ir.WcTypeRef) {
        if(!isSubtype(tp_sub, wt_sup))
            throw new ir.IrError("intervals.expected.subtype", tp_sub.p, tp_sub.wt, wt_sup)
    }
    
    // ______________________________________________________________________
    // Checking Method Bodies
    
    def isReqFulfilled(req: ir.Req): Boolean = log.indentedRes("isReqFulfilled(%s)", req) {
        def is(func: Function2[ir.TeePee, ir.TeePee, Boolean], ps: List[ir.Path], qs: List[ir.Path]) = {
            val tps = teePee(ir.ghostAttrs, ps)
            val tqs = teePee(ir.ghostAttrs, qs)
            forallcross(tps, tqs)(func)
        }
        req match {
            case ir.ReqWritableBy(ps, qs) => is(isWritableBy, ps, qs)
            case ir.ReqReadableBy(ps, qs) => is(isReadableBy, ps, qs)
            case ir.ReqSubintervalOf(ps, qs) => is(isSubintervalOf, ps, qs)
            case ir.ReqHb(ps, qs) => is(userHb, ps, qs)
        }
    }
    
    def checkReqFulfilled(req: ir.Req) {
        if(!isReqFulfilled(req))
            throw new ir.IrError("intervals.requirement.not.met", req)
    }
    
    def checkArgumentTypes(msig: ir.MethodSig, tqs: List[ir.TeePee]) =
        foreachzip(tqs, msig.args.map(_.wt))(checkIsSubtype)
    
    def checkReadable(tp_guard: ir.TeePee) {
        if(!isReadableBy(tp_guard, tp_cur))
            throw new ir.IrError("intervals.not.readable", tp_guard.p)
    }
    
    def checkWritable(tp_guard: ir.TeePee) {
        if(!isWritableBy(tp_guard, tp_cur))
            throw new ir.IrError("intervals.not.writable", tp_guard.p)
    }
        
    def checkNoInvalidated() {
        if(!env.ps_invalidated.isEmpty)
            throw new ir.IrError(
                "intervals.must.assign.first", 
                env.ps_invalidated.mkEnglishString)        
    }

    def checkGoto(blks: Array[ir.Block], succ: ir.Goto) {
        at(succ, ()) {
            log.indented(succ) {
                val blk_tar = blks(succ.b)
                val lvs_tar = blk_tar.args.map(_.name)
                val tps = teePee(ir.noAttrs, succ.ps)
                val subst = PathSubst.vp(lvs_tar, tps.map(_.p))
                val wts_tar = blk_tar.args.map(arg => subst.wtref(arg.wt))
                foreachzip(tps, wts_tar)(checkIsSubtype)                
            }
        }
    }
    
    def checkCallMsig(tp: ir.TeePee, msig: ir.MethodSig, tqs: List[ir.TeePee]) {
        // Cannot invoke a method when there are outstanding invalidated fields:
        checkNoInvalidated()
        
        // If method is not a constructor method, receiver must be constructed:
        if(!msig.as.ctor && tp.wt.as.ctor) 
            throw new ir.IrError("intervals.rcvr.must.be.constructed", tp.p)
            
        // Arguments must have correct type and requirements must be fulfilled:
        checkArgumentTypes(msig, tqs)
        msig.reqs.foreach(checkReqFulfilled)
    }
    
    def checkCall(tp: ir.TeePee, m: ir.MethodName, tqs: List[ir.TeePee]) {
        val msig = substdMethodSig(tp, m, tqs)
        checkCallMsig(tp, msig, tqs)
    }
    
    def checkStatement(stmt: ir.Stmt): Unit = 
        at(stmt, ()) {
            stmt match {                  
                case ir.StmtSuperCtor(m, qs) =>
                    val tp = tp_super
                    val tqs = teePee(ir.noAttrs, qs)
                    val msig_ctor = substdCtorSig(tp, m, tqs)
                    checkCallMsig(tp_super, msig_ctor, tqs)
                    
                case ir.StmtGetField(_, p_o, f) =>
                    val tp_o = teePee(ir.noAttrs, p_o)
                    substdFieldDecl(tp_o, f) match {
                        case ir.GhostFieldDecl(_, _) =>
                            throw new ir.IrError("intervals.not.reified", tp_o.wt.c, f)
                        
                        case ir.ReifiedFieldDecl(_, wt, _, p_guard) =>
                            val tp_guard = teePee(ir.ghostAttrs, p_guard)                    
                            checkReadable(tp_guard)
                    }                    
                    
                case ir.StmtSetField(p_o, f, p_v) =>
                    val tp_o = teePee(ir.noAttrs, p_o)
                    val tp_v = teePee(ir.noAttrs, p_v)
                    
                    substdFieldDecl(tp_o, f) match {
                        case ir.GhostFieldDecl(_, _) => 
                            throw new ir.IrError("intervals.not.reified", tp_o.wt.c, f)
                        
                        case ir.ReifiedFieldDecl(_, wt, _, p_guard) =>
                            val tp_guard = teePee(ir.ghostAttrs, p_guard)
                            checkWritable(tp_guard)                    
                            checkIsSubtype(tp_v, wt)
                    }
                        
                case ir.StmtCall(_, p, m, qs) =>
                    val tp = teePee(ir.noAttrs, p)
                    val tqs = teePee(ir.noAttrs, qs)
                    checkCall(tp, m, tqs)
        
                case ir.StmtSuperCall(_, m, qs) =>
                    val tqs = teePee(ir.noAttrs, qs)
                    checkCall(tp_super, m, tqs)
                
                case ir.StmtNew(x, t, m, qs) =>
                    val cd = classDecl(t.c)
                    val tqs = teePee(ir.noAttrs, qs)
                    
                    if(cd.attrs.interface)
                        throw new ir.IrError("intervals.new.interface", t.c)
                        
                    // Check Ghost Types:           
                    savingEnv {
                        addLvDecl(x, t, None)
                        val tp_x = teePee(x.path)
                        t.ghosts.foreach { g =>
                            val gfd = substdFieldDecl(tp_x, g.f)
                            val tp = teePee(g.p)
                            checkIsSubtype(tp, gfd.wt)
                        }                        
                                        
                        val msig_ctor = substdCtorSig(tp_x, m, tqs)
                        checkCallMsig(teePee(x.path), msig_ctor, tqs)
                    }
                    
                case ir.StmtCast(_, _, _) => ()
                    // TODO Validate casts?  Issue warnings at least?
                    
                case ir.StmtNull(_, _) => ()
                    
                case ir.StmtReturn(p) =>
                    val tp = teePee(ir.noAttrs, p)
                    checkIsSubtype(tp, env.wt_ret)
                    
                case ir.StmtHb(_, _) => () // Wf checks that args are intervals or points                    
                
                case ir.StmtLocks(_, _) => () // Wf checks that args are interval/lock
                
                case ir.StmtSubintervalPush(_, _) => ()
                
                case ir.StmtSubintervalPop(_) => ()
            }
        }
    
        
    // ______________________________________________________________________
    // Inter-method Assumptions
    //
    // When we know that method A must complete before method B is invoked,
    // we can move any relations known at the end of A into B.  In particular, 
    // we can move relations from the constructor to any non-constructor method.
    // There are some complications, however.  We need to ensure we only move
    // a relation (p, q) if the paths p and q refer to the same objects in method B.
    // This basically restricts us to paths beginning with "this".  The next complication
    // is that the interesting relations tend to be between fields that which are being
    // actively modified by method A, which means that these relations will actually be
    // recorded as between fields that are temporarily aliased to local variables.
    //
    // Therefore, what we do is as follows:
    //
    // (1) At the end of the method A, we create a reverse mapping for all temporary
    //     aliases p->lv iff the path p is declared as being written by A (i.e., this.constructor).
    //     This is sound because if it has not changed by the end of A, then
    //     it will never change.
    //
    // (2) We apply this reverse mapping to all relations and then take the subset of
    //     those which involve paths beginning with shared local variables (only "this" right
    //     now).
    
    def extractAssumptions(
        tp_mthd: ir.TeePee, 
        lvs_shared: Set[ir.VarName]
    ): ir.TcEnv = savingEnv {        
        log.indented("extractAssumptions(%s,%s)", tp_mthd, lvs_shared) {
            withCurrent(freshTp(ir.t_interval).p) {
                addHbInter(tp_mthd, tp_cur)

                log("temp=%s", env.temp)
                val tempKeys = env.temp.map(_._1).toList
                val tempValues = env.temp.map(_._2).toList
                val mapFunc = PathSubst.pp(tempValues, tempKeys).path(_)
                
                def filterFunc(p: ir.Path): Boolean = 
                    log.indentedRes("filterFunc(%s)", p) {
                        lvs_shared(p.lv) && !teePee(p).as.mutable                
                    }

                ir.Env.empty
                    .withReadable(env.readable.mapFilter(mapFunc, filterFunc))
                    .withWritable(env.writable.mapFilter(mapFunc, filterFunc))
                    .withHb(env.hb.mapFilter(mapFunc, filterFunc))
                    .withSubinterval(env.subinterval.mapFilter(mapFunc, filterFunc))
                    .withLocks(env.locks.mapFilter(mapFunc, filterFunc))
            }            
        }
    }
    
    // ______________________________________________________________________
    // Checking methods and constructors

    def checkArgumentTypesNonvariant(args_sub: List[ir.LvDecl], args_sup: List[ir.LvDecl]) {
        foreachzip(args_sub, args_sup) { case (arg_sub, arg_sup) =>
            if(arg_sub.wt != arg_sup.wt)
                throw new ir.IrError(
                    "intervals.override.param.type.changed", 
                    arg_sub.name, arg_sub.wt, arg_sup.wt)
        }        
    }
    
    def checkReturnTypeCovariant(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef) {
        if(!isSubtype(freshTp(wt_sub), wt_sup))
            throw new ir.IrError(
                "intervals.override.ret.type.changed", wt_sub, wt_sup)
    }
    
    def checkOverridenReqsImplyOurReqs(reqs_sub: List[ir.Req], reqs_sup: List[ir.Req]) {
        reqs_sup.foreach(addReq)
        reqs_sub.foreach { req_sub =>
            if(!isReqFulfilled(req_sub))
                throw new ir.IrError("intervals.override.adds.req", req_sub)
        }        
    }
    
    /// Changes the names of the arguments in 'msig' to 'lvn'.
    def substArgsInMethodSig(msig: ir.MethodSig, lvn: List[ir.VarName]) = {
        val lvn_msig = msig.args.map(_.name)
        val nameMap = Map(lvn_msig.zip(lvn): _*)
        val subst = PathSubst.vv(lvn_msig, lvn)
        ir.MethodSig(
            subst.tref(msig.t_rcvr),
            msig.as,
            msig.args.map { lv => ir.LvDecl(nameMap(lv.name), subst.wtref(lv.wt)) },
            msig.reqs.map(subst.req),
            subst.wtref(msig.wt_ret)
        )                
    }
    
    // Final environment: the environment on exit from the block
    def checkBlocks(blks: Array[ir.Block], ins: Array[ir.TcEnv], outs: Array[ir.TcEnv]) {
        blks.indices.foreach { b =>
            log.indented("%s: %s", b, blks(b)) {
                savingEnv {
                    env = ins(b)
                    log.env("Initial environment:", env)
                    blks(b).stmts.foreach { stmt =>
                        checkStatement(stmt)
                        addStatement(stmt)
                        log.env("Environment:", env)
                    }
                    blks(b).gotos.foreach(checkGoto(blks, _))
                }
            }
        }
        
        val bs_exit = blks.indices.toList.filter(b =>
            blks(b).gotos.isEmpty) // Exit blocks are those w/ no successors
        env = ir.Env.intersect(bs_exit.map(outs)) // Intersection of all final environments        
        checkNoInvalidated()
    }
        
    def checkMethodDecl(
        cd: ir.ClassDecl,          // class in which the method is declared
        env_ctor_assum: ir.TcEnv,  // relations established by the ctor
        md: ir.MethodDecl          // method to check
    ) = 
        at(md, ()) {
            savingEnv {
                // Define special vars "method" and "this":
                addPerm(ir.lv_mthd, ir.TeePee(ir.t_interval, ir.p_mthd, ir.ghostAttrs))
                pushCurrent(ir.p_mthd)
                
                if(!md.attrs.ctor) { 
                    // For normal methods, type of this is the defining class
                    addPerm(ir.lv_this, ir.TeePee(thisTref(cd), ir.p_this, ir.noAttrs))                     
                    addHbInter(tp_ctor, tp_cur) // ... constructor already happened
                    env = env + env_ctor_assum          // ... add its effects on environment
                } else {
                    // For constructor methods, type of this is the type that originally defined it
                    val t_rcvr = typeOriginallyDefiningMethod(cd.name, md.name).get
                    addPerm(ir.lv_this, ir.TeePee(t_rcvr.ctor, ir.p_this, ir.noAttrs))
                }  
                
                md.args.foreach(addArg)
                setWtRet(md.wt_ret)
                
                savingEnv {
                    overriddenMethodSigs(cd.name, md.name) foreach { msig_sup_0 => 
                        val msig_sup = substArgsInMethodSig(msig_sup_0, md.args.map(_.name))
                        checkArgumentTypesNonvariant(md.args, msig_sup.args)
                        checkReturnTypeCovariant(md.wt_ret, msig_sup.wt_ret)
                        checkOverridenReqsImplyOurReqs(md.reqs, msig_sup.reqs)
                    }                    
                }
                
                md.reqs.foreach(addReq)
                
                val (ins, outs) = iterateMethodBlocks(md.blocks)
                checkBlocks(md.blocks, ins, outs)
            }                     
        }
        
    def checkNoninterfaceConstructorDecl(cd: ir.ClassDecl, md: ir.MethodDecl) = 
        at(md, ir.Env.empty) {
            savingEnv {
                // Define special vars "method" (== this.constructor) and "this":
                addPerm(ir.lv_mthd, ir.TeePee(ir.t_interval, ir.gfd_ctor.thisPath, ir.ghostAttrs))
                pushCurrent(ir.p_mthd)
                addPerm(ir.lv_this, ir.TeePee(thisTref(cd, ir.ctorAttrs), ir.p_this, ir.noAttrs))

                // Check method body:
                md.args.foreach(addArg)
                md.reqs.foreach(addReq)                
                val (ins, outs) = iterateMethodBlocks(md.blocks)
                checkBlocks(md.blocks, ins, outs)
                
                // Compute exit assumptions and store in prog.exportedCtorEnvs:
                env = extractAssumptions(tp_ctor, Set(ir.lv_this)) // Globally valid assums
                log.env("extracted assumptions:", env)
                prog.exportedCtorEnvs += Pair((cd.name, md.name), env) // Store
                env
            }
        }
    
    def checkReifiedFieldDecl(cd: ir.ClassDecl, fd: ir.ReifiedFieldDecl) = 
        at(fd, ()) {
            savingEnv {
                // Rules:
                //
                // The type of a field f with guard p_g in class c 
                // may be dependent on a path p_dep if either:
                // (1) p_dep is constant when p_g is active; or
                // (2) p_dep = this.f' and f' is declared in class c (not a supertype!)
                //
                // Note that a type is dependent on p_dep if p.F appears in the type, so 
                // we must check all prefixes of each dependent path as well.
                val t_this = thisTref(cd, ir.ctorAttrs)
                val tp_this = ir.TeePee(t_this, ir.p_this, ir.noAttrs)
                val tp_mthd = ir.TeePee(ir.t_interval, ir.p_mthd, ir.ghostAttrs)
                addPerm(ir.lv_this, tp_this)
                addPerm(ir.lv_mthd, tp_mthd)
                pushCurrent(tp_mthd.p)
                
                // If an interval class, then this.ctor hb this:
                if(isSubclass(t_this, ir.c_interval))
                    addHbInter(teePee(ir.p_ctor), tp_this)
                
                // Add assumptions as though guard were satisfied.
                // Also check that guards are typed as Interval, Lock, or just Guard
                val tp_guard = teePee(fd.p_guard)
                if(isSubclass(tp_guard.wt, ir.c_interval))
                    addSubintervalOf(tp_cur, tp_guard) 
                else if(isSubclass(tp_guard.wt, ir.c_lock))
                    addLocks(tp_cur, tp_guard)
                else if(tp_guard.wt.c == ir.c_guard)
                    addDeclaredWritableBy(tp_guard, tp_cur)
                else
                    throw new ir.IrError("intervals.invalid.guard.type", tp_guard.wt)                    
                
                // Check that each dependent path is legal:
                fd.wt.dependentPaths.foreach { p_full_dep => 
                    savingEnv {
                        def check(p_dep: ir.Path) {
                            p_dep match {
                                case ir.Path(lv, List()) => 
                                    // Always permitted.

                                case ir.Path(lv, List(f)) if lv == ir.lv_this => 
                                    val tp_dep = teePee(p_dep)
                                    if(!tp_dep.isConstant && !cd.fields.exists(_.name == f))
                                        throw new ir.IrError(
                                            "intervals.illegal.type.dep",
                                            tp_dep.p, tp_guard.p)

                                case ir.Path(lv, f :: rev_fs) =>
                                    check(ir.Path(lv, rev_fs))
                                    val tp_dep = teePee(p_dep)
                                    if(!tp_dep.isConstant)
                                        throw new ir.IrError(
                                            "intervals.illegal.type.dep",
                                            tp_dep.p, tp_guard.p)
                            }
                        }
                        check(p_full_dep)                            
                    }                   
                }                        
            }
        }
    
    def checkGhostFieldDecl(cd: ir.ClassDecl, gfd: ir.GhostFieldDecl) = 
        at(gfd, ()) {
            log.indented(gfd) {
                // Check that ghosts are not shadowed from a super class:
                strictSuperclasses(cd.name).foreach { c =>
                    if(classDecl(c).fields.exists(_.name == gfd.name))
                        throw ir.IrError("intervals.shadowed.ghost", c, gfd.name)
                }
            }
        }
        
    def checkFieldDecl(cd: ir.ClassDecl)(priorNames: Set[ir.FieldName], fd: ir.FieldDecl) = 
        at(fd, priorNames) {
            if(priorNames(fd.name))
                throw new ir.IrError("intervals.duplicate.field", fd.name)
            fd match {
                case rfd: ir.ReifiedFieldDecl => checkReifiedFieldDecl(cd, rfd)
                case gfd: ir.GhostFieldDecl => checkGhostFieldDecl(cd, gfd)
            }
            priorNames + fd.name
        }
    // ______________________________________________________________________
    // Classes and Interfaces
    
    def checkInterfaceClassDecl(cd: ir.ClassDecl) =
        at(cd, ()) {
            savingEnv {
                cd.methods.foreach(checkMethodDecl(cd, ir.Env.empty, _))
            }
        }
        
    def checkNoninterfaceClassDecl(cd: ir.ClassDecl) = 
        at(cd, ()) {
            savingEnv {
                cd.fields.foldLeft(Set.empty[ir.FieldName])(checkFieldDecl(cd))                
                val envs_ctor = cd.ctors.map(checkNoninterfaceConstructorDecl(cd, _))
                val env_all_ctors = ir.Env.intersect(envs_ctor)
                cd.methods.foreach(checkMethodDecl(cd, env_all_ctors, _))                    
            }
        }
            
    def checkClassDecl(cd: ir.ClassDecl) = {
        if(cd.attrs.interface) checkInterfaceClassDecl(cd)
        else checkNoninterfaceClassDecl(cd)        
    }
        
}