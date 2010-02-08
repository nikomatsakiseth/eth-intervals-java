package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Positional
import Util._

class TypeCheck(prog: Prog) extends TracksEnvironment(prog)
{    
    import prog.logStack.log
    import prog.logStack.indexLog
    import prog.logStack.at
    import prog.logStack.indexAt
    import prog.classDecl
    import prog.thisTref
    import prog.typeOriginallyDefiningMethod
    import prog.overriddenMethodSigs
    import prog.isSubclass
    import prog.strictSuperclasses
    
    // ___ Subtyping ________________________________________________________
    
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
            throw new CheckFailure("intervals.expected.subtype", tp_sub.p, tp_sub.wt, wt_sup)
    }
    
    // ___ Statement Stack __________________________________________________
    //
    // When checking method bodies, we use the statement stack to 
    // track the enclosing statements and handle loops, etc.
    
    class StmtStack(
        val stmt: ir.StmtCompound,
        val env_in: ir.TcEnv
    ) {
        var oenv_continue: Option[ir.TcEnv] = None  // only applies to While
        var oenv_break: Option[ir.TcEnv] = None     // applies to all
    }
    
    private var ss_cur = List[StmtStack]()
    
    def withStmt[R](stmt: ir.StmtCompound, env_in: ir.TcEnv)(func: => R) = {
        ss_cur = new StmtStack(stmt, env_in) :: ss_cur
        try { func } finally {
            ss_cur = ss_cur.tail
        }
    }
    
    // ___ Checking method bodies ___________________________________________
    
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
            throw new CheckFailure("intervals.requirement.not.met", req)
    }
    
    def checkArgumentTypes(msig: ir.MethodSig, tqs: List[ir.TeePee]) =
        foreachzip(tqs, msig.args.map(_.wt))(checkIsSubtype)
    
    def checkReadable(tp_guard: ir.TeePee) {
        if(!isReadableBy(tp_guard, tp_cur))
            throw new CheckFailure("intervals.not.readable", tp_guard.p)
    }
    
    def checkWritable(tp_guard: ir.TeePee) {
        if(!isWritableBy(tp_guard, tp_cur))
            throw new CheckFailure("intervals.not.writable", tp_guard.p)
    }
        
    def checkNoInvalidated() {
        if(!env.flow.ps_invalidated.isEmpty)
            throw new CheckFailure(
                "intervals.must.assign.first", 
                env.flow.ps_invalidated.mkEnglishString)        
    }

    def checkCallMsig(tp: ir.TeePee, msig: ir.MethodSig, tqs: List[ir.TeePee]) {
        // Cannot invoke a method when there are outstanding invalidated fields:
        checkNoInvalidated()
        
        // If method is not a constructor method, receiver must be constructed:
        if(!msig.as.ctor && tp.wt.as.ctor) 
            throw new CheckFailure("intervals.rcvr.must.be.constructed", tp.p)
            
        // Arguments must have correct type and requirements must be fulfilled:
        checkArgumentTypes(msig, tqs)
        msig.reqs.foreach(checkReqFulfilled)
        
        // Any method call disrupts potential temporary assocations:
        //     We make these disruptions before checking return value, 
        //     in case they would affect the type.  Haven't thought through
        //     if this can happen or not, but this would be the right time anyhow.
        clearTemp()        
    }
    
    def checkCall(x: ir.VarName, tp: ir.TeePee, m: ir.MethodName, tqs: List[ir.TeePee]) {
        val msig = substdMethodSig(tp, m, tqs)
        checkCallMsig(tp, msig, tqs)
        addLvDecl(x, msig.wt_ret, None)
    }
    
    def mapAndMergeBranchEnv(
        env_in: ir.TcEnv,       // environment on entry to the flow
        env_brk: ir.TcEnv,      // environment at branch statement
        decls: List[ir.LvDecl], // defined variables (phi variable declarations)
        tps: List[ir.TeePee]    // arguments to each phi
    ): ir.TcEnv = {
        val flow_brk = env_brk.flow
        
        // ----------------------------------------------------------------------
        // Check that tps have the correct types.
        
        // XXX For now, we don't check these.  These are used
        //     to carry SSA assignments forward, so any errors
        //     here SHOULD already have been reported anyhow.
        //     If re-enabling this check, uncomment the test blockBranchCheckTypes()
        //     in TestAnalysis.scala.
        //
        //decls.zip(tps).foreach { case (ir.LvDecl(x, wt), tp) =>
        //    checkIsSubtype(tp, wt)
        //}
        
        // ----------------------------------------------------------------------
        // Create a substitution which maps:
        // (1) Method-scope variables to themselves
        // (2) Arguments to the goto() to their respective parameters
        // (3) Everything else to "lv_outOfScope"

        val lv_outOfScope = ir.VarName(prog.fresh("outOfScope"))

        // Map all vars to outOfScope (unless overridden later)
        val map0 = env_brk.perm.keys.foldLeft(Map.empty[ir.Path, ir.Path]) { case (m, x) =>
            m + Pair(x.path, lv_outOfScope.path)
        }
    
        // Map variables shared with env_in to themselves:            
        val map1 = env_in.perm.keys.foldLeft(map0) { case (m, x) => 
            m + Pair(x.path, x.path)
        }

        // Map arguments 'tps' to 'decls':
        val xs_args = decls.map(_.name)
        val map2 = tps.zip(xs_args).foldLeft(map1) { case (m, (tp, x)) =>
            m + Pair(tp.p, x.path) 
        }

        // Map everything else to lv_outOfScope:
        val subst = new PathSubst(map2)

        // ----------------------------------------------------------------------
        // Apply map to relations and keep only those not affecting lv_outOfScope

        def mapMap(map: Map[ir.Path, ir.Path]) =
            map.elements.foldLeft(Map.empty[ir.Path, ir.Path]) { case (r, (p0, q0)) =>
                val p1 = subst.path(p0)
                val q1 = subst.path(q0)
                if(p1.lv != lv_outOfScope && q1.lv != lv_outOfScope) r + Pair(p1, q1)
                else r
            }

        def mapRelation[R <: Relation[ir.Path, R]](rel: R) =
            rel.mapFilter(subst.path, (_.lv != lv_outOfScope))
    
        def mapInvalidated(ps: Set[ir.Path]) =
            ps.map { case p0 =>
                val p1 = subst.path(p0)
                if(p1.lv == lv_outOfScope)
                    throw new CheckFailure("intervals.must.assign.first", p0)
                p1
            }

        env_in
        .addArgs(decls)
        .withFlow(
            FlowEnv(
                mapMap(flow_brk.temp),
                mapInvalidated(flow_brk.ps_invalidated),
                mapRelation(flow_brk.readable),
                mapRelation(flow_brk.writable),
                mapRelation(flow_brk.hb),
                mapRelation(flow_brk.subinterval),
                mapRelation(flow_brk.locks)
            )
        )
    }
    
    def intersect(oenv1: Option[ir.TcEnv], env2: ir.TcEnv) = oenv1 match {
        case None => Some(env2)
        case Some(env1) => Some(env1.intersectFlow(env2.flow))
    }

    def mergeBreakEnv(idx: Int, tps: List[ir.TeePee]) {
        log.indented("mergeBreakEnv(%s,%s)", idx, tps) {
            val ss = ss_cur(idx)

            val env_map = mapAndMergeBranchEnv(ss.env_in, env, ss.stmt.defines, tps)
            log.env("env_map: ", env_map)
            ss.oenv_break = intersect(ss.oenv_break, env_map)            
            if(ss.oenv_break.get != env_map)
                log.env("env_break: ", ss.oenv_break.get)
        }
    }
        
    def mergeContinueEnv(idx: Int, tps: List[ir.TeePee]) {
        log.indented("mergeContinueEnv(%s,%s)", idx, tps) {
            val ss = ss_cur(idx)
            ss.stmt.kind match {
                case ir.Loop(args, _, _) =>
                    val env_map = mapAndMergeBranchEnv(ss.env_in, env, args, tps)
                    log.env("env_map: ", env_map)
                    ss.oenv_continue = intersect(ss.oenv_continue, env_map)
                    if(ss.oenv_continue.get != env_map)
                        log.env("env_continue: ", ss.oenv_continue.get)
                case _ =>
                    throw new CheckFailure(
                        "intervals.internal.error",
                        "continue to non-loop: %s".format(ss))
            }
        }
    }
    
    def computeAndLogLoopArguments(args: List[ir.LvDecl], ps_initial: List[ir.Path]) = {
        log.indented("Loop Arguments:") {
            args.zip(ps_initial).map { case (arg, p) =>
                log.indented("%s = %s", arg, p) {
                    teePee(ir.noAttrs, p)
                }                
            }
        }        
    }
    
    def checkHeadCompoundStatement() {
        val ss = ss_cur.head
        val stmt_compound = ss.stmt
        
        val ft = stmt_compound.kind match {
            case ir.Block(seq) =>
                checkStatementSeq(seq)

            case ir.Switch(seqs) =>
                val env_initial = env
                seqs.foreach { seq =>
                    // If the previous stmt breaks, then 
                    // env == env_initial and this line has no effect.
                    setEnv(env.intersectFlow(env_initial.flow))
                    checkStatementSeq(seq)
                }
                
            case ir.Loop(args, ps_initial, seq) =>
                def iterate(env_continue_before: ir.TcEnv) {
                    log.indented("iterate()") {
                        // Break env is recomputed each iteration:
                        ss.oenv_break = None

                        // Perform an iteration:
                        setEnv(env_continue_before)  
                        checkStatementSeq(seq)
                    }

                    // Repeat until steady state is reached:
                    val env_continue_after = ss.oenv_continue.get
                    if(env_continue_before != env_continue_after)
                        iterate(env_continue_after)
                }
                val tps_initial = computeAndLogLoopArguments(args, ps_initial)
                mergeContinueEnv(0, tps_initial)
                iterate(ss.oenv_continue.get)
                
            case ir.Subinterval(x, ps_locks, seq) =>
                val tps_locks = teePee(ir.noAttrs, ps_locks)                    
                addLvDecl(x, ir.t_interval, None)                
                val tp_x = teePee(x.path)
                addSubintervalOf(tp_x, tp_cur)
                tps_locks.foreach(addLocks(tp_x, _))
                
                // n.b.: We don't try to pop-- that's because if
                // statement seq returns, the environment will be reset to ss.env_in.
                // In any case we will reset environment below so it doens't matter.
                pushCurrent(x.path)
                checkStatementSeq(seq)
                
            case ir.TryCatch(seq_try, seq_catch) =>
                savingEnv { checkStatementSeq(seq_try) }
                
                // Catch conservatively assumes try failed immediately:
                savingEnv { checkStatementSeq(seq_catch) }
        }
        
        log.ifEnabled {
            log.indented("Defines:") {
                stmt_compound.defines.foreach(log(_))
            }            
        }

        ss.oenv_break match {
            case None => // control flow never really comes this way...
                setEnv(ss.env_in) // ...use safe approx.
                log("oenv_break unset, used env_in")
            case Some(env_break) => 
                setEnv(env_break)
                log.env("oenv_break: ", env_break)
        }
    }
    
    def checkStatement(stmt: ir.Stmt) = indexAt(stmt, ()) {
        log.env("Input Environment: ", env)
        stmt match {   
            case stmt_compound: ir.StmtCompound =>
                withStmt(stmt_compound, env) {
                    checkHeadCompoundStatement()
                }
                           
            case ir.StmtSuperCtor(m, qs) =>
                val tp = tp_super
                val tqs = teePee(ir.noAttrs, qs)
                val msig_ctor = substdCtorSig(tp, m, tqs)
                checkCallMsig(tp_super, msig_ctor, tqs)
                
                // Supertype must have been processed first:
                log("Supertype: %s", tp)
                setEnv(env.addFlow(prog.exportedCtorEnvs((tp.wt.c, m))))
                
            case ir.StmtGetField(x, p_o, f) =>
                val tp_o = teePee(ir.noAttrs, p_o)
                substdFieldDecl(tp_o, f) match {
                    case ir.GhostFieldDecl(_, _) =>
                        throw new CheckFailure("intervals.not.reified", tp_o.wt.c, f)
                    
                    case ir.ReifiedFieldDecl(_, wt, _, p_guard) =>
                        val tp_guard = teePee(ir.ghostAttrs, p_guard)                    
                        checkReadable(tp_guard)
                        
                        val p_f = tp_o.p + f // Canonical path of f; valid as f is not a ghost.

                        val op_canon = 
                            if(hbInter(tp_guard, tp_cur)) { // Constant:
                                Some(p_f)
                            } else { // Non-constant:
                                addTemp(p_f, x.path) // Record that p.f == vd, for now.
                                None
                            }

                        addLvDecl(x, wt, op_canon)
                }                    
                
            case ir.StmtSetField(p_o, f, p_v) =>
                val tp_o = teePee(ir.noAttrs, p_o)
                val tp_v = teePee(ir.noAttrs, p_v)
                
                substdFieldDecl(tp_o, f) match {
                    case ir.GhostFieldDecl(_, _) => 
                        throw new CheckFailure("intervals.not.reified", tp_o.wt.c, f)
                    
                    case ir.ReifiedFieldDecl(_, wt, _, p_guard) =>
                        val tp_guard = teePee(ir.ghostAttrs, p_guard)
                        checkWritable(tp_guard)                    
                        checkIsSubtype(tp_v, wt)
                        
                        val p_f = tp_o.p + f // Canonical path of f; valid as f is not a ghost.
                        addTemp(p_f, tp_v.p)
                
                        removeInvalidated(p_f)
                        linkedPaths(tp_o, f).foreach(addInvalidated)                            
                }
                
            case ir.StmtCheckType(p, wt) =>
                val tp = teePee(ir.noAttrs, p)
                checkIsSubtype(tp, wt)
                    
            case ir.StmtCall(x, p, m, qs) =>
                val tp = teePee(ir.noAttrs, p)
                val tqs = teePee(ir.noAttrs, qs)
                checkCall(x, tp, m, tqs)
    
            case ir.StmtSuperCall(x, m, qs) =>
                val tqs = teePee(ir.noAttrs, qs)
                checkCall(x, tp_super, m, tqs)
            
            case ir.StmtNew(x, t, m, qs) =>
                val cd = classDecl(t.c)
                val tqs = teePee(ir.noAttrs, qs)
                
                if(cd.attrs.interface)
                    throw new CheckFailure("intervals.new.interface", t.c)
                    
                // Check Ghost Types:           
                addLvDecl(x, t, None)
                val tp_x = teePee(x.path)
                t.ghosts.foreach { g =>
                    val gfd = substdFieldDecl(tp_x, g.f)
                    val tp = teePee(g.p)
                    checkIsSubtype(tp, gfd.wt)
                }                        
                                
                val msig_ctor = substdCtorSig(tp_x, m, tqs)
                checkCallMsig(teePee(x.path), msig_ctor, tqs)
                
            case ir.StmtCast(x, wt, q) => ()
                // TODO Validate casts?  Issue warnings at least?
                addLvDecl(x, wt, None)
                
            case ir.StmtNull(x, wt) => 
                addLvDecl(x, wt, None)
                
            case ir.StmtReturn(op) =>
                checkNoInvalidated()
                op.foreach { p =>
                    val tp = teePee(ir.noAttrs, p)
                    checkIsSubtype(tp, env.wt_ret)                    
                }
                if(!ss_cur.isEmpty) {
                    mergeBreakEnv(ss_cur.length - 1, List())
                    setEnv(ss_cur.head.env_in)                    
                }
                
            case ir.StmtHb(p, q) =>
                val tp = teePee(ir.noAttrs, p)
                val tq = teePee(ir.noAttrs, q)
                addUserHb(tp, tq)
            
            case ir.StmtLocks(p, q) =>
                val tp = teePee(ir.noAttrs, p)
                val tq = teePee(ir.noAttrs, q)
                addLocks(tp, tq)
            
            case ir.StmtCondBreak(i, ps) =>
                val tps = teePee(ir.noAttrs, ps)
                mergeBreakEnv(i, tps)

            case ir.StmtBreak(i, ps) =>
                val tps = teePee(ir.noAttrs, ps)
                mergeBreakEnv(i, tps)
                setEnv(ss_cur.head.env_in)
            
            case ir.StmtContinue(i, ps) =>
                val tps = teePee(ir.noAttrs, ps)
                mergeContinueEnv(i, tps)
                setEnv(ss_cur.head.env_in)
        }
    }
    
    def checkStatementSeq(seq: ir.StmtSeq) {
        log.indented("checkStatementSeq(%s)", seq) {
            seq.stmts.foreach(checkStatement)            
        }
    }
    
        
    // ___ Inter-method assumptions _________________________________________
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
    ): FlowEnv = savingEnv {        
        log.indented("extractAssumptions(%s,%s)", tp_mthd, lvs_shared) {
            withCurrent(freshTp(ir.t_interval).p) {                
                addHbInter(tp_mthd, tp_cur)
                
                log.env("Input Environment: ", env)

                val tempKeys = flow.temp.map(_._1).toList
                val tempValues = flow.temp.map(_._2).toList
                val mapFunc = PathSubst.pp(tempValues, tempKeys).path(_)
                
                def filterFunc(p: ir.Path): Boolean = 
                    log.indentedRes("filterFunc(%s)", p) {
                        lvs_shared(p.lv) && !teePee(p).as.mutable                
                    }

                FlowEnv.empty
                    .withReadable(flow.readable.mapFilter(mapFunc, filterFunc))
                    .withWritable(flow.writable.mapFilter(mapFunc, filterFunc))
                    .withHb(flow.hb.mapFilter(mapFunc, filterFunc))
                    .withSubinterval(flow.subinterval.mapFilter(mapFunc, filterFunc))
                    .withLocks(flow.locks.mapFilter(mapFunc, filterFunc))
            }            
        }
    }
    
    // ___ Checking methods and constructors ________________________________

    def checkArgumentTypesNonvariant(args_sub: List[ir.LvDecl], args_sup: List[ir.LvDecl]) {
        foreachzip(args_sub, args_sup) { case (arg_sub, arg_sup) =>
            if(arg_sub.wt != arg_sup.wt)
                throw new CheckFailure(
                    "intervals.override.param.type.changed", 
                    arg_sub.name, arg_sub.wt, arg_sup.wt)
        }        
    }
    
    def checkReturnTypeCovariant(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef) {
        if(!isSubtype(freshTp(wt_sub), wt_sup))
            throw new CheckFailure(
                "intervals.override.ret.type.changed", wt_sub, wt_sup)
    }
    
    def checkOverridenReqsImplyOurReqs(reqs_sub: List[ir.Req], reqs_sup: List[ir.Req]) {
        reqs_sup.foreach(addReq)
        reqs_sub.foreach { req_sub =>
            if(!isReqFulfilled(req_sub))
                throw new CheckFailure("intervals.override.adds.req", req_sub)
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
    
    def checkMethodBody(md: ir.MethodDecl) {
        assert(ss_cur.isEmpty)
        checkStatementSeq(md.body)
        assert(ss_cur.isEmpty)
    }
    
    def checkMethodDecl(
        cd: ir.ClassDecl,           // class in which the method is declared
        flow_ctor_assum: FlowEnv,   // relations established by the ctor
        md: ir.MethodDecl           // method to check
    ) = 
        indexAt(md, ()) {
            savingEnv {
                // Define special vars "method" and "this":
                addPerm(ir.lv_mthd, ir.TeePee(ir.t_interval, ir.p_mthd, ir.ghostAttrs))
                pushCurrent(ir.p_mthd)
                
                if(!md.attrs.ctor) { 
                    // For normal methods, type of this is the defining class
                    addPerm(ir.lv_this, ir.TeePee(thisTref(cd), ir.p_this, ir.noAttrs))                     
                    addHbInter(tp_ctor, tp_cur)             // ... constructor already happened
                    setEnv(env.addFlow(flow_ctor_assum))    // ... add its effects on environment
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
                checkMethodBody(md)
            }                     
        }
        
    def checkNoninterfaceConstructorDecl(cd: ir.ClassDecl, md: ir.MethodDecl) = 
        indexAt(md, FlowEnv.empty) {
            savingEnv {
                // Define special vars "method" (== this.constructor) and "this":
                addPerm(ir.lv_mthd, ir.TeePee(ir.t_interval, ir.gfd_ctor.thisPath, ir.ghostAttrs))
                pushCurrent(ir.p_mthd)
                addPerm(ir.lv_this, ir.TeePee(thisTref(cd, ir.ctorAttrs), ir.p_this, ir.noAttrs))

                // Check method body:
                md.args.foreach(addArg)
                md.reqs.foreach(addReq)
                checkMethodBody(md)
                
                // Compute exit assumptions and store in prog.exportedCtorEnvs:
                val flow = extractAssumptions(tp_ctor, Set(ir.lv_this)) // Globally valid assums
                log.flow("extracted assumptions:", flow)
                prog.exportedCtorEnvs += Pair((cd.name, md.name), flow) // Store
                flow
            }
        }
    
    def checkReifiedFieldDecl(cd: ir.ClassDecl, fd: ir.ReifiedFieldDecl) = 
        indexAt(fd, ()) {
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
                    throw new CheckFailure("intervals.invalid.guard.type", tp_guard.wt)                    
                
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
                                        throw new CheckFailure(
                                            "intervals.illegal.type.dep",
                                            tp_dep.p, tp_guard.p)

                                case ir.Path(lv, f :: rev_fs) =>
                                    check(ir.Path(lv, rev_fs))
                                    val tp_dep = teePee(p_dep)
                                    if(!tp_dep.isConstant)
                                        throw new CheckFailure(
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
        indexAt(gfd, ()) {
            log.indented(gfd) {
                // Check that ghosts are not shadowed from a super class:
                strictSuperclasses(cd.name).foreach { c =>
                    if(classDecl(c).fields.exists(_.name == gfd.name))
                        throw new CheckFailure("intervals.shadowed.ghost", c, gfd.name)
                }
            }
        }
        
    def checkFieldDecl(cd: ir.ClassDecl)(priorNames: Set[ir.FieldName], fd: ir.FieldDecl) = 
        indexAt(fd, priorNames) {
            if(priorNames(fd.name))
                throw new CheckFailure("intervals.duplicate.field", fd.name)
            fd match {
                case rfd: ir.ReifiedFieldDecl => checkReifiedFieldDecl(cd, rfd)
                case gfd: ir.GhostFieldDecl => checkGhostFieldDecl(cd, gfd)
            }
            priorNames + fd.name
        }            
        
    // ___ Classes and interfaces ___________________________________________
    
    def checkInterfaceClassDecl(cd: ir.ClassDecl) =
        indexAt(cd, ()) {
            savingEnv {
                cd.methods.foreach(checkMethodDecl(cd, FlowEnv.empty, _))
            }
        }            
        
    def checkNoninterfaceClassDecl(cd: ir.ClassDecl) = 
        indexAt(cd, ()) {
            savingEnv {
                cd.fields.foldLeft(Set.empty[ir.FieldName])(checkFieldDecl(cd))                
                val flows_ctor = cd.ctors.map(checkNoninterfaceConstructorDecl(cd, _))
                val flow_all_ctors = FlowEnv.intersect(flows_ctor)
                cd.methods.foreach(checkMethodDecl(cd, flow_all_ctors, _))                    
            }
        }
            
    def checkClassDecl(cd: ir.ClassDecl) = {
        if(cd.attrs.interface) checkInterfaceClassDecl(cd)
        else checkNoninterfaceClassDecl(cd)        
    }
        
}