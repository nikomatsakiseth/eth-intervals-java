package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Positional
import Util._

class TypeCheck(prog: Prog) extends CheckPhase(prog)
{    
    import prog.logStack.log
    import prog.logStack.indexLog
    import prog.logStack.at
    import prog.logStack.indexAt
    import prog.classDecl
    import prog.strictSuperclasses
    
    /// wt(cp_sub) <: wt_sup
    def checkIsSubtype(env: TcEnv, crp_sub: ir.CanonReifiedPath, wt_sup: ir.WcTypeRef) {
        if(!env.pathHasType(crp_sub, wt_sup))
            throw new CheckFailure("intervals.expected.subtype", crp_sub.p, crp_sub.wt, wt_sup)
    }
    
    // ___ Statement Stack __________________________________________________
    //
    // When checking method bodies, we use the statement stack to 
    // track the enclosing statements and handle loops, etc.
    
    class StmtStack(
        val stmt: ir.StmtCompound,
        val env_in: TcEnv
    ) {
        var oenv_continue: Option[TcEnv] = None  // only applies to While
        var oenv_break: Option[TcEnv] = None     // applies to all
    }
    
    private var ss_cur = List[StmtStack]()
    
    def withStmt[R](stmt: ir.StmtCompound, env_in: TcEnv)(func: => R) = {
        ss_cur = new StmtStack(stmt, env_in) :: ss_cur
        try { func } finally {
            ss_cur = ss_cur.tail
        }
    }
    
    // ___ Checking method bodies ___________________________________________
    
    def isReqFulfilled(env: TcEnv, req: ir.Req): Boolean = log.indented("isReqFulfilled(%s)", req) {
        def is(func: Function2[ir.CanonPath, ir.CanonPath, Boolean], ps: List[ir.Path], qs: List[ir.Path]) = {
            // XXX These args need not be immutable (at least not in all cases).
            val cps = env.immutableGhost(ps)
            val cqs = env.immutableGhost(qs)
            forallcross(cps, cqs)(func)
        }
        req match {
            case ir.ReqWritableBy(ps, qs) => is(env.guardsDataWritableBy, ps, qs)
            case ir.ReqReadableBy(ps, qs) => is(env.guardsDataReadableBy, ps, qs)
            case ir.ReqSubintervalOf(ps, qs) => is(env.isSubintervalOf, ps, qs)
            case ir.ReqHb(ps, qs) => is(env.userHb, ps, qs)
        }
    }
    
    def checkReqFulfilled(env: TcEnv, req: ir.Req) {
        if(!isReqFulfilled(env, req))
            throw new CheckFailure("intervals.requirement.not.met", req)
    }
    
    def checkArgumentTypes(env: TcEnv, msig: ir.MethodSig, cqs: List[ir.CanonReifiedPath]) =
        foreachzip(cqs, msig.wts_args)(checkIsSubtype(env, _, _))
    
    def checkReadable(env: TcEnv, cp_guard: ir.CanonPath) {
        if(!env.guardsDataReadableBy(cp_guard, env.cp_cur))
            throw new CheckFailure("intervals.not.readable", cp_guard.p)
    }
    
    def checkWritable(env: TcEnv, cp_guard: ir.CanonPath) {
        if(!env.guardsDataWritableBy(cp_guard, env.cp_cur))
            throw new CheckFailure("intervals.not.writable", cp_guard.p)
    }
        
    def checkNoInvalidated(env: TcEnv) {
        if(!env.flow.ps_invalidated.isEmpty)
            throw new CheckFailure(
                "intervals.must.assign.first", 
                env.flow.ps_invalidated.mkEnglishString)        
    }

    def processCallMsig(env: TcEnv, tcp: ir.TeeCeePee[_], msig: ir.MethodSig, cqs: List[ir.CanonReifiedPath]) = {
        log.indented("processCallMsig(...)") {
            log.methodSig(false, "msig", msig)
            
            // Cannot invoke a method when there are outstanding invalidated fields:
            checkNoInvalidated(env)

            // Receiver/Arguments must have correct type and requirements must be fulfilled:
            log.indented("arguments")   { checkArgumentTypes(env, msig, cqs) }
            log.indented("reqs")        { msig.reqs.foreach(checkReqFulfilled(env, _)) }

            // Any method call disrupts potential temporary assocations:
            //     We make these disruptions before checking return value, 
            //     in case they would affect the type.  Haven't thought through
            //     if this can happen or not, but this would be the right time anyhow.
            env.clearTemp()            
        }
    }
    
    def processCall(env0: TcEnv, x: ir.VarName, tcp: ir.TeeCeePee[ir.WcTypeRef], m: ir.MethodName, cqs: List[ir.CanonReifiedPath]) = {
        var env = env0
        val msig = env.substdMethodSig(tcp, m, cqs)
        env = processCallMsig(env, tcp, msig, cqs)
        env.addReifiedLocal(x, msig.wt_ret)
    }
    
    def mapAndMergeBranchEnv(
        env_in: TcEnv,          // environment on entry to the flow
        env_brk: TcEnv,         // environment at branch statement
        decls: List[ir.LvDecl], // defined variables (phi variable declarations)
        cps: List[ir.CanonPath] // arguments to each phi
    ): TcEnv = {
        val flow_brk = env_brk.flow
        
        // ----------------------------------------------------------------------
        // Check that cps have the correct types.
        
        // XXX For now, we don't check these.  These are used
        //     to carry SSA assignments forward, so any errors
        //     here SHOULD already have been reported anyhow.
        //     If re-enabling this check, uncomment the test blockBranchCheckTypes()
        //     in TestAnalysis.scala.
        //
        //decls.zip(cps).foreach { case (ir.LvDecl(x, wt), cp) =>
        //    checkIsSubtype(env, cp, wt)
        //}
        
        // ----------------------------------------------------------------------
        // Create a substitution which maps:
        // (1) Method-scope variables to themselves
        // (2) Arguments to the goto() to their respective parameters
        // (3) Everything else to "lv_outOfScope"

        val lv_outOfScope = ir.VarName(prog.fresh("outOfScope"))

        // Map all vars to outOfScope (unless overridden later)
        val map0 = env_brk.perm.keysIterator.foldLeft(Map.empty[ir.Path, ir.Path]) { case (m, x) =>
            m + Pair(x.path, lv_outOfScope.path)
        }
    
        // Map variables shared with env_in to themselves:            
        val map1 = env_in.perm.keysIterator.foldLeft(map0) { case (m, x) => 
            m + Pair(x.path, x.path)
        }

        // Map arguments 'cps' to 'decls':
        val xs_args = decls.map(_.name)
        val map2 = cps.zip(xs_args).foldLeft(map1) { case (m, (cp, x)) =>
            m + Pair(cp.p, x.path) 
        }

        // Map everything else to lv_outOfScope:
        val subst = new PathSubst(map2)

        // ----------------------------------------------------------------------
        // Apply map to relations and keep only those not affecting lv_outOfScope
        
        def inScope(p: ir.Path) = (p.lv != lv_outOfScope)

        def mapMap(map: Map[ir.Path, ir.Path]) =
            map.iterator.foldLeft(Map.empty[ir.Path, ir.Path]) { case (r, (p0, q0)) =>
                val p1 = subst.path(p0)
                val q1 = subst.path(q0)
                if(inScope(p1) && inScope(q1)) r + Pair(p1, q1)
                else r
            }

        def mapPathRelation(rel: PathRelation) =
            flow_brk.mapFilter(rel, subst.path, inScope)
    
        def mapInvalidated(ps: Set[ir.Path]) =
            ps.map { case p0 =>
                val p1 = subst.path(p0)
                if(inScope(p1))
                    p1
                else
                    throw new CheckFailure("intervals.must.assign.first", p0)
            }

        env_in
        .addArgs(decls)
        .copy(flow = 
            FlowEnv(
                flow_brk.nonnull.map(subst.path).filter(inScope),
                mapMap(flow_brk.temp),
                mapInvalidated(flow_brk.ps_invalidated),
                mapPathRelation(flow_brk.readableRel),
                mapPathRelation(flow_brk.writableRel),
                mapPathRelation(flow_brk.hbRel),
                mapPathRelation(flow_brk.subintervalRel),
                mapPathRelation(flow_brk.locksRel)
            )
        )
    }
    
    def intersect(oenv1: Option[TcEnv], env2: TcEnv) = oenv1 match {
        case None => Some(env2)
        case Some(env1) => Some(env1.intersectFlow(env2.flow))
    }

    def mergeBreakEnv(env: TcEnv, idx: Int, cps: List[ir.CanonPath]) {
        log.indented("mergeBreakEnv(%s,%s)", idx, cps) {
            val ss = ss_cur(idx)

            val env_map = mapAndMergeBranchEnv(ss.env_in, env, ss.stmt.defines, cps)
            log.env(false, "env_map: ", env_map)
            ss.oenv_break = intersect(ss.oenv_break, env_map)            
            if(ss.oenv_break.get != env_map)
                log.env(false, "env_break: ", ss.oenv_break.get)
        }
    }
        
    def mergeContinueEnv(env: TcEnv, idx: Int, cps: List[ir.CanonPath]) {
        log.indented("mergeContinueEnv(%s,%s)", idx, cps) {
            val ss = ss_cur(idx)
            ss.stmt.kind match {
                case ir.Loop(args, _, _) =>
                    val env_map = mapAndMergeBranchEnv(ss.env_in, env, args, cps)
                    log.env(false, "env_map: ", env_map)
                    ss.oenv_continue = intersect(ss.oenv_continue, env_map)
                    if(ss.oenv_continue.get != env_map)
                        log.env(false, "env_continue: ", ss.oenv_continue.get)
                case _ =>
                    throw new CheckFailure(
                        "intervals.internal.error",
                        "continue to non-loop: %s".format(ss))
            }
        }
    }
    
    def computeAndLogLoopArguments(env: TcEnv, args: List[ir.LvDecl], ps_initial: List[ir.Path]) = {
        log.indented("Loop Arguments:") {
            args.zip(ps_initial).map { case (arg, p) =>
                log.indented("%s = %s", arg, p) {
                    env.immutableReified(p)
                }                
            }
        }        
    }
    
    def checkHeadCompoundStatement(env0: TcEnv): TcEnv = {
        var env = env0
        val ss = ss_cur.head
        val stmt_compound = ss.stmt
        
        stmt_compound.kind match {
            case ir.Block(seq) =>
                env = checkStatementSeq(env, seq)

            case ir.Switch(seqs) =>
                val env_initial = env
                seqs.foreach { seq =>
                    // If the previous stmt breaks, then 
                    // env == env_initial and this line has no effect.
                    env = env.intersectFlow(env_initial.flow)
                    env = checkStatementSeq(env, seq)
                }
                
            case ir.Loop(args, ps_initial, seq) =>
                def iterate(env_continue_before: TcEnv) {
                    log.indented("iterate()") {
                        // Break env is recomputed each iteration:
                        ss.oenv_break = None

                        // Perform an iteration:
                        env = checkStatementSeq(env_continue_before, seq)
                    }

                    // Repeat until steady state is reached:
                    val env_continue_after = ss.oenv_continue.get
                    if(env_continue_before != env_continue_after)
                        iterate(env_continue_after)
                }
                val tps_initial = computeAndLogLoopArguments(env, args, ps_initial)
                mergeContinueEnv(env, 0, tps_initial)
                iterate(ss.oenv_continue.get)
                
            case ir.Subinterval(x, ps_locks, seq) =>
                val tps_locks = env.immutableReified(ps_locks)                    
                env = env.addReifiedLocal(x, ir.wt_constructedInterval)
                val cp_x = env.canon(x.path)
                env = env.addNonNull(cp_x)
                env = env.addSubintervalOf(cp_x, env.cp_cur)
                env = tps_locks.foldLeft(env)(_.addLocks(cp_x, _))
                
                // n.b.: We don't try to pop-- that's because if
                // statement seq returns, the environment will be reset to ss.env_in.
                // In any case we will reset environment below so it doens't matter.
                env = env.withCurrent(cp_x)
                env = checkStatementSeq(env, seq)
                
            case ir.TryCatch(seq_try, seq_catch) =>
                checkStatementSeq(env, seq_try) // note: environment is lost unless it breaks
                
                // Catch conservatively assumes try failed immediately:
                checkStatementSeq(env, seq_catch)
        }
        
        log.ifEnabled {
            log.indented("Defines:") {
                stmt_compound.defines.foreach(log(_))
            }            
        }

        ss.oenv_break match {
            case None => // control flow never really comes this way...
                log("oenv_break unset, used env_in")
                ss.env_in // ...use safe approx.
            case Some(env_break) => 
                log.env(false, "env_break: ", env_break)
                env_break
        }
    }
    
    def checkStatement(env0: TcEnv, stmt: ir.Stmt) = indexAt(stmt, env0) {
        var env = env0
        log.env(false, "Input Environment: ", env)
        stmt match {   
            case stmt_compound: ir.StmtCompound =>
                withStmt(stmt_compound, env) {
                    checkHeadCompoundStatement(env)
                }
                           
            case ir.StmtSuperCtor(m, qs) =>
                val tcp = env.tcp_super
                val cqs = env.immutableReified(qs)
                val msig_ctor = env.substdCtorSig(tcp, m, cqs)
                log("tcp = %s", tcp)
                log("msig_ctor = %s", msig_ctor)
                env = processCallMsig(env, tcp, msig_ctor, cqs)
                
                // Ctors for all supertypes now complete:
                strictSuperclasses(env.c_cur).foreach { case c =>
                    val cp_cCtor = env.canon(ir.ClassCtorFieldName(c).thisPath)
                    env = env.addHbInter(cp_cCtor, env.cp_cur)
                }
                
                // Supertype must have been processed first:
                log("Supertype: %s", tcp)
                env.addFlow(prog.exportedCtorEnvs((tcp.ty.c, m)))
                
            case ir.StmtGetField(x, p_o, f) =>
                val cp_o = env.immutableReified(p_o)
                env = env.addNonNull(cp_o)
                
                val (_, rfd) = env.substdReifiedFieldDecl(cp_o.toTcp, f)
                val cp_guard = env.immutableGhost(rfd.p_guard)
                checkReadable(env, cp_guard)
                
                val cp_f = env.extendCanonWithReifiedField(cp_o, rfd)
                if(!env.isMutable(cp_f)) {
                    env.addPerm(x, cp_f)                            
                } else {
                    env = env.addReifiedLocal(x, rfd.wt)
                    env.addTemp(cp_f.p, x.path) // Record that p.f == vd, for now.
                }
                
            case ir.StmtSetField(p_o, f, p_v) =>
                val cp_o = env.immutableReified(p_o)
                env = env.addNonNull(cp_o)
                
                val cp_v = env.immutableReified(p_v)
                
                val (tcp_o, rfd) = env.substdReifiedFieldDecl(cp_o.toTcp, f) 
                val cp_guard = env.immutableGhost(rfd.p_guard)
                checkWritable(env, cp_guard)
                checkIsSubtype(env, cp_v, rfd.wt)
                
                val cp_f = env.extendCanonWithReifiedField(cp_o, rfd)                        
                env = env.addTemp(cp_f.p, cp_v.p)
        
                env = env.removeInvalidated(cp_f.p)
                env.linkedPaths(tcp_o, f).foldLeft(env)(_ addInvalidated _)                            
                
            case ir.StmtCheckType(p, wt) =>
                val cp = env.immutableReified(p)
                checkIsSubtype(env, cp, wt)
                env
                    
            case ir.StmtCall(x, p, m, qs) =>
                val cp = env.immutableReified(p)
                env = env.addNonNull(cp)
                val cqs = env.immutableReified(qs)
                env = processCall(env, x, cp.toTcp, m, cqs)
                env
    
            case ir.StmtSuperCall(x, m, qs) =>
                val cqs = env.immutableReified(qs)
                env = processCall(env, x, env.tcp_super, m, cqs)
                env
            
            case ir.StmtNew(x, ct0, m, qs) =>
                val cd = classDecl(ct0.c)
                val cqs = env.immutableReified(qs)
                
                if(cd.attrs.interface)
                    throw new CheckFailure("intervals.new.interface", ct0.c)
                    
                // Supply a default ghost (the current interval) for @Constructor:
                val gs_default = List(ir.Ghost(ir.f_objCtor, env.cp_cur.p))
                val ct = ct0.withDefaultGhosts(gs_default)
                    
                // Check Ghost Types:           
                env = env.addReifiedLocal(x, ct)
                val crp_x = env.immutableReified(x.path)
                val tcp_x = ir.TeeCeePee(crp_x, ct)
                env = env.addNonNull(crp_x)
                ct.ghosts.foreach { g =>
                    env.ghostFieldDecls(ct).find(_.isNamed(g.f)) match {
                        case Some(gfd) => 
                            val cp = env.canon(g.p)
                            if(!env.pathHasSubclass(cp, gfd.c))
                                throw new CheckFailure("intervals.must.be.subclass", cp, gfd.c)
                        case None if (g.f == ir.f_objCtor) =>
                            val cp = env.canon(g.p)
                            if(!env.isSubintervalOf(env.cp_cur, cp)) // also implies must be an interval
                                throw new CheckFailure("intervals.ctor.must.encompass.current", cp, env.cp_cur)
                        case None =>
                            throw new CheckFailure("intervals.internal.error", "No ghost field %s".format(g.f))
                    }
                }                        
                                
                val msig_ctor = env.substdCtorSig(tcp_x, m, cqs)
                processCallMsig(env, tcp_x, msig_ctor, cqs)
                
            case ir.StmtCast(x, wt, q) => ()
                // TODO Validate casts?  Issue warnings at least?
                env.addReifiedLocal(x, wt)
                
            case ir.StmtNull(x, wt) => 
                env.addReifiedLocal(x, wt)
                
            case ir.StmtReturn(op) =>
                checkNoInvalidated(env)
                op.foreach { p =>
                    val cp = env.immutableReified(p)
                    checkIsSubtype(env, cp, env.wt_ret)                    
                }
                if(!ss_cur.isEmpty) {
                    mergeBreakEnv(env, ss_cur.length - 1, List())
                    ss_cur.head.env_in
                } else
                    env
                
            case ir.StmtHb(p, q) =>
                val cp = env.immutableReified(p)
                val cq = env.immutableReified(q)
                env.addUserHb(cp, cq)
            
            case ir.StmtLocks(p, q) =>
                val cp = env.immutableReified(p)
                val cq = env.immutableReified(q)
                env.addLocks(cp, cq)
            
            case ir.StmtCondBreak(i, ps) =>
                val cps = env.immutableReified(ps)
                mergeBreakEnv(env, i, cps)
                env

            case ir.StmtBreak(i, ps) =>
                val cps = env.immutableReified(ps)
                mergeBreakEnv(env, i, cps)
                ss_cur.head.env_in
            
            case ir.StmtContinue(i, ps) =>
                val cps = env.immutableReified(ps)
                mergeContinueEnv(env, i, cps)
                ss_cur.head.env_in
        }
    }
    
    def checkStatementSeq(env0: TcEnv, seq: ir.StmtSeq) = {
        log.indented("checkStatementSeq(%s)", seq) {
            val cp_curOnEntry = env0.cp_cur
            log("cp_curOnEntry: %s", cp_curOnEntry)
            val (cp_seqInterval, env1) = env0.freshCp(ir.c_interval)
            log("cp_seqInterval: %s", cp_seqInterval)
            var env = env1.addSubintervalOf(cp_seqInterval, env1.cp_cur)
            env = env.withCurrent(cp_seqInterval)
            
            seq.stmts.foldLeft[Option[ir.CanonPath]](None) { (ocp_prevInterval, stmt) =>
                val (cp_stmtInterval, env2) = env.freshCp(ir.c_interval)
                log("cp_stmtInterval: %s", cp_stmtInterval)
                env = env2.addSubintervalOf(cp_stmtInterval, cp_seqInterval)
                ocp_prevInterval.foreach { cp_prevInterval =>
                    env = env.addHbInter(cp_prevInterval, cp_stmtInterval)
                }
                env = env.withCurrent(cp_stmtInterval)
                env = checkStatement(env, stmt)
                Some(cp_stmtInterval)
            }
            
            env.withCurrent(cp_curOnEntry)
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
        env0: TcEnv,
        lvs_shared: Set[ir.VarName]
    ): FlowEnv = {
        var env = env0
        log.indented("extractAssumptions(%s)", lvs_shared) {
            val (cp_fresh, env_fresh) = env.freshCp(ir.c_interval)
            env = env_fresh.withCurrent(cp_fresh)
            env = env.addHbInter(env.cp_mthd, env.cp_cur)
            
            log.env(false, "Input Environment: ", env)

            val flow = env.flow
            val tempKeys = flow.temp.map(_._1).toList
            val tempValues = flow.temp.map(_._2).toList
            val mapFunc = PathSubst.pp(tempValues, tempKeys).path(_)
            
            def filterFunc(p: ir.Path): Boolean = 
                log.indented("filterFunc(%s)", p) {
                    lvs_shared(p.lv) && !env.isMutable(env.canon(p))
                }

            // The HB often contains paths like this.Constructor[C].end.
            // These paths cannot be directly canonicalized (right now) because 
            // we cannot handle fields of intervals etc.  Therefore we just
            // canonicalize the interval part and check that IT is immutable.
            // This is safe because we know that intervals never publish their
            // start, end fields until they are immutable, but overall is non-ideal.
            def filterHbFunc(p: ir.Path): Boolean = 
                log.indented("filterFunc(%s)", p) {
                    lvs_shared(p.lv) && (
                        p match {
                            case ir.Path(lv, f :: fs_rev) if (f == ir.f_start || f == ir.f_end) =>
                                val p0 = ir.Path(lv, fs_rev)
                                !env.isMutable(env.canon(p0))
                            case _ => 
                                !env.isMutable(env.canon(p))
                        }
                    )
                }

            FlowEnv.empty
            .withReadableRel(flow.mapFilter(flow.readableRel, mapFunc, filterFunc))
            .withWritableRel(flow.mapFilter(flow.writableRel, mapFunc, filterFunc))
            .withHbRel(flow.mapFilter(flow.hbRel, mapFunc, filterHbFunc))
            .withSubintervalRel(flow.mapFilter(flow.subintervalRel, mapFunc, filterFunc))
            .withLocksRel(flow.mapFilter(flow.locksRel, mapFunc, filterFunc))
        }
    }
    
    // ___ Checking methods and constructors ________________________________

    def checkArgumentTypesNonvariant(env: TcEnv, args_sub: List[ir.LvDecl], args_sup: List[ir.WcTypeRef]) {
        foreachzip(args_sub, args_sup) { case (arg_sub, arg_sup) =>
            if(arg_sub.wt != arg_sup)
                throw new CheckFailure(
                    "intervals.override.param.type.changed", 
                    arg_sub.name, arg_sub.wt, arg_sup)
        }        
    }
    
    def checkReturnTypeCovariant(env: TcEnv, wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef) {
        if(!env.isSubtype(wt_sub, wt_sup))
            throw new CheckFailure(
                "intervals.override.ret.type.changed", wt_sub, wt_sup)
    }
    
    def checkOverridenReqsImplyOurReqs(env0: TcEnv, reqs_sub: List[ir.Req], reqs_sup: List[ir.Req]) {
        val env_sup = env0.addReqs(reqs_sup)
        reqs_sub.foreach { req_sub =>
            if(!isReqFulfilled(env_sup, req_sub))
                throw new CheckFailure("intervals.override.adds.req", req_sub)
        }        
    }
    
    def checkMethodBody(env: TcEnv, md: ir.MethodDecl) {
        assert(ss_cur.isEmpty)
        checkStatementSeq(env, md.body)
        assert(ss_cur.isEmpty)
    }
    
    def checkMethodDecl(
        env_cd: TcEnv, 
        flow_ctor_assum: FlowEnv,   // relations established by the ctor
        md: ir.MethodDecl           // method to check
    ) = 
        indexAt(md, ()) {
            var env = env_cd
            
            // Define special vars "method" and "this":
            env = env.addGhostLocal(ir.lv_mthd, ir.c_interval)
            env = env.withCurrent(env.cp_mthd)
            env = env.addNonNull(env.cp_mthd)
            
            // For normal methods, type of this is the defining class
            val ct_this = ir.ClassType(env.c_cur, List(), List())
            env = env.addReifiedLocal(ir.lv_this, ct_this)
            env = env.addNonNull(env.crp_this)
            
            env = env.addArgs(md.args)
            env = env.withReturnType(md.wt_ret)
            
            env.substdOverriddenMethodSigs(ct_this, md).foreach { msig_sup => 
                checkArgumentTypesNonvariant(env, md.args, msig_sup.wts_args)
                checkReturnTypeCovariant(env, md.wt_ret, msig_sup.wt_ret)
                checkOverridenReqsImplyOurReqs(env, md.reqs, msig_sup.reqs)
            }                    
            
            env = env.addReqs(md.reqs)
            
            // If constructor already happened, add its effects on the environment:
            // XXX Could check for other supertypes if we wanted...
            val p_cdCtor = ir.ClassCtorFieldName(env.c_cur).thisPath
            if(env.hbInter(env.canon(p_cdCtor), env.cp_mthd)) {
                env = env.addFlow(flow_ctor_assum)
            }                
            
            checkMethodBody(env, md)
        }
        
    def checkNoninterfaceConstructorDecl(
        env_cd: TcEnv, 
        md: ir.MethodDecl
    ) = 
        indexAt(md, FlowEnv.empty) {
            var env = env_cd
            
            // Define special vars "method" (== this.constructor) and "this":
            val ct_this = ir.ClassType(
                env.c_cur,
                List(), 
                List()
            )
            env = env.addReifiedLocal(ir.lv_this, ct_this)
            env = env.addNonNull(env.crp_this)
            
            // Method == this.constructor[env.c_cur]
            val p_ctor = ir.ClassCtorFieldName(env.c_cur).thisPath
            val cp_ctor = env.canon(p_ctor)
            env = env.addPerm(ir.lv_mthd, cp_ctor)
            env = env.withCurrent(env.canon(ir.p_mthd))

            // Check method body:
            env = env.addArgs(md.args)
            env = env.addReqs(md.reqs)
            checkMethodBody(env, md)
            
            // Compute exit assumptions and store in prog.exportedCtorEnvs:
            val flow = extractAssumptions(env, Set(ir.lv_this)) // Globally valid assums
            log.flow(false, "extracted assumptions:", flow)
            prog.exportedCtorEnvs += Pair((env.c_cur, md.name), flow) // Store
            flow
        }
        
    def checkReifiedFieldDecl(
        env_cd: TcEnv, 
        fd: ir.ReifiedFieldDecl
    ) = 
        indexAt(fd, ()) {
            var env = env_cd
            val cd = classDecl(env.c_cur)
            
            // Rules:
            //
            // The type of a field f with guard p_g in class c 
            // may be dependent on a path p_dep if either:
            // (1) p_dep is constant when p_g is active; or
            // (2) p_dep = this.f' and f' is declared in class c (not a supertype!)
            //
            // Note that a type is dependent on p_dep if p.F appears in the type, so 
            // we must check all prefixes of each dependent path as well.
            val ct_this = ir.ClassType(env.c_cur, List(), List())
            env = env.addReifiedLocal(ir.lv_this, ct_this)
            env = env.addGhostLocal(ir.lv_mthd, ir.c_interval)
            env = env.withCurrent(env.cp_mthd)
            
            // Add assumptions as though guard were satisfied.
            // Also check that guards are typed as Interval, Lock, or just Guard
            val cp_guard = env.canon(fd.p_guard)
            env = log.indented("adding appr. constraints for cp_guard %s", cp_guard) {
                if(env.pathHasSubclass(cp_guard, ir.c_interval))
                    env.addSubintervalOf(env.cp_cur, cp_guard) 
                else if(env.pathHasSubclass(cp_guard, ir.c_lock))
                    env.addLocks(env.cp_cur, cp_guard)
                else if(env.pathHasSubclass(cp_guard, ir.c_guard))
                    env.addDeclaredWritableBy(cp_guard, env.cp_cur)
                else
                    throw new CheckFailure("intervals.invalid.guard.type", cp_guard)
            }
            
            // Check that each dependent path is legal:
            env.dependentPaths(fd.wt).foreach { p_full_dep =>                 
                def check(p_dep: ir.Path) {
                    log.indented("check_dep_path(%s)", p_dep) {
                        p_dep match {
                            case ir.Path(lv, List()) => 
                                // Always permitted.
                                log("dependent on local var")

                            case ir.Path(ir.lv_this(), List(f)) =>
                                log("dependent on another field of this")
                                val cp_dep = env.canon(p_dep)
                                // Note: the field must be declared in the same class (not a super-
                                // or subclass) as `fd`
                                if(env.isMutable(cp_dep) && !cd.reifiedFieldDecls.exists(_.name == f))
                                    throw new CheckFailure(
                                        "intervals.illegal.type.dep",
                                        cp_dep.p, cp_guard.p)

                            case ir.Path(lv, f :: rev_fs) =>
                                log("misc dependency")
                                check(ir.Path(lv, rev_fs))
                                val cp_dep = env.canon(p_dep)
                                if(env.isMutable(cp_dep))
                                    throw new CheckFailure(
                                        "intervals.illegal.type.dep",
                                        cp_dep.p, cp_guard.p)
                        }
                    }
                }
                
                check(p_full_dep)                            
            }                        
        }
        
    // ___ Classes and interfaces ___________________________________________
    
    def checkInterfaceClassDecl(env: TcEnv, cd: ir.ClassDecl) =
        indexAt(cd, ()) {
            cd.methods.foreach(checkMethodDecl(env, FlowEnv.empty, _))
        }            
        
    def checkNoninterfaceClassDecl(env: TcEnv, cd: ir.ClassDecl) = 
        indexAt(cd, ()) {
            cd.reifiedFieldDecls.foreach(checkReifiedFieldDecl(env, _))
            val flows_ctor = cd.ctors.map(checkNoninterfaceConstructorDecl(env, _))
            val flow_all_ctors = FlowEnv.intersect(flows_ctor)
            cd.methods.foreach(checkMethodDecl(env, flow_all_ctors, _))                    
        }
            
    def checkClassDecl(cd: ir.ClassDecl) = {
        var env = prog.env_empty.copy(c_cur = cd.name)
        if(cd.attrs.interface) checkInterfaceClassDecl(env, cd)
        else checkNoninterfaceClassDecl(env, cd)        
    }
        
}