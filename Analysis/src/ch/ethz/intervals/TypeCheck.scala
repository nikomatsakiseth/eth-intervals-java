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
        if(!env.pathHasType(crp_sub.toTcp, wt_sup))
            throw new CheckFailure(
                "intervals.expected.subtype", 
                crp_sub.p, crp_sub.wt.toUserString, wt_sup.toUserString)
    }
    
    // ___ Statement Stack __________________________________________________
    //
    // When checking method bodies, we use the statement stack to 
    // track the enclosing statements and handle loops, etc.
    
    class StmtScope(
        val env_in: TcEnv,
        val defines: List[ir.LvDecl],
        val loopArgs: Option[List[ir.LvDecl]]
    ) {
        def cp_inter = env_in.cp_cur
        
        var oenv_continue: Option[TcEnv] = None  // only applies to While
        var oenv_break: Option[TcEnv] = None     // applies to all        
    }
    
    // ___ Checking method bodies ___________________________________________
    
    def isReqFulfilled(env: TcEnv, req: ir.Req): Boolean = log.indented("isReqFulfilled(%s)", req) {
        def is(func: Function2[ir.CanonPath, ir.CanonPath, Boolean], ps: List[ir.Path], qs: List[ir.Path]) = {
            // XXX These args need not be immutable (at least not in all cases).
            val cps = env.immutableCanonPaths(ps)
            val cqs = env.immutableCanonPaths(qs)
            forallcross(cps, cqs)(func)
        }
        req match {
            case ir.ReqWritableBy(ps, qs) => is(env.isWritableBy, ps, qs)
            case ir.ReqReadableBy(ps, qs) => is(env.isReadableBy, ps, qs)
            case ir.ReqSuspends(ps, qs) => is(env.suspends, ps, qs)
            case ir.ReqHb(ps, qs) => is(env.userHb, ps, qs)
        }
    }
    
    def checkReqFulfilled(env: TcEnv, m: ir.MethodName, req: ir.Req) {
        if(!isReqFulfilled(env, req)) {
            var reqString = req.toString
            reqString = reqString.replace(env.p_cur.toString, "<method-call>")
            throw new CheckFailure("intervals.requirement.not.met", m, reqString)
        }
            
    }
    
    def checkArgumentTypes(env: TcEnv, msig: ir.MethodSig, cqs: List[ir.CanonReifiedPath]) =
        foreachzip(cqs, msig.wts_args)(checkIsSubtype(env, _, _))
    
    def checkReadable(env: TcEnv, cp_guard: ir.CanonPath) {
        if(!env.isReadableBy(cp_guard, env.cp_cur))
            throw new CheckFailure("intervals.not.readable", cp_guard.p)
    }
    
    def checkWritable(env: TcEnv, cp_guard: ir.CanonPath) {
        if(!env.isWritableBy(cp_guard, env.cp_cur))
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
            log.indented("reqs")        { msig.reqs.foreach(checkReqFulfilled(env, msig.name, _)) }

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
        env.addReifiedLocal(x, msig.wt_ret, msig.ps_is)
    }
    
    def mapAndMergeBranchEnv(
        env_in: TcEnv,         // environment on entry to the flow
        env_brk0: TcEnv,        // environment at branch statement
        decls: List[ir.LvDecl], // defined variables (phi variable declarations)
        cps: List[ir.CanonPath] // arguments to each phi
    ): TcEnv = {
        // ----------------------------------------------------------------------
        // Add a synthetic interval lv_breakInter which happens after 
        // env_in0.cp_cur:
        val (cp_breakInter, env_brk) = {
            val (cp_breakInter, env_brk1) = env_brk0.freshCp(ir.c_interval)
            (cp_breakInter, env_brk1.addHbInter(env_in.cp_cur, cp_breakInter))
        }
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
        // (3) p2 to p1 if (a) ∃ temp map p1 -> p2 and (b) p1 immutable
        // (4) Everything else to "lv_outOfScope"

        val lv_outOfScope = prog.freshVarName

        // Map all vars to outOfScope (unless overridden later)
        var map = env_brk.perm.keysIterator.foldLeft(Map.empty[ir.Path, ir.Path]) { case (m, x) =>
            m + (x.path -> lv_outOfScope.path)
        }
    
        // Map variables shared with env_in to themselves:            
        map = env_in.perm.keysIterator.foldLeft(map) { case (m, x) => 
            m + (x.path -> x.path)
        }

        // Map arguments 'cps' to 'decls':
        val xs_args = decls.map(_.name)
        map = cps.zip(xs_args).foldLeft(map) { case (m, (cp, x)) =>
            m + (cp.p -> x.path) 
        }
        
        // Reverse temp mappings p1->p2 if p1 is immutable in lv_breakInter:
        map = flow_brk.temp.foldLeft(map) { case (m, (p1, p2)) => 
            val cp1 = env_brk.canonPath(p1)
            if(env_brk.isImmutableIn(cp1, cp_breakInter))
                m + (p2 -> p1)
            else
                m
        }

        // Map everything else to lv_outOfScope:
        val subst = new PathSubst(map)

        // ----------------------------------------------------------------------
        // Apply map to relations and keep only those not affecting lv_outOfScope
        
        def inScope(p: ir.Path) = (p.lv != lv_outOfScope)

        def mapMap(map: Map[ir.Path, ir.Path]) =
            map.iterator.foldLeft(Map.empty[ir.Path, ir.Path]) { case (r, (p0, q0)) =>
                // Careful: if (p0->q0) was in the original mapping and p0 was immutable,
                // then q0 will be mapped to p1.
                val p1 = subst.path(p0)
                val q1 = subst.path(q0)
                if(p1 != q1 && inScope(p1) && inScope(q1)) r + (p1 -> q1)
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
                mapPathRelation(flow_brk.inlineRel),
                mapPathRelation(flow_brk.locksRel)
            )
        )
    }
    
    def intersect(oenv1: Option[TcEnv], env2: TcEnv) = oenv1 match {
        case None => Some(env2)
        case Some(env1) => Some(env1.intersectFlow(env2.flow))
    }

    def mergeBreakEnv(
        env: TcEnv, 
        ss_cur: List[StmtScope], 
        idx: Int, 
        cps: List[ir.CanonPath]
    ) {
        log.indented("mergeBreakEnv(%s,%s)", idx, cps) {
            val ss = ss_cur(idx)

            val env_map = mapAndMergeBranchEnv(ss.env_in, env, ss.defines, cps)
            log.env(false, "env_map: ", env_map)
            ss.oenv_break = intersect(ss.oenv_break, env_map)            
            if(ss.oenv_break.get != env_map)
                log.env(false, "env_break: ", ss.oenv_break.get)
        }
    }
        
    def mergeContinueEnv(
        env: TcEnv, 
        ss_cur: List[StmtScope], 
        idx: Int, 
        cps: List[ir.CanonPath]
    ) {
        log.indented("mergeContinueEnv(%s,%s)", idx, cps) {
            val ss = ss_cur(idx)
            ss.loopArgs match {
                case Some(args) =>
                    val env_map = mapAndMergeBranchEnv(ss.env_in, env, args, cps)
                    log.env(false, "env_map: ", env_map)
                    ss.oenv_continue = intersect(ss.oenv_continue, env_map)
                    if(ss.oenv_continue.get != env_map)
                        log.env(false, "env_continue: ", ss.oenv_continue.get)
                case None =>
                    throw new CheckFailure(
                        "intervals.internal.error",
                        "continue to non-loop: %s".format(ss))
            }
        }
    }
    
    def computeAndLogLoopArguments(env: TcEnv, args: List[ir.LvDecl], lvs_initial: List[ir.VarName]) = {
        log.indented("Loop Arguments:") {
            args.zip(lvs_initial).map { case (arg, lv) =>
                log.indented("%s = %s", arg, lv) {
                    env.immutableReifiedLv(lv)
                }                
            }
        }        
    }
    
    def checkCompoundStatement(
        env_in: TcEnv, 
        ss_prev: List[StmtScope],
        stmt_compound: ir.StmtCompound
    ): TcEnv = {
        val oenv_break = stmt_compound.kind match {
            case ir.Block(seq) =>
                val ss = new StmtScope(env_in, stmt_compound.defines, None)
                val ss_cur = ss :: ss_prev
                
                checkStatementSeq(env_in, ss_cur, seq)
                ss.oenv_break

            case ir.Switch(seqs) =>
                val ss = new StmtScope(env_in, stmt_compound.defines, None)
                val ss_cur = ss :: ss_prev
                var env = env_in
                
                seqs.foreach { seq =>
                    // If the previous stmt breaks, then 
                    // env == env_initial and this line has no effect.
                    env = env.intersectFlow(env_in.flow)
                    env = checkStatementSeq(env, ss_cur, seq)
                }
                ss.oenv_break
                
            case ir.Loop(args, lvs_initial, seq) =>
                val ss = new StmtScope(env_in, stmt_compound.defines, Some(args))
                val ss_cur = ss :: ss_prev
                
                def iterate(env_continue_before: TcEnv) {
                    log.indented("iterate()") {
                        // Break env is recomputed each iteration:
                        ss.oenv_break = None

                        // Perform an iteration:
                        checkStatementSeq(env_continue_before, ss_cur, seq)
                    }

                    // Repeat until steady state is reached:
                    val env_continue_after = ss.oenv_continue.get
                    if(env_continue_before != env_continue_after)
                        iterate(env_continue_after)
                }
                
                val tps_initial = computeAndLogLoopArguments(env_in, args, lvs_initial)
                mergeContinueEnv(env_in, ss_cur, 0, tps_initial)
                iterate(ss.oenv_continue.get)
                ss.oenv_break
                
            case ir.InlineInterval(x, ps_locks, seq) =>
                var env = env_in
            
                val tps_locks = env.immutableReifiedLvs(ps_locks)                    
                env = env.addReifiedLocal(x, ir.wt_constructedInterval)
                val cp_x = env.canonLv(x)
                env = env.addNonNull(cp_x)
                env = env.addSuspends(cp_x, env.cp_cur)
                env = tps_locks.foldLeft(env)(_.addLocks(cp_x, _))
                env = env.withCurrent(cp_x)
                
                val ss = new StmtScope(env, stmt_compound.defines, None)
                val ss_cur = ss :: ss_prev                
                env = checkStatementSeq(env, ss_cur, seq)
                ss.oenv_break
                
            case ir.TryCatch(seq_try, seq_catch) =>
                val ss = new StmtScope(env_in, stmt_compound.defines, None)
                val ss_cur = ss :: ss_prev
                
                // Note: environment is lost unless it breaks
                checkStatementSeq(env_in, ss_cur, seq_try) 
                
                // Catch conservatively assumes try failed immediately:
                checkStatementSeq(env_in, ss_cur, seq_catch)
                
                ss.oenv_break
        }
        
        log.ifEnabled {
            log.indented("Defines:") {
                stmt_compound.defines.foreach(log(_))
            }            
        }

        oenv_break match {
            case None => // control flow never really comes this way...
                log("oenv_break unset, used env_in")
                env_in // ...use safe approx.
            case Some(env_break) => 
                log.env(false, "env_break: ", env_break)
                env_break
        }
    }
    
    def checkStatement(env0: TcEnv, ss_cur: List[StmtScope], stmt: ir.Stmt) = indexAt(stmt, env0) {
        var env = env0
        log.env(false, "Input Environment: ", env)
        stmt match {   
            case stmt_compound: ir.StmtCompound =>
                checkCompoundStatement(env, ss_cur, stmt_compound)
                           
            case ir.StmtSuperCtor(m, qs) =>
                val tcp = env.tcp_super
                val cqs = env.immutableReifiedLvs(qs)
                val msig_ctor = env.substdCtorSig(tcp, m, cqs)
                log("tcp = %s", tcp)
                log("msig_ctor = %s", msig_ctor)
                env = processCallMsig(env, tcp, msig_ctor, cqs)
                
                // Ctors for all supertypes now complete:
                strictSuperclasses(env.c_this).foreach { case c =>
                    val cp_cCtor = env.canonPath(ir.ClassCtorFieldName(c).thisPath)
                    env = env.addHbInter(cp_cCtor, env.cp_cur)
                }
                
                // Supertype must have been processed first:
                log("Supertype: %s", tcp)
                env.addFlow(prog.exportedCtorEnvs((tcp.ty.c, m)))
                
            case ir.StmtGetField(lv_def, lv_owner, f) =>
                val cp_owner = env.immutableReifiedLv(lv_owner)
                env = env.addNonNull(cp_owner)
                
                val (_, rfd) = env.substdReifiedFieldDecl(cp_owner.toTcp, f)
                val cp_guard = env.immutableCanonPath(rfd.p_guard)
                checkReadable(env, cp_guard)
                
                val cp_field = env.extendCanonWithReifiedField(cp_owner, rfd)
                if(env.isImmutable(cp_field)) {
                    env.addPerm(lv_def, cp_field)                            
                } else {
                    env = env.addReifiedLocal(lv_def, rfd.wt)
                    env.addTemp(cp_field.p, lv_def.path) // Record that p.f == vd, for now.
                }
                
            case ir.StmtSetField(lv_owner, f, lv_value) =>
                val cp_owner = env.immutableReifiedLv(lv_owner)
                env = env.addNonNull(cp_owner)
                
                val cp_value = env.immutableReifiedLv(lv_value)
                
                val (c_owner, rfd) = env.substdReifiedFieldDecl(cp_owner.toTcp, f) 
                val cp_guard = env.immutableCanonPath(rfd.p_guard)
                checkWritable(env, cp_guard)
                checkIsSubtype(env, cp_value, rfd.wt)
                
                val cp_field = env.extendCanonWithReifiedField(cp_owner, rfd)                        
                env = env.addTemp(cp_field.p, cp_value.p)
        
                env = env.removeInvalidated(cp_field.p)
                env.linkedPaths(cp_owner, c_owner, f).foldLeft(env)(_ addInvalidated _)                            
                
            case ir.StmtCheckType(p, wt) =>
                val cp = env.immutableReifiedLv(p)
                checkIsSubtype(env, cp, wt)
                env
                    
            case ir.StmtCall(x, p, m, qs) =>
                val cp = env.immutableReifiedLv(p)
                env = env.addNonNull(cp)
                val cqs = env.immutableReifiedLvs(qs)
                env = processCall(env, x, cp.toTcp, m, cqs)
                env
    
            case ir.StmtSuperCall(x, m, qs) =>
                val cqs = env.immutableReifiedLvs(qs)
                env = processCall(env, x, env.tcp_super, m, cqs)
                env
            
            case ir.StmtNew(x, ct0, m, qs) =>
                val cd = classDecl(ct0.c)
                val cqs = env.immutableReifiedLvs(qs)
                
                if(cd.attrs.interface)
                    throw new CheckFailure("intervals.new.interface", ct0.c)
                    
                // Supply a default ghost (the current interval) for @Constructor:
                val gs_default = List(ir.Ghost(ir.f_objCtor, env.cp_cur.p))
                val ct = ct0.withDefaultGhosts(gs_default)
                    
                // Check Ghost Types:           
                env = env.addReifiedLocal(x, ct)
                val crp_x = env.immutableReifiedLv(x)
                val tcp_x = ir.TeeCeePee(crp_x, ct)
                env = env.addNonNull(crp_x)
                ct.ghosts.foreach { g =>
                    env.ghostFieldsDeclaredOnType(ct).find(_.isNamed(g.f)) match {
                        case Some(gfd) => 
                            val cp = env.canonPath(g.p)
                            if(!env.pathHasClass(cp, gfd.c))
                                throw new CheckFailure("intervals.must.be.subclass", cp.p, gfd.c)
                        case None if (g.f == ir.f_objCtor) =>
                            val cp = env.canonPath(g.p)
                            if(!env.suspends(env.cp_cur, cp)) // also implies must be an interval
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
                    val cp = env.immutableReifiedLv(p)
                    checkIsSubtype(env, cp, env.wt_ret)                    
                }
                mergeBreakEnv(env, ss_cur, ss_cur.length - 1, List())
                ss_cur.head.env_in
                
            case ir.StmtHb(p, q) =>
                val cp = env.immutableReifiedLv(p)
                val cq = env.immutableReifiedLv(q)
                env.addUserHb(cp, cq)
            
            case ir.StmtLocks(p, q) =>
                val cp = env.immutableReifiedLv(p)
                val cq = env.immutableReifiedLv(q)
                env.addLocks(cp, cq)
            
            case ir.StmtCondBreak(i, ps) =>
                val cps = env.immutableReifiedLvs(ps)
                mergeBreakEnv(env, ss_cur, i, cps)
                env

            case ir.StmtBreak(i, ps) =>
                val cps = env.immutableReifiedLvs(ps)
                mergeBreakEnv(env, ss_cur, i, cps)
                ss_cur.head.env_in
            
            case ir.StmtContinue(i, ps) =>
                val cps = env.immutableReifiedLvs(ps)
                mergeContinueEnv(env, ss_cur, i, cps)
                ss_cur.head.env_in
        }
    }
    
    def checkStatementSeq(env0: TcEnv, ss_cur: List[StmtScope], seq: ir.StmtSeq) = {
        log.indented("checkStatementSeq(%s)", seq) {
            val cp_curOnEntry = env0.cp_cur
            log("cp_curOnEntry: %s", cp_curOnEntry)
            val (cp_seqInterval, env1) = env0.freshCp(ir.c_interval)
            log("cp_seqInterval: %s", cp_seqInterval)
            var env = env1.addSuspends(cp_seqInterval, env1.cp_cur)
            env = env.withCurrent(cp_seqInterval)
            
            seq.stmts.foldLeft[Option[ir.CanonPath]](None) { (ocp_prevInterval, stmt) =>
                val (cp_stmtInterval, env2) = env.freshCp(ir.c_interval)
                log("cp_stmtInterval: %s", cp_stmtInterval)
                env = env2.addSuspends(cp_stmtInterval, cp_seqInterval)
                ocp_prevInterval.foreach { cp_prevInterval =>
                    env = env.addHbInter(cp_prevInterval, cp_stmtInterval)
                }
                env = env.withCurrent(cp_stmtInterval)
                env = checkStatement(env, ss_cur, stmt)
                Some(cp_stmtInterval)
            }
            
            env.withCurrent(cp_curOnEntry)
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
    
    /** Checks the method body and returns a FlowEnv indicating
      * the "exit" relations; that is, those relations that are 
      * established permanently for all subsequent method calls. */
    def checkMethodBody(env: TcEnv, md: ir.MethodDecl): FlowEnv = {
        val ss = new StmtScope(env, List(), None)
        checkStatementSeq(env, List(ss), md.body)
        ss.oenv_break.getOrElse(env).flow
    }
    
    def checkMethodDecl(
        env_cd: TcEnv, 
        flow_ctor_assum: FlowEnv,   // relations established by the ctor
        md: ir.MethodDecl           // method to check
    ): Unit = 
        indexAt(md, ()) {
            var env = env_cd
            
            // Define special vars "method" and "this":
            env = env.addGhostLocal(ir.lv_mthd, ir.c_interval)
            env = env.withCurrent(env.cp_mthd)
            env = env.addNonNull(env.cp_mthd)
            
            // For normal methods, type of this is the defining class
            env = env.addNonNull(env.crp_this)
            
            env = env.addArgs(md.args)
            env = env.withReturnType(md.wt_ret)
            
            env.substdOverriddenMethodSigs(env.ct_this, md).foreach { msig_sup => 
                checkArgumentTypesNonvariant(env, md.args, msig_sup.wts_args)
                checkReturnTypeCovariant(env, md.wt_ret, msig_sup.wt_ret)
                checkOverridenReqsImplyOurReqs(env, md.reqs, msig_sup.reqs)
            }                    
            
            env = env.addReqs(md.reqs)
            
            // If constructor already happened, add its effects on the environment:
            // XXX Could check for other supertypes if we wanted...
            val p_cdCtor = ir.ClassCtorFieldName(env.c_this).thisPath
            if(env.hbInter(env.canonPath(p_cdCtor), env.cp_mthd)) {
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
            
            // Define special var "method" (== this.constructor):
            env = env.addNonNull(env.crp_this)
            
            // Method == this.constructor[env.c_this]
            val p_ctor = ir.ClassCtorFieldName(env.c_this).thisPath
            val cp_ctor = env.canonPath(p_ctor)
            env = env.addPerm(ir.lv_mthd, cp_ctor)
            env = env.withCurrent(env.canonLv(ir.lv_mthd))

            // Check method body:
            env = env.addArgs(md.args)
            env = env.addReqs(md.reqs)
            val flow_exit = checkMethodBody(env, md)
            
            // Compute exit assumptions and store in prog.exportedCtorEnvs:
            log.flow(false, "exit assumptions:", flow_exit)
            prog.exportedCtorEnvs += (env.c_this, md.name) -> flow_exit
            flow_exit
        }
        
    def checkReifiedFieldDecl(
        env_cd: TcEnv, 
        fd: ir.ReifiedFieldDecl
    ) = 
        indexAt(fd, ()) {
            var env = env_cd
            val cd = classDecl(env.c_this)
            
            // Rules:
            //
            // The type of a field f with guard p_g in class c 
            // may be dependent on a path p_dep if either:
            // (1) p_dep is constant when p_g is active; or
            // (2) p_dep = this.f' and f' is declared in class c (not a supertype!)
            //
            // Note that a type is dependent on p_dep if p.F appears in the type, so 
            // we must check all prefixes of each dependent path as well.
            env = env.addGhostLocal(ir.lv_mthd, ir.c_interval)
            env = env.withCurrent(env.cp_mthd)
            
            // Add assumptions as though guard were satisfied.
            // Also check that guards are typed as Interval, Lock, or just Guard
            val cp_guard = env.canonPath(fd.p_guard)
            env = log.indented("adding appr. constraints for cp_guard %s", cp_guard) {
                if(env.pathHasClass(cp_guard, ir.c_interval))
                    env.addSuspends(env.cp_cur, cp_guard) 
                else if(env.pathHasClass(cp_guard, ir.c_lock))
                    env.addLocks(env.cp_cur, cp_guard)
                else if(env.pathHasClass(cp_guard, ir.c_guard))
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
                                // Always permitted, as local variables are immutable.
                                log("dependent on local var")

                            case ir.Path(ir.lv_this(), List(f)) =>
                                log("dependent on another field of this")
                                val cp_dep = env.canonPath(p_dep)
                                if(
                                    // illegal if mutable when guard is writable and...
                                    !env.isImmutable(cp_dep) &&                
                                    // ...f not declared in same class (not sub- or superclass!)
                                    !cd.reifiedFieldDecls.exists(_.name == f)
                                )
                                    throw new CheckFailure(
                                        "intervals.illegal.type.dep",
                                        cp_dep.p, cp_guard.p)

                            case ir.Path(lv, f :: rev_fs) =>
                                log("misc dependency")
                                check(ir.Path(lv, rev_fs))
                                val cp_dep = env.canonPath(p_dep)
                                if(
                                    // illegal if mutable when guard is writable
                                    !env.isImmutable(cp_dep)
                                )
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
        var env = prog.env_empty.withThisClass(cd.name)
        if(cd.attrs.interface) checkInterfaceClassDecl(env, cd)
        else checkNoninterfaceClassDecl(env, cd)        
    }
        
}