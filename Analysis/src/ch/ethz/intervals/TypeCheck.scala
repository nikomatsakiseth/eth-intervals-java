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
    import prog.overriddenMethodSigs
    import prog.strictSuperclasses
    
    def freshCp(wt: ir.WcTypeRef) = {
        val lv = prog.freshVarName
        val cp = ir.CpLv(lv, wt, true)
        addPerm(lv, cp)
        cp
    }
        
    /// wt(cp_sub) <: wt_sup
    def checkIsSubtype(cp_sub: ir.CanonPath, wt_sup: ir.WcTypeRef) {
        if(!env.pathHasType(cp_sub, wt_sup))
            throw new CheckFailure("intervals.expected.subtype", cp_sub.p, cp_sub.wt, wt_sup)
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
    
    def isReqFulfilled(req: ir.Req): Boolean = log.indented("isReqFulfilled(%s)", req) {
        def is(func: Function2[ir.CanonPath, ir.CanonPath, Boolean], ps: List[ir.Path], qs: List[ir.Path]) = {
            // XXX These args need not be immutable (at least not in all cases).
            val cps = immutableGhost(ps)
            val cqs = immutableGhost(qs)
            forallcross(cps, cqs)(func)
        }
        req match {
            case ir.ReqWritableBy(ps, qs) => is(env.guardsDataWritableBy, ps, qs)
            case ir.ReqReadableBy(ps, qs) => is(env.guardsDataReadableBy, ps, qs)
            case ir.ReqSubintervalOf(ps, qs) => is(env.isSubintervalOf, ps, qs)
            case ir.ReqHb(ps, qs) => is(env.userHb, ps, qs)
        }
    }
    
    def checkReqFulfilled(req: ir.Req) {
        if(!isReqFulfilled(req))
            throw new CheckFailure("intervals.requirement.not.met", req)
    }
    
    def checkArgumentTypes(msig: ir.MethodSig, cqs: List[ir.CanonPath]) =
        foreachzip(cqs, msig.args.map(_.wt))(checkIsSubtype)
    
    def checkReadable(cp_guard: ir.CanonPath) {
        if(!env.guardsDataReadableBy(cp_guard, cp_cur))
            throw new CheckFailure("intervals.not.readable", cp_guard.p)
    }
    
    def checkWritable(cp_guard: ir.CanonPath) {
        if(!env.guardsDataWritableBy(cp_guard, cp_cur))
            throw new CheckFailure("intervals.not.writable", cp_guard.p)
    }
        
    def checkNoInvalidated() {
        if(!env.flow.ps_invalidated.isEmpty)
            throw new CheckFailure(
                "intervals.must.assign.first", 
                env.flow.ps_invalidated.mkEnglishString)        
    }

    def checkCallMsig(tcp: ir.TeeCeePee[_], msig: ir.MethodSig, cqs: List[ir.CanonPath]) {
        // Cannot invoke a method when there are outstanding invalidated fields:
        checkNoInvalidated()
            
        // Receiver/Arguments must have correct type and requirements must be fulfilled:
        checkIsSubtype(tcp.cp, msig.wct_rcvr)
        checkArgumentTypes(msig, cqs)
        msig.reqs.foreach(checkReqFulfilled)
        
        // Any method call disrupts potential temporary assocations:
        //     We make these disruptions before checking return value, 
        //     in case they would affect the type.  Haven't thought through
        //     if this can happen or not, but this would be the right time anyhow.
        clearTemp()        
    }
    
    def checkCall(x: ir.VarName, tcp: ir.TeeCeePee[ir.WcTypeRef], m: ir.MethodName, cqs: List[ir.CanonPath]) {
        val msig = env.substdMethodSig(tcp, m, cqs)
        checkCallMsig(tcp, msig, cqs)
        addReifiedLocal(x, msig.wt_ret)
    }
    
    def mapAndMergeBranchEnv(
        env_in: TcEnv,       // environment on entry to the flow
        env_brk: TcEnv,      // environment at branch statement
        decls: List[ir.LvDecl], // defined variables (phi variable declarations)
        cps: List[ir.CanonPath]    // arguments to each phi
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
        //    checkIsSubtype(cp, wt)
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

    def mergeBreakEnv(idx: Int, cps: List[ir.CanonPath]) {
        log.indented("mergeBreakEnv(%s,%s)", idx, cps) {
            val ss = ss_cur(idx)

            val env_map = mapAndMergeBranchEnv(ss.env_in, env, ss.stmt.defines, cps)
            log.env(false, "env_map: ", env_map)
            ss.oenv_break = intersect(ss.oenv_break, env_map)            
            if(ss.oenv_break.get != env_map)
                log.env(false, "env_break: ", ss.oenv_break.get)
        }
    }
        
    def mergeContinueEnv(idx: Int, cps: List[ir.CanonPath]) {
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
    
    def computeAndLogLoopArguments(args: List[ir.LvDecl], ps_initial: List[ir.Path]) = {
        log.indented("Loop Arguments:") {
            args.zip(ps_initial).map { case (arg, p) =>
                log.indented("%s = %s", arg, p) {
                    immutableReified(p)
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
                def iterate(env_continue_before: TcEnv) {
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
                val tps_locks = immutableReified(ps_locks)                    
                addReifiedLocal(x, ir.t_interval)
                val cp_x = env.canon(x.path)
                addNonNull(cp_x)
                addSubintervalOf(cp_x, cp_cur)
                tps_locks.foreach(addLocks(cp_x, _))
                
                // n.b.: We don't try to pop-- that's because if
                // statement seq returns, the environment will be reset to ss.env_in.
                // In any case we will reset environment below so it doens't matter.
                setCurrent(cp_x)
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
                log.env(false, "oenv_break: ", env_break)
        }
    }
    
    def checkStatement(stmt: ir.Stmt) = indexAt(stmt, ()) {
        log.env(false, "Input Environment: ", env)
        stmt match {   
            case stmt_compound: ir.StmtCompound =>
                withStmt(stmt_compound, env) {
                    checkHeadCompoundStatement()
                }
                           
            case ir.StmtSuperCtor(m, qs) =>
                val tcp = tcp_super
                val cqs = immutableReified(qs)
                val msig_ctor = env.substdCtorSig(tcp, m, cqs)
                log("tcp = %s", tcp)
                log("msig_ctor = %s", msig_ctor)
                checkCallMsig(tcp, msig_ctor, cqs)
                
                // Ctors for all supertypes now complete:
                val ct_this = ir.ClassType(
                    env.c_cur,
                    List(), 
                    List()
                )
                setEnv(env.redefineReifiedLocal(ir.lv_this, ct_this))
                
                // Supertype must have been processed first:
                log("Supertype: %s", tcp)
                setEnv(env.addFlow(prog.exportedCtorEnvs((tcp.ty.c, m))))
                
            case ir.StmtGetField(x, p_o, f) =>
                val cp_o = immutableReified(p_o)
                addNonNull(cp_o)
                
                env.substdFieldDecl(cp_o.toTcp, f) match {
                    case (_, _: ir.GhostFieldDecl) =>
                        throw new CheckFailure("intervals.not.reified", cp_o.wt, f)
                    
                    case (_, rfd: ir.ReifiedFieldDecl) =>
                        val cp_guard = immutableGhost(rfd.p_guard)
                        checkReadable(cp_guard)
                        
                        val cp_f = env.extendCanonWithReifiedField(cp_o, rfd)
                        if(!env.isMutable(cp_f)) {
                            addPerm(x, cp_f)                            
                        } else {
                            addReifiedLocal(x, rfd.wt)
                            addTemp(cp_f.p, x.path) // Record that p.f == vd, for now.
                        }
                }                    
                
            case ir.StmtSetField(p_o, f, p_v) =>
                val cp_o = immutableReified(p_o)
                addNonNull(cp_o)
                
                val cp_v = immutableReified(p_v)
                
                env.substdFieldDecl(cp_o.toTcp, f) match {
                    case (_, _: ir.GhostFieldDecl) => 
                        throw new CheckFailure("intervals.not.reified", cp_o.wt, f)
                    
                    case (tcp_o, rfd: ir.ReifiedFieldDecl) =>
                        val cp_guard = immutableGhost(rfd.p_guard)
                        checkWritable(cp_guard)
                        checkIsSubtype(cp_v, rfd.wt)
                        
                        val cp_f = env.extendCanonWithReifiedField(cp_o, rfd)                        
                        addTemp(cp_f.p, cp_v.p)
                
                        removeInvalidated(cp_f.p)
                        env.linkedPaths(tcp_o, f).foreach(addInvalidated)                            
                }
                
            case ir.StmtCheckType(p, wt) =>
                val cp = immutableReified(p)
                checkIsSubtype(cp, wt)
                    
            case ir.StmtCall(x, p, m, qs) =>
                val cp = immutableReified(p)
                addNonNull(cp)
                val cqs = immutableReified(qs)
                checkCall(x, cp.toTcp, m, cqs)
    
            case ir.StmtSuperCall(x, m, qs) =>
                val cqs = immutableReified(qs)
                checkCall(x, tcp_super, m, cqs)
            
            case ir.StmtNew(x, ct, m, qs) =>
                val cd = classDecl(ct.c)
                val cqs = immutableReified(qs)
                
                if(cd.attrs.interface)
                    throw new CheckFailure("intervals.new.interface", ct.c)
                    
                // Check Ghost Types:           
                addReifiedLocal(x, ct)
                val cp_x = env.perm(x)
                val tcp_x = ir.TeeCeePee(cp_x, ct)
                addNonNull(cp_x)
                ct.ghosts.foreach { g =>
                    val (_, gfd) = env.substdFieldDecl(tcp_x, g.f)
                    val cp = env.canon(g.p)
                    checkIsSubtype(cp, gfd.wt)
                }                        
                                
                val msig_ctor = env.substdCtorSig(tcp_x, m, cqs)
                checkCallMsig(tcp_x, msig_ctor, cqs)
                
            case ir.StmtCast(x, wt, q) => ()
                // TODO Validate casts?  Issue warnings at least?
                addReifiedLocal(x, wt)
                
            case ir.StmtNull(x, wt) => 
                addReifiedLocal(x, wt)
                
            case ir.StmtReturn(op) =>
                checkNoInvalidated()
                op.foreach { p =>
                    val cp = immutableReified(p)
                    checkIsSubtype(cp, env.wt_ret)                    
                }
                if(!ss_cur.isEmpty) {
                    mergeBreakEnv(ss_cur.length - 1, List())
                    setEnv(ss_cur.head.env_in)                    
                }
                
            case ir.StmtHb(p, q) =>
                val cp = immutableReified(p)
                val cq = immutableReified(q)
                addUserHb(cp, cq)
            
            case ir.StmtLocks(p, q) =>
                val cp = immutableReified(p)
                val cq = immutableReified(q)
                addLocks(cp, cq)
            
            case ir.StmtCondBreak(i, ps) =>
                val cps = immutableReified(ps)
                mergeBreakEnv(i, cps)

            case ir.StmtBreak(i, ps) =>
                val cps = immutableReified(ps)
                mergeBreakEnv(i, cps)
                setEnv(ss_cur.head.env_in)
            
            case ir.StmtContinue(i, ps) =>
                val cps = immutableReified(ps)
                mergeContinueEnv(i, cps)
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
        cp_mthd: ir.CanonPath, 
        lvs_shared: Set[ir.VarName]
    ): FlowEnv = savingEnv {        
        log.indented("extractAssumptions(%s,%s)", cp_mthd, lvs_shared) {
            setCurrent(freshCp(ir.t_interval))
            addHbInter(cp_mthd, cp_cur)
            
            log.env(false, "Input Environment: ", env)

            val tempKeys = flow.temp.map(_._1).toList
            val tempValues = flow.temp.map(_._2).toList
            val mapFunc = PathSubst.pp(tempValues, tempKeys).path(_)
            
            def filterFunc(p: ir.Path): Boolean = 
                log.indented("filterFunc(%s)", p) {
                    lvs_shared(p.lv) && !env.isMutable(env.canon(p))
                }

            FlowEnv.empty
            .withReadableRel(flow.mapFilter(flow.readableRel, mapFunc, filterFunc))
            .withWritableRel(flow.mapFilter(flow.writableRel, mapFunc, filterFunc))
            .withHbRel(flow.mapFilter(flow.hbRel, mapFunc, filterFunc))
            .withSubintervalRel(flow.mapFilter(flow.subintervalRel, mapFunc, filterFunc))
            .withLocksRel(flow.mapFilter(flow.locksRel, mapFunc, filterFunc))
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
        if(!env.pathHasType(freshCp(wt_sub), wt_sup))
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
            subst.wcClassType(msig.wct_rcvr),
            msig.args.map { lv => ir.LvDecl(nameMap(lv.name), subst.wcTref(lv.wt)) },
            msig.reqs.map(subst.req),
            subst.wcTref(msig.wt_ret)
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
                addGhostLocal(ir.lv_mthd, ir.t_interval)
                val cp_mthd = env.perm(ir.lv_mthd)
                setCurrent(cp_mthd)
                addNonNull(cp_mthd)
                
                // For normal methods, type of this is the defining class
                val ct_this = ir.ClassType(cd.name, List(), List())
                addReifiedLocal(ir.lv_this, ct_this)
                addNonNull(cp_this)
                
                md.args.foreach(addArg)
                setWtRet(md.wt_ret)
                
                savingEnv {
                    overriddenMethodSigs(ct_this, md.name) foreach { msig_sup_0 => 
                        val msig_sup = substArgsInMethodSig(msig_sup_0, md.args.map(_.name))
                        checkArgumentTypesNonvariant(md.args, msig_sup.args)
                        checkReturnTypeCovariant(md.wt_ret, msig_sup.wt_ret)
                        checkOverridenReqsImplyOurReqs(md.reqs, msig_sup.reqs)
                    }                    
                }
                
                md.reqs.foreach(addReq)
                
                // If constructor already happened, add its effects on the environment:
                // XXX Could check for other supertypes if we wanted...
                val p_cdCtor = ir.ClassCtorFieldName(cd.name).thisPath
                if(env.hbInter(env.canon(p_cdCtor), cp_mthd)) {
                    setEnv(env.addFlow(flow_ctor_assum))
                }                
                
                checkMethodBody(md)
            }                     
        }
        
    def checkNoninterfaceConstructorDecl(cd: ir.ClassDecl, md: ir.MethodDecl) = 
        indexAt(md, FlowEnv.empty) {
            savingEnv {
                // Define special vars "method" (== this.constructor) and "this":
                val ct_this = ir.ClassType(
                    cd.name, 
                    List(), 
                    List()
                )
                addReifiedLocal(ir.lv_this, ct_this)
                addNonNull(cp_this)
                
                // Method == this.constructor[cd.name]
                val p_ctor = ir.ClassCtorFieldName(cd.name).thisPath
                val cp_ctor = env.canon(p_ctor)
                addPerm(ir.lv_mthd, cp_ctor)
                setCurrent(env.canon(ir.p_mthd))

                // Check method body:
                md.args.foreach(addArg)
                md.reqs.foreach(addReq)
                checkMethodBody(md)
                
                // Compute exit assumptions and store in prog.exportedCtorEnvs:
                val flow = extractAssumptions(cp_ctor, Set(ir.lv_this)) // Globally valid assums
                log.flow(false, "extracted assumptions:", flow)
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
                val ct_this = ir.ClassType(cd.name, List(), List())
                addReifiedLocal(ir.lv_this, ct_this)
                addGhostLocal(ir.lv_mthd, ir.t_interval)
                val cp_mthd = env.canon(ir.p_mthd)
                setCurrent(cp_mthd)
                
                // Add assumptions as though guard were satisfied.
                // Also check that guards are typed as Interval, Lock, or just Guard
                val cp_guard = env.canon(fd.p_guard)
                log.indented("adding appr. constraints for cp_guard %s", cp_guard) {
                    if(env.isSubclass(cp_guard.wt, ir.c_interval))
                        addSubintervalOf(cp_cur, cp_guard) 
                    else if(env.isSubclass(cp_guard.wt, ir.c_lock))
                        addLocks(cp_cur, cp_guard)
                    else if(env.isSubclass(cp_guard.wt, ir.c_guard))
                        addDeclaredWritableBy(cp_guard, cp_cur)
                    else
                        throw new CheckFailure("intervals.invalid.guard.type", cp_guard.wt)                    
                }
                
                // Check that each dependent path is legal:
                env.dependentPaths(fd.wt).foreach { p_full_dep => 
                    savingEnv {
                        def check(p_dep: ir.Path) {
                            log.indented("check_dep_path(%s)", p_dep) {
                                p_dep match {
                                    case ir.Path(lv, List()) => 
                                        // Always permitted.
                                        log("dependent on local var")

                                    case ir.Path(lv, List(f)) if lv == ir.lv_this => 
                                        log("dependent on another field of this")
                                        val cp_dep = env.canon(p_dep)
                                        if(env.isMutable(cp_dep) && !cd.fields.exists(_.name == f))
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
        setEnv(env.copy(c_cur = cd.name))
        if(cd.attrs.interface) checkInterfaceClassDecl(cd)
        else checkNoninterfaceClassDecl(cd)        
    }
        
}