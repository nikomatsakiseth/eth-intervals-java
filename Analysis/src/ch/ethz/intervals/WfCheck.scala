package ch.ethz.intervals

import Util._

// First pass-- basic sanity checks.
class WfCheck(prog: Prog) extends TracksEnvironment(prog) 
{
    import prog.logStack.log
    import prog.logStack.indexLog
    import prog.logStack.at
    import prog.classDecl
    import prog.unboundGhostFieldsOnClassAndSuperclasses
    import prog.unboundTypeVarsDeclaredOnClassAndSuperclasses
    
    def canonPath = env.canonPath _
    def canonLv = env.canonLv _
    def reifiedPath = env.reifiedPath _
    def reifiedLv = env.reifiedLv _
        
    def checkIsSubclass(cp: ir.CanonPath, cs: ir.ClassName*) {
        if(!cs.exists(env.pathHasSubclass(cp, _)))
            throw new CheckFailure("intervals.expected.subclass.of.any", cp.p, ", ".join(cs))
    }
    
    def checkCanonAndSubclass[X](
        canonizer: (X => ir.CanonPath),
        arg: X, 
        cs: ir.ClassName*
    ): Unit = {
        val cp = canonizer(arg)
        checkIsSubclass(cp, cs: _*)
    }
            
    def checkCanonAndSubclasses[X](
        canonizer: (X => ir.CanonPath),        
        ps: List[X], 
        cs: ir.ClassName*
    ): Unit =
        ps.foreach(checkCanonAndSubclass(canonizer, _, cs: _*))

    def checkWPathWf(wp: ir.WcPath): Unit =
        wp match {
            case ir.WcReadableBy(ps) =>
                checkCanonAndSubclasses(canonPath, ps, ir.c_interval)
                
            case ir.WcWritableBy(ps) =>
                checkCanonAndSubclasses(canonPath, ps, ir.c_interval)
                
            case ir.WcHbNow(ps) =>
                checkCanonAndSubclasses(canonPath, ps, ir.c_interval)
                
            case p: ir.Path =>
                env.canonPath(p)
        }
        
    def checkLengths(l1: List[_], l2: List[_], msg: String) = preservesEnv {
        if(l1.length != l2.length)
            throw new CheckFailure(msg, l1.length, l2.length)
    }
    
    def checkWghostsWf(wt: ir.WcClassType) {
        // Note: we don't check that the arguments
        // match the various ghost bounds etc.  We just
        // check that when the type is constructed.
        
        wt.wghosts.foldLeft(List[ir.FieldName]()) {
            case (l, wg) if l.contains(wg.f) => throw new CheckFailure("intervals.duplicate.ghost", wg.f)
            case (l, wg) => wg.f :: l
        }

        wt.wghosts.map(_.wp).foreach(checkWPathWf)

        // Every ghost should be for some unbound ghost field:
        val gfds_unbound = unboundGhostFieldsOnClassAndSuperclasses(wt.c)
        val expectedFieldNames: Set[ir.FieldName] = gfds_unbound.map(_.name) + ir.f_objCtor
        wt.wghosts.find(wg => !expectedFieldNames(wg.f)).foreach { wg =>
            throw new CheckFailure("intervals.no.such.ghost", wt.c, wg.f)
        }
    }
    
    def checkWtargWf(wta: ir.WcTypeArg) = wta match {
        case ir.BoundedTypeArg(tv, bounds) =>
            bounds.wts_lb.foreach(checkWtrefWf)
            bounds.wts_ub.foreach(checkWtrefWf)
            
        case ir.TypeArg(tv, wt) =>
            checkWtrefWf(wt)
    }
    
    def checkWtargsWf(wt: ir.WcClassType) {
        wt.wtargs.foldLeft(List[ir.TypeVarName]()) {
            case (l, wta) if l.contains(wta.tv) => throw new CheckFailure("intervals.duplicate.type.var", wta.tv)
            case (l, wta) => wta.tv :: l            
        }
        
        wt.wtargs.foreach(checkWtargWf)
        
        // Every type arg should be for some unbound type variable:
        val tvds_unbound = unboundTypeVarsDeclaredOnClassAndSuperclasses(wt.c)
        val expectedTvNames = tvds_unbound.map(_.name)
        wt.wtargs.find(wta => !expectedTvNames(wta.tv)).foreach { wta =>
            throw new CheckFailure("intervals.no.such.type.var", wt.c, wta.tv)            
        }
    }
    
    def checkWtrefWf(wt: ir.WcTypeRef) {
        wt match {
            case pt: ir.PathType =>
                val crp = env.reifiedPath(pt.p)
                val tvds = env.typeVarsDeclaredOnType(crp.wt)
                tvds.find(_.isNamed(pt.tv)) match {
                    case None => throw new CheckFailure("intervals.no.such.type.var", crp, pt.tv)
                    case Some(_) =>
                }
            
            case wt: ir.WcClassType =>
                checkWghostsWf(wt)
        }
    }
    
    def checkCall(tcp: ir.TeeCeePee[ir.WcTypeRef], msig: ir.MethodSig, cqs: List[ir.CanonPath]) {
        checkLengths(msig.wts_args, cqs, "intervals.wrong.number.method.arguments")        
    }
    
    def checkBranch(i: Int, stmts_stack: List[ir.StmtCompound], lvs: List[ir.VarName]) {
        env.reifiedLvs(lvs)
        if(i >= stmts_stack.length)
            throw new CheckFailure("intervals.invalid.stack.index", i, stmts_stack.length)
    }

    def checkStatement(stmts_stack: List[ir.StmtCompound])(stmt: ir.Stmt): Unit =
        at(stmt, ()) {
            stmt match {                  
                case ir.StmtSuperCtor(m, lvs_args) =>
                    val tcp_rcvr = env.tcp_super
                    val cps_args = env.reifiedLvs(lvs_args)
                    val msig_ctor = env.substdCtorSig(tcp_rcvr, m, cps_args)
                    checkCall(tcp_rcvr, msig_ctor, cps_args)
                    
                case ir.StmtGetField(lv_def, lv_owner, f) =>
                    val crp_owner = env.reifiedLv(lv_owner)
                    val (_, rfd) = env.substdReifiedFieldDecl(crp_owner.toTcp, f) 
                    addReifiedLocal(lv_def, rfd.wt)
                    
                case ir.StmtSetField(lv_owner, f, lv_value) =>
                    env.reifiedLv(lv_value)
                    
                    val crp_owner = env.reifiedLv(lv_owner)
                    env.substdReifiedFieldDecl(crp_owner.toTcp, f) 

                case ir.StmtCheckType(lv, wt) =>
                    env.reifiedLv(lv)
                    checkWtrefWf(wt)

                case ir.StmtCall(lv_def, lv_rcvr, m, lvs_args) =>
                    val tcp_rcvr = env.reifiedLv(lv_rcvr).toTcp
                    val cps_args = env.reifiedLvs(lvs_args)
                    val msig = env.substdMethodSig(tcp_rcvr, m, cps_args)
                    checkCall(tcp_rcvr, msig, cps_args)
                    addReifiedLocal(lv_def, msig.wt_ret)
        
                case ir.StmtSuperCall(lv_def, m, qs) =>
                    val tcp = env.tcp_super
                    val cqs = env.reifiedLvs(qs)
                    val msig = env.substdMethodSig(tcp, m, cqs)
                    checkCall(tcp, msig, cqs)
                    addReifiedLocal(lv_def, msig.wt_ret)
                
                case ir.StmtNew(lv_def, ct, m, lvs_args) =>
                    if(classDecl(ct.c).attrs.interface)
                        throw new CheckFailure("intervals.new.interface", ct.c)
                        
                    // XXX currently you cannot specify an explicit
                    //     objCtor for interval subtypes.  This is 
                    //     needed so that we can assume that this.Constructor hb this,
                    //     which is basically always true.  We should however find
                    //     a less restrictive check (see TODO.sp for more details).
                    if(env.isSubclass(ct, ir.c_interval)) {
                        ct.ghosts.find(_.isNamed(ir.f_objCtor)).foreach { g =>
                            throw new CheckFailure("intervals.explicit.objCtor.on.interval", g.p)
                        }
                    }
                        
                    checkWtrefWf(ct)
                    env.canonPaths(ct.ghosts.map(_.p))
                    val cps_args = env.reifiedLvs(lvs_args)
                    
                    // Check that all ghosts on the type C being instantiated are given a value:
                    val gfds_unbound = unboundGhostFieldsOnClassAndSuperclasses(ct.c)
                    gfds_unbound.find(gfd => ct.ghosts.find(_.isNamed(gfd.name)).isEmpty) match {
                        case Some(gfd) => throw new CheckFailure("intervals.no.value.for.ghost", gfd.name)
                        case None =>
                    }
                    
                    // Check that all type vars on the type C being instantiated are given a value:
                    val tvds_unbound = unboundTypeVarsDeclaredOnClassAndSuperclasses(ct.c)
                    tvds_unbound.find(tvd => ct.targs.find(_.isNamed(tvd.name)).isEmpty) match {
                        case Some(tvd) => throw new CheckFailure("intervals.no.value.for.type.var", tvd.name)
                        case None =>
                    }
                    
                    addReifiedLocal(lv_def, ct)
                    val tcp_def = ir.TeeCeePee(env.reifiedLv(lv_def), ct)
                    val msig_ctor = env.substdCtorSig(tcp_def, m, cps_args)
                    checkCall(tcp_def, msig_ctor, cps_args)                    
                    
                case ir.StmtCast(lv_def, wt, lv) => 
                    env.reifiedLv(lv)
                    checkWtrefWf(wt)
                    addReifiedLocal(lv_def, wt)
                    
                case ir.StmtNull(lv_def, wt) => 
                    checkWtrefWf(wt)
                    addReifiedLocal(lv_def, wt)
                    
                case ir.StmtReturn(olv) =>
                    olv.foreach(lv => env.reifiedLv(lv))
                    
                case ir.StmtHb(lv_from, lv_to) =>                
                    checkCanonAndSubclass(env.reifiedLv _, lv_from, ir.c_point, ir.c_interval)
                    checkCanonAndSubclass(env.reifiedLv _, lv_to, ir.c_point, ir.c_interval)
                    
                case ir.StmtLocks(lv_inter, lv_lock) =>
                    checkCanonAndSubclass(env.reifiedLv _, lv_inter, ir.c_interval)
                    checkCanonAndSubclass(env.reifiedLv _, lv_lock, ir.c_lock)
                    
                case ir.StmtBreak(i, lvs) =>
                    checkBranch(i, stmts_stack, lvs)
                    val stmt_compound = stmts_stack(i)
                    checkLengths(stmt_compound.defines, lvs, "intervals.incorrect.number.of.branch.args")

                case ir.StmtCondBreak(i, lvs) =>
                    checkBranch(i, stmts_stack, lvs)
                    val stmt_compound = stmts_stack(i)
                    checkLengths(stmt_compound.defines, lvs, "intervals.incorrect.number.of.branch.args")
                    
                case ir.StmtContinue(i, lvs) =>
                    checkBranch(i, stmts_stack, lvs)
                    val stmt_compound = stmts_stack(i)
                    stmt_compound.kind match {
                        case ir.Loop(args, _, _) =>
                            checkLengths(args, lvs, "intervals.incorrect.number.of.branch.args")
                            
                        case kind =>
                            throw new CheckFailure("intervals.continue.to.nonloop", kind)
                    }

                case stmt_c: ir.StmtCompound =>
                    stmt_c.kind match {
                        case ir.Block(_) =>
                        case ir.Switch(_) =>
                        case ir.Loop(_, lvs_initial, _) =>
                            env.reifiedLvs(lvs_initial)
                        case ir.InlineInterval(x, lvs_locks, _) =>
                            env.reifiedLvs(lvs_locks)
                            addReifiedLocal(x, ir.wt_constructedInterval)
                        case ir.TryCatch(_, _) =>
                    }
                    
                    savingEnv {
                        stmt_c.kind.subseqs.foreach(checkStatementSeq(stmt_c :: stmts_stack))                        
                    }
                
                    stmt_c.defines.foreach(addArg)
            }        
        }
        
    def checkStatementSeq(stmts_stack: List[ir.StmtCompound])(seq: ir.StmtSeq) {
        seq.stmts.foreach(checkStatement(stmts_stack))
    }
    
    def addCheckedArg(arg: ir.LvDecl) {
        addArg(arg)
        checkWtrefWf(arg.wt) 
    }
    
    def checkReq(req: ir.Req) =
        at(req, ()) {
            req match {
                case ir.ReqWritableBy(ps, qs) =>
                    checkCanonAndSubclasses(canonPath, ps, ir.c_guard)
                    checkCanonAndSubclasses(canonPath, qs, ir.c_interval)
                case ir.ReqReadableBy(ps, qs) => 
                    checkCanonAndSubclasses(canonPath, ps, ir.c_guard)
                    checkCanonAndSubclasses(canonPath, qs, ir.c_interval)
                case ir.ReqHb(ps, qs) => 
                    checkCanonAndSubclasses(canonPath, ps, ir.c_point, ir.c_interval)
                    checkCanonAndSubclasses(canonPath, qs, ir.c_point, ir.c_interval)
                case ir.ReqSuspends(ps, qs) => 
                    checkCanonAndSubclasses(canonPath, ps, ir.c_interval)
                    checkCanonAndSubclasses(canonPath, qs, ir.c_interval)
            }   
        }
    
    def checkNoninterfaceMethodDecl(
        cd: ir.ClassDecl,          // class in which the method is declared
        md: ir.MethodDecl          // method to check
    ) = 
        at(md, ()) {
            savingEnv {
                // Define special vars "method" and "this":
                addGhostLocal(ir.lv_mthd, ir.wt_constructedInterval)

                setCurrent(env.canonPath(ir.p_mthd))
                md.args.foreach(addCheckedArg)
                checkWtrefWf(md.wt_ret)                
                md.reqs.foreach(checkReq)
                checkStatementSeq(List())(md.body)
            }         
        }
        
    def checkNoninterfaceConstructorDecl(cd: ir.ClassDecl, md: ir.MethodDecl): Unit = 
        at(md, ()) {
            savingEnv {
                // Define special vars "method" (== this.constructor) and "this":
                val cp_ctor = env.canonPath(ir.ClassCtorFieldName(cd.name).thisPath)
                addPerm(ir.lv_mthd, cp_ctor)

                setCurrent(env.canonPath(ir.p_mthd))
                md.args.foreach(addCheckedArg)
                md.reqs.foreach(checkReq)
        
                // TODO -- Have the checking of super() verify that p_this is a ghost
                // and reify it, and thus permit control-flow.  We then have to propagate
                // p_this between blocks though (yuck).
        
                checkStatementSeq(List())(md.body)
            }          
        }
        
    def checkFieldNameNotShadowed(
        env: TcEnv,
        decl: ({ def name: ir.FieldName })
    ) = {
        log.indented("checkFieldNameNotShadowed(%s)", decl) {
            val f = decl.name
            prog.classAndSuperclasses(env.c_this).foreach { c =>
                log.indented("class(%s)", c) {
                    classDecl(c).ghostFieldDecls.filter(_.isNamed(f)).foreach { gfd =>
                        log("gfd: %s (eq? %s)", gfd, gfd eq decl)
                        if(gfd ne decl)
                            throw new CheckFailure("intervals.shadowed", c, f)                
                    }
                    classDecl(c).reifiedFieldDecls.filter(_.isNamed(f)).foreach { rfd =>
                        log("rfd: %s (eq? %s)", rfd, rfd eq decl)
                        if(rfd ne decl)
                            throw new CheckFailure("intervals.shadowed", c, f)                
                    }
                }
            }                    
        }
    }
    
    def checkReifiedFieldDecl(cd: ir.ClassDecl, rfd: ir.ReifiedFieldDecl): Unit = {
        at(rfd, ()) {
            savingEnv {
                checkFieldNameNotShadowed(env, rfd)
                checkWtrefWf(rfd.wt)
                env.canonPath(rfd.p_guard)
            }
        }        
    }
    
    def checkGhostFieldDecl(cd: ir.ClassDecl, gfd: ir.GhostFieldDecl): Unit = {
        at(gfd, ()) {
            savingEnv {
                checkFieldNameNotShadowed(env, gfd)
                classDecl(gfd.c)
            }
        }        
    }
        
    def checkTypeVarDecl(cd: ir.ClassDecl, tvd: ir.TypeVarDecl): Unit = {
        at(tvd, ()) {
            savingEnv {
                // Check that type vars are not shadowed:
                prog.classAndSuperclasses(cd.name).foreach { c =>
                    classDecl(c).typeVarDecls.filter(_.isNamed(tvd.name)).foreach { tvd1 =>
                        if(tvd ne tvd1)
                            throw new CheckFailure("intervals.shadowed.type.var", c, tvd.name)                        
                    }
                }
                
                tvd.wts_lb.foreach(checkWtrefWf)
            }
        }        
    }
        
    // ___ Classes and interfaces ___________________________________________
    
    def checkIsInterface(c: ir.ClassName) {
        val cd = classDecl(c)
        if(!cd.attrs.interface) 
            throw new CheckFailure("intervals.superType.not.interface", c)
    }
    
    def checkInterfaceSuperclass(c: ir.ClassName) {
        val cd = classDecl(c)
        if(c != ir.c_object) checkIsInterface(c)
    }
    
    def checkInterfaceConstructorDecl(cd: ir.ClassDecl, md: ir.MethodDecl) = {
        at(md, ()) {
            // This doesn't quite work: goal is just to verify that interface ctor's
            // do not work.
            //if(md != ir.md_emptyCtor)
            //    throw new CheckFailure("intervals.invalid.ctor.in.interface")
        }        
    }
    
    def checkInterfaceMethodDecl(cd: ir.ClassDecl, md: ir.MethodDecl) = {
        checkNoninterfaceMethodDecl(cd, md)        
    }
    
    def checkInterfaceClassDecl(cd: ir.ClassDecl) = {
        at(cd, ()) {
            savingEnv {
                // TODO Is this everything?
                setEnv(env.withThisClass(cd.name))
                cd.superClasses.foreach(checkInterfaceSuperclass)
                if(!cd.reifiedFieldDecls.isEmpty)
                    throw new CheckFailure("intervals.interface.with.fields")
                cd.ctors.foreach(checkInterfaceConstructorDecl(cd, _))
                cd.methods.foreach(checkInterfaceMethodDecl(cd, _))
            }
        }        
    }
        
    def checkIsNotInterface(c: ir.ClassName) {
        val cd_super = classDecl(c)
        if(cd_super.attrs.interface) 
            throw new CheckFailure("intervals.superType.interface", c)
    }

    def checkNoninterfaceClassDecl(cd: ir.ClassDecl) = 
        at(cd, ()) {
            savingEnv {
                setEnv(env.withThisClass(cd.name))
                cd.superClasses.take(1).foreach(checkIsNotInterface)
                cd.superClasses.drop(1).foreach(checkIsInterface)
                cd.ghostFieldDecls.foreach(checkGhostFieldDecl(cd, _))
                cd.reifiedFieldDecls.foreach(checkReifiedFieldDecl(cd, _))
                cd.ctors.foreach(checkNoninterfaceConstructorDecl(cd, _))
                cd.methods.foreach(checkNoninterfaceMethodDecl(cd, _))                    
            }
        }
        
    def checkClassDecl(cd: ir.ClassDecl) = log.indented(cd) {        
        if(cd.attrs.interface) checkInterfaceClassDecl(cd)
        else checkNoninterfaceClassDecl(cd)
    }        
}