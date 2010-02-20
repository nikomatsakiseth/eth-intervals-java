package ch.ethz.intervals

// First pass-- basic sanity checks.
class WfCheck(prog: Prog) extends TracksEnvironment(prog) 
{
    import prog.logStack.log
    import prog.logStack.indexLog
    import prog.logStack.at
    import prog.classDecl
    import prog.unboundGhostFieldsOnClassAndSuperclasses
    import prog.unboundTypeVarsDeclaredOnClassAndSuperclasses
        
    def reified(p: ir.Path) = env.reified(p)
    def reified(ps: List[ir.Path]) = env.reified(ps)    
    def rOrGhost(p: ir.Path): ir.CanonPath = env.canon(p)
    def rOrGhost(ps: List[ir.Path]): List[ir.CanonPath] = ps.map(rOrGhost)
    
    def checkIsSubclass(cp: ir.CanonPath, cs: ir.ClassName*) {
        if(!cs.exists(env.pathHasSubclass(cp, _)))
            throw new CheckFailure("intervals.expected.subclass.of.any", cp, cs)
    }
    
    def checkPathWfAndSubclass(
        canonizer: (ir.Path => ir.CanonPath),
        p: ir.Path, 
        cs: ir.ClassName*
    ): Unit = {
        val cp = canonizer(p)
        checkIsSubclass(cp, cs: _*)
    }
            
    def checkPathsWfAndSubclass(
        canonizer: (ir.Path => ir.CanonPath),        
        ps: List[ir.Path], 
        cs: ir.ClassName*
    ): Unit =
        ps.foreach(checkPathWfAndSubclass(canonizer, _, cs: _*))

    def checkPathWf(
        canonizer: (ir.Path => ir.CanonPath),                
        p: ir.Path
    ): Unit = canonizer(p)
        
    def checkPathsWf(
        canonizer: (ir.Path => ir.CanonPath),                
        ps: List[ir.Path]
    ): Unit = ps.foreach(checkPathWf(canonizer, _))
    
    def checkWPathWf(wp: ir.WcPath): Unit =
        wp match {
            case ir.WcReadableBy(ps) =>
                checkPathsWfAndSubclass(rOrGhost, ps, ir.c_interval)
                
            case ir.WcWritableBy(ps) =>
                checkPathsWfAndSubclass(rOrGhost, ps, ir.c_interval)
                
            case ir.WcHbNow(ps) =>
                checkPathsWfAndSubclass(rOrGhost, ps, ir.c_interval)
                
            case p: ir.Path =>
                checkPathWf(rOrGhost, p)
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
            bounds.owts_ub.foreach(_.foreach(checkWtrefWf))
            
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
                val crp = reified(pt.p)
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
    
    def checkBranch(i: Int, stmts_stack: List[ir.StmtCompound], ps: List[ir.Path]) {
        ps.foreach(checkPathWf(reified, _))
        if(i >= stmts_stack.length)
            throw new CheckFailure("intervals.invalid.stack.index", i, stmts_stack.length)
    }

    def checkStatement(stmts_stack: List[ir.StmtCompound])(stmt: ir.Stmt): Unit =
        at(stmt, ()) {
            stmt match {                  
                case ir.StmtSuperCtor(m, qs) =>
                    val tcp = tcp_super
                    val cqs = qs.map(reified)
                    val msig_ctor = env.substdCtorSig(tcp, m, cqs)
                    checkCall(tcp, msig_ctor, cqs)
                    
                case ir.StmtGetField(x, p_o, f) =>
                    val crp_o = reified(p_o)
                    val (_, rfd) = env.substdReifiedFieldDecl(crp_o.toTcp, f) 
                    addReifiedLocal(x, rfd.wt)
                    
                case ir.StmtSetField(p_o, f, p_v) =>
                    checkPathWf(reified, p_v)
                    
                    val crp_o = reified(p_o)
                    env.substdReifiedFieldDecl(crp_o.toTcp, f) 

                case ir.StmtCheckType(p, wt) =>
                    checkPathWf(reified, p)
                    checkWtrefWf(wt)

                case ir.StmtCall(x, p, m, qs) =>
                    val tcp = reified(p).toTcp
                    val cqs = reified(qs)
                    val msig = env.substdMethodSig(tcp, m, cqs)
                    checkCall(tcp, msig, cqs)
                    addReifiedLocal(x, msig.wt_ret)
        
                case ir.StmtSuperCall(x, m, qs) =>
                    val tcp = tcp_super
                    val cqs = reified(qs)
                    val msig = env.substdMethodSig(tcp, m, cqs)
                    checkCall(tcp, msig, cqs)
                    addReifiedLocal(x, msig.wt_ret)
                
                case ir.StmtNew(x, ct, m, qs) =>
                    if(classDecl(ct.c).attrs.interface)
                        throw new CheckFailure("intervals.new.interface", ct.c)
                    checkWtrefWf(ct)
                    checkPathsWf(rOrGhost, ct.ghosts.map(_.p))
                    val cqs = reified(qs)
                    
                    // Check that all ghosts on the type C being instantiated are given a value:
                    val gfds_unbound = unboundGhostFieldsOnClassAndSuperclasses(ct.c)
                    gfds_unbound.find(gfd => env.ghost(ct, gfd.name).isEmpty) match {
                        case Some(gfd) => throw new CheckFailure("intervals.no.value.for.ghost", gfd.name)
                        case None =>
                    }
                    
                    // Check that all type vars on the type C being instantiated are given a value:
                    val tvds_unbound = unboundTypeVarsDeclaredOnClassAndSuperclasses(ct.c)
                    tvds_unbound.find(tvd => env.typeArg(ct, tvd.name).isEmpty) match {
                        case Some(tvd) => throw new CheckFailure("intervals.no.value.for.type.var", tvd.name)
                        case None =>
                    }
                    
                    addReifiedLocal(x, ct)
                    val tcp_x = ir.TeeCeePee(env.reified(x.path), ct)
                    val msig_ctor = env.substdCtorSig(tcp_x, m, cqs)
                    checkCall(tcp_x, msig_ctor, cqs)                    
                    
                case ir.StmtCast(x, wt, p) => 
                    checkPathWf(reified, p)
                    checkWtrefWf(wt)
                    addReifiedLocal(x, wt)
                    
                case ir.StmtNull(x, wt) => 
                    checkWtrefWf(wt)
                    addReifiedLocal(x, wt)
                    
                case ir.StmtReturn(op) =>
                    op match {
                        case Some(p) =>
                            reified(p)                            
                        
                        case None =>
                            if(env.wt_ret != ir.t_void)
                                throw new CheckFailure("intervals.illegal.empty.return", env.wt_ret)
                    }
                    
                case ir.StmtHb(p, q) =>                
                    checkPathWfAndSubclass(reified, p, ir.c_point, ir.c_interval)
                    checkPathWfAndSubclass(reified, q, ir.c_point, ir.c_interval)
                    
                case ir.StmtLocks(p, q) =>
                    checkPathWfAndSubclass(reified, p, ir.c_interval)
                    checkPathWfAndSubclass(reified, q, ir.c_lock)
                    
                case ir.StmtBreak(i, ps) =>
                    checkBranch(i, stmts_stack, ps)
                    val stmt_compound = stmts_stack(i)
                    checkLengths(stmt_compound.defines, ps, "intervals.incorrect.number.of.branch.args")

                case ir.StmtCondBreak(i, ps) =>
                    checkBranch(i, stmts_stack, ps)
                    val stmt_compound = stmts_stack(i)
                    checkLengths(stmt_compound.defines, ps, "intervals.incorrect.number.of.branch.args")
                    
                case ir.StmtContinue(i, ps) =>
                    checkBranch(i, stmts_stack, ps)
                    val stmt_compound = stmts_stack(i)
                    stmt_compound.kind match {
                        case ir.Loop(args, _, _) =>
                            checkLengths(args, ps, "intervals.incorrect.number.of.branch.args")
                            
                        case kind =>
                            throw new CheckFailure("intervals.continue.to.nonloop", kind)
                    }

                case stmt_c: ir.StmtCompound =>
                    stmt_c.kind match {
                        case ir.Block(_) =>
                        case ir.Switch(_) =>
                        case ir.Loop(_, ps_initial, _) =>
                            ps_initial.foreach(checkPathWf(reified, _))
                        case ir.Subinterval(x, ps_locks, _) =>
                            ps_locks.foreach(checkPathWf(reified, _))
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
                    checkPathsWfAndSubclass(rOrGhost, ps, ir.c_guard)
                    checkPathsWfAndSubclass(rOrGhost, qs, ir.c_interval)
                case ir.ReqReadableBy(ps, qs) => 
                    checkPathsWfAndSubclass(rOrGhost, ps, ir.c_guard)
                    checkPathsWfAndSubclass(rOrGhost, qs, ir.c_interval)
                case ir.ReqHb(ps, qs) => 
                    checkPathsWfAndSubclass(rOrGhost, ps, ir.c_point, ir.c_interval)
                    checkPathsWfAndSubclass(rOrGhost, qs, ir.c_point, ir.c_interval)
                case ir.ReqSubintervalOf(ps, qs) => 
                    checkPathsWfAndSubclass(rOrGhost, ps, ir.c_interval)
                    checkPathsWfAndSubclass(rOrGhost, qs, ir.c_interval)
            }   
        }
    
    def addThis(cd: ir.ClassDecl) {
        addReifiedLocal(ir.lv_this, ir.WcClassType(cd.name, List(), List()))
    }

    def checkNoninterfaceMethodDecl(
        cd: ir.ClassDecl,          // class in which the method is declared
        md: ir.MethodDecl          // method to check
    ) = 
        at(md, ()) {
            savingEnv {
                // Define special vars "method" and "this":
                addGhostLocal(ir.lv_mthd, ir.wt_constructedInterval)
                addThis(cd)

                setCurrent(env.canon(ir.p_mthd))
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
                addThis(cd)
                val cp_ctor = env.canon(ir.ClassCtorFieldName(cd.name).thisPath)
                addPerm(ir.lv_mthd, cp_ctor)

                setCurrent(env.canon(ir.p_mthd))
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
        val f = decl.name
        prog.classAndSuperclasses(env.c_cur).foreach { c =>
            classDecl(c).ghostFieldDecls.filter(_.isNamed(f)).foreach { gfd =>
                if(gfd ne decl)
                    throw new CheckFailure("intervals.shadowed", c, f)                
            }
            classDecl(c).reifiedFieldDecls.filter(_.isNamed(f)).foreach { rfd =>
                if(rfd ne decl)
                    throw new CheckFailure("intervals.shadowed", c, f)                
            }
        }        
    }
    
    def checkReifiedFieldDecl(cd: ir.ClassDecl, rfd: ir.ReifiedFieldDecl): Unit = {
        at(rfd, ()) {
            savingEnv {
                addThis(cd)                
                checkFieldNameNotShadowed(env, rfd)
                checkWtrefWf(rfd.wt)
                checkPathWf(rOrGhost, rfd.p_guard)
            }
        }        
    }
    
    def checkGhostFieldDecl(cd: ir.ClassDecl, gfd: ir.GhostFieldDecl): Unit = {
        at(gfd, ()) {
            savingEnv {
                addThis(cd)
                checkFieldNameNotShadowed(env, gfd)
                classDecl(gfd.c)
            }
        }        
    }
        
    def checkTypeVarDecl(cd: ir.ClassDecl, tvd: ir.TypeVarDecl): Unit = {
        at(tvd, ()) {
            savingEnv {
                addThis(cd)

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
                setEnv(env.copy(c_cur = cd.name))
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
                setEnv(env.copy(c_cur = cd.name))
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