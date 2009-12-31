package ch.ethz.intervals

// First pass-- basic sanity checks.
class WfCheck(prog: Prog) 
extends TracksEnvironment(prog) {
    import prog.log
    import prog.classDecl
    import prog.at
    import prog.isSubclass
    import prog.ghostFieldDecls
    import prog.thisTref
    import prog.typeOriginallyDefiningMethod
        
    // In this check, we don't record HB information,
    // so we permit mutable paths at all places:
    val ghostOk = ir.ghostAttrs.withMutable
    val reified = ir.mutableAttrs
    
    def checkIsSubclass(wt: ir.WcTypeRef, cs: ir.ClassName*) {
        if(!cs.exists(isSubclass(wt, _)))
            throw new ir.IrError("intervals.expected.subclass.of.any", wt.c, cs)
    }
    
    def checkPathWfAndSubclass(as: ir.Attrs, p: ir.Path, cs: ir.ClassName*): Unit = {
        val tp = teePee(as, p)
        checkIsSubclass(tp.wt, cs: _*)
    }
            
    def checkPathsWfAndSubclass(as: ir.Attrs, ps: List[ir.Path], cs: ir.ClassName*): Unit =
        ps.foreach(checkPathWfAndSubclass(as, _, cs: _*))
    
    def checkPathWf(as: ir.Attrs, p: ir.Path): Unit =
        teePee(as, p)        

    def checkPathsWf(as: ir.Attrs, ps: List[ir.Path]): Unit =
        teePee(as, ps)        
        
    def checkWPathWf(wp: ir.WcPath): Unit =
        wp match {
            case ir.WcReadableBy(ps) =>
                checkPathsWfAndSubclass(ghostOk, ps, ir.c_interval)
                
            case ir.WcWritableBy(ps) =>
                checkPathsWfAndSubclass(ghostOk, ps, ir.c_interval)
                
            case ir.WcHb(ps, qs) =>
                checkPathsWfAndSubclass(ghostOk, ps, ir.c_interval, ir.c_point)
                checkPathsWfAndSubclass(ghostOk, qs, ir.c_interval, ir.c_point)
                
            case ir.WcLocks(ps) =>
                checkPathsWfAndSubclass(ghostOk, ps, ir.c_lock)
                
            case ir.WcLockedBy(ps) =>
                checkPathsWfAndSubclass(ghostOk, ps, ir.c_interval)
                
            case p: ir.Path =>
                checkPathWf(ghostOk, p)
        }
    
    def checkWfWt(wt: ir.WcTypeRef) {
        wt.wghosts.foldLeft(List[ir.FieldName]()) {
            case (l, wg) if l.contains(wg.f) => throw new ir.IrError("intervals.duplicate.ghost", wg.f)
            case (l, wg) => wg.f :: l
        }
        
        wt.wghosts.map(_.wp).foreach(checkWPathWf)

        val lgfd = ghostFieldDecls(wt.c)
        def notDefined(wg: ir.WcGhost) = !lgfd.exists(_.name == wg.f)        
        wt.wghosts.find(notDefined) match {
            case Some(wg) => throw new ir.IrError("intervals.no.such.ghost", wt.c, wg.f)
            case None =>
        }

        // Note: we don't check that the arguments
        // match the various ghost bounds etc.  We just
        // check that when the type is constructed.
    }

    def checkStatement(stmt: ir.Stmt): Unit =
        at(stmt, ()) {
            stmt match {                  
                case ir.StmtSuperCtor(qs) =>
                    if(!tp_this.as.ghost)
                        throw new ir.IrError("intervals.super.ctor.not.permitted.here")
                    checkPathsWf(reified, qs)
                    
                case ir.StmtGetField(x, p_o, f) =>
                    val tp_o = teePee(reified, p_o)
                    substdFieldDecl(tp_o, f) match {
                        case ir.GhostFieldDecl(_, _) =>
                            throw new ir.IrError("intervals.not.reified", tp_o.wt.c, f)
                        
                        case ir.ReifiedFieldDecl(_, wt, _, p_guard) =>
                            addLvDecl(x, wt, None)
                    }                    
                    
                case ir.StmtSetField(p_o, f, p_v) =>
                    checkPathWf(reified, p_v)
                    
                    val tp_o = teePee(reified, p_o)
                    substdFieldDecl(tp_o, f) match {
                        case ir.GhostFieldDecl(_, _) =>
                            throw new ir.IrError("intervals.not.reified", tp_o.wt.c, f)
                        
                        case ir.ReifiedFieldDecl(_, _, _, _) =>
                    }

                case ir.StmtCall(x, p, m, qs) =>
                    val tp = teePee(reified, p)
                    val tqs = teePee(reified, qs)
                    val msig = substdMethodSig(tp, m, tqs)
                    addLvDecl(x, msig.wt_ret, None)                        
        
                case ir.StmtSuperCall(x, m, qs) =>
                    val tqs = teePee(reified, qs)
                    val msig = substdMethodSig(tp_super, m, tqs)
                    addLvDecl(x, msig.wt_ret, None)                        
                
                case ir.StmtNew(x, t, qs) =>
                    if(classDecl(t.c).attrs.interface)
                        throw new ir.IrError("intervals.new.interface", t.c)
                    checkWfWt(t)
                    checkPathsWf(ghostOk, t.ghosts.map(_.p))
                    checkPathsWf(reified, qs)
                    addLvDecl(x, t, None)
                    
                case ir.StmtCast(x, wt, p) => 
                    checkPathWf(reified, p)
                    checkWfWt(wt)
                    addLvDecl(x, wt, None)
                    
                case ir.StmtNull(x, wt) => 
                    checkWfWt(wt)
                    addLvDecl(x, wt, None)
                    
                case ir.StmtReturn(p) =>
                    teePee(reified, p)
                    
                case ir.StmtHb(p, q) =>                
                    checkPathWfAndSubclass(reified, p, ir.c_point, ir.c_interval)
                    checkPathWfAndSubclass(reified, q, ir.c_point, ir.c_interval)
                    
                case ir.StmtLocks(p, q) =>
                    checkPathWfAndSubclass(reified, p, ir.c_interval)
                    checkPathWfAndSubclass(reified, q, ir.c_lock)
                    
                case ir.StmtSubinterval(x, lp_locks, stmts) =>
                    checkPathsWfAndSubclass(reified, lp_locks, ir.c_lock)                    
                    addLvDecl(x, ir.t_interval, None)                    
                    stmts.foreach(checkStatement)                        
            }        
        }
    
    def checkStatements(stmts: List[ir.Stmt]): Unit =
        stmts.foreach(checkStatement)
    
    def introduceArg(arg: ir.LvDecl) {
        checkWfWt(arg.wt) 
        addPerm(arg.name, ir.TeePee(arg.wt, arg.name.path, ir.noAttrs))        
    }
    
    def checkReq(req: ir.Req) =
        at(req, ()) {
            req match {
                case ir.ReqWritableBy(ps, qs) =>
                    checkPathsWfAndSubclass(ghostOk, ps, ir.c_guard)
                    checkPathsWfAndSubclass(ghostOk, qs, ir.c_interval)
                case ir.ReqReadableBy(ps, qs) => 
                    checkPathsWfAndSubclass(ghostOk, ps, ir.c_guard)
                    checkPathsWfAndSubclass(ghostOk, qs, ir.c_interval)
                case ir.ReqHb(ps, qs) => 
                    checkPathsWfAndSubclass(ghostOk, ps, ir.c_point, ir.c_interval)
                    checkPathsWfAndSubclass(ghostOk, qs, ir.c_point, ir.c_interval)
                case ir.ReqSubintervalOf(ps, qs) => 
                    checkPathsWfAndSubclass(ghostOk, ps, ir.c_interval)
                    checkPathsWfAndSubclass(ghostOk, qs, ir.c_interval)
            }   
        }
    
    def checkGoto(blks: Array[ir.Block], succ: ir.Goto) =
        at(succ, ()) {
            if(succ.b < 0 || succ.b >= blks.length)
                throw new ir.IrError("intervals.invalid.blk.id", succ.b)
            checkPathsWf(reified, succ.ps)        
        }
    
    def checkBlock(blks: Array[ir.Block], b: Int, blk: ir.Block) = 
        log.indented("%s: %s", b, blk) {
            savingEnv {
                blk.args.foreach(introduceArg)                
                checkStatements(blk.stmts)
                blk.gotos.foreach(checkGoto(blks, _))
            }
        }
        
    def checkNoninterfaceMethodDecl(
        cd: ir.ClassDecl,          // class in which the method is declared
        md: ir.MethodDecl          // method to check
    ) = 
        at(md, ()) {
            savingEnv {
                // Define special vars "method" and "this":
                addPerm(ir.lv_mthd, ir.TeePee(ir.t_interval, ir.p_mthd, ir.ghostAttrs))
                if(!md.attrs.ctor) { 
                    // For normal methods, type of this is the defining class
                    addPerm(ir.lv_this, ir.TeePee(thisTref(cd), ir.p_this, ir.noAttrs))                     
                } else {
                    // For constructor methods, type of this is the type that originally defined it
                    val t_rcvr = typeOriginallyDefiningMethod(cd.name, md.name).get
                    addPerm(ir.lv_this, ir.TeePee(t_rcvr.ctor, ir.p_this, ir.noAttrs))
                }  
                
                md.args.foreach(introduceArg)
                checkWfWt(md.wt_ret)                
                md.reqs.foreach(checkReq)                
                md.blocks.zipWithIndex.foreach { case (blk, idx) =>
                    checkBlock(md.blocks, idx, blk)
                }
            }         
        }
        
    def checkNoninterfaceConstructorDecl(cd: ir.ClassDecl, md: ir.MethodDecl): Unit = 
        at(md, ()) {
            savingEnv {
                // Define special vars "method" (== this.constructor) and "this":
                addPerm(ir.lv_mthd, ir.TeePee(ir.t_interval, ir.gfd_ctor.thisPath, ir.ghostAttrs))
                addPerm(ir.lv_this, ir.TeePee(thisTref(cd, ir.ctorAttrs), ir.p_this, ir.ghostAttrs))

                md.args.foreach { case arg => 
                    checkWfWt(arg.wt) 
                    addPerm(arg.name, ir.TeePee(arg.wt, arg.name.path, ir.noAttrs))
                }            
                md.reqs.foreach(checkReq)
                
                val blk0 = md.blocks(0)
                val preSuper = blk0.stmts.takeWhile(!ir.isSuperCtor(_))
                val postSuper = blk0.stmts.dropWhile(!ir.isSuperCtor(_))
                if(postSuper.isEmpty)
                    throw new ir.IrError("intervals.super.ctor.zero")
                    
                // TODO -- Have the checking of super() verify that p_this is a ghost
                // and reify it, and thus permit control-flow.  We then have to propagate
                // p_this between blocks though (yuck).
                    
                // Check statements up until the super() invocation with ghost 'this':
                preSuper.foreach(checkStatement)
                checkStatement(postSuper.head)

                // After invoking super, 'this' becomes reified:
                setPerm(ir.lv_this, ir.TeePee(thisTref(cd, ir.ctorAttrs), ir.p_this, ir.noAttrs))
                checkStatements(postSuper.tail)
            }          
        }
        
    def addThis(cd: ir.ClassDecl, attrs: ir.Attrs) {
        val t_this = thisTref(cd, attrs)
        val tp_this = ir.TeePee(t_this, ir.p_this, ir.noAttrs)
        addPerm(ir.lv_this, tp_this)        
    }
    
    def checkReifiedFieldDecl(cd: ir.ClassDecl, fd: ir.ReifiedFieldDecl): Unit = 
        at(fd, ()) {
            savingEnv {
                addThis(cd, ir.ctorAttrs)
                
                checkWfWt(fd.wt)
                checkPathWf(ghostOk, fd.p_guard)
            }
        }
    
    def checkGhostFieldDecl(cd: ir.ClassDecl, gfd: ir.GhostFieldDecl): Unit = 
        at(gfd, ()) {
            log.indented(gfd) {
                addThis(cd, ir.ctorAttrs)

                // Check that ghosts are not shadowed from a super class:                
                prog.strictSuperclasses(cd.name).foreach { c =>
                    if(classDecl(c).fields.exists(_.name == gfd.name))
                        throw ir.IrError("intervals.shadowed.ghost", c, gfd.name)
                }
                
                checkWfWt(gfd.wt)
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
    
    def checkNotCtor(t: ir.TypeRef) {
        if(t.as.ctor) throw new ir.IrError("intervals.superType.with.ctor.attr") 
    }
    
    def checkIsInterface(t: ir.TypeRef) {
        val cd_super = classDecl(t.c)
        if(!cd_super.attrs.interface) 
            throw new ir.IrError("intervals.superType.not.interface", t.c)        
    }
    
    def checkInterfaceSupertype(t: ir.TypeRef) {
        checkNotCtor(t)
        if(t.c != ir.c_object)
            checkIsInterface(t)
    }
    
    def checkInterfaceConstructorDecl(cd: ir.ClassDecl, md: ir.MethodDecl) =
        at(md, ()) {
            if(!md.blocks.deepEquals(ir.md_ctor_interface.blocks))
                throw new ir.IrError("intervals.invalid.ctor.in.interface")
            // TODO Check other parts of constructor signature. (Very low priority)
        }
    
    def checkInterfaceMethodDecl(cd: ir.ClassDecl, md: ir.MethodDecl) =
        checkNoninterfaceMethodDecl(cd, md)
    
    def checkInterfaceClassDecl(cd: ir.ClassDecl) =
        at(cd, ()) {
            savingEnv {
                // TODO Is this everything?
                cd.superTypes.foreach(checkInterfaceSupertype)
                if(!cd.fields.isEmpty)
                    throw new ir.IrError("intervals.interface.with.fields")
                checkInterfaceConstructorDecl(cd, cd.ctor)
                cd.methods.foreach(checkInterfaceMethodDecl(cd, _))
            }
        }
        
    def checkIsNotInterface(t: ir.TypeRef) {
        val cd_super = classDecl(t.c)
        if(cd_super.attrs.interface) 
            throw new ir.IrError("intervals.superType.interface", t.c)        
    }
    
    def checkNoninterfaceClassDecl(cd: ir.ClassDecl) = 
        at(cd, ()) {
            savingEnv {
                cd.superTypes.foreach(checkNotCtor)
                cd.superTypes.take(1).foreach(checkIsNotInterface)
                cd.superTypes.drop(1).foreach(checkIsInterface)
                cd.fields.foldLeft(Set.empty[ir.FieldName])(checkFieldDecl(cd))
                checkNoninterfaceConstructorDecl(cd, cd.ctor)                    
                cd.methods.foreach(checkNoninterfaceMethodDecl(cd, _))                    
            }
        }
        
    def checkClassDecl(cd: ir.ClassDecl) =
        if(cd.attrs.interface) checkInterfaceClassDecl(cd)
        else checkNoninterfaceClassDecl(cd)
        
    def checkProg = prog.cds_user.foreach(checkClassDecl)    
}