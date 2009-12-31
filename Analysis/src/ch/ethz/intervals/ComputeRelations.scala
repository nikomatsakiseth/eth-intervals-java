package ch.ethz.intervals

import Util._

abstract class ComputeRelations(prog: Prog) 
extends TracksEnvironment(prog)
{
    import prog.log
    import prog.classDecl
    import prog.at
    import prog.isSubclass
    import prog.freshVarName
    
    // ______________________________________________________________________
    // Computing the effects of statements, etc
    
    def addCallMsig(tp: ir.TeePee, msig: ir.MethodSig, tqs: List[ir.TeePee]) {
        // Any method call disrupts potential temporary assocations:
        //     We make these disruptions before checking return value, 
        //     in case they would affect the type.  Haven't thought through
        //     if this can happen or not, but this would be the right time anyhow.
        clearTemp()
    }
    
    def addCall(x: ir.VarName, tp: ir.TeePee, m: ir.MethodName, tqs: List[ir.TeePee]) {
        val msig = substdMethodSig(tp, m, tqs)
        addCallMsig(tp, msig, tqs)
        addLvDecl(x, msig.wt_ret, None)
    }

    def addStatement(stmt: ir.Stmt): Unit =
        at(stmt, ()) {
            stmt match {                  
                case ir.StmtSuperCtor(qs) =>
                    if(!tp_this.as.ghost)
                        throw new ir.IrError("intervals.super.ctor.not.permitted.here")
                    val tp = tp_super
                    val tqs = teePee(ir.noAttrs, qs)
                    val msig_ctor0 = classDecl(tp.wt.c).ctor.msig(cap(tp))
                    val msig_ctor = ghostSubstOfTeePee(tp).methodSig(msig_ctor0)
                    addCallMsig(tp_super, msig_ctor, tqs)
                    
                    // Supertype must have been processed first:
                    env = env + prog.exportedCtorEnvs((tp.wt.c, ir.m_ctor))
                    
                case ir.StmtGetField(x, p_o, f) =>
                    val tp_o = teePee(ir.noAttrs, p_o)
                    substdFieldDecl(tp_o, f) match {
                        case ir.GhostFieldDecl(_, _) => () // illegal, reported in TypeCheck                        
                        case ir.ReifiedFieldDecl(_, wt, _, p_guard) =>
                            val tp_guard = teePee(ir.ghostAttrs, p_guard)                    
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
                        case ir.GhostFieldDecl(_, _) => () // illegal, reported in TypeCheck
                        case ir.ReifiedFieldDecl(_, wt, _, p_guard) =>
                            val tp_guard = teePee(ir.ghostAttrs, p_guard)
                    
                            val p_f = tp_o.p + f // Canonical path of f; valid as f is not a ghost.
                            addTemp(p_f, tp_v.p)
                    
                            removeInvalidated(p_f)
                            linkedPaths(tp_o, f).foreach(addInvalidated)
                    }
                        
                case ir.StmtCall(x, p, m, qs) =>
                    val tp = teePee(ir.noAttrs, p)
                    val tqs = teePee(ir.noAttrs, qs)
                    addCall(x, tp, m, tqs)
        
                case ir.StmtSuperCall(x, m, qs) =>
                    val tqs = teePee(ir.noAttrs, qs)
                    addCall(x, tp_super, m, tqs)
                
                case ir.StmtNew(x, t, qs) =>
                    val cd = classDecl(t.c)
                    val tqs = teePee(ir.noAttrs, qs)
                    
                    if(cd.attrs.interface)
                        throw new ir.IrError("intervals.new.interface", t.c)
                    
                    // Check Ghost Types:           
                    addLvDecl(x, t, None)
                    val tp_x = teePee(x.path)                                        
                    val subst = (
                        ghostSubstOfTeePee(tp_x) + 
                        PathSubst.vp(cd.ctor.args.map(_.name), tqs.map(_.p))
                    )
                    val msig = subst.methodSig(cd.ctor.msig(t))
                    addCallMsig(teePee(x.path), msig, tqs)
                    
                case ir.StmtCast(x, wt, p) => 
                    val tp = teePee(ir.noAttrs, p)
                    
                    // TODO Validate casts?  Issue warnings at least?
                    
                    addLvDecl(x, wt, None)
                    
                case ir.StmtNull(x, wt) => 
                    addLvDecl(x, wt, None)
                    
                case ir.StmtReturn(p) =>
                    
                case ir.StmtHb(p, q) =>
                    val tp = teePee(ir.noAttrs, p)
                    val tq = teePee(ir.noAttrs, q)
                    addUserHb(tp, tq)
                    
                case ir.StmtLocks(p, q) =>
                    val tp = teePee(ir.noAttrs, p)
                    val tq = teePee(ir.noAttrs, q)
                    addLocks(tp, tq)
                    
                case ir.StmtSubintervalPush(x, addCheckedArglocks) =>
                    val ltp_locks = teePee(ir.noAttrs, addCheckedArglocks)
                    addLvDecl(x, ir.t_interval, None)
                    
                    val tp_x = teePee(x.path)
                    addSubintervalOf(tp_x, tp_cur)
                    ltp_locks.foreach(addLocks(tp_x, _))
                    
                    pushCurrent(x.path)
                    
                case ir.StmtSubintervalPop(x) =>
                    popCurrent(x.path)
            }
        }
    
    def addBlock(blk: ir.Block) = {
        blk.stmts.foreach(addStatement)
    }
    
    // Returns an array containing the initial environment before each method block
    def iterateMethodBlocks(blks: Array[ir.Block]): (Array[ir.TcEnv], Array[ir.TcEnv]) = 
        log.indented("iterateMethodBlocks()") {        
            // For each block, a list of the blocks which link to us and the succ they do so with.
            val preds = Array.make(blks.length, List[(Int, ir.Goto)]())
            blks.zipWithIndex.foreach { case (blk, b) => 
                blk.gotos.foreach { succ => preds(succ.b) = (b, succ) :: preds(succ.b) }
            }
        
            // Initial value for each block: method environment
            val env_mthd = env
            val ins = Array.make(blks.length, env_mthd)
            val outs = Array.make(blks.length, env_mthd)
        
            // Computes the "exported" environment via the goto from pred -> b_succ
            def outEnv(b_succ: Int)(pred: (Int, ir.Goto)): ir.TcEnv = at(pred._2, env) {
                val (b_pred, goto) = pred
                val env_pred = outs(b_pred)                    
            
                // ______________________________________________________________________
                // Create a substitution which maps:
                // (1) Method-scope variables to themselves
                // (2) Arguments to the goto() to their respective parameters
                // (3) Everything else to "lv_outOfScope"
            
                val lv_outOfScope = ir.VarName(prog.fresh("outOfScope"))

                // Map all of pred's vars to outOfScope (unless overridden later)
                val map0 = env_pred.perm.keys.foldLeft(Map.empty[ir.Path, ir.Path]) { case (m, x) =>
                    m + Pair(x.path, lv_outOfScope.path)
                }
                
                // Map method-scope variables to themselves:            
                val map1 = env_mthd.perm.keys.foldLeft(map0) { case (m, x) => 
                    m + Pair(x.path, x.path) 
                }
            
                // Map the arguments of the succ() to the arguments of the block:
                val xs_args = blks(b_succ).args.map(_.name)
                val ps_goto = goto.ps
                val map2 = ps_goto.zip(xs_args).foldLeft(map1) { case (m, (p, x)) =>
                    m + Pair(p, x.path) 
                }
            
                // Map everything else to lv_outOfScope:
                val subst = new PathSubst(map2)
            
                // ______________________________________________________________________
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
                            throw new ir.IrError("intervals.must.assign.first", p0)
                        p1
                    }
            
                env_mthd
                .withTemp(mapMap(env_pred.temp))
                .withInvalidated(mapInvalidated(env_pred.addCheckedArginvalidated))
                .withReadable(mapRelation(env_pred.readable))
                .withWritable(mapRelation(env_pred.writable))
                .withHb(mapRelation(env_pred.hb))
                .withSubinterval(mapRelation(env_pred.subinterval))
                .withLocks(mapRelation(env_pred.locks))
            }
        
            // Intersects the environment of all predecessors:    
            def combinePreds(b: Int) = {
                preds(b) match {
                    case List() => env_mthd
                    case hd :: tl => tl.map(outEnv(b)).foldLeft(outEnv(b)(hd))(_ ** _)
                }
            }
        
            // Intersects the environment of all predecessors:
            def procBlock(b: Int) = savingEnv {
                val blk = blks(b)
                log.indentedRes("procBlock(%d): %s", b, blk) {
                    env = combinePreds(b)
                    blk.args.foreach(addArg)
                    ins(b) = env
                    addBlock(blk)
                    if(outs(b) != env) {
                        outs(b) = env
                        true
                    } else
                        false                    
                }
            }

            // Iterate until a fixed point is reached:
            var changed = true
            while(changed)
                changed = blks.indices.foldLeft(false) { case (c, b) => procBlock(b) || c }
            
            (ins, outs)         
        }
    
}