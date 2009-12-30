package ch.ethz.intervals

import Util._

class ComputeRelations(prog: Prog) 
extends TracksEnvironment(prog)
{
    import prog.log
    import prog.classDecl
    import prog.at
    import prog.isSubclass
    
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

    def addStatement(stmt: ir.Stmt) =
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
                    
                case ir.StmtGetField(x, p_o, f) =>
                    val tp_o = teePee(ir.noAttrs, p_o)
                    substdFieldDecl(tp_o, f) match {
                        case ir.GhostFieldDecl(_, _) =>
                            throw new ir.IrError("intervals.not.reified", tp_o.wt.c, f)
                        
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
                        case ir.GhostFieldDecl(_, _) =>
                            throw new ir.IrError("intervals.not.reified", tp_o.wt.c, f)
                        
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
                    
                case ir.StmtSubinterval(x, lp_locks, stmts) =>
                    val ltp_locks = teePee(ir.noAttrs, lp_locks)
                    addLvDecl(x, ir.t_interval, None)
                    
                    val tp_x = teePee(x.path)
                    addSubintervalOf(tp_x, tp_cur)
                    ltp_locks.foreach(addLocks(tp_x, _))
                    
                    withCurrent(tp_x) {
                        stmts.foreach(addStatement)                        
                    }
            }
        }
    
    
}