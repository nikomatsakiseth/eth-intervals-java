package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Positional
import Util._

abstract class TracksEnvironment(prog: Prog) extends CheckPhase(prog) {
    import prog.logStack.log
    import prog.logStack.indexLog
    import prog.logStack.at    
    import prog.isSubclass
    import prog.classDecl
    import prog.unboundGhostFieldsOnClassAndSuperclasses
    
    // ______________________________________________________________________
    // Current Environment
    //
    // We use a mutable field here rather than thread the env through explicitly.

    private var env_private = prog.env_empty
    
    /// Reading and modifying the environment
    def env = env_private    
    def flow = env.flow
    def setEnv(env_new: TcEnv) = env_private = env_new
    def setFlow(flow_new: FlowEnv) = setEnv(env.withFlow(flow_new))
    
    /// Executes g and restores the old environment afterwards:
    def savingEnv[R](func: => R): R = {
        val env_old = env
        try { func } finally { 
            setEnv(env_old)
        }
    }

    /// Checks that this function preserves the environment:
    def preservesEnv[R](func: => R): R = {
        val env_old = env
        try { func } finally { 
            assert(env == env_old) 
        }
    }

    // ______________________________________________________________________
    // Modifying the Environment
    
    def setCurrent(p_cur: ir.Path) = log.indented("pushCurrent(%s)", p_cur) {
        setEnv(env.withCurrent(Some(p_cur)))
    }
    
    def setWtRet(wt_ret: ir.WcTypeRef) = {
        setEnv(env.withRet(wt_ret))
    }

    def addPerm(x: ir.VarName, cp: ir.CanonPath): Unit = {
        setEnv(env.addPerm(x, cp))        
    }

    def addArg(arg: ir.LvDecl) = {
        setEnv(env.addArg(arg))
    }      
        
    def addTemp(p: ir.Path, q: ir.Path) = {
        setEnv(env.addTemp(p, q))
    }
    
    def addReifiedLocal(x: ir.VarName, wt: ir.WcTypeRef) = {
        setEnv(env.addReifiedLocal(x, wt))
    }

    def addGhostLocal(x: ir.VarName, wt: ir.WcTypeRef) = {
        setEnv(env.addReifiedLocal(x, wt))
    }

    def clearTemp() = {
        setEnv(env.clearTemp)
    }        

    def addInvalidated(p: ir.Path) = {
        setEnv(env.addInvalidated(p))
    }

    def removeInvalidated(p: ir.Path) = {
        setEnv(env.removeInvalidated(p))
    }

    def addHbPnt(cp: ir.CanonPath, cq: ir.CanonPath): Unit = {
        setEnv(env.addHbPnt(cp, cq))
    }

    def addHbInter(cp: ir.CanonPath, cq: ir.CanonPath): Unit = {
        setEnv(env.addHbInter(cq, cq))
    }
    
    def addDeclaredReadableBy(cp: ir.CanonPath, cq: ir.CanonPath): Unit = {
        setEnv(env.addDeclaredReadableBy(cp, cq))
    }

    def addDeclaredWritableBy(cp: ir.CanonPath, cq: ir.CanonPath): Unit = {
        setEnv(env.addDeclaredWritableBy(cp, cq))
    }

    def addSubintervalOf(cp: ir.CanonPath, cq: ir.CanonPath): Unit = {
        setEnv(env.addSubintervalOf(cp, cq))
    }

    def addLocks(cp: ir.CanonPath, cq: ir.CanonPath): Unit = {
        setEnv(env.addLocks(cp, cq))
    }

    def addUserHb(tp0: ir.CanonPath, tq0: ir.CanonPath) = {
        setEnv(env.addUserHb(tp0, tq0))
    }
        
    def addNonNull(cp: ir.CanonPath) {
        setEnv(env.addNonNull(cp))
    }
        
    def addUserDeclaredWritableBy(cp: ir.CanonPath, cq: ir.CanonPath) {
        if(isSubclass(tp.wt, ir.c_guard) && isSubclass(tq.wt, ir.c_interval))
            addDeclaredWritableBy(tp, tq)
    }
    
    def addUserDeclaredReadableBy(cp: ir.CanonPath, cq: ir.CanonPath) {
        if(isSubclass(tp.wt, ir.c_guard) && isSubclass(tq.wt, ir.c_interval))
            addDeclaredReadableBy(tp, tq)
    }
    
    def addUserSubintervalOf(cp: ir.CanonPath, cq: ir.CanonPath) {
        if(isSubclass(tp.wt, ir.c_interval) && isSubclass(tq.wt, ir.c_interval))
            addSubintervalOf(tp, tq)
    }
    
    def addReq(req: ir.Req) = 
        log.indented("addReq(%s)", req) {
            at(req, ()) {
                def cpAdd(add: Function2[ir.CanonPath, ir.CanonPath, Unit], ps: List[ir.Path], qs: List[ir.Path]) = {
                    val tps = immutableGhost(ps)
                    val tqs = immutableGhost(qs)
                    foreachcross(tps, tqs)(add)
                }
                req match {
                    case ir.ReqWritableBy(ps, qs) => cpAdd(addUserDeclaredWritableBy, ps, qs)
                    case ir.ReqReadableBy(ps, qs) => cpAdd(addUserDeclaredReadableBy, ps, qs)
                    case ir.ReqHb(ps, qs) => cpAdd(addUserHb, ps, qs)
                    case ir.ReqSubintervalOf(ps, qs) => cpAdd(addUserSubintervalOf, ps, qs)
                }   
            }             
        }
        
    // ___ Canon Paths ______________________________________________________
    
    def immutableReified(p: ir.Path): ir.CanonPath = {
        val cp = env.canon(p)
        if(env.ghost(cp))
            throw new CheckFailure("intervals.must.be.reified", p)
        if(env.mutable(cp))
            throw new CheckFailure("intervals.must.be.immutable", p)
        cp
    }
    
    def immutableReified(ps: List[ir.Path]): List[ir.CanonPath] = {
        ps.map(immutableReified)
    }
    
    def immutableGhost(p: ir.Path) = {
        val cp = env.canon(p)
        if(env.mutable(cp))
            throw new CheckFailure("intervals.must.be.immutable", p)
        cp        
    }
    
    def immutableGhost(ps: List[ir.Path]): List[ir.CanonPath] = {
        ps.map(immutableGhost)
    }
    
    // Note: these are not vals but defs!  This is important
    // because the outcome of ir.CanonPath() depends on the env.
    def ocp_cur = env.op_cur.map(env.canon)
    def cp_cur = ocp_cur.get
    def cp_ctor = env.canon(ir.gfd_ctor.thisPath)
    def cp_this = env.canon(ir.p_this)    
    def cp_super = // tp_super always refers to the FIRST supertype
        prog.sups(env.cap(cp_this)) match {
            case List() => throw new CheckFailure("intervals.no.supertype", cp_this.wt)
            case t_super :: _ => ir.CpLv(ir.lv_this, t_super, false)
        }

}
