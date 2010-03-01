package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Positional
import Util._

abstract class TracksEnvironment(prog: Prog) extends CheckPhase(prog) 
{
    import prog.logStack.log
    import prog.logStack.indexLog
    import prog.logStack.at    
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
    def setFlow(flow_new: FlowEnv) = setEnv(env.copy(flow = flow_new))
    
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
    
    def setCurrent(lv_cur: ir.VarName) = log.indented("setCurrent(%s)", lv_cur) {
        setEnv(env.withCurrent(lv_cur))
    }
    
    def setWtRet(wt_ret: ir.WcTypeRef) = {
        setEnv(env.copy(wt_ret = wt_ret))
    }

    def addPerm(x: ir.VarName, cp: ir.ImmutableCanonPath): Unit = {
        setEnv(env.addPerm(x, cp))        
    }

    def addArg(arg: ir.LvDecl) = {
        setEnv(env.addArg(arg))
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

    // ___ Canon Paths ______________________________________________________
    
    // Note: these are not vals but defs!  This is important
    // because the outcome of ir.CanonPath() depends on the env.
    def cp_cur = env.o_cp_cur.get
    def cp_this = env.cp_this

}
