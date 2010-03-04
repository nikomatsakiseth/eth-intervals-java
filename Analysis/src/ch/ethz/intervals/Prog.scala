package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.util.parsing.input.Positional
import scala.collection.immutable.ListSet
import scala.collection.immutable.Map

import Util._
import ch.ethz.intervals.log.Log
import ch.ethz.intervals.log.LogStack

class Prog(
    val logStack: LogStack,
    val cds_user: List[ir.ClassDecl],
    val cds_sys: List[ir.ClassDecl]
) {
    import logStack.log
    def errors = logStack.errors
    
    val topLevelClassTable = Util.nameMap[ir.AnyClassName, ir.ClassDecl](cds_user ++ cds_sys)
    val env_empty = TcEnv(
        prog        = this,
        classTable  = topLevelClassTable,
        c_this      = ir.c_void,
        o_lv_cur    = None,
        wt_ret      = ir.t_void,
        identityRet = List(),
        perm        = Map(),
        flow        = FlowEnv.empty
    ).addGhostLocal(
        ir.VarName("RacyGuard#racy"), ir.c_RacyGuard // XXX This will be removed once we add support for static fields.
    )

    // ___ Computed results _________________________________________________
    //
    // When we check a class, we store the exported environments from each 
    // of its constructors in this table.  These can be used by subclasses.
    
    var exportedCtorEnvs = Map[(ir.AnyClassName, ir.MethodName), FlowEnv](
        ((ir.c_object, ir.m_init) -> FlowEnv.empty),
        ((ir.c_interval, ir.m_init) -> FlowEnv.empty)
    )
    
    // ___ Fresh variables __________________________________________________

    private val fresh = new Fresh("prog")
    def freshVarName = ir.VarName(fresh.next())    
        
}