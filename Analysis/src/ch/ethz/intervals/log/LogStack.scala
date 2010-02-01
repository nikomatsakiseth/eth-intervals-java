package ch.ethz.intervals.log

import scala.collection.immutable.ListSet
import scala.util.parsing.input.Positional

class LogStack(mainLog: Log) {
    
    // ___ Logging __________________________________________________________
    
    var splitLog = mainLog.splitLog(getClass.getName)
    def indexLog = splitLog.indexLog
    def log = splitLog.detailLog
    
    def withSplitLog[R](newSplitLog: SplitLog)(func: => Unit) = {
        val oldLog = splitLog
        try {
            splitLog = newSplitLog; func
        } finally {
            splitLog = oldLog
        }
    }
    
    // ___ Error Reporting __________________________________________________
    
    var errors = ListSet.empty[ir.Error] // use a list set to keep ordering
    
    def report(err: ir.Error) {
        mainLog("Error: %s", err)
        errors += err
    }
        
    def at[R](loc: Positional, default: => R)(g: => R): R = 
        log.indentedRes(loc) {
            try { g } catch {
                case failure: CheckFailure =>
                    val err = failure.toError(loc.pos)
                    indexLog("Error %s", err)
                    report(err)
                    default
            }
        }    
        
}

