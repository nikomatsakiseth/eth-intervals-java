package ch.ethz.intervals.log

import scala.collection.immutable.ListSet
import scala.util.parsing.input.Positional

class LogStack(mainLog: SplitLog) {
    
    // ___ Logging __________________________________________________________
    
    var splitLog = mainLog
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
        indexLog("Error: %s", err)
        if(mainLog != splitLog)
            mainLog.indexLog.linkTo(splitLog.uri, "Error %s", err)        
        errors += err
    }
        
    def at[R](loc: Positional, default: => R)(g: => R): R = 
        log.indentedRes(loc) {
            try { g } catch {
                case failure: CheckFailure =>
                    val err = failure.toError(loc.pos)
                    report(err)
                    default
            }
        }    
        
}