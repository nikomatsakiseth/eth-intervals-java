package ch.ethz.intervals.log

import ch.ethz.intervals.ir
import ch.ethz.intervals.CheckFailure
import scala.collection.immutable.ListSet
import scala.util.parsing.input.Positional
import scala.util.parsing.input.NoPosition
import ch.ethz.intervals.Util._

class LogStack(mainLog: SplitLog, pertainingTo: List[String]) {
    
    // ___ Logging __________________________________________________________
    
    var splitLogs = List(mainLog)
    def splitLog = splitLogs.head
    def indexLog = splitLog.indexLog
    def errorLog = splitLog.errorLog
    def log = splitLog.detailLog
    
    def withSplitLog[R](newSplitLog: SplitLog)(func: => R) = {
        try {
            splitLogs = newSplitLog :: splitLogs
            func
        } finally {
            splitLog.flush
            splitLogs = splitLogs.tail
        }
    }
    
    // ___ Pertinence _______________________________________________________
    //
    // A somewhat hacky mechanism to enable logging only for specific methods, etc.
    // This whole logging infrastructure needs to be refactored.
    
    private[this] var scopedPertinent = false
    
    def ifPertinent[R](s: String)(func: => R) = {
        if(pertainingTo.isEmpty || scopedPertinent) {
            func
        } else if (pertainingTo.exists(_ glob s)) {
            try {
                scopedPertinent = true
                val pertSplitLog = mainLog.indexLog.splitLog(s)
                log.linkTo(pertSplitLog.uri, "%s", s)
                withSplitLog(pertSplitLog)(func)
            } finally {
                scopedPertinent = false
            }
        } else {
            withSplitLog(SplitLog.devNullSplitLog)(func)
        }
    }
    
    // ___ Error Reporting __________________________________________________
    
    def flush = splitLogs.foreach(_.flush)
    
    var errors = ListSet.empty[ir.Error] // use a list set to keep ordering
    
    def report(err: ir.Error) {
        errorLog("Error: %s at %s", err, err.pos)
        if(mainLog != splitLog)
            mainLog.errorLog.linkTo(splitLog.uri, "Error: %s at %s", err, err.pos)        
        errors += err
    }
    
    private[this] def baseAt[R](aLog: Log, loc: Positional, default: => R)(g: => R): R = 
        try {            
            aLog.indented("At: %s", loc) {
                assert(loc.pos != NoPosition)
                try { g } catch {
                    case failure: CheckFailure =>
                        val err = failure.toError(loc.pos)
                        report(err)
                        default
                } 
            }
        } finally { splitLog.flush }
        
    def indexAt[R](loc: Positional, str: String, default: => R)(g: => R): R = {
        ifPertinent(str) {
            baseAt(indexLog, loc, default)(g)            
        }
    }
        
    def at[R](loc: Positional, default: => R)(g: => R): R = 
        baseAt(log, loc, default)(g) 
        
}