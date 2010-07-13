package harmonic.compiler.debug

import scala.collection.mutable
import ch.ethz.intervals._
import com.smallcultfollowing.lathos
import harmonic.compiler.Util._

class Intervals(server: lathos.LathosServer, root: lathos.Page)
extends lathos.Page
{
    // ___ Data _____________________________________________________________
    
    class IntervalInfo(
        val parentPage: lathos.Page,
        val log: lathos.Page
    ) {
        def < (info: IntervalInfo) = {
            (parentPage.getId < info.parentPage.getId) && (log.getId < info.log.getId)
        }
        
        var interval: Interval = null
        
        // invoked by code in Util:
        def setInterval(inter: Interval) = Intervals.this.synchronized {
            interval = inter
        }
        
        var result: Object = "Wait"
        
        def isWaiting = (result == "Wait")
        def isRunning = (result == "Run")
        def isFailed = (result.isInstanceOf[Throwable])
        def isCompleted = (result == "OK")
        
        // invoked by code in Util:
        def setRunning = Intervals.this.synchronized {
            result = "Run"
        }
        
        // invoked by code in Util:
        def setCompleted = Intervals.this.synchronized {
            result =  "OK"
        }

        // invoked by code in Util:
        def setThrew(thr: Throwable) = Intervals.this.synchronized {
            result = thr
        }
    }
    
    private[this] val infos = new mutable.ArrayBuffer[IntervalInfo]()
    
    def registerInterval(parentPage: lathos.Page, log: lathos.Page): IntervalInfo = synchronized {
        val info = new IntervalInfo(parentPage, log)
        infos += info
        infos.sortWith(_ < _)
        info
    }
    
    // ___ Lathos ___________________________________________________________
    
    override def toString = getId

    override def getId = "Intervals Page"

    override def getParent = root

    override def addContent(content: lathos.PageContent) = {
        throw new UnsupportedOperationException()
    }

    override def renderInLine(out: lathos.Output): Unit = {
        lathos.Lathos.renderInLine(this, out)
    }

    def renderInPage(out: lathos.Output): Unit = synchronized {
        out.startPage(this)
        
        def row(info: IntervalInfo): Unit = {
            out.row(info.parentPage, info.log, info.interval, info.result)
        }
        
        out.par { out.bolded { out.outputText("Failed") } }
        out.table {
            for(info <- infos if info.isFailed) { row(info) }
        }

        out.par { out.bolded { out.outputText("Running") } }
        out.table {
            for(info <- infos if info.isRunning) { row(info) }
        }
        
        out.par { out.bolded { out.outputText("Waiting") } }
        out.table {
            for(info <- infos if info.isWaiting) { row(info) }
        }

        out.par { out.bolded { out.outputText("Completed") } }
        out.table {
            for(info <- infos if info.isCompleted) { row(info) }
        }
        
        out.endPage(this)
    }
    
    root.addContent(this)
}