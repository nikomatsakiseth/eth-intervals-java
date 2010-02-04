package ch.ethz.intervals.log

class SplitLog(
    val uri: String,
    val indexLog: Log, 
    val detailLog: Log
)    

object SplitLog {
    val devNullSplitLog = new SplitLog("", DevNullLog, DevNullLog)
    
    def newFrameset(logDirectory: LogDirectory, name: String) = {
        val f = logDirectory.newFile(name, ".html")
        val lf = logDirectory.newFile(".html")
        val rf = logDirectory.newFile(".html")
        
        val pw = new java.io.PrintWriter(f)            
        pw.write("""
        <html>
            <head><title>%s</title></head>
            <body>
                <frameset cols='30%%,*'>
                    <frame src='%s' name='index'/>
                    <frame src='%s' name='details'/>
                </frameset>
            </body>
        </html>
        """.format(HtmlLog.escape(name), lf.getName, rf.getName))
        pw.close
        
        val detailsLog = new HtmlLog(logDirectory, rf, None)
        val indexLog = new HtmlLog(logDirectory, lf, Some(detailsLog))
        new SplitLog(f.toURI.toString, indexLog, detailsLog)        
    }
}