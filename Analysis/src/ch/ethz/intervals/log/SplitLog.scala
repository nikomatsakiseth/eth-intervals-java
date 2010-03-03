package ch.ethz.intervals.log

class SplitLog(
    val uri: String,
    val indexLog: Log, 
    val errorLog: Log,
    val detailLog: Log
) {
    def flush = {
        indexLog.flush
        errorLog.flush
        detailLog.flush
    }
}

object SplitLog {
    val devNullSplitLog = new SplitLog("", DevNullLog, DevNullLog, DevNullLog)
    
    def newFrameset(logDirectory: LogDirectory, name: String) = {
        val f = logDirectory.newFile(name, ".html")
        val indexFile = logDirectory.newFile(".html")
        val errorFile = logDirectory.newFile(".html")
        val detailFile = logDirectory.newFile(".html")
        
        val pw = new java.io.PrintWriter(f)            
        pw.write("""
        <html>
            <head><title>%s</title></head>
            <body>
                <frameset cols='30%%,*'>
                    <frameset rows='80%%,*'>
                        <frame src='%s' name='index'/>
                        <frame src='%s' name='errors'/>
                    </frameset>
                    <frame src='%s' name='details'/>
                </frameset>
            </body>
        </html>
        """.format(
            HtmlLog.escape(name), 
            indexFile.getName, 
            errorFile.getName, 
            detailFile.getName))
        pw.close
        
        val detailLog = new HtmlLog(logDirectory, detailFile, None)
        val errorLog = new HtmlLog(logDirectory, errorFile, Some(detailLog))
        val indexLog = new HtmlLog(logDirectory, indexFile, Some(detailLog))
        new SplitLog(f.toURI.toString, indexLog, errorLog, detailLog)        
    }
}