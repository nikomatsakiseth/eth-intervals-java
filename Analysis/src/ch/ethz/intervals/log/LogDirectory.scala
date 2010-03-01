package ch.ethz.intervals.log

import java.io.File

class LogDirectory(val dir: File) {
    if(!dir.mkdir)
        throw new RuntimeException("Failed to create log directory: %s".format(dir))
        
    def newFile(prefix: String, suffix: String): File = LogDirectory.newFile(dir, prefix, suffix)
    def newFile(suffix: String): File = newFile("Z"+System.nanoTime, suffix)    
    val mainSplitLog = SplitLog.newFrameset(this, "index")
    val indexLog = mainSplitLog.indexLog
    val detailLog = mainSplitLog.detailLog
}

object LogDirectory {
    def newFile(directory: File, prefix: String, suffix: String): File = {        
        var f = new File(directory, prefix + suffix)
        var counter = 0
        while(f.exists) {
            f = new File(directory, prefix + "-" + counter + suffix)
            counter = counter + 1
        }
        f
    }
    
    def newLogDirectory(baseDirectory: File, name: String): LogDirectory = 
        new LogDirectory(newFile(baseDirectory, name, ""))
}