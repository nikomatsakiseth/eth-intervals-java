package ch.ethz.intervals.log

import java.io.File

class LogDirectory(baseDirectory: File) {
    private def uniqueFile(directory: File, name: String) = {        
        var f = new File(directory, name)
        var counter = 0
        while(f.exists) {
            f = new File(directory, name + "-" + counter)
            counter = counter + 1
        }
        f
    }
    
    val dir: File = {
        val d = uniqueFile(baseDirectory, System.nanoTime.toString)
        if(!d.mkdir)
            throw new RuntimeException("Failed to create log directory: %s".format(d))
        d
    }
    
    def file(n: String, ext: String) = new File(uniqueFile(dir, n) + "." + ext)
    def newFile(ext: String) = file(System.nanoTime.toString, ext)
    
    val indexFile = file("index", "html")
    val indexURI = indexFile.toURI    
    val indexLog = new Log.HtmlLog(this, indexFile, None)
}