package ch.ethz.intervals

class Fresh(prefix: String) {
    
    // we only read every nth word, because otherwise we get too many similar words:
    private[this] val skipLines = 50
    
    private[this] var optDictReader = {
        try {
            val dictFile = new java.io.File("/usr/share/dict/words")
            Some(new java.io.LineNumberReader(new java.io.FileReader(dictFile)))
        } catch {
            case _: java.io.IOException => None
        }        
    }
    
    private[this] var counter = 0
    
    private[this] def nextCounterName = {
        val c = counter
        counter = counter + 1
        "[%s/%d]".format(prefix, c)        
    }
    
    def next() = {
        // Skip 'skipLines' lines:
        optDictReader.foreach { dictReader => 
            for(i <- 1 to skipLines) dictReader.readLine
        }
        
        optDictReader.map(_.readLine) match {
            case Some(null) =>
                optDictReader = None
                nextCounterName
            case Some(word) =>
                "[%s/%s]".format(prefix, word)
            case _ =>
                nextCounterName
        }
    }    
}

