package ch.ethz.intervals

class Fresh(prefix: String) {
    private var dictWords = {
        try {
            val dictFile = new java.io.File("/usr/share/dict/words")
            val dictSource = scala.io.Source.fromFile(dictFile)
            val lines = scala.util.Random.shuffle(dictSource.getLines("\n").toList)
            dictSource.close()
            lines            
        } catch {
            case _: java.io.IOException => List()
        }
    }
    
    private var counter = 0
    def next() = {
        dictWords match {
            case word :: tl =>
                dictWords = tl
                "[%s/%s]".format(prefix, word)
            case List() =>
                val c = counter
                counter = counter + 1
                "[%s/%d]".format(prefix, c)
        }        
    }    
}