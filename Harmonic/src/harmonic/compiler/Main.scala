package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.mutable.Queue

object Main {
    
    def main(args: Array[String]) {
        val config = new Config()
        val err = 
            if(config.loadFrom(args)) {
                val reporter = new Reporter(config)
                val global = new Global(config, reporter)
                global.compile()
                if (reporter.hasErrors) {
                    reporter.print(System.err)                
                    1
                } else 0
            } else 1
        System.exit(err)
    }
    
}