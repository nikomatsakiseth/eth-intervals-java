package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.mutable.Queue

object Main {
    
    def main(args: Array[String]) {
        val config = new Config()
        val err = 
            if(config.loadFrom(args)) {
                val state = new State(config, new Reporter(config))
                Intrinsic(state).add()
                state.loadInitialSources(config.inputFiles.toList)
                state.compile()
                if (state.reporter.hasErrors) {
                    state.reporter.print(System.err)                
                    1
                } else 0
            } else 1
        System.exit(err)
    }
    
}