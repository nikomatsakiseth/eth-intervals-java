package inter.compiler

import scala.collection.immutable.Map
import scala.collection.mutable.Queue

object Main {
    
    def main(args: Array[String]) {
        val config = new Config()
        val err = 
            if(config.loadFrom(args)) {
                val state = new CompilationState(config, new Reporter(config))
                state.toBeParsed ++= config.inputFiles.toList.map(f => (f, None))
                compile(state)
            } else 1
        System.exit(err)
    }
    
    // Note: in the future, these tasks can be parallelized.
    def compile(state: CompilationState): Int = {
        while(true) {
            if(!state.toBeParsed.isEmpty) {
                val (f, exp) = state.toBeParsed.dequeue()
                ParsePass(state, f, exp)
            } else if (!state.toBeLoaded.isEmpty) {
                val (f, exp) = state.toBeLoaded.dequeue()
                LoadPass(state, f, exp)
            } else if (!state.toBeResolved.isEmpty) {
                val cd = state.toBeResolved.dequeue()
                ResolvePass(state, cd)
            } else if (state.reporter.hasErrors) {
                state.reporter.print(System.err)
                return 1
            } else {
                return 0
            }            
        }
        return 0 // to satisfy type checker
    }
    
}