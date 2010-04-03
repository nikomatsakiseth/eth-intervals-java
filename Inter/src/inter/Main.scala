package inter

import scala.collection.immutable.Map
import scala.collection.mutable.Queue

object Main {
    
    def main(args: Array[String]) {
        val config = new Config()
        val err = 
            if(config.loadFrom(args)) {
                compile(CompilationState(
                    config = config,
                    reporter = new Reporter(),
                    toBeParsed = config.inputFiles.toList,
                    toBeLoaded = List(),
                    toBeResolved = List(),
                    parsedClasses = Map()
                ))            
            } else 1
        System.exit(err)
    }
    
    // Note: in the future, these tasks can be parallelized.
    def compile(state: CompilationState): Int = {
        if(!state.toBeParsed.isEmpty) {
            val (f, state1) = state.popToBeParsed
            compile(ParsePass(state1, f))
        } else if (!state.toBeLoaded.isEmpty) {
            val (f, state1) = state.popToBeLoaded
            compile(LoadPass(state1, f))
        } else if (!state.toBeResolved.isEmpty) {
            val (cd, state1) = state.popToBeResolved
            compile(ResolvePass(state1, cd))
        } else if (state.reporter.hasErrors) {
            state.reporter.print(System.err)
            1
        } else {
            0
        }
    }
    
}