package inter.compiler

import scala.collection.immutable.Map
import scala.collection.mutable.Queue

object Main {
    
    def main(args: Array[String]) {
        val config = new Config()
        val err = 
            if(config.loadFrom(args)) {
                compile(CompilationState(
                    config = config,
                    reporter = new Reporter(config),
                    toBeParsed = config.inputFiles.toList.map(f => (f, None)),
                    toBeLoaded = List(),
                    toBeResolved = List(),
                    toBeReflected = List(),
                    toBeTyped = List(),
                    parsedClasses = Map(),
                    resolvedClasses = Map()
                ))            
            } else 1
        System.exit(err)
    }
    
    // Note: in the future, these tasks can be parallelized.
    def compile(state: CompilationState): Int = {
        if(!state.toBeParsed.isEmpty) {
            val ((f, exp), state1) = state.popToBeParsed
            compile(ParsePass(state1, f, exp))
        } else if (!state.toBeLoaded.isEmpty) {
            val ((f, exp), state1) = state.popToBeLoaded
            compile(LoadPass(state1, f, exp))
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