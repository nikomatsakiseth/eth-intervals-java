package inter

import java.io.File

object ParsePass {
    def apply(state: CompilationState, interFile: File) = {
        val javaReader = Util.javaReaderFromFile(interFile)
        val parser = new HlParser()
        parser.parseCompUnitFromJavaReader(javaReader) match {
            case n: parser.NoSuccess => {
                state.reporter.report(n.next.pos, "parse.error", n.msg)
                state
            }
            case parser.Success(compUnit, _) => {
                if(state.config.dumpParsedTrees) {
                    compUnit.println(PrettyPrinter.stdout)
                }
                state.copy(
                    parsedClasses = state.parsedClasses ++ compUnit.classPairs,
                    toBeResolved = compUnit :: state.toBeResolved
                )
            }
        }                
    }
}