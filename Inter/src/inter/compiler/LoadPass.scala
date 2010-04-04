package inter.compiler

import java.io.File

object LoadPass {
    def apply(state: CompilationState, classFile: File, expClass: Option[Name.Qual]) = {
        throw new RuntimeException("TODO-- Implement .class file loading!")
    }
}