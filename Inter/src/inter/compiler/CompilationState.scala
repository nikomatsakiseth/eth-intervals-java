package inter.compiler

import java.io.File

case class CompilationState(
    config: Config,
    reporter: Reporter,
    
    toBeParsed: List[(File, Option[Name.Qual])], // .inter file, and class we expect to find (if any)
    toBeLoaded: List[(File, Option[Name.Qual])], // .class file, and class we expect to find (if any)
    toBeReflected: List[Class[_]],      // via reflection
    toBeResolved: List[Hl.P.CompUnit],
    toBeTyped: List[Hl.RN.ClassDecl],
    
    parsedClasses: Map[Name.Qual, Hl.P.ClassDecl],
    resolvedClasses: Map[Name.Qual, Hl.RN.ClassDecl]
) {
    
    def popToBeParsed = {
        (toBeParsed.head, copy(toBeParsed = toBeParsed.tail))
    }
    
    def popToBeLoaded = {
        (toBeLoaded.head, copy(toBeLoaded = toBeLoaded.tail))
    }
    
    def popToBeResolved = {
        (toBeResolved.head, copy(toBeResolved = toBeResolved.tail))
    }
    
}