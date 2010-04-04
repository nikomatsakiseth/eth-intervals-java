package inter.compiler

import java.io.File

case class CompilationState(
    config: Config,
    reporter: Reporter,
    
    toBeParsed: List[File], // .inter files.
    toBeLoaded: List[File], // .class files.
    toBeProxied: List[Class[_]], // reflection
    toBeResolved: List[Hl.P.CompUnit],
    toBeTyped: List[Hl.RN.ClassDecl],
    
    parsedClasses: Map[QualName, Hl.P.ClassDecl]
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