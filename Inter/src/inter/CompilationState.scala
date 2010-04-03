package inter

import java.io.File

case class CompilationState(
    config: Config,
    reporter: Reporter,
    
    toBeParsed: List[File], // .inter files.
    toBeLoaded: List[File], // .class files.
    toBeResolved: List[Hl.P.CompUnit],
    
    parsedClasses: Map[QualName, Hl.P.ClassDecl]
) {
    
    def popToBeParsed = {
        (toBeParsed.head, copy(toBeParsed = toBeParsed.tail))
    }
    
    def pushToBeParsed(f: File) = copy(toBeParsed = f :: toBeParsed)
    
    def popToBeLoaded = {
        (toBeLoaded.head, copy(toBeLoaded = toBeLoaded.tail))
    }
    
    def pushToBeLoaded(f: File) = copy(toBeLoaded = f :: toBeLoaded)
    
    def popToBeResolved = {
        (toBeResolved.head, copy(toBeResolved = toBeResolved.tail))
    }
    
}