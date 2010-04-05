package inter.compiler

import java.io.File
import scala.collection.mutable.Queue

class CompilationState(
    val config: Config,
    val reporter: Reporter
) {
    val symtab = new SymbolTable()
    val toBeParsed = new Queue[(File, Option[Name.Qual])]() // .inter file, and class we expect to find (if any)
    val toBeLoaded = new Queue[(File, Option[Name.Qual])]() // .class file, and class we expect to find (if any)
    val toBeReflected = new Queue[Class[_]]()               // via reflection
    val toBeResolved = new Queue[Hl.P.CompUnit]()
    val toBeTyped = new Queue[Hl.RN.ClassDecl]()
}