package ch.ethz.intervals

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Set => MutableSet}
import com.sun.source.tree._

import javax.lang.model.element._

class TranslateContext(
    val log: Log,
    val ttf: TranslateTypeFactory
) {
    val cds = new ListBuffer[ir.ClassDecl]()
    
    def addClassInterface(elem: TypeElement, referencedElements: MutableSet[Element]) =
        log.indented("addClassInterface(%s, ...)", elem) {
            val cd = ttf.intClassDecl(referencedElements, elem)
            cds += cd
            log.classDecl("Result: ", cd)
        }
    
    def addClassImplementation(tree: ClassTree) = {        
    }
    
}