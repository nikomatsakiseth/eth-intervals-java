package ch.ethz.intervals

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Set => MutableSet}
import com.sun.source.tree._
import checkers.util.{TreeUtils => TU}

import javax.lang.model.element._

import ch.ethz.intervals.log.LogStack

class TranslateContext(
    val logStack: LogStack,
    val ttf: TranslateTypeFactory
) {
    import logStack.indexLog
    import logStack.log
    
    val cds = new ListBuffer[ir.ClassDecl]()
    
    def addClassInterface(elem: TypeElement, referencedElements: (Element => Boolean)) =
        indexLog.indented("TranslateContext.addClassInterface(%s, ...)", elem) {
            val cd = ttf.intClassDecl(referencedElements, elem)
            cds += cd
            log.classDecl("Result: ", cd)
        }
    
    def addClassImplementation(tree: ClassTree) = { 
        addClassInterface(TU.elementFromDeclaration(tree), _ => true)
    }
    
}