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
            cd.setDefaultPos(ttf.ElementPosition(elem))
            cds += cd
            log.classDecl("Result: ", cd)
        }
    
    def addClassImplementation(tree: ClassTree) = 
        indexLog.indented("TranslateContext.addClassImplementation(tree)", tree) {
            val cd = ttf.implClassDecl(tree)
            cd.setDefaultPos(ttf.TreePosition(tree, (s => s)))
            cds += cd
            log.classDecl("Result: ", cd)
        }
        
    def createProg =
        new Prog(logStack, cds.toList, ir.cds_special)
    
}