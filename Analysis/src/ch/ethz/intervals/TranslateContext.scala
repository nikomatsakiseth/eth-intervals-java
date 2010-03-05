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
            if(!ttf.isArrayElement(elem)) {
                val cds_new = ttf.intClassDecls(referencedElements, elem)
                cds_new.foreach(_.setDefaultPos(ttf.ElementPosition(elem)))
                cds ++= cds_new                
            }
        }
    
    def addClassImplementation(tree: ClassTree) = 
        indexLog.indented("TranslateContext.addClassImplementation(tree)", tree) {
            val cds_new = ttf.implClassDecls(tree)
            cds_new.foreach(_.setDefaultPos(ttf.TreePosition(tree, (s => s))))
            cds ++= cds_new
        }
        
    def createProg =
        new Prog(logStack, cds.toList, ir.cds_special)
    
}