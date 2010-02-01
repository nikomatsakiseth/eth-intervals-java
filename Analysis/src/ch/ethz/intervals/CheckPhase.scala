package ch.ethz.intervals

import scala.collection.mutable.{Set => MutableSet}
import ch.ethz.intervals.log.Log
import ch.ethz.intervals.log.SplitLog

abstract class CheckPhase(val prog: Prog) {
    // ___ Class-check ordering _____________________________________________
    def checkClassDecl(cd: ir.ClassDecl)
    val userClassNames = Set(prog.cds_user.map(_.name): _*)
    val checkedClasses = MutableSet.empty[ir.ClassName]
    def checkClassDeclAfterSuperclasses(cd: ir.ClassDecl) {
        if(!checkedClasses(cd.name) && userClassNames(cd.name)) {
            cd.superClasses.foreach(c => 
                checkClassDeclAfterSuperclasses(prog.classDecl(c)))
            checkClassDecl(cd)          
            checkedClasses += cd.name
        }
    }    
    def checkProg = prog.cds_user.foreach(checkClassDeclAfterSuperclasses)
}