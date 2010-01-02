package ch.ethz.intervals

import scala.collection.mutable.{Set => MutableSet}

trait CheckPhase {
    def prog: Prog
    def checkClassDecl(cd: ir.ClassDecl)
    
    val userClassNames = Set(prog.cds_user.map(_.name): _*)
    val checkedClasses = MutableSet.empty[ir.ClassName]
    def checkClassDeclAfterSuperclasses(cd: ir.ClassDecl) {
        if(!checkedClasses(cd.name) && userClassNames(cd.name)) {
            cd.superClasses.foreach(c => checkClassDeclAfterSuperclasses(prog.classDecl(c)))
            checkClassDecl(cd)          
            checkedClasses += cd.name
        }
    }
    
    def checkProg = prog.log.indented("%s", getClass.getName) {
        prog.cds_user.foreach(checkClassDeclAfterSuperclasses)
    }
}