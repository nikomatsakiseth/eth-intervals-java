package ch.ethz.intervals

import scala.collection.mutable.ListBuffer

class Prog(val cds_user: List[ir.ClassDecl]) {
    val classDecls = cds_user ++ ir.cds_default
    
    // ______________________________________________________________________
    // Class Table
    
    val classTable = Util.nameMap[ir.ClassName, ir.ClassDecl](classDecls)
    def classDecl(c: ir.ClassName) = classTable.get(c) match {
        case Some(cd) => cd
        case None => throw ir.IrError("intervals.no.such.class", c)
    }
        
    // ______________________________________________________________________
    // Fresh Variables
    
    private var counter = 0
    def fresh(nm: String) = {
        val c = counter
        counter = counter + 1
        "%s[%d]".format(nm, c)
    }    
    
    // ______________________________________________________________________
    // Errors
    
    val errors = new ListBuffer[ir.Error]
    def report(loc: ir.Locatable, msg: String, args: Any*) {
        val argList = args.toList.map(_.toString)
        errors += ir.Error(loc, msg, argList)
    }

    def reportError(loc: ir.Locatable, i: ir.IrError) {
        report(loc, i.msg, i.args: _*)
    }
    
}