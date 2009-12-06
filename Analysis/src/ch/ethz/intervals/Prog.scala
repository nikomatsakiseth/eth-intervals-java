package ch.ethz.intervals

class Prog(val classDecls: List[ClassDecl]) {
    
    // ______________________________________________________________________
    // Class Table
    
    val classTable = Util.nameMap(ir.classDecls)
    def classDecl(c: ir.ClassName) = classTable.get(c) match {
        case None => throw IrError("intervals.no.such.class", c)
    }
        
    // ______________________________________________________________________
    // Fresh Variables
    
    private var counter: Int
    def fresh(nm: String) = {
        val c = counter
        counter = counter + 1
        "%s[%d]".format(nm, c)
    }    
    
    // ______________________________________________________________________
    // Errors
    
    val errors: ListBuffer[Error]
    def report(loc: ir.Locatable, msg: String, args: args: Any*) {
        val argList = args.toList.map(_.toString)
        errors += error(loc, msg, args)
    }

    def reportError(loc: ir.Locatable, i: ir.IrError) {
        report(loc, i.msg, i.args: _*)
    }
    
}