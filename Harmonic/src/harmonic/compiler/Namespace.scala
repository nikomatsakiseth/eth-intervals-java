package harmonic.compiler

import ch.ethz.intervals._
import scala.collection.mutable

class Namespace(
    wr: Interval,
    parent: Namespace
) {
    private[this] val vars = new mutable.HashMap[String, VarSymbol]()
    private[this] val methods = new mutable.HashMap[String, MethodSymbol]()
    
    def varCount = {
        assert(Intervals.checkReadable(wr))        
        vars.size
    }
    
    def addVar(nm: String, sym: VarSymbol) = {
        assert(Intervals.checkWritable(wr))        
        vars(nm) = sym
    }
    
    def getVar(nm: String): Option[VarSymbol] = {
        assert(Intervals.checkReadable(wr))        
        vars.get(nm)
    }
    
    def methodCount = {
        assert(Intervals.checkReadable(wr))        
        methods.size
    }
    
    def addMethod(nm: Name.Method, sym: MethodSymbol) = {
        assert(Intervals.checkWritable(wr))        
        methods(nm) = sym
    }
    
    def getMethod(nm: Name.Method): Option[MethodSymbol] = {
        assert(Intervals.checkReadable(wr))        
        methods.get(nm)
    }
}