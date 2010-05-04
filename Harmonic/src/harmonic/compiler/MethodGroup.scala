package harmonic.compiler

import scala.collection.mutable

/** A method group consists of a list of method symbols which override one
  * another.  Methods override one another when (a) they have the same name
  * and the same static parameter types and (b) they are both inherited by
  * a single class. 
  * 
  * Note that two methods defined in unrelated classes A and B
  * can still override one another if a third class C extends both A and B
  * (though this scenario generally results in a static error unless C redefines
  * the method so as to clarify which version, A's or B's, it prefers). */
class MethodGroup( 
    /** Method name */
    val methodName: Name.Method,
    
    /** A representative method signature */
    val msig: MethodSignature[Pattern.Ref]
) {
    /** List of method symbols implementing this method, in MRO order. 
      * Note that some classes may define the same method more than once
      * with various signatures.  In such cases, all of those versions are
      * included in this list, but the first one contains the implementation
      * which should actually be invoked (if any). */
    private[this] val msymsBuffer = new mutable.ListBuffer[MethodSymbol]()
    
    def addMsym(msym: MethodSymbol) = msymsBuffer += msym
    def msyms = msymsBuffer.toList
}