package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable

import Ast.{Lower => in}
import Util._

/** Given a class C, creates a flattened list
  * ExtendsDecl listing the supertypes and the
  * arguments provided to them.  All uses of a
  * given class must provide equivalent arguments
  * up to equality.  The order in which those
  * constructors are executed will be the MRO
  * order. 
  *
  * Requires: 
  * - lower for csym and its supertypes has completed */
class GatherExtends(global: Global) {
    
    //class Data {
    //    
    //    // Extends declarations for (transitive) supertypes of `csym`.
    //    // All substituted so as to be in terms of the parameters of `csym`.        
    //    val result = new mutable.HashMap[Name.Class, List[in.ExtendsArg]]()
    //    
    //    def addExtendsDecl(subst: Subst)(extendsDecl: in.ExtendsDecl) = {
    //        val className = extendsDecl.className.name
    //        val arg = extendsDecl.paths.map(ap => subst.path(ap.path.toPath))
    //        val otherResults = result.get(className).getOrElse(Nil)
    //        results(className) = paths :: otherResults
    //        
    //        
    //    }
    //
    //    def addFor(subst: Subst)(csym: ClassSymbol) = {
    //        csym match {
    //            case csym: ClassFromSource => {
    //                csym.loweredSource.extendsDecls.foreach(addExtendsDecl(subst))                    
    //            }
    //            case _ => 
    //        }
    //    }
    //    
    //}
    //
    //def forSym(csym: ClassFromSource) = {
    //    val data = new Data()
    //}
    
}