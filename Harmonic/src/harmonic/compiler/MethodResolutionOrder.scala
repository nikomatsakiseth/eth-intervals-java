package harmonic.compiler

import scala.collection.mutable
import com.smallcultfollowing.lathos.model.Context

import Util._

/** Computes the C3 linearization order of the supertypes for each
  * class.  This is the same order used in Python and Dylan.
  *
  * For a nice introduction to C3 see: http://www.python.org/download/releases/2.3/mro/ */
case class MethodResolutionOrder(global: Global) {
    def computeForSym(csym: ClassSymbol): List[ClassSymbol] = {
        val log = global.debugServer.context
        
        def merge(superLists: List[List[ClassSymbol]]): List[ClassSymbol] = {
            // From the text linked to above:
            // "Take the head of the first list, i.e L[B1][0]; if this head is not in the tail 
            // of any of the other lists, then add it to the linearization of C and remove it 
            // from the lists in the merge, otherwise look at the head of the next list and 
            // take it, if it is a good head. Then repeat the operation until all the class are
            // removed or it is impossible to find good heads. In this case, it is impossible 
            // to construct the merge, Python 2.3 will refuse to create the class C and 
            // will raise an exception."

            def isGoodHead(superList: List[ClassSymbol]) = {
                val head = superList.head
                superLists.forall { otherList => !otherList.tail.contains(head) }
            }

            superLists.findIndexOf(isGoodHead) match {
                case -1 => {
                    if(!superLists.isEmpty) {
                        Error.AmbiguousInheritance(
                            csym.name, superLists.map(_.head.name)
                        ).report(global, csym.pos)
                    }
                    List()
                }

                case idx => {
                    val head = superLists(idx).head
                    def dropHead(l: List[ClassSymbol]): Option[List[ClassSymbol]] = {
                        l.dropWhile(_ == head) match {
                            case List() => None
                            case list => Some(list)
                        }                    
                    }
                    head :: merge(superLists.flatMap(dropHead))
                }
            }
        }

        val superNames = csym.superClassNames
        val superCsyms = superNames.map(global.csym)
        val superLists = superCsyms.map(_.mro)
        csym :: merge(superLists)
    }    
}