package harmonic.compiler

import scala.collection.mutable

object MethodResolutionOrder {
    class Data {
        /** Maps a symbol to its method resolution order. */
        val mroCache = new mutable.HashMap[ClassSymbol, List[ClassSymbol]]()
    }
}

/** Computes the C3 linearization order of the supertypes for each
  * class.  This is the same order used in Python and Dylan.
  *
  * For a nice introduction to C3 see: http://www.python.org/download/releases/2.3/mro/ */
case class MethodResolutionOrder(state: State) {
    private[this] val data: MethodResolutionOrder.Data = state.data(classOf[MethodResolutionOrder.Data])
    private[this] var stack: List[ClassSymbol] = Nil
    
    /** Merges the lists of MROs for each supertype into one MRO. */
    private[this] def merge(csym: ClassSymbol, superLists: List[List[ClassSymbol]]): List[ClassSymbol] = {
        // From the text above:
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
                        csym.name, superLists.map(_.head)
                    ).report(state, csym.pos)
                }
                List()
            }
            
            case idx => {
                val before = superLists.take(idx)
                val after = superLists.drop(idx + 1)
                superLists(idx) match {
                    case List(head) => head :: merge(csym, before ::: after)
                    case head :: tl => head :: merge(csym, before ::: (tl :: after))
                }
            }
        }
    }
    
    /** Returns C3 order for `csym`.  This list is never empty.*/
    def forSym(csym: ClassSymbol): List[ClassSymbol] = {
        data.mroCache.get(csym) match {
            case Some(order) => order
            
            case None if stack.contains(csym) => {
                val idx = stack.indexOf(csym)
                Error.CircularInheritance(
                    csym.name, stack.take(idx).reverse
                ).report(state, csym.pos)
                data.mroCache(csym) = List(csym)
                List(csym)
            }
            
            case None => {
                stack = csym :: stack
                val superNames = csym.superClassNames
                val superCsyms = superNames.map(state.classes)
                val superLists = superCsyms.map(forSym)
                val list = csym :: merge(csym, superLists)
                data.mroCache(csym) = list
                stack = stack.tail
                list
            }
        }
    }
    
    def forClassType(classTy: Type.Class): List[ClassSymbol] = {
        forSym(state.csym(classTy.name))
    }
    
}