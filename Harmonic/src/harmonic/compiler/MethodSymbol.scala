package harmonic.compiler

import scala.util.parsing.input.Position
import scala.collection.mutable

object MethodSymbol {
    
    def error(name: Name.Method, clsName: Name.Class) = {
        val parameterPatterns = name.parts.zipWithIndex.map { case (_, i) => 
            Pattern.Var(Name.LocalVar("arg%d".format(i)), Type.Object)
        }
        new MethodSymbol(
            pos       = InterPosition.forClassNamed(clsName),
            modifiers = Modifier.Set.empty,
            kind      = MethodKind.ErrorMethod, 
            clsName   = clsName, 
            name      = name, 
            MethodSignature(Type.Null, Type.Null, parameterPatterns)
        ) {
            override def isError = true
        }
    }
    
}

class MethodSymbol(
    val pos: Position,
    val modifiers: Modifier.Set,
    val kind: MethodKind,            /** Intrinsic, harmonic, java, etc. */
    val clsName: Name.Class,         /** Class in which the method is defined. */
    val name: Name.Method,           /** Name of the method. */
    val msig: MethodSignature[Pattern.Method]
) extends Symbol {
    override def toString = "MethodSymbol(%s, %x)".format(name, System.identityHashCode(this))
    
    def isFromClassNamed(aName: Name.Qual) = (clsName == aName)
    def isNamed(aName: Name.Method) = (name == aName)
    
    def substForFlatArgs(flatArgs: List[Path.Typed]) = {
        msig.parameterPatterns.flatMap(_.varNames).zip(flatArgs).foldLeft(Subst.empty) {
            case (s, (x, tp)) => s + (x.toPath -> tp.toPath)
        }
    }
    
    /** For methods whose source will be emitted, we compute the 
      * overridden methods.  The ordering is significant,
      * because when super is invoked it will proceed to the
      * next implementation in the list.  For methods on
      * reflected classes or classes loaded from .class files,
      * this list is simply empty. */
    val overrides = new mutable.ListBuffer[MethodSymbol]()
}
