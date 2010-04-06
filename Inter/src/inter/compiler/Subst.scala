package inter.compiler

import scala.collection.immutable.Map

class Subst(private val map: Map[Name.Path, Name.Path]) {
    
    def +(s: Subst) = new Subst(map ++ s.map)
    def +(p: Pair[Name.Path, Name.Path]) = new Subst(map + p)
    
    def pattern(p: Symbol.Pattern): Symbol.Pattern
    def ty(t: Symbol.Type): Symbol.Type
}

object Subst {
    val empty = new Subst(Map())
}