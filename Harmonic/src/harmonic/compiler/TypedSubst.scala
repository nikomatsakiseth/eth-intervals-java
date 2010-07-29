package harmonic.compiler

import scala.collection.immutable.Map

import Ast.{Lower => in}
import Util._

class TypedSubst(
    val lvmap: Map[LocalSymbol, SPath[Reified]],
    val fmap: Map[(LocalSymbol, FieldSymbol), SPath[Reified]]
) {
    
    def +(p: Pair[LocalSymbol, SPath[Reified]]): TypedSubst = {
        new TypedSubst(lvmap + p, fmap)
    }
    
    def plusField(p: Pair[(LocalSymbol, FieldSymbol), SPath[Reified]]) = {
        new TypedSubst(lvmap, fmap + p)        
    }
    
}

object TypedSubst {
    val empty = new TypedSubst(Map(), Map())
}