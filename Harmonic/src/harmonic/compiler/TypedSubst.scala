package harmonic.compiler

import scala.collection.immutable.Map

import Ast.{Lower => in}
import Util._

class TypedSubst(
    val lvmap: Map[VarSymbol.Local, SPath[Reified]],
    val fmap: Map[(VarSymbol.Local, VarSymbol.Field), SPath[Reified]]
) {
    
    def +(p: Pair[VarSymbol.Local, SPath[Reified]]): TypedSubst = {
        new TypedSubst(lvmap + p, fmap)
    }
    
    def plusField(p: Pair[(VarSymbol.Local, VarSymbol.Field), SPath[Reified]]) = {
        new TypedSubst(lvmap, fmap + p)        
    }
    
}

object TypedSubst {
    val empty = new TypedSubst(Map(), Map())
}