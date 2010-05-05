package harmonic.compiler

object VarSymbol {
    type Any = VarSymbol[Name.Var]
    type Field = VarSymbol[Name.Member]
    type Local = VarSymbol[Name.LocalVar]
    
    def error[N <: Name.Var](name: N, optExpTy: Option[Type.Ref]) = {
        val ty = optExpTy.getOrElse(Type.Null)
        new VarSymbol(Modifier.Set.empty, name, ty) {
            override def isError = true
        }
    }
}

class VarSymbol[+N <: Name.Var](
    val modifiers: Modifier.Set,
    val name: N,
    val ty: Type.Ref
) extends Symbol {
    override def toString = "%s(%s, %x)".format(
        getClass.getSimpleName,
        name, 
        System.identityHashCode(this)
    )
    
    def isNamed(aName: Name.Var) = (name == aName)    
}