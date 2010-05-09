package harmonic.compiler

object VarSymbol {
    type Any = VarSymbol[Name.Var]
    
    class Field(
        val modifiers: Modifier.Set,
        val name: Name.Member,
        val ty: Type.Ref,
        val kind: FieldKind
    ) extends VarSymbol[Name.Member]
    
    def errorField(name: Name.Member, optExpTy: Option[Type.Ref]) = {
        val ty = optExpTy.getOrElse(Type.Null)
        new Field(Modifier.Set.empty, name, ty, FieldKind.Harmonic) {
            override def isError = true
        }
    }
    
    class Local(
        val modifiers: Modifier.Set,
        val name: Name.LocalVar,
        val ty: Type.Ref
    ) extends VarSymbol[Name.LocalVar]
    
    def errorLocal(name: Name.LocalVar, optExpTy: Option[Type.Ref]) = {
        val ty = optExpTy.getOrElse(Type.Null)
        new Local(Modifier.Set.empty, name, ty) {
            override def isError = true
        }
    }
}

abstract class VarSymbol[+N <: Name.Var] extends Symbol {
    val modifiers: Modifier.Set
    val name: N
    val ty: Type.Ref
    
    override def toString = "%s(%s, %x)".format(
        getClass.getSimpleName,
        name, 
        System.identityHashCode(this)
    )
    
    def isNamed(aName: Name.Var) = (name == aName)    
}