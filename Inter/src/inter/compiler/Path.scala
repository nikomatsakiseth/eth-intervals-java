package inter.compiler

object Path {
    sealed abstract class Ref
    case class Base(v: Name.Var) extends Ref
    case class Field(base: Path.Ref, f: Name.Var) extends Ref
    
    def fromLoweredAst(node: Ast.Lower.AstPath): Path.Ref = node match {
        case Ast.Lower.Var(name, _, _) => Path.Base(name.name)
        case Ast.Lower.PathField(owner, name, _, _) => Path.Field(fromLoweredAst(owner), name.name) 
    }
    
    val This = Path.Base(Name.ThisVar)    
    val Method = Path.Base(Name.MethodVar)
    
    sealed abstract class Canon {
        def sym: Symbol.Var
        def ty: Type.Ref
    }
    case class CanonBase(v: Name.Var, sym: Symbol.Var, ty: Type.Ref) extends Canon
    case class CanonField(base: Path.Canon, sym: Symbol.Var, ty: Type.Ref) extends Canon
}