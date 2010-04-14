package inter.compiler

object Pattern {
    
    sealed abstract class Ref {
        def ty: Type.Ref
    }
    
    case class Var(
        val name: Name.Var,
        val ty: Type.Ref
    ) extends Ref
    
    case class Tuple(patterns: List[Ref]) extends Ref {
        def ty = Type.Tuple(patterns.map(_.ty))
    }
    
    def createVarSymbols(p: Pattern.Ref): List[Symbol.Var] = p match {
        case Pattern.Var(name, ty) => List(new Symbol.Var(name, ty))
        case Pattern.Tuple(patterns) => patterns.flatMap(createVarSymbols)
    }
    
}