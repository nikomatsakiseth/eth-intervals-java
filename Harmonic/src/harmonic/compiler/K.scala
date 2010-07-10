package harmonic.compiler

import harmonic.compiler.inference.Fact

// "K" for "Knowledge"
object K {
    
    case class PathExists(path: Path.Ref) extends Fact.Forward
    case class TypeExists(ty: Type) extends Fact.Forward
    
    // ___ Two paths ________________________________________________________
    
    sealed trait Paths {
        def left: Path.Ref
        def right: Path.Ref
        def withPaths(left: Path.Ref, right: Path.Ref): inference.Fact
    }
    
    object Paths {
        def unapply(fact: Paths) = Some(fact.left, fact.right)
    }
    
    sealed trait ForwardPaths extends Paths with Fact.Binary[Path.Ref, Path.Ref] {
        def withPaths(left: Path.Ref, right: Path.Ref): inference.Fact.Forward
    }
    
    sealed trait ChildParent extends ForwardPaths {
        def child: Path.Ref
        def parent: Path.Ref
        
        def left = child
        def right = parent
    }
    
    sealed trait GuardInter extends Paths with Fact.Backward {
        def guard: Path.Ref
        def inter: Path.Ref
        
        def left = guard
        def right = inter
    }
    
    final case class PathEq(left: Path.Ref, right: Path.Ref) extends ForwardPaths {
        def withPaths(left: Path.Ref, right: Path.Ref) = PathEq(left, right)        
    }
    final case class Hb(left: Path.Ref, right: Path.Ref) extends ForwardPaths {
        def withPaths(left: Path.Ref, right: Path.Ref) = Hb(left, right)                
    }
    final case class SubOf(child: Path.Ref, parent: Path.Ref) extends ChildParent {
        def withPaths(left: Path.Ref, right: Path.Ref) = SubOf(left, right)                        
    }
    final case class InlineSubOf(child: Path.Ref, parent: Path.Ref) extends ChildParent {
        def withPaths(left: Path.Ref, right: Path.Ref) = InlineSubOf(left, right)                                
    }
    final case class Locks(inter: Path.Ref, lock: Path.Ref) extends Paths with Fact.Forward {
        def left = inter
        def right = lock
        def withPaths(left: Path.Ref, right: Path.Ref) = Locks(left, right)                        
    }
    
    final case class PermitsWr(guard: Path.Ref, inter: Path.Ref) extends GuardInter {
        def withPaths(left: Path.Ref, right: Path.Ref) = PermitsWr(left, right)                                
    }
    final case class PermitsRd(guard: Path.Ref, inter: Path.Ref) extends GuardInter {
        def withPaths(left: Path.Ref, right: Path.Ref) = PermitsRd(left, right)                        
    }
    final case class EnsuresFinal(guard: Path.Ref, inter: Path.Ref) extends GuardInter {
        def withPaths(left: Path.Ref, right: Path.Ref) = EnsuresFinal(left, right)                                
    }

    // ___ Two types ________________________________________________________
    
    sealed trait Types extends Fact.Binary[Type, Type] {
        def left: Type
        def right: Type
        def withTypes(left: Type, right: Type): inference.Fact
    }
 
    object Types {
        def unapply(fact: Types) = Some(fact.left, fact.right)
    }
    
    // Two types are equivalent.
    final case class TypeEq(left: Type, right: Type) extends Types {
        def withTypes(left: Type, right: Type) = TypeEq(left, right)                
    }
    
    // left ub right == "left upper-bounded-by right"
    final case class TypeUb(sub: Type, sup: Type) extends Types {
        def left = sub
        def right = sup
        def withTypes(left: Type, right: Type) = TypeUb(left, right)
    }
    
    // ___ Mixed paths and types ____________________________________________
    
    final case class HasType(path: Path.Ref, ty: Type) extends Fact.Forward
    
}