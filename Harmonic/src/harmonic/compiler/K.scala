package harmonic.compiler

import harmonic.compiler.inference.Fact

// "K" for "Knowledge"
object K {
    
    case class PathExists(path: Path) extends Fact.Forward
    case class TypeExists(ty: Type) extends Fact.Forward
    
    // ___ Two paths ________________________________________________________
    
    sealed trait Paths {
        def left: Path
        def right: Path
        def withPaths(left: Path, right: Path): inference.Fact
    }
    
    object Paths {
        def unapply(fact: Paths) = Some(fact.left, fact.right)
    }
    
    sealed trait ForwardPaths extends Paths with Fact.Binary[Path, Path] {
        def withPaths(left: Path, right: Path): inference.Fact.Forward
    }
    
    sealed trait ChildParent extends ForwardPaths {
        def child: Path
        def parent: Path
        
        def left = child
        def right = parent
    }
    
    sealed trait GuardInter extends Paths with Fact.Backward {
        def guard: Path
        def inter: Path
        
        def left = guard
        def right = inter
    }
    
    object GuardInter {
        def unapply(fact: GuardInter) = Some(fact.guard, fact.inter)
    }
    
    final case class PathEq(left: Path, right: Path) extends ForwardPaths {
        def withPaths(left: Path, right: Path) = PathEq(left, right)        
    }
    final case class Hb(left: Path, right: Path) extends ForwardPaths {
        def withPaths(left: Path, right: Path) = Hb(left, right)                
    }
    final case class SubOf(child: Path, parent: Path) extends ChildParent {
        def withPaths(left: Path, right: Path) = SubOf(left, right)                        
    }
    final case class InlineSubOf(child: Path, parent: Path) extends ChildParent {
        def withPaths(left: Path, right: Path) = InlineSubOf(left, right)                                
    }
    final case class Locks(inter: Path, lock: Path) extends Paths with Fact.Forward {
        def left = inter
        def right = lock
        def withPaths(left: Path, right: Path) = Locks(left, right)                        
    }
    
    final case class PermitsWr(guard: Path, inter: Path) extends GuardInter {
        def withPaths(left: Path, right: Path) = PermitsWr(left, right)                                
    }
    final case class PermitsRd(guard: Path, inter: Path) extends GuardInter {
        def withPaths(left: Path, right: Path) = PermitsRd(left, right)                        
    }
    final case class EnsuresFinal(guard: Path, inter: Path) extends GuardInter {
        def withPaths(left: Path, right: Path) = EnsuresFinal(left, right)                                
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
    
    // True if the path is (a) re-ified and (b) the object at runtime will have type `ty`.
    // Note that this fact is never true if `path` refers to a ghost.
    final case class HasType(path: Path, ty: Type) extends Fact.Binary[Path, Type] {
        def left = path
        def right = ty
    }

    // True if the object to which `path` refers will be an instance of `cls`.
    // This is computable even for ghosts, unlike HasType.
    final case class HasClass(path: Path, cls: Name.Class) extends Fact.Backward 
    
}