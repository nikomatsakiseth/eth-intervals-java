package inter.compiler

import scala.collection.immutable.Set

/** The environment used during a type check. */
case class Env(
    state: CompilationState,
    
    /** In-scope local variables. */
    locals: Map[Name.Var, Symbol.Var],
    
    /** Relations. */
    pathRels: List[(Path.Ref, PcRel, Path.Ref)],
    typeRels: List[(Type.Var, TcRel, Type.Ref)]
) {
    
    // ___ Querying the relations ___________________________________________
    
    private[this] def pathsRelatedBy(Rel: PcRel): List[(Path.Ref, Path.Ref)] = pathRels.flatMap { 
        case (p1, Rel, p2) => Some((p1, p2))
        case _ => None
    }
    
    private[this] def pathsRelatedBy(Rel: PcRel, P2: Path.Ref): List[Path.Ref] = pathRels.flatMap { 
        case (p1, Rel, P2) => Some(p1)
        case _ => None
    }
    
    private[this] def pathsRelatedBy(P1: Path.Ref, Rel: PcRel): List[Path.Ref] = pathRels.flatMap { 
        case (P1, Rel, p2) => Some(p2)
        case _ => None
    }
    
    // ___ Canonicalization _________________________________________________   

    /** Returns the set of paths that are known to be equatable with `p1` */
//    def canons(p1: Path.Ref): Path.Canons = p1 match {
//        case Path.Base(v) => locals.get(v) match {
//            case None => {
//            }
//            case Some(sym) =>
//        }
//        case Path.Field(base, f) =>
//    }
       
//    def isSubclass(ty_sub: Type.Ref, ty_sup: Type.Ref): Boolean = {
//        (ty_sub, ty_sup) match {
//            case (Type.Var(path_sub, var_sub), Type.Var(path_sup, var_sup)) => false // XXX
//            
//            case (Type.Class(name_sub, args_sub), Type.Class(name_sup, arg_sup)) => false // XXX
//                
//            case (Type.Tuple(tys_sub), Type.Tuple(tys_sup)) => 
//                tys_sub.zip(tys_sup).forall { case (s, t) => isSubclass(s, t) }
//            case (Type.Null, _) => 
//                true
//            case _ => 
//                false
//        }
//    }
//    
//    def matchesByClass(pattern: Symbol.Pattern, ty: Type.Ref): Boolean = {
//        (pattern, ty) match {
//            // Unpack singleton tuples:
//            case (_, Type.Tuple(List(subty))) => matches(pattern, subty)
//            case (Symbol.Tuple(List(subpattern)), _) => matches(subpattern, ty)
//            
//            // Unpack matching tuples:
//            case (Symbol.Tuple(subpatterns), Type.Tuple(subtys)) if sameLength(subpatterns, subtys) =>
//                subpatterns.zip(subtys).forall { case (p, t) => matches(p, t) }
//
//            // Check for singleton tuples:
//            case (Symbol.VarPattern(_, patty), ty) => isSubclass(patty, ty)
//            case _ => false
//        }
//    }
//  
}