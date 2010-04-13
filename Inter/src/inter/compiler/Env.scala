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
    
    // ___ Equatability _____________________________________________________

    /** Returns the set of paths that are known to be equatable with `p1` */
//    def equatable(p1: Path.Ref): Set[Path.Ref] = p1 match {
//        case Path.Base(v) => locals.get(v) match {
//            case None => {
//            }
//            case Some(sym) =>
//        }
//        case Path.Field(base, f) =>
//    }
    
}