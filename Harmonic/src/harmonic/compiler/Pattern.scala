package harmonic.compiler

object Pattern {
    
    /** Base type for all patterns.  Contains no names
      * for the variables being assigned to. */
    sealed abstract trait Anon {
        def ty: Type.Ref
        def varTys: List[Type.Ref]
    }
    
    /** Base type for patterns that include variable names. */
    sealed abstract trait Ref extends Anon
    
    /** An anonymous reference to a variable. */
    sealed trait AnonVar extends Anon {
        def varTys = List(ty)        
    }
    
    object AnonVar {
        def unapply(anon: AnonVar) = Some(anon.ty)
    }
    
    sealed trait AnonTuple extends Anon {
        def patterns: List[Pattern.Anon]
        def ty: Type.Tuple = Type.Tuple(patterns.map(_.ty))
        def varTys = patterns.flatMap(_.varTys)
    }
    
    object AnonTuple {
        def unapply(anon: AnonTuple) = Some(anon.patterns)
    }
    
    case class SubstdVar(ty: Type.Ref) extends AnonVar
    
    case class SubstdTuple(patterns: List[Pattern.Anon]) extends AnonTuple
    
    case class Var(
        name: Name.Var,
        ty: Type.Ref
    ) extends AnonVar with Ref
    
    case class Tuple(patterns: List[Pattern.Ref]) extends AnonTuple with Ref
    
    def createVarSymbols(p: Pattern.Ref): List[Symbol.Var] = p match {
        case Pattern.Var(name, ty) => List(new Symbol.Var(name, ty))
        case Pattern.Tuple(patterns) => patterns.flatMap(createVarSymbols)
    }
    
    private case object NoMatch extends Exception
    
    /** Creates a substitution between `pat_from` and `pat_to`, or throws `NoMatch`. */
    private[this] def doSubst(pat_from: Pattern.Ref, pat_to: Pattern.Ref): Subst = {
        (pat_from, pat_to) match {
            // Tuples:
            case (Pattern.Tuple(List(p)), _) => doSubst(p, pat_to)
            case (_, Pattern.Tuple(List(p))) => doSubst(pat_from, p)
            case (Pattern.Tuple(pats_from), Pattern.Tuple(pats_to)) => doSubsts(pats_from, pats_to)
                
            // Vars:
            case (Pattern.Var(name_from, _), Pattern.Var(name_to, _)) =>
                Subst(name_from.toPath -> name_to.toPath)
            
            // Otherwise, no match:
            case _ => throw NoMatch
        }
    }
    
    /** Creates a substitution between `pats_from` and `pats_to`, or throws `NoMatch`. */
    private[this] def doSubsts(pats_from: List[Pattern.Ref], pats_to: List[Pattern.Ref]): Subst = {
        if(Util.sameLength(pats_from, pats_to)) {
            pats_from.zip(pats_to).foldLeft(Subst.empty) {
                case (s, (f, t)) => s + doSubst(f, t)
            }                            
        } else throw NoMatch
    }    
    
    /** Tries to create a subst from `pats_from` to `pats_to`; 
      * returns None if unable because the patterns don't match. */
    def optSubst(pats_from: List[Pattern.Ref], pats_to: List[Pattern.Ref]) = {
        try {
            Some(doSubsts(pats_from, pats_to))
        } catch {
            case NoMatch => None
        }
    }
    
}