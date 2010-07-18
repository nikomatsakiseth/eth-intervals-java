package harmonic.compiler

import java.util.concurrent.atomic.AtomicInteger

object Pattern {

    // ___ Any pattern ______________________________________________________
    //
    // Can be either Anon or Ref.  Warning: Do not compare for equality with
    // Any patterns!  An Anon and a Ref are never equal.  Annoyingly, there is 
    // no good way to enforce this, except that there is no is() method on 
    // Any.
    
    sealed trait Any {
        def ty: Type
        def varTys: List[Type]
        def toAnon: Anon
    }
    
    sealed trait AnyVar extends Any {
        def ty: Type
        def varTys = List(ty)
        def toAnon: AnonVar
    }
    
    object AnyVar {
        def unapply(p: AnyVar) = Some(p.ty)
    }
    
    sealed trait AnyTuple extends Any {
        def ty: Type.Tuple = Type.Tuple(patterns.map(_.ty))
        def patterns: List[Pattern.Any]
        def varTys = patterns.flatMap(_.varTys)
        def toAnon: AnonTuple
    }
    
    object AnyTuple {
        def unapply(p: AnyTuple) = Some(p.patterns)
    }
    
    // ___ Anonymous patterns _______________________________________________
    
    sealed trait Anon extends Any {
        def is(pat: Anon) = (this == pat)
    }
    
    sealed case class AnonVar(ty: Type) extends Anon with AnyVar {
        def toAnon = this
        override def toString = ty.toString
    }
    
    sealed case class AnonTuple(patterns: List[Pattern.Anon]) extends Anon with AnyTuple {
        def toAnon = this
        override def toString = "(%s)".format(patterns.mkString(", "))        
    }
    
    val EmptyAnonTuple = AnonTuple(Nil)
    
    // ___ Named patterns ___________________________________________________
    
    sealed trait Ref extends Any {
        def is(pat: Ref) = (this == pat)
        def toAnon: Anon
        def varNames: List[Name.LocalVar]
    }
    
    case class Var(name: Name.LocalVar, ty: Type) extends Ref with AnyVar {
        def toAnon = AnonVar(ty)
        def varNames = List(name)
        override def toString = "%s: %s".format(name, ty)
    }
    
    case class Tuple(patterns: List[Pattern.Ref]) extends Ref with AnyTuple {
        def toAnon = AnonTuple(patterns.map(_.toAnon))
        def varNames = patterns.flatMap(_.varNames)
        override def toString = "(%s)".format(patterns.mkString(", "))
    }
    
    val EmptyTuple = Tuple(Nil)
    
    // ___ Helpers __________________________________________________________
    
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
    
    /** Converts a list of anonymous patterns to named patterns, created names like arg0, ..., argN */
    def makeRefs(anons: List[Pattern.Anon]): List[Pattern.Ref] = {
        val ctr = new AtomicInteger(0)
        anons.map(makeRef(ctr))
    }
    
    /** Converts an anonymous pattern to a named one, created names like arg3, arg7, etc.
      * The counter `ctr` is used to track the most recent name given out. */
    def makeRef(ctr: AtomicInteger)(anon: Pattern.Anon): Pattern.Ref = {
        anon match {
            case Pattern.AnonTuple(anons) => 
                Pattern.Tuple(anons.map(makeRef(ctr)))
            case Pattern.AnonVar(ty) => 
                Pattern.Var(
                    Name.LocalVar("arg%d".format(ctr.getAndIncrement)),
                    ty
                )
        }
    }
    
}