package inter.compiler

import scala.collection.immutable.Map

class Subst(private val map: Map[Path.Ref, Path.Ref]) {
    
    def +(s: Subst) = new Subst(map ++ s.map)
    def +(p: Pair[Path.Ref, Path.Ref]) = new Subst(map + p)
    
    def path(p: Path.Ref): Path.Ref = (map.get(p), p) match {
        case (Some(q), _) => q
        case (None, Path.Base(v)) => Path.Base(v)
        case (None, Path.Field(owner, f)) => Path.Field(path(owner), f)
    }
    
    /** Caution: The names of the Variables in the pattern are
      * not substitued and are no longer relevant after substitution. 
      * Should really introduce a new type for this but I've been too lazy. */
    def pattern(p: Pattern.Ref): Pattern.Ref = p match {
        case Pattern.Var(n, t) => Pattern.Var(n, ty(t))
        case Pattern.Tuple(patterns) => Pattern.Tuple(patterns.map(pattern))
    }
    
    def ty(t: Type.Ref): Type.Ref = t match {
        case Type.Var(p, tvar) => Type.Var(path(p), tvar)
        case Type.Class(clsName, targs) => Type.Class(clsName, targs.map(typeArg))
        case Type.Tuple(tys) => Type.Tuple(tys.map(ty))
        case Type.Null => Type.Null
    }
    
    def typeArg(targ: Type.Arg): Type.Arg = targ match {
        case Type.PathArg(n, r, p) => Type.PathArg(n, r, path(p))
        case Type.TypeArg(n, r, t) => Type.TypeArg(n, r, ty(t))
    }
    
    /** Caution: a substituted method signature does not affect 
      * the names of the formal parameters. But those names are
      * no longer relevant to the substitued types! */
    def methodSignature(msig: Symbol.MethodSignature) = Symbol.MethodSignature(
        returnTy = ty(msig.returnTy),
        receiver = Pattern.Var(msig.receiver.name, ty(msig.receiver.ty)),
        parameterPatterns = msig.parameterPatterns.map(pattern)
    )
    
}

object Subst {
    val empty = new Subst(Map())
    
    def apply(pairs: (Path.Ref, Path.Ref)*) = new Subst(Map(pairs: _*))
}