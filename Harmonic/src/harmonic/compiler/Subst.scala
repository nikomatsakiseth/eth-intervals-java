package harmonic.compiler

import scala.collection.immutable.Map

class Subst(private val map: Map[Path.Ref, Path.Ref]) {
    
    def +(s: Subst) = new Subst(map ++ s.map)
    def +(p: Pair[Path.Ref, Path.Ref]) = new Subst(map + p)
    
    def path(p: Path.Ref): Path.Ref = (map.get(p), p) match {
        case (Some(q), _) => q
        case (None, Path.Base(v)) => Path.Base(v)
        case (None, Path.Field(owner, f)) => Path.Field(path(owner), f)
    }
    
    def pattern(p: Pattern.Anon): Pattern.Anon = p match {
        case Pattern.AnonVar(t) => Pattern.SubstdVar(ty(t))
        case Pattern.AnonTuple(patterns) => Pattern.SubstdTuple(patterns.map(pattern))
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
    
    def methodSignature(msig: Symbol.MethodSignature[Pattern.Anon]) = {
        Symbol.MethodSignature(
            returnTy = ty(msig.returnTy),
            receiverTy = ty(msig.receiverTy),
            parameterPatterns = msig.parameterPatterns.map(pattern)
        )
    }
    
}

object Subst {
    val empty = new Subst(Map())
    
    def apply(pairs: (Path.Ref, Path.Ref)*) = new Subst(Map(pairs: _*))
}