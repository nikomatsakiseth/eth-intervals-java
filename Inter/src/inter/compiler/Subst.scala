package inter.compiler

import scala.collection.immutable.Map

class Subst(private val map: Map[Name.Path, Name.Path]) {
    
    def +(s: Subst) = new Subst(map ++ s.map)
    def +(p: Pair[Name.Path, Name.Path]) = new Subst(map + p)
    
    def path(p: Name.Path): Name.Path = (map.get(p), p) match {
        case (Some(q), _) => q
        case (None, Name.PathBase(v)) => Name.PathBase(v)
        case (None, Name.PathField(owner, f)) => Name.PathField(path(owner), f)
    }
    
    def pattern(p: Symbol.Pattern): Symbol.Pattern = p match {
        case Symbol.VarPattern(n, t) => Symbol.VarPattern(n, ty(t))
        case Symbol.TuplePattern(patterns) => Symbol.TuplePattern(patterns.map(pattern))
    }
    
    def ty(t: Type.Ref): Type.Ref = t match {
        case Type.Path(p, tvar) => Type.Path(path(p), tvar)
        case Type.Class(clsName, targs) => Type.Class(clsName, targs.map(typeArg))
        case Type.Tuple(tys) => Type.Tuple(tys.map(ty))
        case Type.Null => Type.Null
    }
    
    def typeArg(targ: Type.Arg): Type.Arg = targ match {
        case Type.PathArg(n, r, p) => Type.PathArg(n, r, path(p))
        case Type.TypeArg(n, r, t) => Type.TypeArg(n, r, ty(t))
    }
    
}

object Subst {
    val empty = new Subst(Map())
    
    def apply(pairs: (Name.Path, Name.Path)*) = new Subst(Map(pairs: _*))
}