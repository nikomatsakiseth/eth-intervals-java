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
    
    def ty(t: Symbol.Type): Symbol.Type = t match {
        case Symbol.PathType(p, tvar) => Symbol.PathType(path(p), tvar)
        case Symbol.ClassType(clsName, targs) => Symbol.ClassType(clsName, targs.map(typeArg))
        case Symbol.TupleType(tys) => Symbol.TupleType(tys.map(ty))
        case Symbol.NullType => Symbol.NullType
    }
    
    def typeArg(targ: Symbol.TypeArg): Symbol.TypeArg = targ match {
        case Symbol.PathTypeArg(n, r, p) => Symbol.PathTypeArg(n, r, path(p))
        case Symbol.TypeTypeArg(n, r, t) => Symbol.TypeTypeArg(n, r, ty(t))
    }
    
}

object Subst {
    val empty = new Subst(Map())
    
    def apply(pairs: (Name.Path, Name.Path)*) = new Subst(Map(pairs: _*))
}