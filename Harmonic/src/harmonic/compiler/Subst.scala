package harmonic.compiler

import scala.collection.immutable.Map

class Subst(private val map: Map[Path.Ref, Path.Ref]) {
    
    def +(s: Subst) = new Subst(map ++ s.map)
    def +(p: Pair[Path.Ref, Path.Ref]) = new Subst(map + p)
    
    def owner(owner: Path.Owner): Path.Owner = owner match {
        case Path.Static => Path.Static
        case owner: Path.Ref => path(owner)
    }
    
    def path(p: Path.Ref): Path.Ref = (map.get(p), p) match {
        case (Some(q), _) => 
            q
        case (None, Path.Local(v)) => 
            p
        case (None, Path.Constant(obj)) => 
            p
        case (None, Path.Field(o, f)) => 
            Path.Field(owner(o), f)
        case (None, Path.Cast(ty, base)) => 
            Path.Cast(ty, path(base))
        case (None, Path.Index(array, index)) => 
            Path.Index(path(array), path(index))
        case (None, Path.Tuple(paths)) => 
            Path.Tuple(paths.map(path))
        case (None, Path.Call(receiver, methodId, args)) => 
            Path.Call(owner(receiver), methodId, args.map(path))
    }
    
    def fact(fact: inference.Fact): inference.Fact = fact match {
        case fact @ K.Paths(l, r) => fact.withPaths(path(l), path(r))
        case fact @ K.Types(l, r) => fact.withTypes(ty(l), ty(r))
        case K.HasType(p, t) => K.HasType(path(p), ty(t))
    }
    
    def pattern(p: Pattern.Anon): Pattern.Anon = p match {
        case Pattern.AnonVar(t) => Pattern.SubstdVar(ty(t))
        case Pattern.AnonTuple(patterns) => Pattern.SubstdTuple(patterns.map(pattern))
    }
    
    def ty(t: Type): Type = t match {
        case Type.Member(p, tvar) => Type.Member(path(p), tvar)
        case Type.Class(className, targs) => Type.Class(className, targs.map(typeArg))
        case Type.Tuple(tys) => Type.Tuple(tys.map(ty))
        case Type.Null => Type.Null
    }
    
    def typeArg(targ: Type.Arg): Type.Arg = targ match {
        case Type.PathArg(n, r, p) => Type.PathArg(n, r, path(p))
        case Type.TypeArg(n, r, t) => Type.TypeArg(n, r, ty(t))
    }
    
    def methodSignature(msig: MethodSignature[Pattern.Anon]) = {
        MethodSignature(
            returnTy = ty(msig.returnTy),
            parameterPatterns = msig.parameterPatterns.map(pattern)
        )
    }
    
}

object Subst {
    val empty = new Subst(Map())

    def apply(pairs: (Path.Ref, Path.Ref)*): Subst = {
        new Subst(Map(pairs: _*))
    }
    
    def vp(lists: (List[Name.LocalVar], List[Path.Ref])) = {
        val (vars, paths) = lists
        new Subst(Map(vars.map(_.toPath).zip(paths): _*))
    }
    
    def vt(lists: (List[Name.LocalVar], List[SPath.Typed])) = {
        val (vars, paths) = lists
        vp(vars -> paths.map(_.toPath))
    }
}