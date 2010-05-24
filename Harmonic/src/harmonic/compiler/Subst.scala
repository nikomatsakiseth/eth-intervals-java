package harmonic.compiler

import scala.collection.immutable.Map

import Ast.{Lower => in}
import Util._

class Subst(
    lvmap: Map[VarSymbol.Local, Path.Typed],
    fmap: Map[(VarSymbol.Local, VarSymbol.Field), Path.Typed]
) {
    
    private[this] lazy val untypedSubst = {
        val subst = lvmap.foldLeft(Subst.empty) { case (s, (l, t)) =>
            s + (l.toPath -> t.toPath)
        }
        fmap.foldLeft(subst) { case (s, ((l, f), t)) =>
            s + (Path.Field(l.toPath, f.name) -> t.toPath)
        }
    }
    
    def +(p: Pair[VarSymbol.Local, Path.Typed]): Subst = {
        new Subst(lvmap + p, fmap)
    }
    
    def plusField(p: Pair[(VarSymbol.Local, VarSymbol.Field), Path.Typed]) = {
        new Subst(lvmap, fmap + p)        
    }
    
    def methodSignature(msig: MethodSignature[Pattern.Anon]) = {
        untypedSubst.methodSignature(msig)
    }
    
    def path(path: Path.Typed): Path.Typed = {
        path match {
            // Perform substitutions:
            
            case Path.Base(vsym: VarSymbol.Local) 
            if lvmap.isDefinedAt(vsym) =>
                lvmap(vsym)
        
            case Path.Field(Path.Base(vsym: VarSymbol.Local), fsym) 
            if fmap.isDefinedAt((vsym, fsym)) => 
                fmap((vsym, fsym))
                
            case Path.BaseCall(msym, msig, args) =>
                Path.BaseCall(msym, methodSignature(msig), args.map(path))
                
            case Path.Call(receiver, msym, msig, args) =>
                Path.Call(path(receiver), msym, methodSignature(msig), args.map(path))
                
            // Pass through:
            
            case Path.Base(_) | Path.Constant(_) =>
                path

            case Path.Field(base, fsym) => 
                Path.Field(path(base), fsym)
                
            case Path.Cast(ty, path) =>
                Path.Cast(ty, path(path))
            
            case Path.Index(array, index) =>
                Path.Index(path(array), path(index))
                
            case Path.Tuple(paths) =>
                Path.Tuple(paths.map(path))
        }
    }
    
    def pattern(p: Pattern.Anon): Pattern.Anon = p match {
        case Pattern.AnonVar(t) => Pattern.SubstdVar(ty(t))
        case Pattern.AnonTuple(patterns) => Pattern.SubstdTuple(patterns.map(pattern))
    }
    
    def ty(t: Type.Ref): Type.Ref = t match {
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
    val empty = new Subst(Map(), Map())
}