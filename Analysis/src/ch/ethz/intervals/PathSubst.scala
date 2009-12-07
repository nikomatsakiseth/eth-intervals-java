package ch.ethz.intervals

import scala.collection.immutable.Map

class PathSubst(m: Map[ir.Path, ir.Path]) {
    
    def path(p: ir.Path): ir.Path =
        (m.get(p), p) match {
            case (Some(q), _) => q
            case (None, ir.Path(lv, List())) => p
            case (None, ir.Path(lv, f :: fs)) => path(lv ++ fs) + f
        }
        
    def wpath(wp: ir.WcPath) = wp match {
        case ir.WcUnkPath => ir.WcUnkPath
        case p: ir.Path => path(p)
    }
        
    def interval(i: ir.Interval) =
        ir.Interval(i.ps.map(path), i.qs.map(path))
        
    def effect(e: ir.Effect): ir.Effect = e match {
        case ir.EffectInterval(i, e) => ir.EffectInterval(interval(i), effect(e))
        case ir.EffectMethod(p, m, q) => ir.EffectMethod(path(p), m, path(q))
        case ir.EffectFixed(k, p) => ir.EffectFixed(k, path(p))
        case ir.EffectLock(ps, e) => ir.EffectLock(ps.map(path), effect(e))
        case ir.EffectUnion(es) => ir.EffectUnion(es.map(effect))
        case ir.EffectNone => ir.EffectNone
        case ir.EffectAny => ir.EffectAny
    }

    def over(ov: ir.Over) = 
        ir.Over(ov.m, ov.lv, new PathSubst(m - ov.lv.path).effect(ov.e))
        
    def wtref(wt: ir.WcTypeRef) =
        ir.WcTypeRef(wt.c, wt.wpaths.map(wpath), wt.overs.map(over))
    
    def tref(t: ir.TypeRef) = 
        ir.TypeRef(t.c, t.paths.map(path), t.overs.map(over))
        
    def ghostFieldDecl(fd: ir.GhostFieldDecl) =
        ir.GhostFieldDecl(fd.name, wtref(fd.wt))
        
    def realFieldDecl(fd: ir.RealFieldDecl) =
        ir.RealFieldDecl(fd.mods, wtref(fd.wt), fd.name, path(fd.guard))

    def fieldDecl(fd: ir.FieldDecl): ir.FieldDecl = fd match {
        case gfd: ir.GhostFieldDecl => ghostFieldDecl(gfd)
        case rfd: ir.RealFieldDecl => realFieldDecl(rfd)
    }
    
    def lvDecl(lv: ir.LvDecl) = 
        ir.LvDecl(lv.name, wtref(lv.wt))

    def methodSig(msig: ir.MethodSig) = {
        val subst = new PathSubst(m - msig.arg.name.path)
        ir.MethodSig(subst.effect(msig.e), subst.lvDecl(msig.arg), subst.wtref(msig.wt_ret))
    }
}

object PathSubst {
    def apply(p: ir.Path, q: ir.Path): PathSubst = 
        new PathSubst(Map((p, q)))
    def apply(ps: List[ir.Path], qs: List[ir.Path]): PathSubst =
        new PathSubst(Map(ps.zip(qs): _*))
    def apply(lv: ir.VarName, q: ir.Path): PathSubst = 
        apply(lv.path, q)
    def apply(lv1: ir.VarName, lv2: ir.VarName): PathSubst =
        apply(lv1.path, lv2.path)
}