package ch.ethz.intervals

import scala.collection.immutable.Map

class PathSubst(m: Map[ir.Path, ir.Path]) {
    
    def path(p: ir.Path) =
        (m.get(p), p) match {
            case (Some(q), _) => q
            case (None, ir.path(lv, List())) => p
            case (None, ir.path(lv, f :: fs)) => path(lv ++ fs) + f
        }
        
    def wpath(wp: ir.WcPath) = wo match {
        case ir.WcUnkObj => ir.WcUnkObj
        case p: ir.Path => path(p)
    }
        
    def interval(i: ir.Interval) =
        ir.Interval(i.ps.map(obj), i.qs.map(obj))
        
    def effect(e: ir.Effect): ir.Effect = e match {
        case ir.EffectShift(i, e) => ir.EffectShift(interval(i), effect(e))
        case ir.EffectMethod(x, m, y) => ir.EffectMethod(path(x), m, path(y))
        case ir.EffectFixed(k, x) => ir.EffectFixed(k, path(x))
        case ir.EffectUnion(es) => ir.EffectUnion(es.map(effect))
    }
        
    def over(ov: ir.Over) = 
        ir.Over(ov.m, ov.y, new PathSubst(m - ov.lv.path).effects(ov.es))
        
    def wtref(wt: ir.WcTypeRef) =
        ir.WcTypeRef(t.c, t.wpaths.map(wpath), t.overs.map(over))
    
    def tref(t: ir.TypeRef) = 
        ir.TypeRef(t.c, t.paths.map(path), t.overs.map(over))
        
    def methodSig(msig: ir.MethodSig)
}

object PathSubst {
    def apply(p: ir.Path, q: ir.Path): PathSubst = 
        new PathSubst(Map((p, q)))
    def apply(ps: List[ir.Path], qs: List[ir.Path]): PathSubst =
        new PathSubst(Map(ps.zip(qs)))
    def apply(lv: ir.VarName, q: ir.Path): PathSubst = 
        apply(lv.path, q)
}