package ch.ethz.intervals

import scala.collection.immutable.Map

class ObjSubst(m: Map[ir.VarName, ir.Obj]) {
    
    def lv(x: ir.VarName) = 
        m.get(x) match {
            case Some(o) => o
            case None => ir.Obj(x, List())
        }

    def obj(o: ir.Obj) =
        lv(o.x) ++ o.fs
        
    def wobj(wo: ir.WcObj) = wo match {
        case ir.WcUnkObj => ir.WcUnkObj
        case o: ir.Obj => obj(o)
    }
        
    def point(p: ir.Point) =
        ir.Point(obj(p.o), p.side)
        
    def interval(i: ir.Interval) =
        ir.Interval(i.ns.map(obj), i.ms.map(obj))
        
    def effect(e: ir.Effect): ir.Effect = e match {
        case ir.EffectShift(i, e) => ir.EffectShift(interval(i), effect(e))
        case ir.EffectMethod(o, m, p) => ir.EffectMethod(obj(o), m, obj(p))
        case ir.EffectFixed(k, o) => ir.EffectFixed(k, obj(o))
        case ir.EffectUnion(es) => ir.EffectUnion(es.map(effect))
    }
        
    def over(ov: ir.Over) = 
        ir.Overrid(ov.m, ov.y, new ObjSubst(m - ov.y).effects(ov.es))
    
    def tref(t: ir.TypeRef) = 
        ir.TypeRef(t.c, t.objs.map(obj), t.overs.map(over))
        
    def methodSig(msig: ir.MethodSig)
}

object ObjSubst {
    def apply(x: ir.VarName, o: ir.Obj): ObjSubst = 
        new ObjSubst(Map((x,o)))
    def apply(xs: List[ir.VarName], os: List[ir.Obj]): ObjSubst =
        new ObjSubst(Map(xs.zip(os)))
}