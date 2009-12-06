package ch.ethz.intervals

import scala.collection.immutable.Map

class LvSubst(m: Map[ir.VarName, ir.VarName]) {
    
    def lv(x: ir.VarName) = 
        m.get(x) match {
            case Some(y) => y
            case None => x
        }

    def obj(o: ir.Obj) =
        ir.Obj(lv(o.x), o.fs)
    
    def tref(t: ir.TypeRef) = 
        ir.TypeRef(t.c, t.objs.map(obj), t.mthds.map(obj))
    
}