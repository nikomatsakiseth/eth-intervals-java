package ch.ethz.intervals

import scala.collection.immutable.Map

class PathSubst(m: Map[ir.Path, ir.Path]) extends BaseSubst {
    
    def path(p: ir.Path): ir.Path =
        (m.get(p), p) match {
            case (Some(q), _) => q
            case (None, ir.Path(lv, List())) => p
            case (None, ir.Path(lv, f :: fs)) => path(lv ++ fs) + f
        }

}

object PathSubst {
    def pp(p: ir.Path, q: ir.Path): PathSubst = 
        new PathSubst(Map((p, q)))
    def pp(ps: List[ir.Path], qs: List[ir.Path]): PathSubst =
        new PathSubst(Map(ps.zip(qs): _*))
    def vp(lv: ir.VarName, q: ir.Path): PathSubst = 
        pp(lv.path, q)
    def vp(lv: List[ir.VarName], q: List[ir.Path]): PathSubst = 
        pp(lv.map(_.path), q)
    def vv(lv1: ir.VarName, lv2: ir.VarName): PathSubst =
        pp(lv1.path, lv2.path)
}