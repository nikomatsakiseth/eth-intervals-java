package ch.ethz.intervals

import scala.collection.immutable.ListMap
import ir./

class PathSubst(private val m: Map[ir.Path, ir.Path]) extends BaseSubst {
    
    def +(subst: PathSubst) =
        new PathSubst(m ++ subst.m)
    
    def path(p: ir.Path): ir.Path =
        (m.get(p), p) match {
            case (Some(q), _) => q
            case (None, ir.PathLv(lv)) => p
            case (None, p_base / f) => path(p_base) / f
        }

}

object PathSubst {
    def pp(p: ir.Path, q: ir.Path): PathSubst = 
        new PathSubst(ListMap((p, q)))
    def pp(ps: List[ir.Path], qs: List[ir.Path]): PathSubst =
        new PathSubst(ListMap(ps.zip(qs): _*))
    def vp(lv: ir.VarName, q: ir.Path): PathSubst = 
        pp(lv.path, q)
    def vp(lv: List[ir.VarName], q: List[ir.Path]): PathSubst = 
        pp(lv.map(_.path), q)
    def vv(lv1: ir.VarName, lv2: ir.VarName): PathSubst =
        pp(lv1.path, lv2.path)
    def vv(lv1: List[ir.VarName], lv2: List[ir.VarName]): PathSubst =
        pp(lv1.map(_.path), lv2.map(_.path))
}