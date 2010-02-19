package ch.ethz.intervals

import Util._

abstract class BaseSubst {
    def path(p: ir.Path): ir.Path
    
    def ghost(g: ir.Ghost): ir.Ghost =
        ir.Ghost(g.f, path(g.p))
        
    def pathType(pt: ir.PathType): ir.PathType =
        ir.PathType(path(pt.p), pt.tv)
    
    def classType(ct: ir.ClassType): ir.ClassType =
        ir.ClassType(ct.c, ct.ghosts.map(ghost), ct.targs.map(typeArg))
        
    def wcClassType(wct: ir.WcClassType): ir.WcClassType =
        ir.WcClassType(wct.c, wct.wghosts.map(wghost), wct.wtargs.map(wcTypeArg))
        
    def wcTref(wt: ir.WcTypeRef): ir.WcTypeRef = wt match {
        case pt: ir.PathType => pathType(pt)
        case ct: ir.ClassType => classType(ct)
        case wct: ir.WcClassType => wcClassType(wct)
    }
    
    def typeArg(ta: ir.TypeArg): ir.TypeArg = {
        ir.TypeArg(ta.tv, wcTref(ta.wt))
    }
    
    def typeBounds(bounds: ir.TypeBounds) = bounds match {
        case ir.TypeBounds(wts_lb, owts_ub) =>
            ir.TypeBounds(wts_lb.map(wcTref), owts_ub.map(_.map(wcTref)))
    }
        
    def wcTypeArg(wta: ir.WcTypeArg): ir.WcTypeArg = wta match {
        case ir.BoundedTypeArg(tv, bounds) =>
            ir.BoundedTypeArg(tv, typeBounds(bounds))
        case ta: ir.TypeArg => 
            typeArg(ta)
    }
        
    def wpath(wp: ir.WcPath): ir.WcPath = wp match {
        case ir.WcReadableBy(ps) => ir.WcReadableBy(ps.map(path))
        case ir.WcWritableBy(ps) => ir.WcWritableBy(ps.map(path))
        case ir.WcHbNow(ps) => ir.WcHbNow(ps.map(path))
        case p: ir.Path => path(p)
    }
    
    def wghost(g: ir.WcGhost): ir.WcGhost =
        ir.WcGhost(g.f, wpath(g.wp))
    
    def req(r: ir.Req): ir.Req = (r match {
        case ir.ReqWritableBy(lp, lq) => ir.ReqWritableBy(lp.map(path), lq.map(path))
        case ir.ReqReadableBy(lp, lq) => ir.ReqReadableBy(lp.map(path), lq.map(path))
        case ir.ReqSubintervalOf(lp, lq) => ir.ReqSubintervalOf(lp.map(path), lq.map(path))
        case ir.ReqHb(lp, lq) => ir.ReqHb(lp.map(path), lq.map(path))
    }).withPos(r.pos)
        
    def reifiedFieldDecl(fd: ir.ReifiedFieldDecl): ir.ReifiedFieldDecl =
        ir.ReifiedFieldDecl(fd.as, wcTref(fd.wt), fd.name, path(fd.p_guard)).withPos(fd.pos)
        
    def lvDecl(lv: ir.LvDecl): ir.LvDecl = 
        ir.LvDecl(lv.name, wcTref(lv.wt))

    // XXX Respect potential capture
    def methodSig(msig: ir.MethodSig): ir.MethodSig =
        ir.MethodSig(
            wcClassType(msig.wct_rcvr),
            msig.args.map(lvDecl), 
            msig.reqs.map(req),
            wcTref(msig.wt_ret))        
}