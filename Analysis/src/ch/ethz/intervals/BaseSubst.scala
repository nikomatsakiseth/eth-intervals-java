package ch.ethz.intervals

import Util._

abstract class BaseSubst {
    def path(p: ir.Path): ir.Path
    
    def ghost(g: ir.Ghost) =
        ir.Ghost(g.f, path(g.p))
    
    def tref(t: ir.TypeRef) = 
        ir.TypeRef(t.c, t.ghosts.map(ghost), t.as)
        
    def wpath(wp: ir.WcPath) = wp match {
        case ir.WcHb(ps, qs) => ir.WcHb(ps.map(path), qs.map(path))
        case ir.WcReadableBy(ps) => ir.WcReadableBy(ps.map(path))
        case ir.WcWritableBy(ps) => ir.WcWritableBy(ps.map(path))
        case ir.WcLocks(ps) => ir.WcLocks(ps.map(path))
        case ir.WcLockedBy(ps) => ir.WcLockedBy(ps.map(path))
        case p: ir.Path => path(p)
    }
    
    def wghost(g: ir.WcGhost) =
        ir.WcGhost(g.f, wpath(g.wp))
    
    def wtref(wt: ir.WcTypeRef) =
        ir.WcTypeRef(wt.c, wt.wghosts.map(wghost), wt.as)
        
    def req(r: ir.Req) = (r match {
        case ir.ReqWritableBy(lp, lq) => ir.ReqWritableBy(lp.map(path), lq.map(path))
        case ir.ReqReadableBy(lp, lq) => ir.ReqReadableBy(lp.map(path), lq.map(path))
        case ir.ReqSubintervalOf(lp, lq) => ir.ReqSubintervalOf(lp.map(path), lq.map(path))
        case ir.ReqHb(lp, lq) => ir.ReqHb(lp.map(path), lq.map(path))
    }).withPos(r.pos)
        
    def ghostFieldDecl(fd: ir.GhostFieldDecl) =
        ir.GhostFieldDecl(wtref(fd.wt), fd.name).withPos(fd.pos)
        
    def reifiedFieldDecl(fd: ir.ReifiedFieldDecl) =
        ir.ReifiedFieldDecl(fd.as, wtref(fd.wt), fd.name, path(fd.p_guard)).withPos(fd.pos)
        
    def fieldDecl(fd: ir.FieldDecl) = fd match {
        case gfd: ir.GhostFieldDecl => ghostFieldDecl(gfd)
        case rfd: ir.ReifiedFieldDecl => reifiedFieldDecl(rfd)
    }    

    def lvDecl(lv: ir.LvDecl) = 
        ir.LvDecl(lv.name, wtref(lv.wt))

    // XXX Respect potential capture
    def methodSig(msig: ir.MethodSig) =
        ir.MethodSig(
            tref(msig.t_rcvr),
            msig.as,
            msig.args.map(lvDecl), 
            msig.reqs.map(req),
            wtref(msig.wt_ret))        
}