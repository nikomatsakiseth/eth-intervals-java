package ch.ethz.intervals

abstract class BaseSubst {
    def path(p: ir.Path): ir.Path
    
    def wpath(wp: ir.WcPath) = wp match {
        case ir.WcHb(ps, qs) => ir.WcHb(ps.map(path), qs.map(path))
        case ir.WcReadable(ps) => ir.WcReadable(ps.map(path))
        case ir.WcWritable(ps) => ir.WcWritable(ps.map(path))
        case ir.WcLocks(ps) => ir.WcLocks(ps.map(path))
        case ir.WcLockedBy(ps) => ir.WcLockedBy(ps.map(path))
        case p: ir.Path => path(p)
    }
    
    def wtref(wt: ir.WcTypeRef) =
        ir.WcTypeRef(wt.c, wt.wpaths.map(wpath), wt.as)
    
    def tref(t: ir.TypeRef) = 
        ir.TypeRef(t.c, t.paths.map(path), t.as)
        
    def req(r: ir.Req) = r match {
        case ir.ReqWritableBy(lp, lq) => ir.ReqWritableBy(lp.map(path), lq.map(path))
        case ir.ReqReadableBy(lp, lq) => ir.ReqReadableBy(lp.map(path), lq.map(path))
        case ir.ReqSubintervalOf(lp, lq) => ir.ReqSubintervalOf(lp.map(path), lq.map(path))
        case ir.ReqHb(lp, lq) => ir.ReqHb(lp.map(path), lq.map(path))
    }
        
    def ghostDecl(fd: ir.GhostDecl) =
        ir.GhostDecl(wtref(fd.wt), fd.name)
        
    def fieldDecl(fd: ir.FieldDecl) =
        ir.FieldDecl(fd.as, wtref(fd.wt), fd.name, path(fd.p_guard))

    def lvDecl(lv: ir.LvDecl) = 
        ir.LvDecl(lv.name, wtref(lv.wt))

    def methodSig(msig: ir.MethodSig) =
        ir.MethodSig(
            msig.as,
            msig.args.map(lvDecl), 
            msig.reqs.map(req),
            wtref(msig.wt_ret))
}