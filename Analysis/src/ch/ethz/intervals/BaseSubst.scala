package ch.ethz.intervals

abstract class BaseSubst {
    def path(p: ir.Path): ir.Path
    
    def wpath(wp: ir.WcPath) = wp match {
        case ir.WcHb(ps, qs) => ir.WcHb(ps.map(path), qs.map(path))
        case ir.WcHbEq(ps, qs) => ir.WcHbEq(ps.map(path), qs.map(path))
        case p: ir.Path => path(p)
    }
    
    def wtref(wt: ir.WcTypeRef) =
        ir.WcTypeRef(wt.c, wt.wpaths.map(wpath), wt.as)
    
    def tref(t: ir.TypeRef) = 
        ir.TypeRef(t.c, t.paths.map(path), t.as)
        
    def req(r: ir.Req) = r match {
        case ir.ReqHb(p, qs) => ir.ReqHb(path(p), qs.map(path))
        case ir.ReqHbEq(p, qs) => ir.ReqHbEq(path(p), qs.map(path))
        case ir.ReqEq(p, q) => ir.ReqEq(path(p), path(q))
        case ir.ReqLocks(p, qs) => ir.ReqLocks(path(p), qs.map(path))
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