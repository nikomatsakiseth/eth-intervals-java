package ch.ethz.intervals

abstract class BaseSubst {
    def path(p: ir.Path): ir.Path
    
    def wpath(wp: ir.WcPath) = wp match {
        case ir.WcHb(ps, qs) => ir.WcHb(ps.map(path), qs.map(path))
        case ir.WcHbEq(ps, qs) => ir.WcHbEq(ps.map(path), qs.map(path))
        case p: ir.Path => path(p)
    }
    
    def wtref(wt: ir.WcTypeRef) =
        ir.WcTypeRef(wt.c, wt.wpaths.map(wpath))
    
    def tref(t: ir.TypeRef) = 
        ir.TypeRef(t.c, t.paths.map(path))
        
    def req(r: ir.Req) = r match {
        case ir.ReqHb(p, qs) => ir.ReqHb(path(p), qs.map(path))
        case ir.ReqHbEq(p, qs) => ir.ReqHbEq(path(p), qs.map(path))
        case ir.ReqEq(lv, q) => ir.ReqEqPath(path(lv.path), path(q))
        case ir.ReqEqPath(p, q) => ir.ReqEqPath(path(p), path(q))
        case ir.ReqLocks(p, qs) => ir.ReqLocks(path(p), qs.map(path))
    }
        
    def ghostFieldDecl(fd: ir.GhostFieldDecl) =
        ir.GhostFieldDecl(wtref(fd.wt), fd.name)
        
    def realFieldDecl(fd: ir.RealFieldDecl) =
        ir.RealFieldDecl(wtref(fd.wt), fd.name, path(fd.p_guard))

    def fieldDecl(fd: ir.FieldDecl): ir.FieldDecl = fd match {
        case gfd: ir.GhostFieldDecl => ghostFieldDecl(gfd)
        case rfd: ir.RealFieldDecl => realFieldDecl(rfd)
    }
    
    def lvDecl(lv: ir.LvDecl) = 
        ir.LvDecl(lv.name, wtref(lv.wt))

    def methodSig(msig: ir.MethodSig) =
        ir.MethodSig(
            msig.args.map(lvDecl), 
            msig.reqs.map(req),
            wtref(msig.wt_ret))
}