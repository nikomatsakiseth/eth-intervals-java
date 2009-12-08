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
        case ir.ReqHb(ps, qs) => ir.ReqHb(ps.map(path), qs.map(path))
        case ir.ReqHbEq(ps, qs) => ir.ReqHbEq(ps.map(path), qs.map(path))
        case ir.ReqEq(p, q) => ir.ReqEq(path(p), path(q))
        case ir.ReqLocks(p, qs) => ir.ReqLocks(path(p), qs.map(path))
    }
        
    def ghostFieldDecl(fd: ir.GhostFieldDecl) =
        ir.GhostFieldDecl(fd.name, wtref(fd.wt))
        
    def realFieldDecl(fd: ir.RealFieldDecl) =
        ir.RealFieldDecl(wtref(fd.wt), fd.name, path(fd.guard))

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