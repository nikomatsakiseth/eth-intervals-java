package ch.ethz.intervals

abstract class BaseSubst {
    def without(lvs: List[ir.VarName]): BaseSubst
    def path(p: ir.Path): ir.Path
    
    def wpath(wp: ir.WcPath) = wp match {
        case ir.WcUnkPath => ir.WcUnkPath
        case p: ir.Path => path(p)
    }
        
    def interval(i: ir.Interval) =
        ir.Interval(i.ps.map(path), i.qs.map(path))
        
    def effect(e: ir.Effect): ir.Effect = e match {
        case ir.EffectInterval(i, e) => ir.EffectInterval(interval(i), effect(e))
        case ir.EffectMethod(p, m, qs) => ir.EffectMethod(path(p), m, qs.map(path))
        case ir.EffectFixed(k, p) => ir.EffectFixed(k, path(p))
        case ir.EffectLock(ps, e) => ir.EffectLock(ps.map(path), effect(e))
        case ir.EffectUnion(es) => ir.EffectUnion(es.map(effect))
        case ir.EffectNone => ir.EffectNone
        case ir.EffectAny => ir.EffectAny
    }

    def over(ov: ir.Over) = 
        ir.Over(ov.m, ov.args, without(ov.args).effect(ov.e))
        
    def wtref(wt: ir.WcTypeRef) =
        ir.WcTypeRef(wt.c, wt.wpaths.map(wpath), wt.overs.map(over))
    
    def tref(t: ir.TypeRef) = 
        ir.TypeRef(t.c, t.paths.map(path), t.overs.map(over))
        
    def ghostFieldDecl(fd: ir.GhostFieldDecl) =
        ir.GhostFieldDecl(fd.name, wtref(fd.wt))
        
    def realFieldDecl(fd: ir.RealFieldDecl) =
        ir.RealFieldDecl(fd.mods, wtref(fd.wt), fd.name, path(fd.guard))

    def fieldDecl(fd: ir.FieldDecl): ir.FieldDecl = fd match {
        case gfd: ir.GhostFieldDecl => ghostFieldDecl(gfd)
        case rfd: ir.RealFieldDecl => realFieldDecl(rfd)
    }
    
    def disj(d: ir.DisjointDecl) =
        ir.DisjointDecl(d.ps.map(path))
    
    def lvDecl(lv: ir.LvDecl) = 
        ir.LvDecl(lv.name, wtref(lv.wt))

    def methodSig(msig: ir.MethodSig) =
        ir.MethodSig(
            effect(msig.e), 
            msig.args.map(lvDecl), 
            msig.disjoints.map(disj),
            wtref(msig.wt_ret))
}