package ch.ethz.intervals

sealed case class TcEnv(
    ps_cur: List[ir.Path],                  // current interval
    wt_ret: ir.WcTypeRef,                   // return type of current method
    perm: Map[ir.VarName, ir.TeePee],       // permanent equivalences, hold for duration of method
    flow: FlowEnv
) {
    def withCurrent(ps_cur: List[ir.Path]) = TcEnv(ps_cur, wt_ret, perm, flow)
    def withRet(wt_ret: ir.WcTypeRef) = TcEnv(ps_cur, wt_ret, perm, flow)
    def withPerm(perm: Map[ir.VarName, ir.TeePee]) = TcEnv(ps_cur, wt_ret, perm, flow)
    def withFlow(flow: FlowEnv) = TcEnv(ps_cur, wt_ret, perm, flow)
    
    def addPerm(x: ir.VarName, tq: ir.TeePee) = {
        perm.get(x) match {
            case Some(_) => throw new CheckFailure("intervals.shadowed", x)
            case None => withPerm(perm + Pair(x, tq))
        }    
    }
    
    def addArg(arg: ir.LvDecl) =
        addPerm(arg.name, ir.TeePee(arg.wt, arg.name.path, ir.noAttrs))
    
    def addArgs(args: List[ir.LvDecl]) =
        args.foldLeft(this) { case (e, a) => e.addArg(a) }        
        
    def addFlow(flow1: FlowEnv) =
        withFlow(flow + flow1)
        
    def intersectFlow(flow1: FlowEnv) =
        withFlow(flow ** flow1)
}

