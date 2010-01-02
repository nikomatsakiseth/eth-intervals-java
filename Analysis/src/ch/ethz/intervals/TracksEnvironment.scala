package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Positional
import Util._

abstract class TracksEnvironment(prog: Prog) 
extends CheckPhase {
    
    import prog.log
    import prog.isSubclass
    import prog.classDecl
    import prog.at
    
    // ______________________________________________________________________
    // Current Environment
    //
    // We use a mutable field here rather than thread the env through explicitly.
    
    var env = ir.Env.empty

    /// Executes g and restores the old environment afterwards:
    def savingEnv[R](g: => R): R = {
        val oldEnv = env
        try { g } finally { env = oldEnv }
    }

    /// Checks that this function preserves the environment:
    def preservesEnv[R](g: => R): R = {
        val oldEnv = env
        try { g } finally { assert(env == oldEnv) }
    }

    // ______________________________________________________________________
    // Modifying the Environment
    
    def pushCurrent(p_cur: ir.Path) =
        log.indented("pushCurrent(%s)", p_cur) {
            env = env.withCurrent(p_cur :: env.ps_cur)
        }
    
    def popCurrent(p_cur: ir.Path) =
        log.indented("popCurrent(%s)", p_cur) {
            assert(env.ps_cur.head == p_cur)
            env = env.withCurrent(env.ps_cur.tail)
        }
        
    def withCurrent[R](p_cur: ir.Path)(g: => R): R = 
        log.indented("withCurrent(%s)", p_cur) {
            try { pushCurrent(p_cur); g } finally { popCurrent(p_cur) }            
        }

    def setWtRet(wt_ret: ir.WcTypeRef) = {
        env = env.withRet(wt_ret)
    }

    /// Add or overwrite a permanent mapping.
    /// Permanent mappings persist across function calls.
    def setPerm(x: ir.VarName, tq: ir.TeePee): Unit = 
        log.indented("addPerm(%s,%s)", x, tq) {
            env = env.withPerm(env.perm + Pair(x, tq))
        }        

    /// Add but not overwrite a permanent mapping.
    /// \see setPerm()
    def addPerm(x: ir.VarName, tq: ir.TeePee): Unit =
        env.perm.get(x) match {
            case Some(_) => throw new ir.IrError("intervals.shadowed", x)
            case None => setPerm(x, tq)
        }    

    def addLvDecl(x: ir.VarName, wt: ir.WcTypeRef, op_canon: Option[ir.Path]) =
        addPerm(x, ir.TeePee(
            wt,
            op_canon.getOrElse(x.path), // default canonical path for x is just x
            ir.noAttrs))                // all local vars are (a) reified and (b) immutable

    def addArg(arg: ir.LvDecl) =
        addPerm(arg.name, ir.TeePee(arg.wt, arg.name.path, ir.noAttrs))
        
    def addTemp(p: ir.Path, q: ir.Path) =
        log.indented("addTemp(%s,%s)", p, q) {
            env = env.withTemp(env.temp + Pair(p, q))
        }

    def clearTemp() =
        log.indented("clearTemp()") {
            env = env.withTemp(Map())
        }        

    def addInvalidated(p: ir.Path) =
        log.indented("addInvalidated(%s)", p) {
            env = env.withInvalidated(env.ps_invalidated + p)
        }        

    def removeInvalidated(p: ir.Path) =
        log.indented("removeInvalidated(%s)", p) {
            env = env.withInvalidated(env.ps_invalidated - p)
        }

    def addHbPnt(tp: ir.TeePee, tq: ir.TeePee): Unit = 
        log.indented("addHb(%s,%s)", tp, tq) {
            assert(tp.isConstant && tq.isConstant)
            assert(isSubclass(tp.wt, ir.c_point))
            assert(isSubclass(tq.wt, ir.c_point))
            env = env.withHb(env.hb + (tp.p, tq.p))
        }

    def addHbInter(tp: ir.TeePee, tq: ir.TeePee): Unit = 
        log.indented("addHb(%s,%s)", tp, tq) {
            assert(tp.isConstant && tq.isConstant)
            assert(isSubclass(tp.wt, ir.c_interval))
            assert(isSubclass(tq.wt, ir.c_interval))
            env = env.withHb(env.hb + (tp.p.end, tq.p.start))
        }

    def addDeclaredReadableBy(tp: ir.TeePee, tq: ir.TeePee): Unit = 
        log.indented("addDeclaredReadableBy(%s, %s)", tp, tq) {
            assert(tp.isConstant && tq.isConstant)
            assert(isSubclass(tp.wt, ir.c_guard))
            assert(isSubclass(tq.wt, ir.c_interval))
            env = env.withReadable(env.readable + (tp.p, tq.p))
        }

    def addDeclaredWritableBy(tp: ir.TeePee, tq: ir.TeePee): Unit = 
        log.indented("addDeclaredWritableBy(%s, %s)", tp, tq) {
            // This assertion is not valid during checkReifiedFieldDecl():
            //assert(tp.isConstant && tq.isConstant)
            assert(isSubclass(tp.wt, ir.c_guard))
            assert(isSubclass(tq.wt, ir.c_interval))
            env = env.withWritable(env.writable + (tp.p, tq.p))
        }

    def addSubintervalOf(tp: ir.TeePee, tq: ir.TeePee): Unit = 
        log.indented("addSubintervalOf(%s, %s)", tp, tq) {
            // This assertion is not valid during checkReifiedFieldDecl():
            //assert(tp.isConstant && tq.isConstant)
            assert(isSubclass(tp.wt, ir.c_interval))
            assert(isSubclass(tq.wt, ir.c_interval))
            env = env.withHb(env.hb + (tq.p.start, tp.p.start) + (tp.p.end, tq.p.end))
            env = env.withSubinterval(env.subinterval + (tp.p, tq.p))
        }

    def addLocks(tp: ir.TeePee, tq: ir.TeePee): Unit =
        log.indented("addLocks(%s, %s)", tp, tq) {
            // This assertion is not valid during checkReifiedFieldDecl:
            //assert(tp.isConstant && tq.isConstant)
            assert(isSubclass(tp.wt, ir.c_interval))
            assert(isSubclass(tq.wt, ir.c_lock))
            env = env.withLocks(env.locks + (tp.p, tq.p))
        }

    def equiv(tp: ir.TeePee, tq: ir.TeePee): Boolean = 
        log.indentedRes("%s equiv %s?", tp, tq) {
            tp.p == tq.p
        }

    // Only enabled when needed:
    def logHb() = ()
//        log.indented("HB") {
//            for((p, q) <- env.hb.allPairs)
//                log("%s hb %s", p, q)            
//        }

    // Note: we don't check that tp, tq are points, although
    // they should be, because we sometimes permit Locks for convenience.
    def hbPnt(tp: ir.TeePee, tq: ir.TeePee): Boolean = 
        log.indentedRes("%s hb[point] %s?", tp, tq) {
            env.hb.contains(tp.p, tq.p)
        }

    // Note: we don't check that tp, tq are intervals, although
    // they should be, because we sometimes permit Locks for convenience.
    def hbInter(tp: ir.TeePee, tq: ir.TeePee): Boolean = 
        log.indentedRes("%s hb[inter] %s?", tp, tq) {
            logHb()
            env.hb.contains(tp.p.end, tq.p.start)
        }

    def superintervals(tp: ir.TeePee): Set[ir.Path] = {
        env.subinterval(tp.p)
    }

    def isSubintervalOf(tp: ir.TeePee, tq: ir.TeePee): Boolean = 
        log.indentedRes("%s subinterval of %s?", tp, tq) {
            env.subinterval.contains(tp.p, tq.p)
        }

    def declaredReadableBy(tp: ir.TeePee, tq: ir.TeePee): Boolean = 
        log.indentedRes("%s readable by %s?", tp, tq) {
            env.readable.contains(tp.p, tq.p)
        }

    def declaredWritableBy(tp: ir.TeePee, tq: ir.TeePee): Boolean = 
        log.indentedRes("%s writable by %s?", tp, tq) {
            env.writable.contains(tp.p, tq.p)
        }

    def locks(tp: ir.TeePee, tq: ir.TeePee): Boolean = 
        log.indentedRes("%s locks %s?", tp, tq) {
            env.locks.contains(tp.p, tq.p) || superintervals(tp).exists(env.locks.contains(_, tq.p))
        }        

    def isWritableBy(tp: ir.TeePee, tq: ir.TeePee): Boolean =
        log.indentedRes("%s isWritableBy %s?", tp, tq) {
            declaredWritableBy(tp, tq) ||
            equiv(tp, tq) || // interval tp writable by itself
            locks(tq, tp) || // lock tp writable by an interval tq which locks tp
            isSubintervalOf(tq, tp) // interval tp writable by any subinterval tq 
        }

    def userHbPair(tp0: ir.TeePee, tq0: ir.TeePee): Option[(ir.TeePee, ir.TeePee)] = 
        log.indentedRes("checkedHbPair(%s,%s)", tp0, tq0) {
            def makePoint(tp: ir.TeePee, f: ir.FieldName) =
                if(isSubclass(tp.wt, ir.c_point)) Some(tp)
                else if(isSubclass(tp.wt, ir.c_interval)) Some(ir.TeePee(ir.t_point, tp.p + f, tp.as))
                else None
                
            (makePoint(tp0, ir.f_end), makePoint(tq0, ir.f_start)) match {
                case (Some(tp1), Some(tq1)) => Some((tp1, tq1))
                case _ => None
            }
        }
        
    def userHb(tp0: ir.TeePee, tq0: ir.TeePee) = 
        log.indentedRes("userHb(%s,%s)", tp0, tq0) {
            userHbPair(tp0, tq0) match {
                case Some((tp1, tq1)) => hbPnt(tp1, tq1)
                case None => false
            }
        }
        
    def addUserHb(tp0: ir.TeePee, tq0: ir.TeePee) = 
        log.indented("checkAndAddHb(%s,%s)", tp0, tq0) {
            userHbPair(tp0, tq0) match {
                case Some((tp1, tq1)) => addHbPnt(tp1, tq1)
                case None =>
            }
        }
        
    def addUserDeclaredWritableBy(tp: ir.TeePee, tq: ir.TeePee) {
        if(isSubclass(tp.wt, ir.c_guard) && isSubclass(tq.wt, ir.c_interval))
            addDeclaredWritableBy(tp, tq)
    }
    
    def addUserDeclaredReadableBy(tp: ir.TeePee, tq: ir.TeePee) {
        if(isSubclass(tp.wt, ir.c_guard) && isSubclass(tq.wt, ir.c_interval))
            addDeclaredReadableBy(tp, tq)
    }
    
    def addUserSubintervalOf(tp: ir.TeePee, tq: ir.TeePee) {
        if(isSubclass(tp.wt, ir.c_interval) && isSubclass(tq.wt, ir.c_interval))
            addSubintervalOf(tp, tq)
    }
    
    def addReq(req: ir.Req) = 
        at(req, ()) {
            log.indented("addReq(%s)", req) {
                def teePeeAdd(add: Function2[ir.TeePee, ir.TeePee, Unit], ps: List[ir.Path], qs: List[ir.Path]) = {
                    val tps = teePee(ir.ghostAttrs, ps)
                    val tqs = teePee(ir.ghostAttrs, qs)
                    foreachcross(tps, tqs)(add)
                }
                req match {
                    case ir.ReqWritableBy(ps, qs) => teePeeAdd(addUserDeclaredWritableBy, ps, qs)
                    case ir.ReqReadableBy(ps, qs) => teePeeAdd(addUserDeclaredReadableBy, ps, qs)
                    case ir.ReqHb(ps, qs) => teePeeAdd(addUserHb, ps, qs)
                    case ir.ReqSubintervalOf(ps, qs) => teePeeAdd(addUserSubintervalOf, ps, qs)
                }   
            }             
        }
        
    def isReadableBy(tp: ir.TeePee, tq: ir.TeePee): Boolean = 
        log.indentedRes("%s isReadableBy %s?", tp, tq) {
            declaredReadableBy(tp, tq) ||
            hbInter(tp, tq) ||
            isWritableBy(tp, tq)            
        }
    
    // ______________________________________________________________________
    // TeePees
    //
    // A TeePee is a typed, canonical path.  The value of a TeePees is 
    // generally constant during the current method, with one exception:
    // a TeePee may reference fields of the this object guarded by the
    // current interval or held under lock, which could then be mutated.  

    /// Returns a list of ghosts for tp.wt.
    /// Any wildcards or missing ghosts in tp.wt are replaced with a path based on 'tp'.
    /// So if tp.wt was Foo<f: ?><g: q>, the result would be List(<f: p.f>, <g: q>) where p = tp.p.
    def ghosts(tp: ir.TeePee): List[ir.Ghost] = preservesEnv {
        prog.ghostFieldDecls(tp.wt.c).map { gfd =>
            tp.wt.owghost(gfd.name) match {
                case Some(p: ir.Path) => ir.Ghost(gfd.name, p)
                case _ => ir.Ghost(gfd.name, tp.p + gfd.name)
            }
        }
    }

    /// Creates a subst from 'tp' that includes 'this→tp.p' and also
    /// substitutes all ghost fields 'this.F' with their correct values.
    def ghostSubstOfTeePee(tp: ir.TeePee): PathSubst = preservesEnv {
        val cd = classDecl(tp.wt.c)
        val lg_tp = ghosts(tp)
        PathSubst.pp(
            ir.p_this :: lg_tp.map(g => ir.p_this + g.f),
            tp.p      :: lg_tp.map(g => g.p)
        )     
    }

    /// Captures tp.wt, using ghostPaths(tp) for the type args.
    def cap(tp: ir.TeePee): ir.TypeRef = preservesEnv {
        ir.TypeRef(tp.wt.c, ghosts(tp), tp.wt.as)
    }

    /// Field decl for tp.f
    def substdFieldDecl(tp: ir.TeePee, f: ir.FieldName) = preservesEnv {
        val rfd = prog.fieldDecl(tp.wt.c, f)
        ghostSubstOfTeePee(tp).fieldDecl(rfd)
    }

    /// Method sig for constructor tp(tqs)
    def substdCtorSig(tp: ir.TeePee, tqs: List[ir.TeePee]) = preservesEnv {
        val msig = classDecl(tp.wt.c).ctor.msig(cap(tp))
        val subst = ghostSubstOfTeePee(tp) + PathSubst.vp(msig.args.map(_.name), tqs.map(_.p))
        subst.methodSig(msig)
    }

    /// Method sig for tp.m(tqs)
    def substdMethodSig(tp: ir.TeePee, m: ir.MethodName, tqs: List[ir.TeePee]) = preservesEnv {
        prog.methodSig(tp.wt.c, m) match {
            case Some(msig) =>
                val subst = ghostSubstOfTeePee(tp) + PathSubst.vp(msig.args.map(_.name), tqs.map(_.p))
                subst.methodSig(msig)        
            case None =>
                throw ir.IrError("intervals.no.such.method", tp.wt.c, m)
        }
    }

    // Helper for teePee(): Computes the teePee for a path p_0.f where f is 
    // a ghost field of (post-substitution) type wt_f.
    def ghostTeePee(tp_0: ir.TeePee, f: ir.FieldName, wt_f: ir.WcTypeRef): ir.TeePee = {
         // Unless the type of p_0 tells us what f is mapped to,
         // tp_f = p_0.f will be the resulting path:
         val tp_f = ir.TeePee(wt_f, tp_0.p + f, tp_0.as.withGhost)                    

         // Does p_0's type specify a value for ghost field f?
         tp_0.wt.owghost(f) match {

             // p_0 had a type with a precise path like Foo<f: q>
             // where q != p_0.f.  If q == p_0.f, this gives us no
             // information: It only tells us p_0's shadow argument f is p_0.f.
             // This can result from the capturing process.
             case Some(q: ir.Path) if q != tp_f.p =>
                 teePee(q)

             // p_0 had a type like Foo<f: ps hb qs>, so canonical
             // version is p_0.f but we add relations that ps hb p_0.f hb qs.
             case Some(ir.WcHb(ps, qs)) => 
                 ps.foreach { case p => addUserHb(teePee(p), tp_f) }
                 qs.foreach { case q => addUserHb(tp_f, teePee(q)) }
                 tp_f

             // p_0 had a type like Foo<f: ps hb qs>, so canonical
             // version is p_0.f but we add relations that ps hb p_0.f hb qs.
             case Some(ir.WcReadableBy(ps)) => 
                 ps.foreach { p => addDeclaredReadableBy(tp_f, teePee(p)) }
                 tp_f

             // p_0 had a type like Foo<f: ps hb qs>, so canonical
             // version is p_0.f but we add relations that ps hb p_0.f hb qs.
             case Some(ir.WcWritableBy(ps)) => 
                 ps.foreach { p => addDeclaredWritableBy(tp_f, teePee(p)) }
                 tp_f

             // Otherwise, just use canonical version of p_0.f with no relations.
             case _ =>
                 tp_f

        }        
    }

    // Helper for teePee(): Computes the teePee for a path p_0.f where f is 
    // a reified field of (post-substitution) type wt_f and with guard p_guard.
    def reifiedTeePee(tp_0: ir.TeePee, f: ir.FieldName, wt_f: ir.WcTypeRef, p_guard: ir.Path): ir.TeePee = {
        val tp_guard = teePee(p_guard)
        val as_f = // determine if f is immutable in current method:
            if(!tp_guard.isConstant)
                tp_0.as.withMutable // guard not yet constant? mutable
            else 
                otp_cur match {
                    case Some(tp_cur) if hbInter(tp_guard, tp_cur) => 
                        tp_0.as
                    case _ => // not guarded by closed interval? mutable
                        tp_0.as.withMutable
                }
        ir.TeePee(wt_f, tp_0.p + f, as_f)        
    }

    /// Constructs a TeePee for p_1 
    def teePee(p_1: ir.Path): ir.TeePee = log.indentedRes("teePee(%s)", p_1) {
        if(env.temp.contains(p_1))
            teePee(env.temp(p_1))
        else 
            p_1 match {
                case ir.Path(lv, List()) => // all local variables should be in env.perm
                    env.perm.get(lv) match {
                        case Some(tp) => tp
                        case None => throw new ir.IrError("intervals.no.such.variable", lv)
                    }

                case ir.Path(lv, f :: rev_fs) =>
                    val tp_0 = teePee(ir.Path(lv, rev_fs))

                    if(f == ir.f_ctor) {
                        // The path is p_0.ctor, which we handle specially:
                        val tp_f = ir.TeePee(ir.t_interval, tp_0.p + f, tp_0.as.withGhost)
                        if(!tp_0.wt.as.ctor) // is tp_0 fully constructed?
                            otp_cur.foreach(tp_cur =>
                                addUserHb(tp_f, tp_cur)) // then .ctor happened before (now constant)
                        tp_f                    
                    } else if (f == ir.f_super) {
                        // The path is p_0.super, which we handle specially:
                        val tp_f = ir.TeePee(ir.t_interval, tp_0.p + f, tp_0.as.withGhost)
                        otp_cur.foreach(tp_cur =>
                            addUserHb(tp_f, tp_cur)) // .super always happened before (now constant)
                        tp_f
                    } else 
                        substdFieldDecl(tp_0, f) match {
                            case ir.GhostFieldDecl(wt_f, _) => 
                                ghostTeePee(tp_0, f, wt_f)
                            case ir.ReifiedFieldDecl(_, wt_f, _, p_guard) => 
                                reifiedTeePee(tp_0, f, wt_f, p_guard)
                        }
            }
    }

    /// Constructs TeePees for all of 'ps'
    def teePee(ps: List[ir.Path]): List[ir.TeePee] = 
        ps.map(teePee)

    /// Constructs a 'tp' for 'p' and checks that it has at most the attributes 'as'
    def teePee(as: ir.Attrs, p: ir.Path): ir.TeePee = {
        val tp = teePee(p)
        val unwanted = tp.as.diff(as)
        if(!unwanted.isEmpty)
            throw new ir.IrError("intervals.illegal.path.attr", p, unwanted.mkEnglishString)
        tp
    }

    /// Constructs tps for 'ps' and checks that they have at most the attributes 'as'
    def teePee(as: ir.Attrs, ps: List[ir.Path]): List[ir.TeePee] = 
        ps.map(teePee(as, _))

    // Note: these are not vals but defs!  This is important
    // because the outcome of teePee() depends on the env.
    def otp_cur = if(env.ps_cur.isEmpty) None else Some(teePee(env.ps_cur.head))
    def tp_ctor = teePee(ir.gfd_ctor.thisPath)
    def tp_this = teePee(ir.p_this)    
    def tp_super = // tp_super always refers to the FIRST supertype
        prog.sups(cap(tp_this)) match {
            case List() => throw new ir.IrError("intervals.no.supertype", tp_this.wt)
            case t_super :: _ => ir.TeePee(t_super, ir.p_this, tp_this.as)
        }

    // ______________________________________________________________________
    // Linked Fields
    
    /// Returns true if 'wt' is linked to 'p': that is,
    /// if it depends on 'p' in some way.  'p' must be
    /// a canonical path.
    def isLinkedWt(p: ir.Path, wt: ir.WcTypeRef) = preservesEnv {
        wt.wghosts.exists(_.wp.dependentOn(p))        
    }
    
    /// A field f_l is linked to tp_o.f if its type is invalidated
    /// when p.f changes.  This occurs when p.f appears in f_l's type.
    /// The rules we enforce when checking field decls. guarantee that
    /// all linked fields either (a) occur in the same class defn as f
    /// or (b) are guarded by some interval which has not yet happened.
    def linkedPaths(tp_o: ir.TeePee, f: ir.FieldName) = preservesEnv {
        val cd = classDecl(tp_o.wt.c)
        val p_f = f.thisPath
        
        // only reified fields can be linked to another field
        val lrfd = cd.fields.foldLeft(List[ir.ReifiedFieldDecl]()) {
            case (l, rfd: ir.ReifiedFieldDecl) => rfd :: l
            case (l, _) => l
        }
        
        // find fields where tp_o.f appears in the type
        val fds_maybe_linked = lrfd.filter { rfd => isLinkedWt(p_f, rfd.wt) }
        
        // screen out those which cannot have been written yet (and whose 
        // value is therefore null, which is valid for any type)
        lazy val subst = ghostSubstOfTeePee(tp_o)
        val fds_linked = fds_maybe_linked.filter { rfd =>
            !otp_cur.exists(tp_cur =>
                hbInter(tp_cur, teePee(subst.path(rfd.p_guard))))
        }
        
        // map to the canonical path for the field
        fds_linked.map { fd => subst.path(fd.thisPath) }
    }
    
}