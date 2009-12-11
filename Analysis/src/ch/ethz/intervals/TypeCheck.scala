package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.collection.mutable.ListBuffer
import Util._

/*
Notes of Notation:

Foo<f: p>
---------

We often use the notation Foo<f: p> to mean a type Foo 
value p for its ghost parameter f.  This notation combines
the class declaration, which must have been something like:
    class Foo<wt f> { ... }
and the normal reference a user would use:
    Foo<p>
into one more concise entity Foo<f: p> that provides both
the name (but not type) and value of the ghost argument(s).
*/

class TypeCheck(log: Log, prog: Prog) {    
    import prog.classDecl
    import prog.fresh

    // ______________________________________________________________________
    // Environment
    
    var env = ir.TcEnv(
        Map(
            (ir.lv_readOnly, ir.TeePee(ir.t_interval, ir.p_readOnly, false)),
            (ir.lv_cur, ir.TeePee(ir.t_interval, ir.p_cur, false))
        ),
        Map(),
        ListSet.empty,
        ir.Relation.emptyTrans,
        ir.Relation.emptyTransRefl,
        ir.Relation.empty,
        ListSet.empty
    )
    
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
    
    def addPerm(p: ir.Path, tq: ir.TeePee): Unit = 
        log.indented("addPerm(%s,%s)", p, tp) {
            env.perm.get(p) match {
                case Some(_) =>
                    throw new ir.IrError("intervals.shadowed", p)
                case None =>
                    env = ir.TcEnv(
                        env.lvs + Pair(p, tq),
                        env.hb,
                        env.hbeq,
                        env.locks)
            }
        }

    // ______________________________________________________________________
    // Errors

    def at[R](loc: ir.Locatable, default: R)(g: => R): R = 
        try {
            g
        } catch {
            case err: ir.IrError =>
                prog.reportError(loc, err)
                log("Error: %s", err)
                default           
        }
    
    // ______________________________________________________________________
    // Basic Type Operations: Finding fields, methods, supertypes
        
    /// subst. ghosts of t
    def ghostSubst(t: ir.TypeRef) = preservesEnv {
        val ghostPaths = classDecl(t.c).ghosts.map(_.thisPath)
        PathSubst.pp(ghostPaths, t.paths)
    }

    /// supertype of t
    def sup(t: ir.TypeRef): Option[ir.TypeRef] = preservesEnv {
        val cd = classDecl(t.c)
        cd.superType.map { case t_1 => ghostSubst(t).tref(t_1) }
    }

    /// Field decl for t0::f 
    def realFieldDecl(c: ir.ClassName, f: ir.FieldName): ir.FieldDecl = preservesEnv {
        log.indentedRes("realFieldDecl(%s,%s)", c, f) {
            def search(t: ir.TypeRef): ir.FieldDecl = 
                log.indentedRes("search(%s)", t) {
                    val cd = classDecl(t.c)
                    cd.fields.find(_.name == f) match {
                        case Some(fd) => fd
                        case None => sup(t) match {
                            case Some(t_1) => ghostSubst(t_1).realFieldDecl(search(t_1))
                            case None => throw ir.IrError("intervals.no.such.field", c, f)
                        }
                    }
                }
            val cd = classDecl(c)
            search(cd.thisTref)
        }            
    }

    /// Method sig for t0::m()
    def methodSig(c: ir.ClassName, m: ir.MethodName): ir.MethodSig = preservesEnv {
        log.indentedRes("methodSig(%s,%s)", c, m) {
            def search(t: ir.TypeRef): ir.MethodSig = {
                val cd = classDecl(t.c)
                cd.methods.find(_.name == m) match {
                    case Some(md) => md.msig
                    case None => sup(t) match {
                        case Some(t_1) => ghostSubst(t_1).methodSig(search(t_1))
                        case None => throw ir.IrError("intervals.no.such.method", c, m)
                    }
                }
            }
            val cd = classDecl(c)
            search(cd.thisTref)
        }
    }
    
    // ______________________________________________________________________
    // TeePees
    //
    // A TeePee is a typed, canonical path.  The value of a TeePees is 
    // generally constant during the current method, with one exception:
    // a TeePee may reference fields of the this object guarded by the
    // current interval or held under lock, which could then be mutated.  
    
    /// Returns a list of paths for the ghosts of tp.wt.
    /// Any wildcards in tp.wt are replaced with a path based on 'tp'.
    /// So if tp.wt was Foo<f: ?, g: q>, the result would be List(tp.p + f, q).
    def ghostPaths(tp: ir.TeePee): List[ir.Path] = preservesEnv {
        val cd = classDecl(tp.wt.c)
        cd.ghosts.zip(tp.wt.wpaths).map {
            case (_, s: ir.Path) => s
            case (gfd, _) => tp.p + gfd.name
        }        
    }
    
    /// Creates a subst from 'tp' that replaces 'this' and any of its ghosts.
    def ghostSubst(tp: ir.TeePee): PathSubst = preservesEnv {
        val cd = classDecl(tp.wt.c)
        PathSubst.pp(
            ir.p_this :: cd.ghosts.map(_.thisPath),
            tp.p      :: ghostPaths(tp)
        )     
    }
    
    /// Captures tp.wt, using ghostPaths(tp) for the type args.
    def cap(tp: ir.TeePee): ir.TypeRef = preservesEnv {
        ir.TypeRef(tp.wt.c, ghostPaths(tp))                
    }
    
    /// Field decl for tp.f
    def substdFieldDecl(tp: ir.TeePee, f: ir.FieldName) = preservesEnv {
        val rfd = realFieldDecl(tp.wt.c, f)
        ghostSubst(tp).realFieldDecl(rfd)
    }
    
    /// Method sig for tp.m(tqs)
    def substdMethodSig(tp: ir.TeePee, m: ir.MethodName, tqs: List[ir.TeePee]) = preservesEnv {
        val msig = methodSig(tp.wt.c, m)
        val subst = ghostSubst(tp) + PathSubst.vp(msig.args.map(_.name), tqs.map(_.p))
        subst.methodSig(msig)        
    }
    
    /// Constructs a TeePee for p_1 
    def teePee(p_1: ir.Path): ir.TeePee = log.indentedRes("teePee(%s)", p0) {
        if(env.perm.contains(p_1))
            env.perm(p_1)
        else if(env.temp.contains(p_1))
            teePee(env.temp(p_1))
        else p_1 match {
            case ir.Path(lv, List()) => // all local variables should be in env.perm
                throw new ir.IrError("intervals.no.such.variable", lv)
                
            case ir.Path(lv, f :: rev_fs) =>
                val p_0 = ir.Path(lv, rev_fs) // p_1 == p_0.f
                val tp_0 = teePee(p_0)
                
                def ghostTeePee(gfd_f: ir.GhostFieldDecl) = {
                    val wt_f = ghostSubst(tp_0).wtref(gfd_f.wt)
                    ir.TeePee(wt_f, p_0 + f, tp_0.as.withGhost)                    
                }

                // First look for a ghost with the name "f", then a real field:
                val cd_0 = classDecl(tp_0.wt.c)
                cd_0.ghosts.zip(tp_0.wt.wpaths).find(_._1.name == f) match {
                    
                    // Ghost with precise path specified:
                    case Some((gfd_f, q: ir.Path)) => 
                        if(q != p_0 + f) 
                            // p_0 had a type like Foo<f: q>, so canonical
                            // version of p_0.f is canonical version of q:
                            teePee(q)
                        else
                            // p_0 had a type like Foo<f: p_0.f>, which gives us no information:
                            // It only tells us p_0's shadow argument f is p_0.f.
                            // This can result from the capturing process, when p_0 had
                            // a wildcard path (see below).
                            ghostTeePee(gfd_f)
                            
                    // p_0 had a type like Foo<f: ps hb qs>, so canonical
                    // version is p_0.f but we add relations that ps hb p_0.f hb qs.
                    case Some((gfd_f, ir.WcHb(ps, qs))) => 
                        val tp_f = ghostTeePee(gfd_f)
                        ps.foreach { case p => addHb(teePee(p), tp_f) }
                        qs.foreach { case p => addHb(tp_f, teePee(q)) }
                        tp_f
                    
                    // p_0 had a type like Foo<f: ps hbeq qs>, so canonical
                    // version is p_0.f but we add relations that ps hb p_0.f hbeq qs.
                    case Some((gfd_f, ir.WcHbEq(ps, qs))) => 
                        val tp_f = ghostTeePee(gfd_f)
                        ps.foreach { case p => addHbEq(teePee(p), tp_f) }
                        qs.foreach { case p => addHbEq(tp_f, teePee(q)) }
                        tp_f
                        
                    // The path is p_0.constructor, which we handle specially:
                    case None if f == ir.f_ctor =>
                        val tp_f = ir.TeePee(ir.t_interval, p_0 + f, tp.as.withGhost)
                        if(tp_0.wt.as.ctor) // is tp_0 fully constructed?
                            addHb(tp_f, tp_mthd) // then we may assume p_0.ctor -> method
                        tp_f
                        
                    // The path p_0.f names a real field f:
                    case None =>
                        val rfd_f = substdFieldDecl(tp_0, f)
                        val tp_guard = constructTeePee(rfd_f.p_guard)
                        val as_f = // determine if f is immutable in current method:
                            if(!tp_guard.isConstant)
                                tp_0.as.withMutable // guard not yet constant? mutable
                            else if(!hb(tp_guard, tp_mthd))
                                tp_0.as.withMutable // not guarded by closed interval? mutable
                            else
                                tp_0.as // p_0 mutable? mutable
                        ir.TeePee(rfd_f.wt, p_0 + f, as_f)
                }
        }
    }
    
    /// Constructs TeePees for all of 'ps'
    def teePee(ps: List[ir.Path]): List[ir.TeePee] = 
        ps.map(teePee)

    /// Constructs a 'tp' for 'p' and checks that it has at most the attributes 'as'
    def teePee(as: ir.Attrs, p: ir.Path): ir.TeePee = {
        val tp = teePeeAny(as)
        val unwanted = tp.as.diff(as)
        if(!unwanted.isEmpty)
            throw new ir.IrError("intervals.illegal.path.attr", p, unwanted.mkEnglishString)
    }
        
    /// Constructs tps for 'ps' and checks that they have at most the attributes 'as'
    def teePee(as: ir.Attrs, ps: List[ir.Path]): List[ir.TeePee] = 
        ps.map(teePee(as, _))
        
    // Note: these are not vals but defs!  This is important
    // because the outcome of teePee() depends on the env.
    def tp_cur = teePee(ir.p_cur)
    def tp_mthd = teePee(ir.p_mthd)
    def tp_this = teePee(ir.p_this)
    
    // ______________________________________________________________________
    // Well-formedness
    //
    // Very basic sanity checks.
    
    def checkWfReq(req: ir.Req) = preservesEnv { req match {
        case ir.ReqHb(p, qs) => teePee(p); teePee(qs)
        case ir.ReqHbEq(p, qs) => teePee(p); teePee(qs)
        case ir.ReqEq(lv, p) => teePee(lv.path); teePee(p)
        case ir.ReqEqPath(p, q) => teePee(p); teePee(q)
        case ir.ReqLocks(p, qs) => teePee(p); teePee(qs)
    } }
    
    def checkWfWt(wt: ir.WcTypeRef) = preservesEnv {
        val cd = classDecl(wt.c)
        
        if(cd.ghosts.length != wt.wpaths.length)
            throw new ir.IrError(
                "intervals.wrong.number.of.ghosts", 
                cd.ghosts.length, wt.wpaths.length)
                
        // Note: we don't check that the arguments
        // match the various ghost bounds etc.  We just
        // check that when the type is constructed.
    }

    // ______________________________________________________________________
    // Env Relations
    
    def addTemp(p: ir.Path, q: ir.Path) {
        log.indented("addCanon(%s,%s)", p, q) {
            env = ir.TcEnv(
                env.perm,
                env.temp + Pair(p, q),
                env.fs_invalidated,
                env.hb,
                env.hbeq,
                env.locks,
                env.owned)
        }
    }
    
    def clearTemp() {
        log.indented("addCanon(%s,%s)", p, q) {
            env = ir.TcEnv(
                env.perm,
                List(),
                env.fs_invalidated,
                env.hb,
                env.hbeq,
                env.locks,
                env.owned)
        }        
    }
    
    def addInvalidated(p: ir.Path) {
        log.indented("addInvalidated(%s)", p) {
            env = ir.TcEnv(
                env.perm,
                env.temp,
                env.fs_invalidated + f,
                env.hb,
                env.hbeq,
                env.locks,
                env.owned)
        }        
    }
        
    def removeInvalidated(p: ir.Path) {
        log.indented("removeInvalidated(%s)", f) {
            env = ir.TcEnv(
                env.perm,
                env.temp,
                env.fs_invalidated - p,
                env.hb,
                env.hbeq,
                env.locks,
                env.owned)
        }        
    }
        
    def addHb(tp: ir.TeePee, tq: ir.TeePee): Unit = log.indented("addHb(%s,%s)", tp, tq) {
        assert(isSubtype(tp, ir.t_interval))
        assert(isSubtype(tq, ir.t_interval))
        assert(tp.isConstant && tq.isConstant)
        env = ir.TcEnv(
            env.perm,
            env.temp,
            env.fs_invalidated,
            env.hb + (tp.p, tq.p),
            env.hbeq + (tp.p, tq.p),
            env.locks,
            env.owned)
    }
    
    def addHbEq(tp: ir.TeePee, tq: ir.TeePee): Unit = log.indented("addHbEq(%s,%s)", tp, tq) {
        assert(isSubtype(tp, ir.t_interval))
        assert(isSubtype(tq, ir.t_interval))
        assert(tp.isConstant && tq.isConstant)
        env = ir.TcEnv(
            env.perm,
            env.temp,
            env.fs_invalidated,
            env.hb,
            env.hbeq + (tp.p, tq.p),
            env.locks,
            env.owned)
    }
    
    def addLocks(tp: ir.TeePee, tq: ir.TeePee): Unit = log.indented("addLocks(%s,%s)", tp, tq) {
        assert(isSubtype(tp, ir.t_interval))
        assert(isSubtype(tq, ir.t_lock))
        assert(tp.isConstant && tq.isConstant)
        env = ir.TcEnv(
            env.perm,
            env.temp,
            env.fs_invalidated,
            env.hb,
            env.hbeq,
            env.locks + (tp.p, tq.p),
            env.owned)
    }
    
    def addOwned(tp: ir.TeePee): Unit = log.indented("addOwned(%s)", tp) {
        env = ir.TcEnv(
            env.perm,
            env.temp,
            env.fs_invalidated,
            env.hb,
            env.hbeq,
            env.locks,
            env.owned + tp.p)
    }
    
    def hb(p: ir.TeePee, q: ir.TeePee): Boolean = log.indentedRes("%s hb %s?", p, q) {
        env.hb.contains(p.p, q.p)
    }
    
    def hbeq(p: ir.TeePee, q: ir.TeePee): Boolean = log.indentedRes("%s hbeq %s?", p, q) {
        env.hbeq.contains(p.p, q.p)
    }
    
    def equiv(p: ir.TeePee, q: ir.TeePee): Boolean = log.indentedRes("%s eq %s?", p, q) {
        p.p == q.p
    }
    
    def locks(p: ir.TeePee, q: ir.TeePee): Boolean = log.indentedRes("%s locks %s?", p, q) {
        env.locks.contains(p.p, q.p)
    }
    
    def owned(p: ir.TeePee): Boolean = log.indentedRes("%s owned?", p) {
        env.owned.contains(p.p)
    }
    
    def addReq(req: ir.Req) = {
        def teePeeAdd(add: Function2[ir.TeePee, ir.TeePee, Unit], p: ir.Path, qs: List[ir.Path]) = {
            val tp = teePee(p)
            val tqs = teePee(qs)
            tqs.foreach(add(tp, _))
        }
        req match {
            case ir.ReqHb(p, qs) => teePeeAdd(addHb, p, qs)
            case ir.ReqHbEq(p, qs) => teePeeAdd(addHbEq, p, qs)
            case ir.ReqEqPath(p, q) => addTemp(p, q) // XXX Check for cycles or something
            case ir.ReqLocks(p, qs) => teePeeAdd(addLocks, p, qs)
        }   
    } 
    
    // ______________________________________________________________________
    // Subtyping    
    
    /// wp <= wq
    def isSubpath(p: ir.Path, wq: ir.WcPath) = preservesEnv {
        log.indentedRes("%s <= %s?", wp, wq) {                
            // Here we just use teePee() without checking that it is 
            // immutable.  That's safe because we never ADD to the relations
            // unless the teePee is immutable. 
            //
            // XXX Is this REALLY true...?
            val tp = teePee(p)
            wq match {
                case q: ir.Path =>
                    equiv(tp, teePee(q))
                case ir.WcHb(lq_1, lq_2) =>
                    lq_1.forall { q_1 => hb(teePee(q_1), tp) } &&
                    lq_2.forall { q_2 => hb(tp, teePee(q_2)) }
                case ir.WcHbEq(lq_1, lq_2) =>
                    lq_1.forall { q_1 => hbeq(teePee(q_1), tp) } &&
                    lq_2.forall { q_2 => hbeq(tp, teePee(q_2)) }
                case ir.WcLocks(lq) =>
                    lq.forall { q => locks(tp, teePee(q)) }
                case ir.WcLockedBy(lq) =>
                    lq.forall { q => locks(teePee(q), tp) }
            }
        }
    }
    
    /// t_sub <: wt_sup
    def isSubtype(t_sub: ir.TypeRef, wt_sup: ir.WcTypeRef): Boolean = preservesEnv {
        log.indentedRes("%s <: %s?", t_sub, wt_sup) {
            // (t <: t constructor) but (t constructor not <: t)
            if(wt_sup.as.ctor && !t.as.ctor) 
                false
            
            // c<P> <: c<WP> iff P <= WP
            if(t_sub.c == wt_sup.c)
                forallzip(t_sub.wpaths, wt_sup.wpaths)(isSubpath) 
            else // else walk to supertype of t_sub
                sup(t_sub) match {
                    case None => false
                    case Some(t) => isSubtype(t, wt_sup)
                }                
        }        
    }
    
    def isSubtype(tp_sub: ir.TeePee, wt_sup: ir.WcTypeRef): Boolean = preservesEnv {
        isSubtype(cap(tp_sub), wt_sup)        
    }
        
    /// wt_sub <: wt_sup
    def checkIsSubtype(tp_sub: ir.TeePee, wt_sup: ir.WcTypeRef) {
        if(!isSubtype(tp_sub, wt_sup))
            throw new ir.IrError("intervals.expected.subtype", tp_sub.p, tp_sub.wt, wt_sup)
    }
        
    def isReqFulfilled(req: ir.Req): Boolean = req match {
        case ir.ReqOwned(p) =>   
            val tp = teePee(p)
            owner(tp.p) || (
                if(isSubtype(tp, ir.t_interval))
                    isReqFulfilled(ir.ReqEq(ir.p_cur, tp)) ||
                    isReqFulfilled(ir.ReqEq(ir.p_mthd, tp))
                else if(isSubtype(tp, ir.t_lock))
                    isReqFulfilled(ir.ReqLocks(ir.p_cur, tp))
                else
                    false
            )
        case ir.ReqHb(p, qs) => qs.forall(hb(p, _))
        case ir.ReqHbEq(p, qs) => qs.forall(hbeq(p, _))
        case ir.ReqEq(p, q) => equiv(teePee(p), teePee(q))
        case ir.ReqLocks(p, qs) => qs.forall(locks(p, _))
    }
    
    def checkReqFulfilled(req: ir.Req) {
        if(!isReqFulfilled(req))
            throw new ir.IrError("intervals.requirement.not.met", req)
    }
    
    def checkLengths(l1: List[_], l2: List[_], msg: String) = preservesEnv {
        if(l1.length != l2.length)
            throw new ir.IrError(msg, l1.length, l2.length)
    }
    
    def checkArgumentTypes(msig: ir.MethodSig, tqs: List[ir.TeePee]) = preservesEnv {
        checkLengths(msig.args, tqs, "intervals.wrong.number.method.arguments")
        foreachzip(tqs, msig.args.map(_.wt))(checkIsSubtype)
    }
    
    def checkLvDecl(vd: ir.LvDecl, owt: Option[ir.WcTypeRef], op_canon: Option[ir.Path]) = {
        
        checkWfWt(vd.wt)
        
        owt.foreach (savingEnv { case wt =>
            val lv = ir.VarName(fresh("val"))
            val tp = ir.TeePee(wt, lv.path, true)
            addPerm(lv.path, tp)
            checkIsSubtype(tp, vd.wt)
        })
        
        addPerm(vd.name.path, ir.TeePee(
            vd.wt, 
            op_canon.getOrElse(vd.name.path), // default canonical path for x is just x
            ir.noAttrs))                      // all local vars are (a) reified and (b) immutable
    }
    
    def checkCurrentLocks(tp_guard: ir.TeePee) = preservesEnv {
        if(!locks(tp_cur, tp_guard))
            throw new ir.IrError("intervals.no.lock", tp_guard.p)
    }
    
    /// Returns true if 'wt' is linked to 'p': that is,
    /// if it depends on 'p' in some way.  'p' must be
    /// a canonical path.
    def isLinkedWt(p: ir.Path, wt: ir.WcTypeRef) = preservesEnv {
        wt.wpaths.exists(_.dependentOn(p))        
    }
    
    /// A field f_l is linked to tp_o.f if its type is invalidated
    /// when p.f changes.  This occurs when p.f appears in f_l's type.
    /// The rules we enforce when checking field decls. guarantee that
    /// all linked fields either (a) occur in the same class defn as f
    /// or (b) are guarded by some interval which has not yet happened.
    def linkedPaths(tp_o: ir.TeePee, f: ir.FieldName, tp_guard: ir.TeePee) = preservesEnv {
        val cd = classDecl(tp_o.wt.c)
        val p_f = f.thisPath
        
        // find fields where tp_o.f appears in the type
        val fds_maybe_linked = cd.fields.filter { rfd => isLinkedWt(p_f, rfd.wt) }
        
        // screen out those which are guarded by future intervals
        lazy val subst = ghostSubst(tp_o)
        val fds_linked = fds_maybe_linked.filter { rfd =>
            !hb(tp_guard, subst.path(rfd.p_guard))
        }
        
        // map to the canonical path for the field
        fds_linked.map { fd => subst.path(fd.thisPath) }
    }
    
    def checkReadable(tp_guard: ir.TeePee) {
        if(!owned(tp_guard)) {
            if(isSubtype(tp_guard, ir.t_interval)) {
                if(!hbeq(tp_guard, tp_mthd) && !hbeq(tp_guard, tp_cur))
                    throw new ir.IrError("intervals.no.hb", tp_guard.p, tp_mthd.p)
            } else if(isSubtype(tp_guard, ir.t_lock)) {
                checkCurrentLocks(tp_guard)
            } else {
                throw new ir.IrError("intervals.not.owned", tp_guard.p)
            }
        }
    }
    
    def checkWritable(tp_guard: ir.TeePee) {
        if(!owned(tp_guard)) {
            if(isSubtype(tp_guard, ir.t_interval)) {
                if(!equiv(tp_guard, tp_cur) && !equiv(tp_guard, tp_mthd))
                    throw new ir.IrError("intervals.not.current", tp_guard.p)
            } else if(isSubtype(tp_guard, ir.t_lock)) {
                checkCurrentLocks(tp_guard)
            } else {
                throw new ir.IrError("intervals.not.owned", tp_guard.p)
            }
        }
    }    
        
    def checkNoInvalidated() {
        if(!env.fs_invalidated.isEmpty)
            throw new ir.IrError(
                "intervals.must.assign.first", 
                env.fs_invalidated.mkEnglishString)        
    }
    
    def checkStatement(stmt: ir.Stmt) = log.indented(stmt) {
        at(stmt, ()) {
            stmt match {  
                case ir.StmtNull(vd) =>
                    checkLvDecl(vd, None, None)

                case ir.StmtGetField(vd, p_o, f) =>
                    val tp_o = teePee(ir.noAttrs, p_o)
                    val fd = substdFieldDecl(tp_o, f)
                    val tp_guard = teePee(ir.ghostAttrs, fd.p_guard)                    
                    val p_f = tp_o.p + f // Canonical path of f; valid as f is not a ghost.

                    val op_canon = 
                        if(hb(tp_guard, tp_mthd)) { // Constant:
                            Some(p_f)
                        } else { // Non-constant:
                            checkReadable(tp_guard)
                            addTemp(p_f, vd.name.path) // Record that p.f == vd, for now.
                            None
                        }
            
                    checkLvDecl(vd, Some(fd.wt), op)
                    
                case ir.StmtSetField(p_o, f, p_v) =>
                    val tp_o = teePee(ir.noAttrs, p_o)
                    val tp_v = teePee(ir.noAttrs, p_v)
                    val fd = substdFieldDecl(tp_o, f)
                    checkIsSubtype(tp_v, fd.wt)
                    
                    val tp_guard = teePee(ir.ghostAttrs, fd.p_guard)
                    checkWritable(tp_guard)
                    val p_f = tp_o.p + f // Canonical path of f; valid as f is not a ghost.
                    addTemp(p_f, tp_v)
                    
                    env.removeInvalidated(p_f)
                    linkedPaths(tp_o, f, tp_guard).foreach(addInvalidated)
                        
                case ir.StmtCall(vd, p, m, qs) =>
                    val tp = teePee(ir.noAttrs)
                    val tqs = teePee(ir.noAttrs, qs)
                    val msig = substdMethodSig(tp, m, tqs)
                    
                    // Cannot invoke a method when there are outstanding invalidated fields:
                    checkNoInvalidated()
                    
                    // If method is not a constructor method, receiver must be constructed:
                    if(!msig.as.ctor && tp.wt.as.ctor) 
                        throw new ir.IrError("intervals.rcvr.must.be.constructed", p)                        
                        
                    // Arguments must have correct type and requirements must be fulfilled:
                    checkArgumentTypes(msig, tqs)
                    msig.reqs.foreach(checkReqFulfilled)
                    
                    // Any method call disrupts potential temporary assocations:
                    //     We make these disruptions before checking return value, 
                    //     in case they would affect the type.  Haven't thought through
                    //     if this can happen or not, but this would be the right time anyhow.
                    clearTemp()
                    
                    checkLvDecl(vd, Some(msig.wt_ret), None)
        
                case ir.StmtNew(vd, t, qs) =>
                    checkWfWt(t)
                    val cd = classDecl(t.c)
                    
                    // Cannot invoke a method when there are outstanding invalidated fields:
                    checkNoInvalidated()
                    
                    // Check Ghost Types:
                    checkLengths(cd.ghosts, t.paths, "intervals.wrong.number.ghost.arguments")
                    val substGhosts = PathSubst.pp(
                        ir.p_this    :: cd.ghosts.map(_.thisPath), 
                        vd.name.path :: t.paths)
                    foreachzip(cd.ghosts, t.paths) { case (gfd, p) =>
                        val tp = teePee(p)
                        checkIsSubtype(tp, substGhosts.wtref(gfd.wt))
                    }

                    // Check Argument Types:
                    val ctor = cd.ctor
                    val tqs = teePee(qs)
                    val subst = substGhosts + PathSubst.pp(ctor.args.map(_.name), tqs.map(_.p))
                    val msig = subst.methodSig(ctor.msig)
                    checkArgumentTypes(msig, tqs)

                    // Check Constructor Requirements:
                    msig.reqs.foreach(checkReqFulfilled)
                    
                    // Executing constructor code could invalidate our alias assumptions:
                    clearTemp()                    
                    
                    checkLvDecl(vd, Some(t), None)
                    
                case ir.StmtHb(p, q) =>
                    val tp = teePee(ir.noAttrs, p)
                    val tq = teePee(ir.noAttrs, q)
                    
                    // XXX Rejigger HB to be based on points:
                    //checkIsSubtype(wt_path(p), ir.t_point)
                    //checkIsSubtype(wt_path(q), ir.t_point)
                    
                    checkIsSubtype(tp, ir.t_interval)
                    checkIsSubtype(tq, ir.t_interval)
                    addHb(tp, tq)
                    
                case ir.StmtLocks(p, q) =>
                    val tp = teePee(ir.noAttrs, p)
                    val tq = teePee(ir.noAttrs, q)
                    checkIsSubtype(tp, ir.t_interval)
                    checkIsSubtype(tq, ir.t_lock)
                    addLocks(tp, tq)
            }
        }
    }
    
    def checkFieldDecl(cd: ir.ClassDecl)(rfd: ir.FieldDecl) = savingEnv {
        log.indented(rfd) {
            at(rfd, ()) {
                checkWfWt(rfd.wt)
                
                val tp_guard = teePee(rfd.p_guard)
                
                // Rules:
                //
                // The type of a field f in class c may be dependent on a path p_dep if either:
                // (1) p_dep is constant when p_g is active; or
                // (2) p_dep = this.f' and f' is declared in class c (not a supertype!)
                //
                // Note that a type is dependent on p_dep if p.F appears in the type, so 
                // we must check all prefixes of each dependent path as well.
                
                rfd.wt.dependentPaths.foreach { p_full_dep =>
                    
                    def check(p_dep: ir.Path) {
                        p_dep match {
                            case ir.Path(lv, List()) => 
                                // Always permitted.

                            case ir.Path(lv, List(f)) if lv == ir.lv_this => 
                                val tp_dep = teePee(p_dep)
                                if(!tp_dep.isConstant && !cd.fields.exists(_.name == f))
                                    throw new ir.IrError(
                                        "intervals.illegal.type.dep",
                                        tp_dep.p, tp_guard.p)

                            case ir.Path(lv, f :: rev_fs) =>
                                checkDependentPath(ir.Path(l, rev_fs))
                                val tp_dep = teePee(p_dep)
                                if(!tp_dep.isConstant)
                                    throw new ir.IrError(
                                        "intervals.illegal.type.dep",
                                        tp_dep.p, tp_guard.p)
                        }
                    }

                    savingEnv {                        
                        // XXX Add some logic regarding this.constructor.  Probably we want
                        // XXX constructor fields as well?
                        addPerm(ir.p_cur, tp_guard) // use guard as the current interval
                        check(p_full_dep)
                    }                   
                }
                
                // Guards must be of type Interval, Lock, or Object:
                //   (Object is used to write generic code dependent on either interval or lock)
                if(!isSubtype(tp_guard, ir.t_interval) &&
                    !isSubtype(tp_guard, ir.t_lock) &&
                    tp_guard.wt.c != ir.c_object)
                    throw new ir.IrError("intervals.invalid.guard", tp_guard.p)
            }
        }
    }
    
    def checkMethodDecl(md: ir.MethodDecl) = savingEnv {
        log.indented(md) {
            at(md, ()) {
                md.args.foreach { case arg => 
                    checkWfWt(arg.wt)
                    addLv(arg.name, ir.TeePee(arg.wt, arg.name.path, true))
                }
                
                md.reqs.foreach(checkWfReq)
                md.reqs.foreach(addReq)
                
                md.stmts.foreach(checkStatement)
                
                md.p_ret.foreach { p_ret =>
                    val tp_ret = teePee(p_ret)
                    checkReified(tp_ret)
                    checkIsSubtype(tp_ret, md.wt_ret)
                }
            }         
        }
    }
    
    def checkClassDecl(cd: ir.ClassDecl) = savingEnv {
        log.indented(cd) {
            at(cd, ()) {
                addLv(ir.lv_this, ir.TeePee(cd.thisTref, ir.p_this, true)) 
                cd.fields.foreach(checkFieldDecl)
                
                addHb(ir.p_readOnly, ir.p_cur) // HACK
                
                // XXX Need to extract visible effects of ctor
                // XXX and save them in environment for other
                // XXX methods.
                savingEnv {
                    addLv(ir.lv_mthd, ir.TeePee(ir.t_interval, ir.gfd_ctor.thisPath, false))
                    checkMethodDecl(cd.ctor)                    
                }
                
                savingEnv {
                    addLv(ir.lv_mthd, ir.TeePee(ir.t_interval, ir.p_mthd, false))
                    cd.ctor.reqs.foreach(addReq)
                    addHb(ir.gfd_ctor.thisPath, ir.p_mthd)
                    cd.methods.foreach(checkMethodDecl)                    
                }
            }
        }
    }
    
    def check = prog.cds_user.foreach(checkClassDecl)
}