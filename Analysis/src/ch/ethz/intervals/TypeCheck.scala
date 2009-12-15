package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Positional
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

    def freshVarName = ir.VarName(prog.fresh("tmp"))

    // ______________________________________________________________________
    // Environment
    
    var env = ir.TcEnv(
        ir.p_mthd,
        Map(),
        Map(),
        ListSet.empty,
        ir.Relation.emptyTrans,
        ir.Relation.emptyTrans,
        ir.Relation.emptyTrans,
        ir.Relation.emptyTrans,
        ir.Relation.empty
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

    def setCurrent(tp: ir.TeePee): Unit =
        log.indented("setCurrent(%s)", tp) {
            env = ir.TcEnv(
                tp.p,
                env.perm,
                env.temp,
                env.lp_invalidated,
                env.readable,
                env.writable,
                env.hb,
                env.subinterval,
                env.locks)
        }

    /// Add or overwrite a permanent mapping.
    /// Permanent mappings persist across function calls.
    def setPerm(p: ir.Path, tq: ir.TeePee): Unit = 
        log.indented("addPerm(%s,%s)", p, tq) {
            env = ir.TcEnv(
                env.p_cur,
                env.perm + Pair(p, tq),
                env.temp,
                env.lp_invalidated,
                env.readable,
                env.writable,
                env.hb,
                env.subinterval,
                env.locks)
        }
        
    
    /// Add but not overwrite a permanent mapping.
    /// \see setPerm()
    def addPerm(p: ir.Path, tq: ir.TeePee): Unit =
        env.perm.get(p) match {
            case Some(_) => throw new ir.IrError("intervals.shadowed", p)
            case None => setPerm(p, tq)
        }

    // ______________________________________________________________________
    // Errors

    def at[R](loc: Positional, default: R)(g: => R): R = 
        log.indentedRes(loc) {
            try {
                g
            } catch {
                case err: ir.IrError =>
                    prog.reportError(loc, err)
                    log("Error: %s", err)
                    default           
            }            
        }
    
    // ______________________________________________________________________
    // Basic Type Operations: Finding fields, methods, supertypes
        
    /// Creates a substitution to the supertype 'tp' that replaces ghosts 
    /// with their definitions, including the implicit ghost this.constructor
    def superSubst(t: ir.TypeRef) = preservesEnv {
        val ghostPaths = classDecl(t.c).ghosts.map(_.thisPath)
        PathSubst.pp(
            ir.p_ctor  :: ghostPaths,
            ir.p_super :: t.paths
        )     
    }

    /// supertype of t
    def sup(t: ir.TypeRef): Option[ir.TypeRef] = preservesEnv {
        val cd = classDecl(t.c)
        cd.superType.map { case t_1 => superSubst(t).tref(t_1) }
    }

    /// Field decl for t0::f 
    def fieldDecl(c: ir.ClassName, f: ir.FieldName): ir.FieldDecl = preservesEnv {
        log.indentedRes("fieldDecl(%s,%s)", c, f) {
            def search(t: ir.TypeRef): ir.FieldDecl = 
                log.indentedRes("search(%s)", t) {
                    val cd = classDecl(t.c)
                    cd.fields.find(_.name == f) match {
                        case Some(fd) => fd
                        case None => sup(t) match {
                            case Some(t_1) => superSubst(t_1).fieldDecl(search(t_1))
                            case None => throw ir.IrError("intervals.no.such.field", c, f)
                        }
                    }
                }
            val cd = classDecl(c)
            search(cd.thisTref)
        }            
    }

    /// Method sig for c0::m()
    def methodSig(c0: ir.ClassName, m: ir.MethodName): Option[ir.MethodSig] = preservesEnv {
        log.indentedRes("methodSig(%s,%s)", c0, m) {
            def search(c: ir.ClassName): Option[ir.MethodSig] = {
                val cd = classDecl(c)
                cd.methods.find(_.name == m) match {
                    case Some(md) => 
                        Some(md.msig(cd.thisTref))
                    case None => cd.superType match {
                        case Some(t) => search(t.c).map(superSubst(t).methodSig)
                        case None => None
                    }
                }
            }
            search(c0)
        }
    }
    
    /// Returns the (potentially super-)type that defined the method 'm' in the class 'c'
    def typeOriginallyDefiningMethod(c: ir.ClassName, m: ir.MethodName): Option[ir.TypeRef] = {
        val cd = classDecl(c)
        cd.superType.mapToOption(t_sup => 
            typeOriginallyDefiningMethod(t_sup.c, m).map(superSubst(t_sup).tref)
        ).orElse(
            if(cd.methods.exists(_.name == m)) Some(cd.thisTref)
            else None
        )
    }    
    
    /// Returns the signature of 'md' in the supertype, if any.
    def overriddenMethodSig(c: ir.ClassName, m: ir.MethodName): Option[ir.MethodSig] = {
        val cd = classDecl(c)
        cd.superType.mapToOption(t_sup => 
            methodSig(t_sup.c, m).map(superSubst(t_sup).methodSig)
        )
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
        ir.TypeRef(tp.wt.c, ghostPaths(tp), tp.wt.as)
    }
    
    /// Field decl for tp.f
    def substdFieldDecl(tp: ir.TeePee, f: ir.FieldName) = preservesEnv {
        val rfd = fieldDecl(tp.wt.c, f)
        ghostSubst(tp).fieldDecl(rfd)
    }
    
    /// Method sig for tp.m(tqs)
    def substdMethodSig(tp: ir.TeePee, m: ir.MethodName, tqs: List[ir.TeePee]) = preservesEnv {
        methodSig(tp.wt.c, m) match {
            case Some(msig) =>
                val subst = ghostSubst(tp) + PathSubst.vp(msig.args.map(_.name), tqs.map(_.p))
                subst.methodSig(msig)        
            case None =>
                throw ir.IrError("intervals.no.such.method", tp.wt.c, m)
        }
    }
    
    /// Constructs a TeePee for p_1 
    def teePee(p_1: ir.Path): ir.TeePee = log.indentedRes("teePee(%s)", p_1) {
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
                
                def ghostTeePee(gd_f: ir.GhostDecl) = {
                    val wt_f = ghostSubst(tp_0).wtref(gd_f.wt)
                    ir.TeePee(wt_f, p_0 + f, tp_0.as.withGhost)                    
                }

                // First look for a ghost with the name "f", then a real field:
                val cd_0 = classDecl(tp_0.wt.c)
                cd_0.ghosts.zip(tp_0.wt.wpaths).find(_._1.name == f) match {
                    
                    // Ghost with precise path specified:
                    case Some((gd_f, q: ir.Path)) => 
                        if(q != p_0 + f) 
                            // p_0 had a type like Foo<f: q>, so canonical
                            // version of p_0.f is canonical version of q:
                            teePee(q)
                        else
                            // p_0 had a type like Foo<f: p_0.f>, which gives us no information:
                            // It only tells us p_0's shadow argument f is p_0.f.
                            // This can result from the capturing process, when p_0 had
                            // a wildcard path (see below).
                            ghostTeePee(gd_f)
                            
                    // p_0 had a type like Foo<f: ps hb qs>, so canonical
                    // version is p_0.f but we add relations that ps hb p_0.f hb qs.
                    case Some((gd_f, ir.WcHb(ps, qs))) => 
                        val tp_f = ghostTeePee(gd_f)
                        ps.foreach { case p => addHb(teePee(p), tp_f) }
                        qs.foreach { case q => addHb(tp_f, teePee(q)) }
                        tp_f
                    
                    // p_0 had a type like Foo<f: ps hb qs>, so canonical
                    // version is p_0.f but we add relations that ps hb p_0.f hb qs.
                    case Some((gd_f, ir.WcReadableBy(ps))) => 
                        val tp_f = ghostTeePee(gd_f)
                        ps.foreach { p => addDeclaredReadableBy(tp_f, teePee(p)) }
                        tp_f
                    
                    // p_0 had a type like Foo<f: ps hb qs>, so canonical
                    // version is p_0.f but we add relations that ps hb p_0.f hb qs.
                    case Some((gd_f, ir.WcWritableBy(ps))) => 
                        val tp_f = ghostTeePee(gd_f)
                        ps.foreach { p => addDeclaredWritableBy(tp_f, teePee(p)) }
                        tp_f
                    
                    // The path is p_0.constructor, which we handle specially:
                    case None if f == ir.f_ctor =>
                        val tp_f = ir.TeePee(ir.t_interval, p_0 + f, tp_0.as.withGhost)
                        if(!tp_0.wt.as.ctor) // is tp_0 fully constructed?
                            addDeclaredReadableBy(tp_f, tp_cur) // then .constructor is readable
                        tp_f
                        
                    // The path is p_0.super, which we handle specially:
                    case None if f == ir.f_super =>
                        val tp_f = ir.TeePee(ir.t_interval, p_0 + f, tp_0.as.withGhost)
                        addDeclaredReadableBy(tp_f, tp_cur) // x.super is always readable
                        tp_f
                        
                    // The path p_0.f names a real field f:
                    case None =>
                        val rfd_f = substdFieldDecl(tp_0, f)
                        val tp_guard = teePee(rfd_f.p_guard)
                        val as_f = // determine if f is immutable in current method:
                            if(!tp_guard.isConstant)
                                tp_0.as.withMutable // guard not yet constant? mutable
                            else if(!hb(tp_guard, tp_cur))
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
    def tp_cur = teePee(env.p_cur)
    def tp_ctor = teePee(ir.gd_ctor.thisPath)
    def tp_this = teePee(ir.p_this)    
    def tp_super =
        sup(cap(tp_this)) match {
            case None => throw new ir.IrError("intervals.no.supertype", tp_this.wt)
            case Some(t_super) => ir.TeePee(t_super, ir.p_this, tp_this.as)
        }
    
    // ______________________________________________________________________
    // Well-formedness
    //
    // Very basic sanity checks.  Most semantic checking, for example that
    // fields are correctly named, is done when constructing a teePee.
    
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
    
    def addTemp(p: ir.Path, q: ir.Path) =
        log.indented("addTemp(%s,%s)", p, q) {
            env = ir.TcEnv(
                env.p_cur,
                env.perm,
                env.temp + Pair(p, q),
                env.lp_invalidated,
                env.readable,
                env.writable,
                env.hb,
                env.subinterval,
                env.locks)
        }
    
    def clearTemp() =
        log.indented("clearTemp()") {
            env = ir.TcEnv(
                env.p_cur,
                env.perm,
                Map(),
                env.lp_invalidated,
                env.readable,
                env.writable,
                env.hb,
                env.subinterval,
                env.locks)
        }        
    
    def addInvalidated(p: ir.Path) =
        log.indented("addInvalidated(%s)", p) {
            env = ir.TcEnv(
                env.p_cur,
                env.perm,
                env.temp,
                env.lp_invalidated + p,
                env.readable,
                env.writable,
                env.hb,
                env.subinterval,
                env.locks)
        }        
        
    def removeInvalidated(p: ir.Path) =
        log.indented("removeInvalidated(%s)", p) {
            env = ir.TcEnv(
                env.p_cur,
                env.perm,
                env.temp,
                env.lp_invalidated - p,
                env.readable,
                env.writable,
                env.hb,
                env.subinterval,
                env.locks)
        }
        
    def addHb(tp: ir.TeePee, tq: ir.TeePee): Unit = 
        log.indented("addHb(%s,%s)", tp, tq) {
            assert(isSubtype(tp, ir.t_interval))
            assert(isSubtype(tq, ir.t_interval))
            assert(tp.isConstant && tq.isConstant)
            env = ir.TcEnv(
                env.p_cur,
                env.perm,
                env.temp,
                env.lp_invalidated,            
                env.readable,
                env.writable,
                env.hb + (tp.p, tq.p),
                env.subinterval,
                env.locks)
        }
    
    def addDeclaredReadableBy(tp: ir.TeePee, tq: ir.TeePee): Unit = 
        log.indented("addDeclaredReadableBy(%s, %s)", tp, tq) {
            assert(tp.isConstant && tq.isConstant)
            env = ir.TcEnv(
                env.p_cur,
                env.perm,
                env.temp,
                env.lp_invalidated,            
                env.readable + (tp.p, tq.p),
                env.writable,
                env.hb,
                env.subinterval,
                env.locks)
        }
    
    def addDeclaredWritableBy(tp: ir.TeePee, tq: ir.TeePee): Unit = 
        log.indented("addDeclaredWritableBy(%s, %s)", tp, tq) {
            assert(tp.isConstant && tq.isConstant)
            env = ir.TcEnv(
                env.p_cur,
                env.perm,
                env.temp,
                env.lp_invalidated,            
                env.readable,
                env.writable + (tp.p, tq.p),
                env.hb,
                env.subinterval,
                env.locks)
        }
    
    def addSubintervalOf(tp: ir.TeePee, tq: ir.TeePee): Unit = 
        log.indented("addSubintervalOf(%s, %s)", tp, tq) {
            assert(tp.isConstant && tq.isConstant)
            env = ir.TcEnv(
                env.p_cur,
                env.perm,
                env.temp,
                env.lp_invalidated,            
                env.readable,
                env.writable,
                env.hb,
                env.subinterval + (tp.p, tq.p),
                env.locks)
        }
        
    def addLocks(tp: ir.TeePee, tq: ir.TeePee): Unit =
        log.indented("addLocks(%s, %s)", tp, tq) {
            assert(tp.isConstant && tq.isConstant)
            env = ir.TcEnv(
                env.p_cur,
                env.perm,
                env.temp,
                env.lp_invalidated,            
                env.readable,
                env.writable,
                env.hb,
                env.subinterval,
                env.locks)
        }
        
    def equiv(tp: ir.TeePee, tq: ir.TeePee): Boolean = 
        log.indentedRes("%s equiv %s?", tp, tq) {
            tp.p == tq.p
        }
    
    def hb(tp: ir.TeePee, tq: ir.TeePee): Boolean = 
        log.indentedRes("%s hb %s?", tp, tq) {
            env.hb.contains(tp.p, tq.p) || superintervals(tq).exists(env.hb.contains(tp.p, _))
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
        
    def isReadableBy(tp: ir.TeePee, tq: ir.TeePee): Boolean = 
        log.indentedRes("%s isReadableBy %s?", tp, tq) {
            declaredReadableBy(tp, tq) ||
            hb(tp, tq) ||
            isWritableBy(tp, tq)            
        }
    
    def addReq(req: ir.Req) = 
        log.indented("addReq(%s)", req) {
            def teePeeAdd(add: Function2[ir.TeePee, ir.TeePee, Unit], ps: List[ir.Path], qs: List[ir.Path]) = {
                val tps = teePee(ir.ghostAttrs, ps)
                val tqs = teePee(ir.ghostAttrs, qs)
                foreachcross(tps, tqs)(add)
            }
            req match {
                case ir.ReqWritableBy(ps, qs) => teePeeAdd(addDeclaredWritableBy, ps, qs)
                case ir.ReqReadableBy(ps, qs) => teePeeAdd(addDeclaredReadableBy, ps, qs)
                case ir.ReqHb(ps, qs) => teePeeAdd(addHb, ps, qs)
                case ir.ReqSubintervalOf(ps, qs) => teePeeAdd(addSubintervalOf, ps, qs)
            }   
        } 
    
    // ______________________________________________________________________
    // Subtyping    
    
    /// wp <= wq
    def isSubpath(p: ir.Path, wq: ir.WcPath) = 
        log.indentedRes("%s <= %s?", p, wq) {                
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
                case ir.WcReadableBy(lq) =>
                    lq.forall { q => isReadableBy(tp, teePee(q)) }
                case ir.WcWritableBy(lq) =>
                    lq.forall { q => isWritableBy(tp, teePee(q)) }
                case ir.WcLocks(lq) =>
                    lq.forall { q => locks(tp, teePee(q)) }
                case ir.WcLockedBy(lq) =>
                    lq.forall { q => locks(teePee(q), tp) }
            }
        }
    
    /// t_sub <: wt_sup
    def isSubtype(t_sub: ir.TypeRef, wt_sup: ir.WcTypeRef): Boolean = 
        log.indentedRes("%s <: %s?", t_sub, wt_sup) {            
            log("t_sub.ctor=%s wt_sup.as.ctor=%s", t_sub.as.ctor, wt_sup.as.ctor)
            if(t_sub.c == wt_sup.c) {
                (!t_sub.as.ctor || wt_sup.as.ctor) && // (t <: t ctor) but (t ctor not <: t)
                forallzip(t_sub.paths, wt_sup.wpaths)(isSubpath) // c<P> <: c<WP> iff P <= WP
            } else // else walk to supertype of t_sub
                sup(t_sub) match {
                    case None => false
                    case Some(t) => isSubtype(t, wt_sup)
                }            
        }        
    
    def isSubtype(tp_sub: ir.TeePee, wt_sup: ir.WcTypeRef): Boolean = 
        isSubtype(cap(tp_sub), wt_sup)
        
    def freshTp(wt: ir.WcTypeRef) = {
        val lv = freshVarName
        val tp = ir.TeePee(wt, lv.path, ir.ghostAttrs)
        addPerm(lv.path, tp)
        tp
    }
        
    /// wt(tp_sub) <: wt_sup
    def checkIsSubtype(tp_sub: ir.TeePee, wt_sup: ir.WcTypeRef) {
        if(!isSubtype(tp_sub, wt_sup))
            throw new ir.IrError("intervals.expected.subtype", tp_sub.p, tp_sub.wt, wt_sup)
    }
    
    def isReqFulfilled(req: ir.Req): Boolean = log.indentedRes("isReqFulfilled(%s)", req) {
        def is(func: Function2[ir.TeePee, ir.TeePee, Boolean], ps: List[ir.Path], qs: List[ir.Path]) = {
            val tps = teePee(ir.ghostAttrs, ps)
            val tqs = teePee(ir.ghostAttrs, qs)
            forallcross(tps, tqs)(func)
        }
        req match {
            case ir.ReqWritableBy(ps, qs) => is(isWritableBy, ps, qs)
            case ir.ReqReadableBy(ps, qs) => is(isReadableBy, ps, qs)
            case ir.ReqSubintervalOf(ps, qs) => is(isSubintervalOf, ps, qs)
            case ir.ReqHb(ps, qs) => is(hb, ps, qs)
        }
    }
    
    def checkReqFulfilled(req: ir.Req) {
        if(!isReqFulfilled(req))
            throw new ir.IrError("intervals.requirement.not.met", req)
    }
    
    def checkLengths(l1: List[_], l2: List[_], msg: String) = preservesEnv {
        if(l1.length != l2.length)
            throw new ir.IrError(msg, l1.length, l2.length)
    }
    
    def checkArgumentTypes(msig: ir.MethodSig, tqs: List[ir.TeePee]) = {
        checkLengths(msig.args, tqs, "intervals.wrong.number.method.arguments")
        foreachzip(tqs, msig.args.map(_.wt))(checkIsSubtype)
    }
    
    def checkLvDecl(vd: ir.LvDecl, owt: Option[ir.WcTypeRef], op_canon: Option[ir.Path]) = {
        
        checkWfWt(vd.wt)
        
        owt.foreach (savingEnv { case wt =>
            checkIsSubtype(freshTp(wt), vd.wt)
        })

        addPerm(vd.name.path, ir.TeePee(
            vd.wt, 
            op_canon.getOrElse(vd.name.path), // default canonical path for x is just x
            ir.noAttrs))                      // all local vars are (a) reified and (b) immutable
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
            !hb(tp_guard, teePee(subst.path(rfd.p_guard)))
        }
        
        // map to the canonical path for the field
        fds_linked.map { fd => subst.path(fd.thisPath) }
    }
    
    def checkReadable(tp_guard: ir.TeePee) {
        if(!isReadableBy(tp_guard, tp_cur))
            throw new ir.IrError("intervals.not.readable", tp_guard.p)
    }
    
    def checkWritable(tp_guard: ir.TeePee) {
        if(!isWritableBy(tp_guard, tp_cur))
            throw new ir.IrError("intervals.not.writable", tp_guard.p)
    }
        
    def checkNoInvalidated() {
        if(!env.lp_invalidated.isEmpty)
            throw new ir.IrError(
                "intervals.must.assign.first", 
                env.lp_invalidated.mkEnglishString)        
    }

    def checkCallMsig(tp: ir.TeePee, msig: ir.MethodSig, tqs: List[ir.TeePee]) {
        // Cannot invoke a method when there are outstanding invalidated fields:
        checkNoInvalidated()
        
        // If method is not a constructor method, receiver must be constructed:
        if(!msig.as.ctor && tp.wt.as.ctor) 
            throw new ir.IrError("intervals.rcvr.must.be.constructed", tp.p)
            
        // Arguments must have correct type and requirements must be fulfilled:
        checkArgumentTypes(msig, tqs)
        msig.reqs.foreach(checkReqFulfilled)
        
        // Any method call disrupts potential temporary assocations:
        //     We make these disruptions before checking return value, 
        //     in case they would affect the type.  Haven't thought through
        //     if this can happen or not, but this would be the right time anyhow.
        clearTemp()        
    }
    
    def checkCall(vd: ir.LvDecl, tp: ir.TeePee, m: ir.MethodName, tqs: List[ir.TeePee]) {
        val msig = substdMethodSig(tp, m, tqs)
        checkCallMsig(tp, msig, tqs)
        checkLvDecl(vd, Some(msig.wt_ret), None)                        
    }
    
    def checkStatement(stmt: ir.Stmt) = 
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
                        if(hb(tp_guard, tp_cur)) { // Constant:
                            Some(p_f)
                        } else { // Non-constant:
                            checkReadable(tp_guard)
                            addTemp(p_f, vd.name.path) // Record that p.f == vd, for now.
                            None
                        }
            
                    checkLvDecl(vd, Some(fd.wt), op_canon)
                    
                case ir.StmtSetField(p_o, f, p_v) =>
                    val tp_o = teePee(ir.noAttrs, p_o)
                    val tp_v = teePee(ir.noAttrs, p_v)
                    val fd = substdFieldDecl(tp_o, f)
                    
                    val tp_guard = teePee(ir.ghostAttrs, fd.p_guard)
                    checkWritable(tp_guard)
                    
                    checkIsSubtype(tp_v, fd.wt)
                    
                    val p_f = tp_o.p + f // Canonical path of f; valid as f is not a ghost.
                    addTemp(p_f, tp_v.p)
                    
                    removeInvalidated(p_f)
                    linkedPaths(tp_o, f, tp_guard).foreach(addInvalidated)
                        
                case ir.StmtCall(vd, p, m, qs) =>
                    val tp = teePee(ir.noAttrs, p)
                    val tqs = teePee(ir.noAttrs, qs)
                    checkCall(vd, tp, m, tqs)
        
                case ir.StmtSuperCall(vd, m, qs) =>
                    val tqs = teePee(ir.noAttrs, qs)
                    checkCall(vd, tp_super, m, tqs)
                
                case ir.StmtSuperCtor(qs) =>
                    val tp = tp_super
                    val tqs = teePee(ir.noAttrs, qs)
                    val msig_ctor0 = classDecl(tp.wt.c).ctor.msig(cap(tp))
                    val msig_ctor = ghostSubst(tp).methodSig(msig_ctor0)
                    checkCallMsig(tp_super, msig_ctor, tqs)
                
                case ir.StmtNew(x, t, qs) =>
                    checkWfWt(t)
                    val cd = classDecl(t.c)
                    val tqs = teePee(ir.noAttrs, qs)
                    
                    // Check Ghost Types:
                    checkLengths(cd.ghosts, t.paths, "intervals.wrong.number.ghost.arguments")
                    val substGhosts = PathSubst.pp(
                        ir.p_this   :: cd.ghosts.map(_.thisPath), 
                        x.path      :: t.paths)
                    foreachzip(cd.ghosts, t.paths) { case (gfd, p) =>
                        val tp = teePee(p)
                        checkIsSubtype(tp, substGhosts.wtref(gfd.wt))
                    }
                    
                    checkLvDecl(ir.LvDecl(x, t), Some(t), None)

                    val subst = substGhosts + PathSubst.vp(cd.ctor.args.map(_.name), tqs.map(_.p))
                    val msig = subst.methodSig(cd.ctor.msig(t))
                    checkArgumentTypes(msig, tqs)
                    msig.reqs.foreach(checkReqFulfilled)
                    clearTemp()                    
                    
                    
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
    
    def isSuperCtor(s: ir.Stmt) = s match {
        case ir.StmtSuperCtor(_) => true
        case _ => false
    }
    
    def checkArgumentTypesNonvariant(args_sub: List[ir.LvDecl], args_sup: List[ir.LvDecl]) {
        foreachzip(args_sub, args_sup) { case (arg_sub, arg_sup) =>
            if(arg_sub.wt != arg_sup.wt)
                throw new ir.IrError(
                    "intervals.override.param.type.changed", 
                    arg_sub.name, arg_sub.wt, arg_sup.wt)
        }        
    }
    
    def checkReturnTypeCovariant(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef) {
        if(!isSubtype(freshTp(wt_sub), wt_sup))
            throw new ir.IrError(
                "intervals.override.ret.type.changed", wt_sub, wt_sup)
    }
    
    def checkOverridenReqsImplyOurReqs(reqs_sub: List[ir.Req], reqs_sup: List[ir.Req]) {
        reqs_sup.foreach(addReq)
        reqs_sub.foreach { req_sub =>
            if(!isReqFulfilled(req_sub))
                throw new ir.IrError("intervals.override.adds.req", req_sub)
        }        
    }
    
    /// Changes the names of the arguments in 'msig' to 'lvn'.
    def substArgsInMethodSig(msig: ir.MethodSig, lvn: List[ir.VarName]) = {
        val lvn_msig = msig.args.map(_.name)
        val nameMap = Map(lvn_msig.zip(lvn): _*)
        val subst = PathSubst.vv(lvn_msig, lvn)
        ir.MethodSig(
            subst.tref(msig.t_rcvr),
            msig.as,
            msig.args.map { lv => ir.LvDecl(nameMap(lv.name), subst.wtref(lv.wt)) },
            msig.reqs.map(subst.req),
            subst.wtref(msig.wt_ret)
        )                
    }
    
    def checkMethodDecl(cd: ir.ClassDecl, md: ir.MethodDecl) = 
        at(md, ()) {
            savingEnv {
                // Define special vars "method" and "this":
                addPerm(ir.p_mthd, ir.TeePee(ir.t_interval, ir.p_mthd, ir.ghostAttrs))
                if(!md.attrs.ctor) { 
                    // For normal methods, type of this is the defining class
                    addPerm(ir.p_this, ir.TeePee(cd.thisTref, ir.p_this, ir.noAttrs))                     
                    addHb(tp_ctor, tp_cur)                    
                } else {
                    // For constructor methods, type of this is the type that originally defined it
                    val t_rcvr = typeOriginallyDefiningMethod(cd.name, md.name).get
                    addPerm(ir.p_this, ir.TeePee(t_rcvr.ctor, ir.p_this, ir.noAttrs))
                }  
                
                md.args.foreach { case arg => 
                    checkWfWt(arg.wt) 
                    addPerm(arg.name.path, ir.TeePee(arg.wt, arg.name.path, ir.noAttrs))
                }
                
                savingEnv {
                    overriddenMethodSig(cd.name, md.name) foreach { msig_sup_0 => 
                        val msig_sup = substArgsInMethodSig(msig_sup_0, md.args.map(_.name))
                        checkArgumentTypesNonvariant(md.args, msig_sup.args)
                        checkReturnTypeCovariant(md.wt_ret, msig_sup.wt_ret)
                        checkOverridenReqsImplyOurReqs(md.reqs, msig_sup.reqs)
                    }                    
                }
                
                md.reqs.foreach(addReq)
                
                if(md.stmts.exists(isSuperCtor)) // super(...) only permitted in constructors
                    throw new ir.IrError("intervals.super.ctor.in.mthd")
                    
                md.stmts.foreach(checkStatement)
                
                md.op_ret.foreach { p_ret =>
                    val tp_ret = teePee(ir.noAttrs, p_ret)
                    checkIsSubtype(tp_ret, md.wt_ret)
                }
            }         
        }
    
    def checkConstructorDecl(cd: ir.ClassDecl, md: ir.MethodDecl) = 
        at(md, ()) {
            savingEnv {
                // Define special vars "method" (== this.constructor) and "this":
                addPerm(ir.p_mthd, ir.TeePee(ir.t_interval, ir.gd_ctor.thisPath, ir.ghostAttrs))
                addPerm(ir.p_this, ir.TeePee(cd.thisTref(ir.ctorAttrs), ir.p_this, ir.ghostAttrs))

                md.args.foreach { case arg => 
                    checkWfWt(arg.wt) 
                    addPerm(arg.name.path, ir.TeePee(arg.wt, arg.name.path, ir.noAttrs))
                }            
                md.reqs.foreach(addReq)

                val preSuper = md.stmts.takeWhile(!isSuperCtor(_))
                val postSuper = md.stmts.dropWhile(!isSuperCtor(_))
                if(postSuper.isEmpty)
                    throw new ir.IrError("intervals.super.ctor.zero")
                if(postSuper.tail.exists(isSuperCtor))
                    throw new ir.IrError("intervals.super.ctor.twice")

                // Check statements up until the super() invocation with ghost 'this':
                preSuper.foreach(checkStatement)
                checkStatement(postSuper.head)

                // After invoking super, 'this' becomes reified:
                setPerm(ir.p_this, ir.TeePee(cd.thisTref(ir.ctorAttrs), ir.p_this, ir.noAttrs))
                postSuper.tail.foreach(checkStatement)  

                md.op_ret.foreach { _ => throw new ir.IrError("intervals.ctor.with.ret") }                
            }          
        }
    
    def checkFieldDecl(cd: ir.ClassDecl, fd: ir.FieldDecl) = 
        at(fd, ()) {
            savingEnv {
                // Rules:
                //
                // The type of a field f in class c may be dependent on a path p_dep if either:
                // (1) p_dep is constant when p_g is active; or
                // (2) p_dep = this.f' and f' is declared in class c (not a supertype!)
                //
                // Note that a type is dependent on p_dep if p.F appears in the type, so 
                // we must check all prefixes of each dependent path as well.
                addPerm(ir.p_this, ir.TeePee(cd.thisTref, ir.p_this, ir.noAttrs))                     
        
                fd.wt.dependentPaths.foreach { p_full_dep => 
                    savingEnv {
                        // XXX Really need to implement the full logic here.
                        if(fd.as.ctor)
                            // Field accessible from constructor.
                            addPerm(ir.p_mthd, tp_ctor)
                        else {
                            // Field inaccessible from constructor.
                            addPerm(ir.p_mthd, ir.TeePee(ir.t_interval, ir.p_mthd, ir.ghostAttrs))
                            addHb(tp_ctor, tp_cur)
                        }
                            
                        val tp_guard = teePee(fd.p_guard)
                        setCurrent(tp_guard) // use guard as the current interval
                        
                        // Guards must be of type Interval, Lock, or Object:
                        if(!isSubtype(tp_guard, ir.t_interval) &&
                            !isSubtype(tp_guard, ir.t_lock) &&
                            tp_guard.wt.c != ir.c_object)
                            throw new ir.IrError("intervals.invalid.guard", tp_guard.p)                        
                        
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
                                    check(ir.Path(lv, rev_fs))
                                    val tp_dep = teePee(p_dep)
                                    if(!tp_dep.isConstant)
                                        throw new ir.IrError(
                                            "intervals.illegal.type.dep",
                                            tp_dep.p, tp_guard.p)
                            }
                        }                        
                        check(p_full_dep)
                    }                   
                }
            }
        }
    
    // ______________________________________________________________________
    // Permitted Prefixes
    //
    // This code prevents cyclic references between ghosts and fields by
    // requiring that ghosts and fields can only refer to entries that
    // appear textually earlier in the class definition.  Actually, I'm not
    // sure if this check is strictly needed: I thought it was needed
    // to prevent teePee() from looping as it follows one path to the other,
    // but now I think that won't happen in any case.
    //
    // XXX Kill this code?  Probably a good reason for it.    
    
    def checkUsePermittedPrefix(
        perm_prefixes: List[ir.Path],
        p: ir.Path
    ) {
        val perm_prefixes1 = ir.p_ctor :: perm_prefixes
        if(p != ir.p_this && !perm_prefixes1.exists(p.hasPrefix(_)))
            throw new ir.IrError("intervals.illegal.path", p)
    }

    def checkFieldDeclsUsePermittedPrefixes(
        cd: ir.ClassDecl, 
        fds_prev: List[ir.FieldDecl],
        fds_next: List[ir.FieldDecl]
    ): Unit = preservesEnv {
        fds_next match {
            case List() => 
            case fd :: fds_remaining =>
                at(fd, ()) {
                    checkWfWt(fd.wt)
            
                    val perm_prefixes = cd.ghosts.map(_.thisPath) ++ fds_prev.map(_.thisPath)
                    checkUsePermittedPrefix(perm_prefixes, fd.p_guard)
                    fd.wt.dependentPaths.foreach(checkUsePermittedPrefix(perm_prefixes, _))
                    
                    checkFieldDeclsUsePermittedPrefixes(cd, fd :: fds_prev, fds_remaining)
                }
        }
    }
    
    def checkGhostDeclsUsePermittedPrefixes(
        cd: ir.ClassDecl, 
        prev: List[ir.GhostDecl], 
        next: List[ir.GhostDecl]
    ): Unit = preservesEnv {
        next match {
            case List() =>
            case gd :: gds_remaining =>
                at(gd, ()) {
                    prev.find(_.name == gd.name) match {
                        case Some(_) => throw new ir.IrError("intervals.duplicate.ghost", gd.name)
                        case None =>
                    }
                    
                    checkWfWt(gd.wt)
                    
                    val perm_prefixes = prev.map(_.thisPath)
                    gd.wt.dependentPaths.foreach(checkUsePermittedPrefix(perm_prefixes, _))
                    
                    checkGhostDeclsUsePermittedPrefixes(cd, gd :: prev, gds_remaining)
                }
        }
    }
    
    def checkClassDecl(cd: ir.ClassDecl) = 
        at(cd, ()) {
            savingEnv {
                cd.superType.foreach { t =>
                    if(t.as.ctor) throw new ir.IrError("intervals.superType.with.ctor.attr") 
                }
                checkGhostDeclsUsePermittedPrefixes(cd, List(), cd.ghosts)                
                checkFieldDeclsUsePermittedPrefixes(cd, List(), cd.fields)                
                
                // XXX Need to extract visible effects of ctor
                // XXX and save them in environment for other
                // XXX methods.
                checkConstructorDecl(cd, cd.ctor)                    
                cd.methods.foreach(checkMethodDecl(cd, _))                    
            }
        }
    
    def check = prog.cds_user.foreach(checkClassDecl)
}