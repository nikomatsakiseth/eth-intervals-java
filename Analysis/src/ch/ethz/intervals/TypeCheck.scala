package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Positional
import Util._

class TypeCheck(log: Log, prog: Prog) {    
    import prog.classDecl

    def freshVarName = ir.VarName(prog.fresh("tmp"))

    // ______________________________________________________________________
    // Environment
    
    val emptyEnv = ir.TcEnv(
        ir.p_mthd,
        ir.t_void,
        Map(),
        Map(),
        ListSet.empty,
        IntransitiveRelation.empty,
        IntransitiveRelation.empty,
        TransitiveRelation.empty,
        TransitiveRelation.empty,
        IntransitiveRelation.empty
    )
    var env = emptyEnv
    
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
    
    def withCurrent[R](tp: ir.TeePee)(g: => R): R = {
        val p_old = env.p_cur
        try {
            log.indented("withCurrent(%s)", tp) {
                env = ir.TcEnv(
                    tp.p,
                    env.wt_ret,
                    env.perm,
                    env.temp,
                    env.lp_invalidated,
                    env.readable,
                    env.writable,
                    env.hb,
                    env.subinterval,
                    env.locks
                )
                g
            }            
        } finally {
            env = ir.TcEnv(
                p_old,
                env.wt_ret,
                env.perm,
                env.temp,
                env.lp_invalidated,
                env.readable,
                env.writable,
                env.hb,
                env.subinterval,
                env.locks
            )            
        }
    }
    
    def setWtRet(wt_ret: ir.WcTypeRef) = {
        env = ir.TcEnv(
            env.p_cur,
            wt_ret,
            env.perm,
            env.temp,
            env.lp_invalidated,
            env.readable,
            env.writable,
            env.hb,
            env.subinterval,
            env.locks            
        )
    }

    /// Add or overwrite a permanent mapping.
    /// Permanent mappings persist across function calls.
    def setPerm(p: ir.Path, tq: ir.TeePee): Unit = 
        log.indented("addPerm(%s,%s)", p, tq) {
            env = ir.TcEnv(
                env.p_cur,
                env.wt_ret,
                env.perm + Pair(p, tq),
                env.temp,
                env.lp_invalidated,
                env.readable,
                env.writable,
                env.hb,
                env.subinterval,
                env.locks
            )
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
    
    // Is wt an erased subtype of class c?
    def isSubclass(wt: ir.WcTypeRef, c: ir.ClassName): Boolean = {
        (wt.c == c) || {            
            val cd = classDecl(wt.c)
            cd.superTypes.exists { t => isSubclass(t, c) }
        }
    }
    
    def strictSuperclasses(c0: ir.ClassName): Set[ir.ClassName] = {
        def accumulate(sc: Set[ir.ClassName], c: ir.ClassName): Set[ir.ClassName] = {
            classDecl(c).superTypes.map(_.c).foldLeft(sc + c)(accumulate)
        }
        accumulate(Set.empty, c0) - c0
    }
    
    // Collection of all ghost fields declared on type 'c_0'.
    // A ghost field from a supertype c_1 is included so long as there is path
    // from c_0 to c_1 that does not define the value of the ghost.
    def ghostFieldDecls(c_0: ir.ClassName): List[ir.GhostFieldDecl] = preservesEnv {
        def lgfd(c: ir.ClassName, suppress: Set[ir.FieldName]): List[ir.GhostFieldDecl] = {
            log.indentedRes("lgfd(%s)", c) {
                val cd = classDecl(c)
                val lgfd_super = cd.superTypes.flatMap { t =>
                    lgfd(t.c, suppress ++ t.ghosts.map(_.f))
                }            
                cd.fields.foldLeft(lgfd_super) {
                    case (l, gfd: ir.GhostFieldDecl) if !suppress(gfd.name) => gfd :: l
                    case (l, _) => l
                }                
            }
        }
        lgfd(c_0, ListSet.empty)
    }
    
    /// For a type 't=c<F: P>' creates a substitution '[this.F→P]'.
    /// Ghosts F'#F defined on the class c are not substituted.
    /// @see ghostSubstOfTeePee()
    def ghostSubstOfType(t: ir.TypeRef): PathSubst = 
        PathSubst.pp(
            t.ghosts.map(g => ir.p_this + g.f),
            t.ghosts.map(g => g.p)
        )        
    
    /// Augments ghostSubstOfType with a mapping 'this.ctor→this.super'
    def superSubstOfType(t: ir.TypeRef) =
        PathSubst.pp(ir.p_ctor, ir.p_super) + ghostSubstOfType(t)

    /// For a class c with ghost fields F, yields a type c<F: this.F>{as}
    def thisTref(cd: ir.ClassDecl, as: ir.Attrs): ir.TypeRef = {
        val lgfds = ghostFieldDecls(cd.name)
        ir.TypeRef(cd.name, lgfds.map(_.ghost), as)
    }    

    /// For a class c with ghost fields F, yields a type c<F: this.F>
    def thisTref(cd: ir.ClassDecl): ir.TypeRef =
        thisTref(cd, ir.noAttrs)
    
    /// supertypes of t
    def sups(t: ir.TypeRef): List[ir.TypeRef] = preservesEnv {
        val cd = classDecl(t.c)
        lazy val subst_t = ghostSubstOfType(t)        
        cd.superTypes.map { case t_1 =>
            val t_extends = ghostSubstOfType(t_1).tref(thisTref(classDecl(t_1.c)))
            subst_t.tref(t_extends)
        }
    }
    
    /// Field decl for t0::f 
    def fieldDecl(c0: ir.ClassName, f: ir.FieldName): ir.FieldDecl = preservesEnv {
        log.indentedRes("fieldDecl(%s,%s)", c0, f) {
            def search(c: ir.ClassName): Option[ir.FieldDecl] = 
                log.indentedRes("search(%s)", c) {
                    val cd = classDecl(c)
                    cd.fields.find(_.name == f) match {
                        case Some(fd) => Some(fd)
                        case None => cd.superTypes.firstSomeReturned { t =>
                            search(t.c).map(superSubstOfType(t).fieldDecl)
                        }
                    }
                }
            search(c0) match {
                case None => throw ir.IrError("intervals.no.such.field", c0, f)
                case Some(fd) => fd
            }
        }            
    }

    /// Method sig for c0::m()
    def methodSig(c0: ir.ClassName, m: ir.MethodName): Option[ir.MethodSig] = preservesEnv {
        log.indentedRes("methodSig(%s,%s)", c0, m) {
            def search(c: ir.ClassName): Option[ir.MethodSig] = {
                val cd = classDecl(c)
                cd.methods.find(_.name == m) match {
                    case Some(md) => 
                        Some(md.msig(thisTref(cd)))
                    case None => cd.superTypes.firstSomeReturned { t =>
                        search(t.c).map(superSubstOfType(t).methodSig)
                    }
                }
            }
            search(c0)
        }
    }
    
    /// Returns the (potentially super-)type that defined the method 'm' in the class 'c'
    def typeOriginallyDefiningMethod(c: ir.ClassName, m: ir.MethodName): Option[ir.TypeRef] = {
        val cd = classDecl(c)
        cd.superTypes.firstSomeReturned(t_sup => 
            typeOriginallyDefiningMethod(t_sup.c, m).map(superSubstOfType(t_sup).tref)
        ).orElse(
            if(cd.methods.exists(_.name == m)) Some(thisTref(cd))
            else None
        )
    }    
    
    /// Returns the signatures for any methods 'm' defined in the supertypes of 'c'.
    def overriddenMethodSigs(c: ir.ClassName, m: ir.MethodName): List[ir.MethodSig] = {
        val cd = classDecl(c)
        cd.superTypes.foldRight(List[ir.MethodSig]()) { case (t, l) =>
            methodSig(t.c, m).map(superSubstOfType(t).methodSig) match {
                case None => l
                case Some(msig) => msig :: l
            }
        }
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
        ghostFieldDecls(tp.wt.c).map { gfd =>
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
        val rfd = fieldDecl(tp.wt.c, f)
        ghostSubstOfTeePee(tp).fieldDecl(rfd)
    }
    
    /// Method sig for tp.m(tqs)
    def substdMethodSig(tp: ir.TeePee, m: ir.MethodName, tqs: List[ir.TeePee]) = preservesEnv {
        methodSig(tp.wt.c, m) match {
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
                 ps.foreach { case p => checkAndAddHb(teePee(p), tp_f) }
                 qs.foreach { case q => checkAndAddHb(tp_f, teePee(q)) }
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
            else if(!hbInter(tp_guard, tp_cur))
                tp_0.as.withMutable // not guarded by closed interval? mutable
            else
                tp_0.as // p_0 mutable? mutable
        ir.TeePee(wt_f, tp_0.p + f, as_f)        
    }
    
    /// Constructs a TeePee for p_1 
    def teePee(p_1: ir.Path): ir.TeePee = log.indentedRes("teePee(%s)", p_1) {
        if(env.perm.contains(p_1))
            env.perm(p_1)
        else if(env.temp.contains(p_1))
            teePee(env.temp(p_1))
        else 
            p_1 match {
                case ir.Path(lv, List()) => // all local variables should be in env.perm
                    throw new ir.IrError("intervals.no.such.variable", lv)
                
                case ir.Path(lv, f :: rev_fs) =>
                    val tp_0 = teePee(ir.Path(lv, rev_fs))
                
                    if(f == ir.f_ctor) {
                        // The path is p_0.ctor, which we handle specially:
                        val tp_f = ir.TeePee(ir.t_interval, tp_0.p + f, tp_0.as.withGhost)
                        if(!tp_0.wt.as.ctor) // is tp_0 fully constructed?
                            checkAndAddHb(tp_f, tp_cur) // then .ctor happened before (now constant)
                        tp_f                    
                    } else if (f == ir.f_super) {
                        // The path is p_0.super, which we handle specially:
                        val tp_f = ir.TeePee(ir.t_interval, tp_0.p + f, tp_0.as.withGhost)
                        checkAndAddHb(tp_f, tp_cur) // .super always happened before (now constant)
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
    def tp_cur = teePee(env.p_cur)
    def tp_cur_start = teePee(env.p_cur.start)
    def tp_cur_end = teePee(env.p_cur.end)
    def tp_ctor = teePee(ir.gfd_ctor.thisPath)
    def tp_ctor_start = teePee(ir.gfd_ctor.thisPath.start)
    def tp_ctor_end = teePee(ir.gfd_ctor.thisPath.end)
    def tp_this = teePee(ir.p_this)    
    def tp_super = // tp_super always refers to the FIRST supertype
        sups(cap(tp_this)) match {
            case List() => throw new ir.IrError("intervals.no.supertype", tp_this.wt)
            case t_super :: _ => ir.TeePee(t_super, ir.p_this, tp_this.as)
        }
    
    // ______________________________________________________________________
    // Well-formedness
    //
    // Very basic sanity checks.  Most semantic checking, for example that
    // fields are correctly named, is done when constructing a teePee.
    
    def checkWfWt(wt: ir.WcTypeRef) = preservesEnv {
        wt.wghosts.foldLeft(List[ir.FieldName]()) {
            case (l, wg) if l.contains(wg.f) => throw new ir.IrError("intervals.duplicate.ghost", wg.f)
            case (l, wg) => wg.f :: l
        }
        
        val lgfd = ghostFieldDecls(wt.c)
        def notDefined(wg: ir.WcGhost) = !lgfd.exists(_.name == wg.f)        
        wt.wghosts.find(notDefined) match {
            case Some(wg) => throw new ir.IrError("intervals.no.such.ghost", wt.c, wg.f)
            case None =>
        }
                
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
                env.wt_ret,
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
                env.wt_ret,
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
                env.wt_ret,
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
                env.wt_ret,
                env.perm,
                env.temp,
                env.lp_invalidated - p,
                env.readable,
                env.writable,
                env.hb,
                env.subinterval,
                env.locks)
        }
        
    def addHbPnt(tp: ir.TeePee, tq: ir.TeePee): Unit = 
        log.indented("addHb(%s,%s)", tp, tq) {
            assert(tp.isConstant && tq.isConstant)
            assert(isSubclass(tp.wt, ir.c_point))
            assert(isSubclass(tq.wt, ir.c_point))
            env = ir.TcEnv(
                env.p_cur,
                env.wt_ret,
                env.perm,
                env.temp,
                env.lp_invalidated,            
                env.readable,
                env.writable,
                env.hb + (tp.p, tq.p),
                env.subinterval,
                env.locks)
        }
        
    def addHbInter(tp: ir.TeePee, tq: ir.TeePee): Unit = 
        log.indented("addHb(%s,%s)", tp, tq) {
            assert(tp.isConstant && tq.isConstant)
            assert(isSubclass(tp.wt, ir.c_interval))
            assert(isSubclass(tq.wt, ir.c_interval))
            env = ir.TcEnv(
                env.p_cur,
                env.wt_ret,
                env.perm,
                env.temp,
                env.lp_invalidated,
                env.readable,
                env.writable,
                env.hb + (tp.p.end, tq.p.start),
                env.subinterval,
                env.locks)
        }
                
    def addDeclaredReadableBy(tp: ir.TeePee, tq: ir.TeePee): Unit = 
        log.indented("addDeclaredReadableBy(%s, %s)", tp, tq) {
            assert(tp.isConstant && tq.isConstant)
            assert(isSubclass(tp.wt, ir.c_guard))
            assert(isSubclass(tq.wt, ir.c_interval))
            env = ir.TcEnv(
                env.p_cur,
                env.wt_ret,
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
            // This assertion is not valid during checkReifiedFieldDecl():
            //assert(tp.isConstant && tq.isConstant)
            assert(isSubclass(tp.wt, ir.c_guard))
            assert(isSubclass(tq.wt, ir.c_interval))
            env = ir.TcEnv(
                env.p_cur,
                env.wt_ret,
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
            // This assertion is not valid during checkReifiedFieldDecl():
            //assert(tp.isConstant && tq.isConstant)
            assert(isSubclass(tp.wt, ir.c_interval))
            assert(isSubclass(tq.wt, ir.c_interval))
            env = ir.TcEnv(
                env.p_cur,
                env.wt_ret,
                env.perm,
                env.temp,
                env.lp_invalidated,            
                env.readable,
                env.writable,
                env.hb + (tq.p.start, tp.p.start) + (tp.p.end, tq.p.end),
                env.subinterval + (tp.p, tq.p),
                env.locks)
        }
        
    def addLocks(tp: ir.TeePee, tq: ir.TeePee): Unit =
        log.indented("addLocks(%s, %s)", tp, tq) {
            // This assertion is not valid during checkReifiedFieldDecl:
            //assert(tp.isConstant && tq.isConstant)
            assert(isSubclass(tp.wt, ir.c_interval))
            assert(isSubclass(tq.wt, ir.c_lock))
            env = ir.TcEnv(
                env.p_cur,
                env.wt_ret,
                env.perm,
                env.temp,
                env.lp_invalidated,            
                env.readable,
                env.writable,
                env.hb,
                env.subinterval,
                env.locks + (tp.p, tq.p))
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
        
    def isReadableBy(tp: ir.TeePee, tq: ir.TeePee): Boolean = 
        log.indentedRes("%s isReadableBy %s?", tp, tq) {
            declaredReadableBy(tp, tq) ||
            hbInter(tp, tq) ||
            isWritableBy(tp, tq)            
        }
    
    def checkedHbPair(tp: ir.TeePee, tq: ir.TeePee): (ir.TeePee, ir.TeePee) =
        log.indentedRes("checkedHbPair(%s,%s)", tp, tq) {
            (
                if(tp.wt.c == ir.c_point) tp
                else {
                    checkIsSubtype(tp, ir.t_interval)
                    ir.TeePee(ir.t_point, tp.p.end, tp.as) // Assumes that field .end exists
                },
                if(tq.wt.c == ir.c_point) tq
                else {
                    checkIsSubtype(tq, ir.t_interval)
                    ir.TeePee(ir.t_point, tq.p.start, tq.as) // Assumes that field .start exists
                }
            )
        }
        
    def checkedHb(tp: ir.TeePee, tq: ir.TeePee) = 
        log.indentedRes("checkedHb(%s,%s)", tp, tq) {
            val (tp1, tq1) = checkedHbPair(tp, tq)
            hbPnt(tp1, tq1)
        }
    
    def checkAndAddHb(tp: ir.TeePee, tq: ir.TeePee) = 
        log.indented("checkAndAddHb(%s,%s)", tp, tq) {
            val (tp1, tq1) = checkedHbPair(tp, tq)
            addHbPnt(tp1, tq1)
        }
    
    def checkAndAddReq(req: ir.Req) = 
        at(req, ()) {
            log.indented("checkAndAddReq(%s)", req) {
                def teePeeAdd(add: Function2[ir.TeePee, ir.TeePee, Unit], ps: List[ir.Path], qs: List[ir.Path]) = {
                    val tps = teePee(ir.ghostAttrs, ps)
                    val tqs = teePee(ir.ghostAttrs, qs)
                    foreachcross(tps, tqs)(add)
                }
                req match {
                    case ir.ReqWritableBy(ps, qs) => teePeeAdd(addDeclaredWritableBy, ps, qs)
                    case ir.ReqReadableBy(ps, qs) => teePeeAdd(addDeclaredReadableBy, ps, qs)
                    case ir.ReqHb(ps, qs) => teePeeAdd(checkAndAddHb, ps, qs)
                    case ir.ReqSubintervalOf(ps, qs) => teePeeAdd(addSubintervalOf, ps, qs)
                }   
            }             
        }
    
    // ______________________________________________________________________
    // Subtyping    
    
    /// wp <= wq
    def isSubpath(p: ir.Path, wq: ir.WcPath) = savingEnv {
        log.indentedRes("%s <= %s?", p, wq) {                
            // Here we just use teePee() without checking that the resulting
            // paths are immutable.  That's safe because we never ADD to the 
            // relations unless the teePee is immutable. 
            //
            // XXX Is this REALLY true...?
            val tp = teePee(p)
            wq match {
                case q: ir.Path =>
                    equiv(tp, teePee(q))
                case ir.WcHb(lq_1, lq_2) =>
                    lq_1.forall { q_1 => checkedHb(teePee(q_1), tp) } &&
                    lq_2.forall { q_2 => checkedHb(tp, teePee(q_2)) }
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
    }
    
    /// ∃ g∈t.ghosts . (g.f == f, g.p <= wp)?
    def isSubghost(t_sub: ir.TypeRef, wt_ghost: ir.WcGhost) = preservesEnv {
        t_sub.oghost(wt_ghost.f) match {
            case Some(p) => isSubpath(p, wt_ghost.wp)
            case None => false
        }
    }
    
    /// t_sub <: wt_sup
    def isSubtype(t_sub: ir.TypeRef, wt_sup: ir.WcTypeRef): Boolean = preservesEnv {
        log.indentedRes("%s <: %s?", t_sub, wt_sup) {            
            log("t_sub.ctor=%s wt_sup.as.ctor=%s", t_sub.as.ctor, wt_sup.as.ctor)
            if(t_sub.c == wt_sup.c) {
                (!t_sub.as.ctor || wt_sup.as.ctor) && // (t <: t ctor) but (t ctor not <: t)
                wt_sup.wghosts.forall(isSubghost(t_sub, _)) // c<F: P> <: c<F: WP> iff F:P <= F:WP
            } else // else walk to supertype(s) of t_sub
                sups(t_sub).exists(isSubtype(_, wt_sup))
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
            case ir.ReqHb(ps, qs) => is(checkedHb, ps, qs)
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
    
    def checkLvDecl(x: ir.VarName, wt: ir.WcTypeRef, op_canon: Option[ir.Path]) = {        
        addPerm(x.path, ir.TeePee(
            wt,
            op_canon.getOrElse(x.path), // default canonical path for x is just x
            ir.noAttrs))                // all local vars are (a) reified and (b) immutable
    }
    
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
            !hbInter(tp_cur, teePee(subst.path(rfd.p_guard)))
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

    def checkSucc(blks: Array[ir.Block], succ: ir.Succ) {
        at(succ, ()) {
            log.indented(succ) {
                val blk_tar = blks(succ.b)
                checkLengths(blk_tar.args, succ.ps, "intervals.succ.args")
                val lvs_tar = blk_tar.args.map(_.name)
                val tps = teePee(ir.noAttrs, succ.ps)
                val subst = PathSubst.vp(lvs_tar, tps.map(_.p))
                val wts_tar = blk_tar.args.map(arg => subst.wtref(arg.wt))
                foreachzip(tps, wts_tar)(checkIsSubtype)                
            }
        }
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
    
    def checkCall(x: ir.VarName, tp: ir.TeePee, m: ir.MethodName, tqs: List[ir.TeePee]) {
        val msig = substdMethodSig(tp, m, tqs)
        checkCallMsig(tp, msig, tqs)
        checkLvDecl(x, msig.wt_ret, None)                        
    }
    
    def checkStatement(stmt: ir.Stmt): Unit = 
        at(stmt, ()) {
            stmt match {                  
                case ir.StmtSuperCtor(qs) =>
                    if(!tp_this.as.ghost)
                        throw new ir.IrError("intervals.super.ctor.not.permitted.here")
                    val tp = tp_super
                    val tqs = teePee(ir.noAttrs, qs)
                    val msig_ctor0 = classDecl(tp.wt.c).ctor.msig(cap(tp))
                    val msig_ctor = ghostSubstOfTeePee(tp).methodSig(msig_ctor0)
                    checkCallMsig(tp_super, msig_ctor, tqs)
                    
                case ir.StmtGetField(x, p_o, f) =>
                    val tp_o = teePee(ir.noAttrs, p_o)
                    substdFieldDecl(tp_o, f) match {
                        case ir.GhostFieldDecl(_, _) =>
                            throw new ir.IrError("intervals.not.reified", tp_o.wt.c, f)
                        
                        case ir.ReifiedFieldDecl(_, wt, _, p_guard) =>
                            val tp_guard = teePee(ir.ghostAttrs, p_guard)                    
                            val p_f = tp_o.p + f // Canonical path of f; valid as f is not a ghost.

                            val op_canon = 
                                if(hbInter(tp_guard, tp_cur)) { // Constant:
                                    Some(p_f)
                                } else { // Non-constant:
                                    checkReadable(tp_guard)
                                    addTemp(p_f, x.path) // Record that p.f == vd, for now.
                                    None
                                }

                            checkLvDecl(x, wt, op_canon)
                    }                    
                    
                case ir.StmtSetField(p_o, f, p_v) =>
                    val tp_o = teePee(ir.noAttrs, p_o)
                    val tp_v = teePee(ir.noAttrs, p_v)
                    
                    substdFieldDecl(tp_o, f) match {
                        case ir.GhostFieldDecl(_, _) =>
                            throw new ir.IrError("intervals.not.reified", tp_o.wt.c, f)
                        
                        case ir.ReifiedFieldDecl(_, wt, _, p_guard) =>
                            val tp_guard = teePee(ir.ghostAttrs, p_guard)
                            checkWritable(tp_guard)
                    
                            checkIsSubtype(tp_v, wt)
                    
                            val p_f = tp_o.p + f // Canonical path of f; valid as f is not a ghost.
                            addTemp(p_f, tp_v.p)
                    
                            removeInvalidated(p_f)
                            linkedPaths(tp_o, f).foreach(addInvalidated)
                    }
                        
                case ir.StmtCall(x, p, m, qs) =>
                    val tp = teePee(ir.noAttrs, p)
                    val tqs = teePee(ir.noAttrs, qs)
                    checkCall(x, tp, m, tqs)
        
                case ir.StmtSuperCall(x, m, qs) =>
                    val tqs = teePee(ir.noAttrs, qs)
                    checkCall(x, tp_super, m, tqs)
                
                case ir.StmtNew(x, t, qs) =>
                    checkWfWt(t)
                    val cd = classDecl(t.c)
                    val tqs = teePee(ir.noAttrs, qs)
                    
                    if(cd.attrs.interface)
                        throw new ir.IrError("intervals.new.interface", t.c)
                    
                    // Check Ghost Types:           
                    checkLvDecl(x, t, None)
                    val tp_x = teePee(x.path)
                    t.ghosts.foreach { g =>
                        val gfd = substdFieldDecl(tp_x, g.f)
                        val tp = teePee(g.p)
                        checkIsSubtype(tp, gfd.wt)
                    }
                                        
                    val subst = (
                        ghostSubstOfTeePee(tp_x) + 
                        PathSubst.vp(cd.ctor.args.map(_.name), tqs.map(_.p))
                    )
                    val msig = subst.methodSig(cd.ctor.msig(t))
                    checkCallMsig(teePee(x.path), msig, tqs)
                    
                case ir.StmtCast(x, wt, p) => 
                    checkWfWt(wt)
                    val tp = teePee(ir.noAttrs, p)
                    
                    // TODO Validate casts?  Issue warnings at least?
                    
                    checkLvDecl(x, wt, None)
                    
                case ir.StmtNull(x, wt) => 
                    checkWfWt(wt)
                    checkLvDecl(x, wt, None)
                    
                case ir.StmtReturn(p) =>
                    val tp = teePee(ir.noAttrs, p)
                    checkIsSubtype(tp, env.wt_ret)
                    
                case ir.StmtHb(p, q) =>
                    val tp = teePee(ir.noAttrs, p)
                    val tq = teePee(ir.noAttrs, q)
                    checkAndAddHb(tp, tq)
                    
                case ir.StmtLocks(p, q) =>
                    val tp = teePee(ir.noAttrs, p)
                    val tq = teePee(ir.noAttrs, q)
                    checkIsSubtype(tp, ir.t_interval)
                    checkIsSubtype(tq, ir.t_lock)
                    addLocks(tp, tq)
                    
                case ir.StmtSubinterval(x, lp_locks, stmts) =>
                    val ltp_locks = teePee(ir.noAttrs, lp_locks)
                    checkLvDecl(x, ir.t_interval, None)
                    
                    val tp_x = teePee(x.path)
                    addSubintervalOf(tp_x, tp_cur)
                    ltp_locks.foreach(addLocks(tp_x, _))
                    
                    withCurrent(tp_x) {
                        stmts.foreach(checkStatement)                        
                    }
            }
        }
    
    def checkStatements(stmts: List[ir.Stmt]) {
        stmts.foreach(checkStatement)
        checkNoInvalidated()
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
        reqs_sup.foreach(checkAndAddReq)
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
    
    def introduceArg(arg: ir.LvDecl) {
        checkWfWt(arg.wt) 
        addPerm(arg.name.path, ir.TeePee(arg.wt, arg.name.path, ir.noAttrs))        
    }
    
    def checkBlock(blks: Array[ir.Block], b: Int, blk: ir.Block) = 
        log.indented("%s: %s", b, blk) {
            savingEnv {
                blk.args.foreach(introduceArg)                
                checkStatements(blk.stmts)
                blk.succs.foreach(checkSucc(blks, _))
            }
        }
        
    def checkNoninterfaceMethodDecl(
        cd: ir.ClassDecl,          // class in which the method is declared
        env_ctor_assum: ir.TcEnv,  // relations established by the ctor
        md: ir.MethodDecl          // method to check
    ) = 
        at(md, ()) {
            savingEnv {
                // Define special vars "method" and "this":
                addPerm(ir.p_mthd, ir.TeePee(ir.t_interval, ir.p_mthd, ir.ghostAttrs))
                if(!md.attrs.ctor) { 
                    // For normal methods, type of this is the defining class
                    addPerm(ir.p_this, ir.TeePee(thisTref(cd), ir.p_this, ir.noAttrs))                     
                    addHbPnt(tp_ctor_end, tp_cur_start) // ... constructor already happened
                    env = env + env_ctor_assum          // ... add its effects on environment
                } else {
                    // For constructor methods, type of this is the type that originally defined it
                    val t_rcvr = typeOriginallyDefiningMethod(cd.name, md.name).get
                    addPerm(ir.p_this, ir.TeePee(t_rcvr.ctor, ir.p_this, ir.noAttrs))
                }  
                
                md.args.foreach(introduceArg)
                checkWfWt(md.wt_ret)
                setWtRet(md.wt_ret)
                
                savingEnv {
                    overriddenMethodSigs(cd.name, md.name) foreach { msig_sup_0 => 
                        val msig_sup = substArgsInMethodSig(msig_sup_0, md.args.map(_.name))
                        checkArgumentTypesNonvariant(md.args, msig_sup.args)
                        checkReturnTypeCovariant(md.wt_ret, msig_sup.wt_ret)
                        checkOverridenReqsImplyOurReqs(md.reqs, msig_sup.reqs)
                    }                    
                }
                
                md.reqs.foreach(checkAndAddReq)
                
                md.blocks.zipWithIndex.foreach { case (blk, idx) =>
                    checkBlock(md.blocks, idx, blk)
                }
            }         
        }
        
    def checkNoninterfaceConstructorDecl(cd: ir.ClassDecl, md: ir.MethodDecl) = 
        at(md, emptyEnv) {
            savingEnv {
                // Define special vars "method" (== this.constructor) and "this":
                addPerm(ir.p_mthd, ir.TeePee(ir.t_interval, ir.gfd_ctor.thisPath, ir.ghostAttrs))
                addPerm(ir.p_this, ir.TeePee(thisTref(cd, ir.ctorAttrs), ir.p_this, ir.ghostAttrs))

                md.args.foreach { case arg => 
                    checkWfWt(arg.wt) 
                    addPerm(arg.name.path, ir.TeePee(arg.wt, arg.name.path, ir.noAttrs))
                }            
                md.reqs.foreach(checkAndAddReq)
                
                val blk0 = md.blocks(0)
                val preSuper = blk0.stmts.takeWhile(!isSuperCtor(_))
                val postSuper = blk0.stmts.dropWhile(!isSuperCtor(_))
                if(postSuper.isEmpty)
                    throw new ir.IrError("intervals.super.ctor.zero")
                    
                // TODO -- Have the checking of super() verify that p_this is a ghost
                // and reify it, and thus permit control-flow.  We then have to propagate
                // p_this between blocks though (yuck).
                    
                // Check statements up until the super() invocation with ghost 'this':
                preSuper.foreach(checkStatement)
                checkStatement(postSuper.head)

                // After invoking super, 'this' becomes reified:
                setPerm(ir.p_this, ir.TeePee(thisTref(cd, ir.ctorAttrs), ir.p_this, ir.noAttrs))
                checkStatements(postSuper.tail)

                extractAssumptions(tp_ctor_end, Set(ir.lv_this))
            }          
        }
    
    def checkReifiedFieldDecl(cd: ir.ClassDecl, fd: ir.ReifiedFieldDecl) = 
        at(fd, ()) {
            savingEnv {
                checkWfWt(fd.wt)
                
                // Rules:
                //
                // The type of a field f with guard p_g in class c 
                // may be dependent on a path p_dep if either:
                // (1) p_dep is constant when p_g is active; or
                // (2) p_dep = this.f' and f' is declared in class c (not a supertype!)
                //
                // Note that a type is dependent on p_dep if p.F appears in the type, so 
                // we must check all prefixes of each dependent path as well.
                val t_this = thisTref(cd, ir.ctorAttrs)
                val tp_this = ir.TeePee(t_this, ir.p_this, ir.noAttrs)
                val tp_mthd = ir.TeePee(ir.t_interval, ir.p_mthd, ir.ghostAttrs)
                addPerm(ir.p_this, tp_this)
                addPerm(ir.p_mthd, tp_mthd)
                
                // If an interval class, then this.ctor hb this:
                if(isSubclass(t_this, ir.c_interval))
                    addHbInter(teePee(ir.p_ctor), tp_this)
                
                // Add assumptions as though guard were satisfied.
                // Also check that guards are typed as Interval, Lock, or just Guard
                val tp_guard = teePee(fd.p_guard)
                if(isSubclass(tp_guard.wt, ir.c_interval))
                    addSubintervalOf(tp_cur, tp_guard) 
                else if(isSubclass(tp_guard.wt, ir.c_lock))
                    addLocks(tp_cur, tp_guard)
                else if(tp_guard.wt.c == ir.c_guard)
                    addDeclaredWritableBy(tp_guard, tp_cur)
                else
                    throw new ir.IrError("intervals.invalid.guard.type", tp_guard.wt)                    
                
                // Check that each dependent path is legal:
                fd.wt.dependentPaths.foreach { p_full_dep => 
                    savingEnv {
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
    
    def checkGhostFieldDecl(cd: ir.ClassDecl, gfd: ir.GhostFieldDecl) = 
        at(gfd, ()) {
            log.indented(gfd) {
                // Check that ghosts are not shadowed from a super class:
                strictSuperclasses(cd.name).foreach { c =>
                    if(classDecl(c).fields.exists(_.name == gfd.name))
                        throw ir.IrError("intervals.shadowed.ghost", c, gfd.name)
                }
            }
        }
        
    def checkFieldDecl(cd: ir.ClassDecl)(priorNames: Set[ir.FieldName], fd: ir.FieldDecl) = 
        at(fd, priorNames) {
            if(priorNames(fd.name))
                throw new ir.IrError("intervals.duplicate.field", fd.name)
            fd match {
                case rfd: ir.ReifiedFieldDecl => checkReifiedFieldDecl(cd, rfd)
                case gfd: ir.GhostFieldDecl => checkGhostFieldDecl(cd, gfd)
            }
            priorNames + fd.name
        }
        
    // ______________________________________________________________________
    // Inter-method Assumptions
    //
    // When we know that method A must complete before method B is invoked,
    // we can move any relations known at the end of A into B.  In particular, 
    // we can move relations from the constructor to any non-constructor method.
    // There are some complications, however.  We need to ensure we only move
    // a relation (p, q) if the paths p and q refer to the same objects in method B.
    // This basically restricts us to paths beginning with "this".  The next complication
    // is that the interesting relations tend to be between fields that which are being
    // actively modified by method A, which means that these relations will actually be
    // recorded as between fields that are temporarily aliased to local variables.
    //
    // Therefore, what we do is as follows:
    //
    // (1) At the end of the method A, we create a reverse mapping for all temporary
    //     aliases p->lv iff the path p is declared as being written by A (i.e., this.constructor).
    //     This is sound because if it has not changed by the end of A, then
    //     it will never change.
    //
    // (2) We apply this reverse mapping to all relations and then take the subset of
    //     those which involve paths beginning with shared local variables (only "this" right
    //     now).
    
    def mapFilter[R <: Relation[ir.Path,R]](
        rel: R,
        mapFunc: (ir.Path => ir.Path), 
        filterFunc: (ir.Path => Boolean)
    ): R =
        rel.all.foldLeft(rel.empty) { case (r, (p, q)) =>
            val p1 = mapFunc(p)
            val q1 = mapFunc(q)
            if(filterFunc(p1) && filterFunc(q1))
                r + (p1, q1)
            else
                r
        }
        
    def extractAssumptions(
        tp_mthd_end: ir.TeePee, 
        lvs_shared: Set[ir.VarName]
    ): ir.TcEnv = savingEnv {        
        log.indented("extractAssumptions(%s,%s)", tp_mthd_end, lvs_shared) {
            withCurrent(freshTp(ir.t_interval)) {
                addHbPnt(tp_mthd_end, tp_cur_start)

                log("temp=%s", env.temp)
                val tempKeys = env.temp.map(_._1).toList
                val tempValues = env.temp.map(_._2).toList
                val mapFunc = PathSubst.pp(tempValues, tempKeys).path(_)
                
                def filterFunc(p: ir.Path): Boolean = 
                    log.indentedRes("filterFunc(%s)", p) {
                        lvs_shared(p.lv) && !teePee(p).as.mutable                
                    }
                    
                ir.TcEnv(
                    env.p_cur,
                    env.wt_ret,
                    env.perm,
                    env.temp,
                    env.lp_invalidated,
                    mapFilter(env.readable, mapFunc, filterFunc),
                    mapFilter(env.writable, mapFunc, filterFunc),
                    mapFilter(env.hb, mapFunc, filterFunc),
                    mapFilter(env.subinterval, mapFunc, filterFunc),
                    mapFilter(env.locks, mapFunc, filterFunc)
                )
            }            
        }
    }

    // ______________________________________________________________________
    // Classes and Interfaces
    
    def checkNotCtor(t: ir.TypeRef) {
        if(t.as.ctor) throw new ir.IrError("intervals.superType.with.ctor.attr") 
    }
    
    def checkIsInterface(t: ir.TypeRef) {
        val cd_super = classDecl(t.c)
        if(!cd_super.attrs.interface) 
            throw new ir.IrError("intervals.superType.not.interface", t.c)        
    }
    
    def checkInterfaceSupertype(t: ir.TypeRef) {
        checkNotCtor(t)
        if(t.c != ir.c_object)
            checkIsInterface(t)
    }
    
    def checkInterfaceConstructorDecl(cd: ir.ClassDecl, md: ir.MethodDecl) =
        at(md, ()) {
            if(!md.blocks.deepEquals(ir.md_ctor_interface.blocks))
                throw new ir.IrError("intervals.invalid.ctor.in.interface")
            // TODO Check other parts of constructor signature. (Very low priority)
        }
    
    def checkInterfaceMethodDecl(cd: ir.ClassDecl, md: ir.MethodDecl) =
        checkNoninterfaceMethodDecl(cd, emptyEnv, md)
    
    def checkInterfaceClassDecl(cd: ir.ClassDecl) =
        at(cd, ()) {
            savingEnv {
                // TODO Is this everything?
                cd.superTypes.foreach(checkInterfaceSupertype)
                if(!cd.fields.isEmpty)
                    throw new ir.IrError("intervals.interface.with.fields")
                checkInterfaceConstructorDecl(cd, cd.ctor)
                cd.methods.foreach(checkInterfaceMethodDecl(cd, _))
            }
        }
        
    def checkIsNotInterface(t: ir.TypeRef) {
        val cd_super = classDecl(t.c)
        if(cd_super.attrs.interface) 
            throw new ir.IrError("intervals.superType.interface", t.c)        
    }
    
    def checkNoninterfaceClassDecl(cd: ir.ClassDecl) = 
        at(cd, ()) {
            savingEnv {
                cd.superTypes.foreach(checkNotCtor)
                cd.superTypes.take(1).foreach(checkIsNotInterface)
                cd.superTypes.drop(1).foreach(checkIsInterface)
                cd.fields.foldLeft(Set.empty[ir.FieldName])(checkFieldDecl(cd))
                val env_ctor_assum = checkNoninterfaceConstructorDecl(cd, cd.ctor)                    
                cd.methods.foreach(checkNoninterfaceMethodDecl(cd, env_ctor_assum, _))                    
            }
        }
        
    def checkClassDecl(cd: ir.ClassDecl) =
        if(cd.attrs.interface) checkInterfaceClassDecl(cd)
        else checkNoninterfaceClassDecl(cd)
        
    def check = prog.cds_user.foreach(checkClassDecl)
}