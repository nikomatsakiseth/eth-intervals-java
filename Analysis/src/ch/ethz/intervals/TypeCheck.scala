package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import Util.forallzip

class TypeCheck(prog: Prog) {    
    import prog.classDecl
    import prog.fresh
    
    // Easier to use mutability than to thread 
    // this through everywhere:
    var env: ir.TcEnv

    /// capture an object ref.
    def capObj(wo: ir.WcObjPath) = wo match {
        case o: ir.ObjPath => o
        case ir.WcUnkObj => ir.ObjPath(fresh("Cap"))
    }
    
    def canon(x: ir.VarName): ir.Obj = x.o
    
    /// capture wt into a TypeRef
    def cap(wt: ir.WcTypeRef): ir.TypeRef = 
        ir.TypeRef(wt.c, wt.objs.map(capObj), wt.mthds)

    /// subst. for obj. param. of t
    def objSubst(t: ir.TypeRef) =
        ObjSubst(classDecl(t.c).objs, t.objs)

    /// supertype of t
    def sup(t: ir.TypeRef): Option[ir.TypeRef] = {
        val cd = classDecl(t.c)
        cd.superType match {
            case None => None
            case Some(t_1) => objSubst(t).tref(t_1)
        }
    }
    
    def fieldDecl(ot: ir.TypeRef, f: ir.FieldName) = {
        def search(t: ir.TypeRef): ir.FieldDecl = {
            val cd = classDecl(t.c)
            cd.fields.find(_.name == f) match {
                case Some(fd) => objSubst(t).varDecl(fd)
                case None => sup(t) match {
                    case Some(t_1) => search(t_1)
                    case None => throw IrError("intervals.no.such.field", ot, f)
                }
            }
        }
        search(ot)        
    }
    
    def methodSig(ot: ir.TypeRef, m: ir.MethodName): ir.MethodSig = {
        def search(t: ir.TypeRef): ir.MethodSig = {
            val cd = classDecl(t.c)
            cd.methods.find(_.name == m) match {
                case Some(md) => objSubst(t).msig(md.msig)
                case None => sup(t) match {
                    case Some(t_1) => search(t_1)
                    case None => throw IrError("intervals.no.such.method", ot, m)
                }
            }
        }
        search(ot)
    }
    
    /// type of a local variable ref.
    def lv(x: ir.VarName) =
        env.lvs(x)
    
    /// type of an object reference.
    def obj(o: ir.Obj): ir.WcTypeRef = o match {
        case ir.Obj(x, List()) => lv(x)
        case ir.Obj(x, f :: fs) => 
            val o_1 = ir.Obj(x, fs)
            val t = cap(obj(o_1))
            val fd = fieldDecl(t, f)
            if(fd.isFinal) fd.wt
            else throw new ir.IrError("intervals.not.final", o_1, t, f)
    }

    /// method sig. for o.m(p) with subst. performed
    def substdMethodSig(o: ir.Obj, m: ir.MethodName, p: ir.Obj) = {
        val t_o = cap(obj(o))
        val t_p = cap(obj(p))
        val msig = methodSig(t_r, m)
        checkIsSubtype(t_p, msig.arg.wt)
        val subst = ObjSubst(List(ir.x_this, msig.arg.x), List(o, p))
        subst.msig(msig)        
    }
    
    /// effects of invoking m(p) if rcvr has type wt
    def effect(wt: ir.AnyTypeRef, m: ir.MethodName, p: ir.Obj) = {
        wt.overs.find(_.m == m) match {
            case Some(ov) => ObjSubst(ov.y, p).effect(ov.e)
            case None =>
                val t = cap(wt)
                val msig = methodSig(t, m)
                ObjSubst(msig.arg.x, p).effect(msig.e)
        }
    }
    
    /// o_sub <= wo_sup
    def isSubobj(o_sub: ir.Obj, wo_sup: ir.WcObj) = wo_sup match {
        case ir.WcObj => true
        case o_sup: ir.Obj => o_sub == o_sup
    }

    /// n→m in the minimal interpretation
    def hb(n: ir.Point, m: ir.Point): Boolean =
        env.min.contains(n, m)
        
    /// n either is m or n→m
    def hbeq(n: ir.Point, m: ir.Point): Boolean =
        n == m || hb(n, m)
    
    /// i_sub is a smaller-or-equal span of time than i_sup
    def isSubinterval(i_sub: ir.Interval, i_sup: ir.Interval) =
        forallcross(i_sup.ns, i_sub.ns, hbeq)
        && forallcross(i_sub.ms, i_sup.ms, hbeq)
        
    def isSubeffect(e_sub0: ir.Effect, e_sup0: ir.Effect): Boolean = (e_sub0, e_sup0) match {
        case (ir.EffectNone, _) => true
        case (_, ir.EffectNone) => false
        
        case (ir.EffectUnion(es), e_sup) => es.forall(isSubeffect(_, e_sup))
        case (ir.EffectMethod(o, m, p), e_sup) =>
            isSubeffect(effect(obj(o), m, p), e_sup)
            
        case (e_sub, ir.EffectUnion(es)) => es.exists(isSubeffect(e_sub, _))
        case (e_sub, ir.EffectMethod(o, m, p)) =>
            isSubeffect(e_sub, effect(obj(o), m, p))
            
        case (ir.EffectInterval(i_sub, e_sub), ir.EffectInterval(i_sup, e_sup)) =>
            isSubinterval(i_sub, i_sup) && isSubeffect(e_sub, e_sup)
        case (ir.EffectFixed(_, o_sub), ir.EffectFixed(ir.Wr, o_sup)) =>
            isSubobj(o_sub, o_sup)
        case (ir.EffectFixed(ir.Rd, o_sub), ir.EffectFixed(ir.Rd, o_sup)) =>
            isSubobj(o_sub, o_sup)
        case (ir.EffectLock(os_sub, e_sub), ir.EffectLock(os_sup, e_sup)) =>
            os_sub.forall(o_sub =>
                os_sup.exists(o_sup =>
                    isSubobj(o_sub, o_sup)))
            && isSubeffect(e_sub, e_sup)
            
        case _ =>
            false
    }
    
    /// are effects of ov
    def isSubover(ov_sub: ir.Over, ov_sup: ir.Over): Boolean = {
        if(ov_sub.m == ov_sup.m) {
            val f = fresh("Sub")
            val es_sub = LvSubst(ov_sub.y, f).effect(ov_sub.e)
            val es_sup = LvSubst(ov_sub.y, f).effect(ov_sup.e)
            isSubeffect(es_sub, es_sup)
        } else
            false
    }
    
    def isSubtype(t_sub: ir.TypeRef, wt_sup: ir.AnyTypeRef): Boolean = {
        if(t_sub.c != wt_sub.c) {
            sup(t_sub) match {
                case None => false
                case Some(t_1) => isSubtype(t_1, wt_sup)
            }
        } else {
            forallzip(t_sub.objs, wt_sup.objs, isSubobj) &&
            t_sub.overs.forall(ov_sub => 
                isSubeffect(ov_sub.e, effect(wt_sup, ov_sub.m, ov_sub.y.o)))
        }
    }
    
    def checkIsSubtype(t_sub: ir.TypeRef, wt_sup: ir.AnyTypeRef) {
        if(!isSubover(t_sub, wt_sup))
            throw new ir.IrError("intervals.expected.subtype", t_sub, wt_sup)
    }
    
    /// types x.m(y), returning result type and effects
    def call(x: ir.VarName, m: ir.MethodName, y: ir.VarName) = {
        val t_x = lv(x), t_y = lv(y)
        val o_x = canon(x), o_y = canon(y)
        
        // Compute effects:
        val e0 = effect(t_x, m, canon(y))
        val e = ObjSubst(ir.x_this, x).effect(e)
        
        // Check argument type matches:
        val msig = methodSig(t_x, m)
        checkIsSubtype(t_y, msig.arg.wt)
        
        // Compute return type:
        val wt = 
            ObjSubst(
                List(ir.x_this, msig.arg.x), 
                List(o_x, o_y)
            ).wtref(msig.wt_ret)
                    
        (wt, e)
    }
    
    def expr(e: ir.Expr): (ir.AnyTypeRef, Effects) = e match {
        
        case ir.ExprCall(x, m, y) =>
            call(x, m, y)
            
        case ir.ExprField(x, f) =>
            val t_x = lv(x), o_x = canon(x)
            val fd0 = fieldDecl(t_x, f)
            val fd = ObjSubst(ir.x_this, o_x).fieldDecl(fd)
            (fd.wt, ir.EffectFixed(ir.Rd, fd.guard))
            
        case ir.ExprNew(t: ir.TypeRef) =>
            checkTref(t)
            (t, ir.EffectNone)
            
        case ir.ExprNewInterval(b, t, gs) =>
            val o_b = canon(b)
            val (_, e0) = call(t, ir.m_run, ir.x_new)
            val os_g = xs.map(canon)
            val e_l = ir.EffectLock(os_g, e0)
            val e_i = ir.EffectShift(ir.x_new, e_l)
            (ir.TypeRef(ir.c_interval, List(o_b), List()), e_i)
            
        case ir.ExprNewGuard() =>
            (ir.TypeRef(ir.c_guard, List(), List()), ir.EffectNone)
            
    }
    
    def stmt(s: ir.Stmt): Effects = {
        
    }
}