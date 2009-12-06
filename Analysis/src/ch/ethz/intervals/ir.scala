package ch.ethz.intervals

import scala.collection.immutable.Set

object ir {
    
    /*
    Naming Conventions
    
    x, y, z -> local variables
    f -> field names
    m -> method names
    c -> class names
    e -> expressions
    */
    
    sealed abstract class Modifier
    sealed case class Final extends Modifier
    
    sealed abstract class Name(val name: String)
    case class VarName(name: String) extends Name(name) {
        def o = ir.Obj(this, List())
    }
    case class FieldName(name: String) extends Name(name)
    case class MethodName(name: String) extends Name(name)
    case class ClassName(name: String) extends Name(name)
    
    abstract class Locatable {
        var srcLoc: Object
    }
    
    def at[I <: Locatable](i: I, pos: Object): I = {
      i.srcLoc = pos
      i
    }
    
    sealed case class Error(
        loc: Locatable,
        msg: String,
        args: List[Object]
    )
    
    sealed case class ClassDecl(
        name: ClassName,
        objs: String,
        superType: Option[TypeRef],
        fields: List[FieldDecl],
        methods: List[MethodDecl]
    ) extends Locatable
    
    sealed case class MethodDecl(
        name: MethodName,
        e: Effect,
        stmts: List[Stmt],
        e_ret: Expr,
        arg: VarDecl,
        wt_ret: WcTypeRef
    ) extends Locatable {
        def msig = at(MethodSig(es, arg, t_ret), srcLoc)
    }
    
    sealed case class MethodSig(
        e: Effect,
        arg: VarDecl,
        wt_ret: WcTypeRef
    ) extends Locatable
    
    sealed abstract class VarDecl {
        val x: VarName
        val wt: WcTypeRef
    }
    
    case class LvDecl(
        x: VarName,
        wt: WcTypeRef
    ) extends VarDecl
        
    sealed case class FieldDecl(
        guard: ir.Obj,
        mods: Set[Modifier]
    ) extends VarDecl {
        def isFinal = mods.contains(Final)
    }
    
    sealed abstract class Stmt extends Locatable
    case class StmtVarDecl(vd: LvDecl, e: Expr) extends Stmt
    case class StmtAssignField(x: VarName, f: FieldName, y: VarName) extends Stmt
    case class StmtAddHb(n: Point, m: Point) extends Expr
    
    sealed abstract class Expr extends Locatable
    case class ExprCall(x: VarName, m: MethodName, y: VarName) extends Expr
    case class ExprField(x: VarName, f: FieldName) extends Expr
    case class ExprNew(t: TypeRef) extends Expr
    case class ExprNewInterval(x: VarName, task: VarName, guards: VarName) extends Expr
    case class ExprNewGuard() extends Expr
    
    sealed abstract class AnyTypeRef {
        val c: ClassName
        val objs: List[WcObj]
        val overs: List[Over]
    }
    
    case class WcTypeRef(
        c: ClassName,
        objs: List[WcObj],
        overs: List[Over]
    ) extends AnyTypeRef
    
    sealed abstract class WcObj
    case object WcUnkObj extends WcObj
    
    sealed case class TypeRef(
        c: ClassName,
        objs: List[Obj],
        overs: List[Over]        
    ) extends AnyTypeRef
    
    sealed case class Obj(
        x: VarName, rev_fs: List[VarName] // Fields stored in reverse order!
    ) extends WcObjPath {
        def fs = rev_fs.reverse
        def +(f: ir.FieldName) = Obj(x, f :: rev_fs)
        def ++(fs: List[ir.FieldName]) = fs.foldLeft(this)(_ + _)
    }
    
    sealed case class Over(
        m: MethodName,
        y: VarName,
        e: Effect
    )
    
    sealed case class Point(
        o: Obj,
        s: Side
    )
    
    sealed abstract class Side
    case class Start extends Side
    case class End extends Side

    sealed abstract class Effect
    sealed case class EffectShift(i: Interval, e: ir.Effect)
    sealed case class EffectLock(os: List[Obj], e: ir.Effect)
    sealed case class EffectMethod(o: Obj, m: MethodName, p: Obj)
    sealed case class EffectFixed(k: ActionKind, o: Obj)
    sealed case class EffectUnion(es: List[ir.Effect])
    sealed case object EffectNone
    
    /// An interval puts a bound on a point p in time.
    /// ∀n∈ns.n→p, ∀m∈ms.p→m
    sealed case class Interval(ns: List[Point], ms: List[Point])
    
    sealed abstract class Action
    case class ActionFixed(k: ActionKind, o: Obj)
    case class ActionMethod(o: Obj, m: MethodName, p: Obj)
    
    sealed abstract class ActionKind
    case class Rd extends ActionKind
    case class Wr extends ActionKind
    
    class Graph(
        hbs: Util.MultiMap[ir.Point, ir.Point]
    ) {
        private var cachedTc: Option[Util.MultiMap[ir.Point, ir.Point]] = 
            None        
        private def tc = {
            cachedTc match {
                case Some(t) => t
                case None =>
                    val t = Util.transitiveClosure(hbs)
                    cachedTc = Some(t)
                    t
            }
        }
        
        def contains(n: ir.Point, m: ir.Point) =
            tc.contains(n, m)
    }
    
    case class TcEnv(
        lvs: Map[ir.VarName, ir.WcTypeRef],
        min: Graph
    )
    
    case class IrError(msg: String, args: Any*) 
    extends RuntimeException
    
    val x_this = ir.VarName("this")
    val x_new = ir.VarName("new")
    val o_this = ir.Obj("this")
    val m_run = ir.MethodName("run")
    val c_inter = ir.ClassName("ch.ethz.intervals.Interval")
    val c_guard = ir.ClassName("ch.ethz.intervals.Guard")    
}