package ch.ethz.intervals

import scala.collection.immutable.Set

object ir {
    
    /*
    Naming Conventions
    
    lv -> variable name
    p, q, r -> paths
    i, j -> intervals
    e, f -> effects
    f -> field names
    m -> method names
    c -> class names
    ex -> expressions
    */
    
    sealed abstract class Modifier
    sealed case class Final extends Modifier
    
    sealed abstract class Name(val name: String)
    case class VarName(name: String) extends Name(name) {
        def path = ir.Path(this, List())
        def +(f: FieldName) = path + f
        def ++(fs: List[FieldName]) = path ++ fs
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
        ghosts: List[GhostFieldDecl],
        superType: Option[TypeRef],
        fields: List[RealFieldDecl],
        methods: List[MethodDecl]
    ) extends Locatable
    
    sealed case class MethodDecl(
        wt_ret: WcTypeRef
        name: MethodName,
        arg: LvDecl,
        e: Effect,
        stmts: List[Stmt],
        ex_ret: Expr,
    ) extends Locatable {
        def msig = at(MethodSig(es, arg, t_ret), srcLoc)
    }
    
    sealed case class MethodSig(
        e: Effect,
        arg: LvDecl,
        wt_ret: WcTypeRef
    ) extends Locatable
    
    case class LvDecl(
        name: VarName,
        wt: WcTypeRef
    ) 

    sealed abstract class FieldDecl {
        val wt: WcTypeRef
        val name: FieldName
        val isFinal: Boolean
    }
    
    sealed case class GhostFieldDecl(
        name: FieldName,
        wt: WcTypeRef
    ) extends FieldDecl {
        def isFinal = true
    }
    
    sealed case class RealFieldDecl(
        mods: Set[Modifier],
        wt: WcTypeRef,
        name: FieldName,
        guard: Path
    ) extends FieldDecl {
        def isFinal = mods.contains(Final)
    }
    
    sealed abstract class Stmt extends Locatable
    case class StmtVarDecl(vd: LvDecl, ex: Expr) extends Stmt
    case class StmtAssignField(p: Path, f: FieldName, q: Path) extends Stmt
    case class StmtAddHb(p: Path, q: Path) extends Stmt
    
    sealed abstract class Expr extends Locatable
    case class ExprCall(p: Path, m: MethodName, q: Path) extends Expr
    case class ExprField(p: Path, f: FieldName) extends Expr
    case class ExprNew(t: TypeRef) extends Expr
    case class ExprNewInterval(bound: Path, task: VarName, guards: VarName) extends Expr
    case class ExprNewGuard(p: Path) extends Expr
    
    case class WcTypeRef(
        c: ClassName,
        wpaths: List[WcPath],
        overs: List[Over]
    ) extends TypeRef
    
    case class TypeRef(
        override c: ClassName,
        paths: List[Path],
        override overs: List[Over]
    ) extends WcTypeRef(c, paths, overs)
    
    sealed abstract class WcPath
    case object WcUnkPath extends WcPath
    
    sealed case class Path(
        x: VarName, rev_fs: List[VarName] // Fields stored in reverse order!
    ) extends WcPath {
        def fs = rev_fs.reverse
        def +(f: ir.FieldName) = Path(x, f :: rev_fs)
        def ++(fs: List[ir.FieldName]) = fs.foldLeft(this)(_ + _)
    }
    
    sealed case class Over(
        m: MethodName,
        lv: VarName,
        e: Effect
    )
    
    sealed abstract class Effect
    sealed case class EffectShift(i: Interval, e: ir.Effect)
    sealed case class EffectLock(ps: List[Path], e: ir.Effect)
    sealed case class EffectMethod(p: Path, m: MethodName, q: Path)
    sealed case class EffectFixed(k: ActionKind, p: Path)
    sealed case class EffectUnion(es: List[ir.Effect])
    sealed case object EffectNone
    sealed case object EffectAny
    
    def union(e_0: ir.Effect, e_1: ir.Effect) = (e_0, e_1) match {
        case (ir.EffectUnion(es_0), ir.EffectUnion(es_1)) =>
            ir.EffectUnion(es_0 ++ es_1)
        case (ir.EffectUnion(es), e) | (e, ir.EffectUnion(es)) =>
            ir.EffectUnion(e :: es)
        case _ =>
            ir.EffectUnion(List(e_0, e_1))
    }
    
    /// Bounds a point r: ∀p∈ps.p→r, ∀q∈qs.r→q
    sealed case class Interval(ps: List[Path], qs: List[Path])
    
    sealed abstract class ActionKind
    case class Rd extends ActionKind
    case class Wr extends ActionKind
    
    class Graph(
        hbs: Util.MultiMap[Path, Path]
    ) {
        private var cachedTc: Option[Util.MultiMap[Path, Path]] = 
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
        
        def contains(n: Path, m: Path) =
            tc.contains(n, m)
    }
    
    case class TcEnv(
        lvs: Map[ir.VarName, ir.WcTypeRef],
        min: Graph
    )
    
    case class IrError(msg: String, args: Any*) 
    extends RuntimeException
    
    val p_this = ir.VarName("this").path
    val p_new = ir.VarName("new").path
    val p_end = ir.VarName("end").path
    
    val p_schedule = ir.VarName("schedule").path
    val p_current = ir.VarName("current").path
    val p_root = ir.VarName("root").path
    
    val f_start = ir.FieldName("start")
    val f_end = ir.FieldName("end")
    
    val m_run = ir.MethodName("run")
    
    val c_inter = ir.ClassName("ch.ethz.intervals.Interval")
    val c_point = ir.ClassName("ch.ethz.intervals.Point")
    val c_guard = ir.ClassName("ch.ethz.intervals.Guard")    
    val c_task = ir.ClassName("ch.ethz.intervals.Task")
    
    val wt_inter = ir.WcTypeRef(c_inter, List(ir.WcUnkPath), List())
    val wt_point = ir.WcTypeRef(c_point, List(ir.WcUnkPath), List())
    val wt_guard = ir.WcTypeRef(c_guard, List(ir.WcUnkPath), List())
    val wt_task = ir.WcTypeRef(c_task, List(), List(ir.Over(m_run, p_end, ir.EffectAny)))
    
    val objectClassDefn = 
}