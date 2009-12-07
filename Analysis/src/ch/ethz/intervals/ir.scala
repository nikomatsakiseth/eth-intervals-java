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
    case object Final extends Modifier
    
    sealed abstract class Name(name: String) {
        override def toString = name
    }
    case class VarName(name: String) extends Name(name) {
        def path = ir.Path(this, List())
        def +(f: FieldName) = path + f
        def ++(fs: List[FieldName]) = path ++ fs
    }
    case class FieldName(name: String) extends Name(name)
    case class MethodName(name: String) extends Name(name)
    case class ClassName(name: String) extends Name(name)
    
    abstract class Locatable {
        var srcLoc: Object = null
    }
    
    def at[I <: Locatable](i: I, pos: Object): I = {
      i.srcLoc = pos
      i
    }
    
    sealed case class Error(
        loc: Locatable,
        msg: String,
        args: List[String]
    )
    
    sealed case class ClassDecl(
        name: ClassName,
        ghosts: List[GhostFieldDecl],
        superType: Option[TypeRef],
        fields: List[RealFieldDecl],
        methods: List[MethodDecl]
    ) extends Locatable {
        def allFields = ghosts ++ fields
    }
    
    sealed case class MethodDecl(
        wt_ret: WcTypeRef,
        name: MethodName,
        arg: LvDecl,
        e: Effect,
        stmts: List[Stmt],
        ex_ret: Expr
    ) extends Locatable {
        def msig = at(MethodSig(e, arg, wt_ret), srcLoc)
    }
    
    sealed case class MethodSig(
        e: Effect,
        arg: LvDecl,
        wt_ret: WcTypeRef
    ) extends Locatable
    
    sealed case class LvDecl(
        name: VarName,
        wt: WcTypeRef
    ) 

    sealed abstract class FieldDecl {
        val wt: WcTypeRef
        val name: FieldName
        def isFinal: Boolean
        def isGhost: Boolean
        
        def thisPath = p_this + name
    }
    
    sealed case class GhostFieldDecl(
        name: FieldName,
        wt: WcTypeRef
    ) extends FieldDecl {
        def isFinal = true
        def isGhost = true
    }
    
    sealed case class RealFieldDecl(
        mods: List[Modifier],
        wt: WcTypeRef,
        name: FieldName,
        guard: Path
    ) extends FieldDecl {
        def isFinal = mods.contains(Final)
        def isGhost = false
    }
    
    sealed abstract class Stmt extends Locatable
    sealed case class StmtVarDecl(vd: LvDecl, ex: Expr) extends Stmt
    sealed case class StmtAssignField(p: Path, f: FieldName, q: Path) extends Stmt
    sealed case class StmtAddHb(p: Path, q: Path) extends Stmt
    
    sealed abstract class Expr extends Locatable
    sealed case class ExprCall(p: Path, m: MethodName, q: Path) extends Expr
    sealed case class ExprField(p: Path, f: FieldName) extends Expr
    sealed case class ExprNew(t: TypeRef) extends Expr
    sealed case class ExprNewInterval(bound: Path, task: Path, guards: List[Path]) extends Expr
    sealed case class ExprNewGuard(p: Path) extends Expr
    case object ExprNull extends Expr
    
    sealed case class WcTypeRef(
        c: ClassName,
        wpaths: List[WcPath],
        overs: List[Over]
    ) 
    
    sealed case class TypeRef(
        override val c: ClassName,
        paths: List[Path],
        override val overs: List[Over]
    ) extends WcTypeRef(c, paths, overs)
    
    sealed abstract class WcPath
    case object WcUnkPath extends WcPath
    
    sealed case class Path(
        lv: VarName, rev_fs: List[FieldName] // Fields stored in reverse order!
    ) extends WcPath {
        def fs = rev_fs.reverse
        def +(f: ir.FieldName) = Path(lv, f :: rev_fs)
        def ++(fs: List[ir.FieldName]) = fs.foldLeft(this)(_ + _)
    }
    
    sealed case class Over(
        m: MethodName,
        lv: VarName,
        e: Effect
    )
    
    sealed abstract class Effect
    sealed case class EffectInterval(i: Interval, e: ir.Effect) extends Effect
    sealed case class EffectLock(ps: List[Path], e: ir.Effect) extends Effect
    sealed case class EffectMethod(p: Path, m: MethodName, q: Path) extends Effect
    sealed case class EffectFixed(k: ActionKind, p: Path) extends Effect
    sealed case class EffectUnion(es: List[ir.Effect]) extends Effect
    case object EffectNone extends Effect
    case object EffectAny extends Effect

    /// Unions two effects.  Avoid "stupid" final effects like
    /// union with the empty set, etc.
    def union(e_0: ir.Effect, e_1: ir.Effect) = (e_0, e_1) match {
        case (ir.EffectNone, e) => e
        case (e, ir.EffectNone) => e
        
        case (ir.EffectAny, _) => ir.EffectAny
        case (_, ir.EffectAny) => ir.EffectAny
        
        case (ir.EffectUnion(es_0), ir.EffectUnion(es_1)) => ir.EffectUnion(es_0 ++ es_1)
        case (ir.EffectUnion(es), e) => ir.EffectUnion(e :: es)
        case (e, ir.EffectUnion(es)) => ir.EffectUnion(e :: es)
        
        case _ => ir.EffectUnion(List(e_0, e_1))
    }
    
    /// Bounds a point r: ∀p∈ps.p→r, ∀q∈qs.r→q
    sealed case class Interval(ps: List[Path], qs: List[Path])
    
    sealed abstract class ActionKind
    case object Rd extends ActionKind
    case object Wr extends ActionKind
    
    sealed class Graph(
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
        
        def +(p: Path, q: Path) =
            new Graph(hbs + Pair(p, q))
        
        def contains(p: Path, q: Path) =
            tc.contains(p, q)
    }
    object Graph {
        val empty = new Graph(Util.MultiMap.empty[Path, Path])
    }
    
    sealed case class TcEnv(
        lvs: Map[ir.VarName, ir.WcTypeRef],
        min: Graph
    )
    
    case class IrError(msg: String, args: Any*) 
    extends RuntimeException
    
    val lv_end = ir.VarName("end")
    
    val p_this = ir.VarName("this").path
    val p_new = ir.VarName("new").path
    
    val p_schedule = ir.VarName("schedule").path
    val p_current = ir.VarName("current").path
    val p_root = ir.VarName("root").path

    val p_locked = ir.VarName("locked").path
    val p_readOnly = ir.VarName("readOnly").path
    
    val f_start = ir.FieldName("start")
    val f_end = ir.FieldName("end")
    
    val m_run = ir.MethodName("run")
    
    val c_object = ir.ClassName("java.lang.Object")
    val c_void = ir.ClassName("java.lang.Void")
    val c_interval = ir.ClassName("ch.ethz.intervals.Interval")
    val c_point = ir.ClassName("ch.ethz.intervals.Point")
    val c_guard = ir.ClassName("ch.ethz.intervals.Guard")    
    val c_task = ir.ClassName("ch.ethz.intervals.Task")
    
    val t_object = ir.TypeRef(c_object, List(), List())
    val t_void = ir.TypeRef(c_void, List(), List())
    val wt_interval = ir.WcTypeRef(c_interval, List(ir.WcUnkPath), List())
    val wt_point = ir.WcTypeRef(c_point, List(ir.WcUnkPath), List())
    val wt_guard = ir.WcTypeRef(c_guard, List(ir.WcUnkPath), List())
    val wt_task = ir.WcTypeRef(c_task, List(), List(ir.Over(m_run, lv_end, ir.EffectAny)))
    
    /// Returns an interval p.start -> p.end
    def startEnd(p: ir.Path): ir.Interval =
        Interval(List(p + f_start), List(p + f_end))
            
    val cds_default = List(
        ClassDecl(
            /* Name:    */ c_object,
            /* Ghosts:  */ List(),
            /* Extends: */ None,
            /* Fields:  */ List(),
            /* Methods: */ List()
        ),
        ClassDecl(
            /* Name:    */ c_void,
            /* Ghosts:  */ List(),
            /* Extends: */ Some(t_object),
            /* Fields:  */ List(),
            /* Methods: */ List()
        ),
        ClassDecl(
            /* Name:    */ c_interval,
            /* Ghosts:  */ List(
                GhostFieldDecl(FieldName("bound"), wt_point)),
            /* Extends: */ Some(t_object),
            /* Fields:  */ List(
                RealFieldDecl(List(Final), wt_point, f_start, p_readOnly), // XXX bound
                RealFieldDecl(List(Final), wt_point, f_end, p_readOnly)),  // XXX bound
            /* Methods: */ List()
        ),
        ClassDecl(
            /* Name:    */ c_task,
            /* Ghosts:  */ List(),
            /* Extends: */ Some(t_object),
            /* Fields:  */ List(),
            /* Methods: */ List(
                MethodDecl(
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_run, 
                    /* arg:    */ ir.LvDecl(lv_end, wt_point),
                    /* effect: */ ir.EffectAny,
                    /* stmts:  */ List(),
                    /* ex_ret: */ ir.ExprNull
                    ))
        ),
        ClassDecl(
            /* Name:    */ c_point,
            /* Ghosts:  */ List(
                GhostFieldDecl(FieldName("bound"), wt_point)),
            /* Extends: */ Some(t_object),
            /* Fields:  */ List(),
            /* Methods: */ List()
        ),
        ClassDecl(
            /* Name:    */ c_guard,
            /* Ghosts:  */ List(
                GhostFieldDecl(FieldName("parent"), wt_guard)),
            /* Extends: */ Some(t_object),
            /* Fields:  */ List(),
            /* Methods: */ List()
        )
    )
   
}