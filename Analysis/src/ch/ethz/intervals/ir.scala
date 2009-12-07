package ch.ethz.intervals

import scala.collection.immutable.Set
import Util._

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
        
        override def toString =
            "class %s<%s> extends %s".format(
                name, ", ".join(ghosts), superType)        
    }
    
    sealed case class MethodDecl(
        wt_ret: WcTypeRef,
        name: MethodName,
        args: List[LvDecl],
        e: Effect,
        stmts: List[Stmt],
        ex_ret: Expr
    ) extends Locatable {
        def msig = at(MethodSig(e, args, wt_ret), srcLoc)
        
        override def toString =
            "%s %s(%s) %s".format(wt_ret, name, ", ".join(args), e)
    }
    
    sealed case class MethodSig(
        e: Effect,
        args: List[LvDecl],
        wt_ret: WcTypeRef
    ) extends Locatable {
        override def toString = "(%s _(%s) %s)".format(wt_ret, ", ".join(args), e)
    }
    
    sealed case class LvDecl(
        name: VarName,
        wt: WcTypeRef
    ) {
        override def toString = "%s %s".format(wt, name)
    }

    sealed abstract class FieldDecl extends Locatable {
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
        
        override def toString = "%s %s".format(wt, name)
    }
    
    sealed case class RealFieldDecl(
        mods: List[Modifier],
        wt: WcTypeRef,
        name: FieldName,
        guard: Path
    ) extends FieldDecl {
        def isFinal = mods.contains(Final)
        def isGhost = false
        
        override def toString = 
            "%s%s %s guardedBy %s".format(" ".join("", mods, " "), wt, name, guard)
    }
    
    sealed abstract class Stmt extends Locatable
    sealed case class StmtVarDecl(vd: LvDecl, ex: Expr) extends Stmt {
        override def toString = "%s = %s;".format(vd, ex)
    }
    sealed case class StmtAssignField(p: Path, f: FieldName, q: Path) extends Stmt {
        override def toString = "%s->%s = %s;".format(p, f, q)
    }
    sealed case class StmtAddHb(p: Path, q: Path) extends Stmt {
        override def toString = "add %s->%s;".format(p, q)        
    }
    
    sealed abstract class Expr extends Locatable
    sealed case class ExprCall(p: Path, m: MethodName, qs: List[Path]) extends Expr {
        override def toString = "%s->%s(%s)".format(p, m, qs)
    }
    sealed case class ExprField(p: Path, f: FieldName) extends Expr {
        override def toString = "%s->%s".format(p, f)
    }
    sealed case class ExprNew(t: TypeRef) extends Expr {
        override def toString = "new %s()".format(t)
    }
    sealed case class ExprNewInterval(bound: Path, task: Path, guards: List[Path]) extends Expr {
        override def toString = "interval %s %s (%s)".format(bound, task, ", ".join(guards))
    }
    sealed case class ExprNewGuard(p: Path) extends Expr {
        override def toString = "guard %s".format(p)
    }
    case object ExprNull extends Expr {
        override def toString = "null"
    }
    
    sealed case class WcTypeRef(
        c: ClassName,
        wpaths: List[WcPath],
        overs: List[Over]
    ) {
        override def toString = "%s<%s>%s".format(c, ", ".join(wpaths), "".join(overs))
    }
    
    sealed case class TypeRef(
        override val c: ClassName,
        paths: List[Path],
        override val overs: List[Over]
    ) extends WcTypeRef(c, paths, overs)
    
    sealed abstract class WcPath
    case object WcUnkPath extends WcPath {
        override def toString = "?"
    }
    
    sealed case class Path(
        lv: VarName, rev_fs: List[FieldName] // Fields stored in reverse order!
    ) extends WcPath {
        def fs = rev_fs.reverse
        def +(f: ir.FieldName) = Path(lv, f :: rev_fs)
        def ++(fs: List[ir.FieldName]) = fs.foldLeft(this)(_ + _)
        
        override def toString = lv.name + ".".join(".", fs)
    }
    
    sealed case class Over(
        m: MethodName,
        args: List[VarName],
        e: Effect
    ) {
        override def toString = "[%s(%s)=%s]".format(m, ", ".join(args), e)
    }
    
    sealed abstract class Effect
    sealed case class EffectInterval(i: Interval, e: ir.Effect) extends Effect {
        override def toString = "%s:%s".format(i, e)
    }
    sealed case class EffectLock(ps: List[Path], e: ir.Effect) extends Effect {
        override def toString = "(%s)/%s".format(", ".join(ps), e)
    }
    sealed case class EffectMethod(p: Path, m: MethodName, qs: List[Path]) extends Effect {
        override def toString = "%s->%s(%s)".format(p, m, ", ".join(qs))
    }
    sealed case class EffectFixed(k: ActionKind, p: Path) extends Effect {
        override def toString = "%s(%s)".format(k, p)
    }
    sealed case class EffectUnion(es: List[ir.Effect]) extends Effect {
        override def toString = ", ".join("(", es, ")")
    }
    case object EffectNone extends Effect {
        override def toString = "0"
    }
    case object EffectAny extends Effect {
        override def toString = "?"
    }

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
    sealed case class Interval(ps: List[Path], qs: List[Path]) {
        override def toString = 
            "(%s-%s)".format(" ".join(ps), " ".join(qs))
    }
    
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
    extends RuntimeException {
        override def toString = "%s(%s)".format(msg, ", ".join(args.toList))
    }
    
    val lv_end = ir.VarName("end")
    val lv_this = ir.VarName("this")
    
    val p_this = lv_this.path
    val p_new = ir.VarName("new").path
    
    val p_schedule = ir.VarName("schedule").path
    val p_current = ir.VarName("current").path
    val p_root = ir.VarName("root").path

    val p_locked = ir.VarName("locked").path
    val p_readOnly = ir.VarName("readOnly").path
    
    val f_start = ir.FieldName("start")
    val f_end = ir.FieldName("end")
    
    val m_run = ir.MethodName("run")
    
    val c_object = ir.ClassName("Object")
    val c_void = ir.ClassName("Void")
    val c_interval = ir.ClassName("Interval")
    val c_point = ir.ClassName("Point")
    val c_guard = ir.ClassName("Guard")    
    val c_task = ir.ClassName("Task")
    
    val t_object = ir.TypeRef(c_object, List(), List())
    val t_void = ir.TypeRef(c_void, List(), List())
    val wt_interval = ir.WcTypeRef(c_interval, List(ir.WcUnkPath), List())
    val wt_point = ir.WcTypeRef(c_point, List(ir.WcUnkPath), List())
    val wt_guard = ir.WcTypeRef(c_guard, List(ir.WcUnkPath), List())
    val wt_task = ir.WcTypeRef(c_task, List(), List(ir.Over(m_run, List(lv_end), ir.EffectAny)))
    
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
                    /* arg:    */ List(ir.LvDecl(lv_end, wt_point)),
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