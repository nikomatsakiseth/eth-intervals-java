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
        ctor: MethodDecl,
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
        reqs: List[Req],
        stmts: List[Stmt],
        ex_ret: Expr
    ) extends Locatable {
        def msig = at(MethodSig(args, reqs, wt_ret), srcLoc)
        
        override def toString =
            "%s %s(%s)%s".format(
                wt_ret, name, ", ".join(args), 
                "".join(" ", reqs))
    }
    
    sealed case class MethodSig(
        args: List[LvDecl],
        reqs: List[Req],
        wt_ret: WcTypeRef
    ) extends Locatable {
        override def toString = "(%s _(%s) %s)".format(
            wt_ret, ", ".join(args), "".join("requires ", reqs))
    }
    
    sealed case class HbDecl(p: ir.Path, q: ir.Path) extends Locatable {
        override def toString = "%s hb %s".format(p, q)
    }
    
    sealed case class LockDecl(p: ir.Path) extends Locatable {
        override def toString = "locks %s".format(p)
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
        wt: WcTypeRef,
        name: FieldName,
        guard: Path
    ) extends FieldDecl {
        def isFinal = (guard == p_readOnly)
        def isGhost = false
        
        override def toString = 
            "%s %s guardedBy %s".format(wt, name, guard)
    }
    
    sealed abstract class Stmt extends Locatable
    sealed case class StmtVarDecl(vd: LvDecl, ex: Expr) extends Stmt {
        override def toString = "%s = %s;".format(vd, ex)
    }
    sealed case class StmtAssignField(p: Path, f: FieldName, q: Path) extends Stmt {
        override def toString = "%s->%s = %s;".format(p, f, q)
    }
    sealed case class StmtHb(p: Path, q: Path) extends Stmt {
        override def toString = "%s hb %s;".format(p, q)        
    }
    sealed case class StmtLocks(p: Path, q: Path) extends Stmt {
        override def toString = "%s locks %s;".format(p, q)        
    }
    
    sealed abstract class Expr 
    sealed case class ExprCall(p: Path, m: MethodName, qs: List[Path]) extends Expr {
        override def toString = "%s->%s(%s)".format(p, m, qs)
    }
    sealed case class ExprField(p: Path, f: FieldName) extends Expr {
        override def toString = "%s->%s".format(p, f)
    }
    sealed case class ExprNew(t: TypeRef, qs: List[Path]) extends Expr {
        override def toString = "new %s(%s)".format(t, ", ".join(qs))
    }
    case object ExprNull extends Expr {
        override def toString = "null"
    }
    
    sealed case class WcTypeRef(
        c: ClassName,
        wpaths: List[WcPath]
    ) {
        override def toString = "%s<%s>".format(c, ", ".join(wpaths))
    }
    
    sealed case class TypeRef(
        override val c: ClassName,
        paths: List[Path]
    ) extends WcTypeRef(c, paths)
    
    sealed abstract class WcPath
    sealed case class WcHb(ps: List[Path], qs: List[Path]) extends WcPath {
        override def toString = {
            (ps match { case List() => ""; case _ => ", ".join(ps) + " " }) +
            "hb" + 
            (qs match { case List() => ""; case _ => " " + ", ".join(qs) })
        }
    }
    sealed case class WcHbEq(ps: List[Path], qs: List[Path]) extends WcPath {
        override def toString = {
            (ps match { case List() => ""; case _ => ", ".join(ps) + " " }) +
            "hbeq" + 
            (qs match { case List() => ""; case _ => " " + ", ".join(qs) })
        }
    }
        
    sealed case class Path(
        lv: VarName, rev_fs: List[FieldName] // Fields stored in reverse order!
    ) extends WcPath {
        def fs = rev_fs.reverse
        def +(f: ir.FieldName) = Path(lv, f :: rev_fs)
        def ++(fs: List[ir.FieldName]) = fs.foldLeft(this)(_ + _)
        
        override def toString = lv.name + ".".join(".", fs)
    }
    
    sealed abstract class Req
    sealed case class ReqHb(ps: List[Path], qs: List[Path]) extends Req {
        override def toString = "%s hb %s".format(", ".join(ps), ", ".join(qs))
    }
    sealed case class ReqHbEq(ps: List[Path], qs: List[Path]) extends Req {
        override def toString = "%s hbeq %s".format(", ".join(ps), ", ".join(qs))
    }
    sealed case class ReqEq(p: Path, q: Path) extends Req {
        override def toString = "%s == %s".format(p, q)
    }
    sealed case class ReqLocks(p: Path, qs: List[Path]) extends Req {
        override def toString = "%s locks %s".format(p, ", ".join(qs))
    }
    
    sealed class Relation(
        rels: Util.MultiMap[Path, Path],
        transitive: Boolean,
        reflexive: Boolean
    ) {
        private var cachedTc: Option[Util.MultiMap[Path, Path]] = 
            None        
        private def tc = {
            cachedTc match {
                case Some(t) => t
                case None =>
                    val t = Util.transitiveClosure(rels)
                    cachedTc = Some(t)
                    t
            }
        }
        
        def +(p: Path, q: Path) =
            new Relation(rels + Pair(p, q), transitive, reflexive)
            
        def apply(p: Path): Set[Path] = {
            val base =
                if(transitive)
                    tc(p)
                else
                    rels(p)
            if(reflexive)
                base + p
            else
                base
        }
        
        def contains(p: Path, q: Path) =
            if(reflexive && p == q)
                true
            else if(transitive)
                tc.contains(p, q)
            else
                rels.contains(p, q)
    }
    object Relation {
        private val emptyMap = Util.MultiMap.empty[Path, Path]
        val empty = new Relation(emptyMap, false, false)
        val emptyTrans = new Relation(emptyMap, true, false)
        val emptyTransRefl = new Relation(emptyMap, true, false)
    }
    
    sealed case class TcEnv(
        lvs: Map[ir.VarName, ir.WcTypeRef],
        hb: Relation,
        hbeq: Relation,
        eq: Relation,
        locks: Relation
    )
    
    case class IrError(msg: String, args: Any*) 
    extends RuntimeException {
        override def toString = "%s(%s)".format(msg, ", ".join(args.toList))
    }
    
    val lv_this = ir.VarName("this")
    val lv_new = ir.VarName("new")
    val lv_end = ir.VarName("end")
    val lv_root = ir.VarName("root")
    val lv_ctor = ir.VarName("constructor")
    val lv_mthd = ir.VarName("method")
    val lv_cur = ir.VarName("current")
    val lv_readOnly = ir.VarName("readOnly")
    
    val p_this = lv_this.path
    val p_new = lv_new.path
    val p_root = lv_root.path
    val p_ctor = lv_ctor.path
    val p_mthd = lv_mthd.path
    val p_cur = lv_cur.path
    val p_readOnly = lv_readOnly.path

    val f_creator = ir.FieldName("creator")    
    val f_start = ir.FieldName("start")
    val f_end = ir.FieldName("end")
    
    val m_ctor = ir.MethodName("constructor")
    val m_toString = ir.MethodName("toString")
    val m_run = ir.MethodName("run")
    
    val c_object = ir.ClassName("Object")
    val c_void = ir.ClassName("Void")
    val c_interval = ir.ClassName("Interval")
    val c_point = ir.ClassName("Point")
    val c_lock = ir.ClassName("Lock")    
    val c_string = ir.ClassName("String")
    
    val t_void = ir.TypeRef(c_void, List())
    val t_string = ir.TypeRef(c_void, List())
    val t_interval = ir.TypeRef(c_interval, List())
    val t_point = ir.TypeRef(c_point, List())
    val t_lock = ir.TypeRef(c_lock, List())
    
    val gfd_creator = GhostFieldDecl(f_creator, t_interval)
    val t_objectCreator = ir.TypeRef(c_object, List(gfd_creator.thisPath))
    val t_objectReadOnly = ir.TypeRef(c_object, List(p_readOnly))
    
    val cds_default = List(
        ClassDecl(
            /* Name:    */  c_object,
            /* Ghosts:  */  List(gfd_creator),
            /* Extends: */  None,
            /* Ctor:    */  MethodDecl(
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_ctor, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* stmts:  */ List(),
                    /* ex_ret: */ ir.ExprNull
                    ),
            /* Fields:  */  List(),
            /* Methods: */  List(
                MethodDecl(
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_toString, 
                    /* args:   */ List(),
                    /* reqs:   */ List(ir.ReqHbEq(List(p_cur), List(gfd_creator.thisPath))),
                    /* stmts:  */ List(),
                    /* ex_ret: */ ir.ExprNull
                    ))                
        ),
        ClassDecl(
            /* Name:    */  c_void,
            /* Ghosts:  */  List(),
            /* Extends: */  Some(t_objectReadOnly),
            /* Ctor:    */  MethodDecl(
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_ctor, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* stmts:  */ List(),
                    /* ex_ret: */ ir.ExprNull
                    ),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Name:    */  c_string,
            /* Ghosts:  */  List(),
            /* Extends: */  Some(t_objectReadOnly),
            /* Ctor:    */  MethodDecl(
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_ctor, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* stmts:  */ List(),
                    /* ex_ret: */ ir.ExprNull
                    ),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Name:    */  c_interval,
            /* Ghosts:  */  List(),
            /* Extends: */  Some(t_objectReadOnly),
            /* Ctor:    */  MethodDecl(
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_ctor, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* stmts:  */ List(),
                    /* ex_ret: */ ir.ExprNull
                    ),
            /* Fields:  */  List(
                RealFieldDecl(t_point, f_start, p_readOnly),
                RealFieldDecl(t_point, f_end, p_readOnly)),
            /* Methods: */  List(
                MethodDecl(
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_run, 
                    /* args:   */ List(),
                    /* reqs:   */ List(ir.ReqEq(p_cur, p_this)),
                    /* stmts:  */ List(),
                    /* ex_ret: */ ir.ExprNull
                    ))                
        ),
        ClassDecl(
            /* Name:    */  c_point,
            /* Ghosts:  */  List(),
            /* Extends: */  Some(t_objectReadOnly),
            /* Ctor:    */  MethodDecl(
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_ctor, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* stmts:  */ List(),
                    /* ex_ret: */ ir.ExprNull
                    ),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Name:    */  c_lock,
            /* Ghosts:  */  List(),
            /* Extends: */  Some(t_objectReadOnly),
            /* Ctor:    */  MethodDecl(
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_ctor, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* stmts:  */ List(),
                    /* ex_ret: */ ir.ExprNull
                    ),
            /* Fields:  */  List(),
            /* Methods: */  List()
        )
    )
   
}