package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.util.parsing.input.Positional
import Util._

    
/*

______________________________________________________________________
Naming Conventions

Naming conventions generally follow the formal typing rules,
although in some cases I elected to use longer names in the code.
Short names begin with a letter indicating the type of the
variable or field:

b -> block id
x -> local variable name
p,q -> path
tp,tq -> typed path (TeePee)
wp,wq -> wildcard path
f -> field name
m -> method name
c -> class name
cd, md, fd -> class, method, field decl
blk -> block definition
t -> type (TypeRef)
wt -> wildcard type (WcTypeRef)

The type is then optionally followed by an underscore
with a more descriptive tag, like p_r for the receiver path.

To indicate lists, options, and sets we mimic the scale
syntax and use an "l", "o", or "s" prefix: lp_r, op_r, sp_r.
In practice, however, lists and sets are often denoted using an "s" suffix
instead: "ps", "qs", although that convention probably ought to be deprecated.

______________________________________________________________________
Control-Flow Graph Assumptions

We assumed that the method control-flow graph is dominated by block 0
and post-dominated by block n-1.  We further assume that every path
leads to a return or throw statement (unless the method is void in the Java code); 
we do not treat return statements specially with respect to control-flow.  
Instead, we assume that blocks with returns or throws end immediately with an
edge to block n-1.  These assumptions are currently not verified.

*/

object ir {
    
    // ______________________________________________________________________
    // Attributes attached to paths, types, methods
    
    sealed abstract class Attr(c: String, val adj: String) {
        override def toString = c
    }
    
    // Attrs for class definitions:
    case object AttrInterface extends Attr("i", "interface")

    // Attrs for types:
    case object AttrCtor extends Attr("c", "constructor")
    
    // Attrs for paths:
    case object AttrGhost extends Attr("g", "ghost")
    case object AttrMutable extends Attr("m", "mutable")
    
    sealed case class Attrs(private val s: Set[Attr]) {
        def +(a: Attr) = Attrs(s + a)
        
        def interface = s.contains(AttrInterface)
        def withInterface = this + AttrInterface
        
        def ctor = s.contains(AttrCtor)
        def withCtor = this + AttrCtor
        
        def ghost = s.contains(AttrGhost)
        def withGhost = this + AttrGhost
        
        def mutable = s.contains(AttrMutable)
        def withMutable = this + AttrMutable
        
        def diff(as: Attrs): Set[Attr] = s -- as.s
        
        override def toString = "{%s}".format(s.mkString(""))
    }
    val noAttrs = Attrs(ListSet.empty)
    val ctorAttrs = Attrs(ListSet(AttrCtor))
    val ghostAttrs = Attrs(ListSet(AttrGhost))
    val allPathAttrs = Attrs(ListSet(AttrGhost, AttrMutable))
    
    // ______________________________________________________________________
    // Names of variables, fields, methods, classes
    
    sealed abstract class Name(name: String) {
        override def toString = name
    }
    case class VarName(name: String) extends Name(name) {
        def path = ir.Path(this, List())
        def +(f: FieldName) = path + f
        def ++(fs: List[FieldName]) = path ++ fs
    }
    case class FieldName(name: String) extends Name(name) {
        def thisPath = p_this + this
    }
    case class MethodName(name: String) extends Name(name)
    case class ClassName(name: String) extends Name(name)
    
    // ______________________________________________________________________
    // Error reporting
    
    sealed case class Error(
        loc: Positional,
        msg: String,
        args: List[String]
    ) {
        override def toString = "%s(%s)".format(msg, args.mkString(", "))
    }
    
    // ______________________________________________________________________
    // Abstract syntax tree
    
    sealed case class ClassDecl(
        attrs: Attrs,       
        name: ClassName,
        ghosts: List[GhostDecl],
        superTypes: List[TypeRef],
        reqs: List[Req],
        ctor: MethodDecl,
        fields: List[FieldDecl],
        methods: List[MethodDecl]
    ) extends Positional {
        def thisTref: ir.TypeRef = thisTref(noAttrs)
        def thisTref(as: Attrs): ir.TypeRef = TypeRef(name, ghosts.map(_.thisPath), as)
        
        override def toString =
            "class %s<%s>%s extends %s".format(
                name, ghosts.mkString(", "), "".join(" ", reqs), superTypes.mkString(", "))
    }
    
    sealed case class MethodDecl(
        attrs: Attrs,       
        wt_ret: WcTypeRef,
        name: MethodName,
        args: List[LvDecl],
        reqs: List[Req],
        blocks: Array[Block] // See notes on CFG structure at the top of this file.
    ) extends Positional {
        def msig(t_rcvr: TypeRef) = MethodSig(t_rcvr, attrs, args, reqs, wt_ret)
        
        override def toString =
            "%s %s %s(%s)%s".format(
                attrs, wt_ret, name, args.mkString(", "), "".join(" ", reqs))
    }
    
    sealed case class MethodSig(
        t_rcvr: TypeRef,
        as: Attrs,
        args: List[LvDecl],
        reqs: List[Req],
        wt_ret: WcTypeRef
    ) {
        override def toString = "(%s %s (%s)::_(%s)%s)".format(
            as, wt_ret, t_rcvr, args.mkString(", "), reqs.mkString(""))            
    }
    
    sealed case class LvDecl(
        name: VarName,
        wt: WcTypeRef
    ) {
        override def toString = "%s %s".format(wt, name)
    }

    sealed case class GhostDecl(
        wt: WcTypeRef,
        name: FieldName
    ) extends Positional {
        def thisPath = name.thisPath
        override def toString = "%s %s".format(wt, name)
    }
    
    sealed case class FieldDecl(
        as: Attrs,       
        wt: WcTypeRef,
        name: FieldName,
        p_guard: Path
    ) extends Positional {
        def thisPath = name.thisPath
        override def toString = 
            "%s %s %s requires %s".format(as, wt, name, p_guard)
    }
    
    sealed case class Block(
        args: List[LvDecl],
        stmts: List[Stmt],
        succs: List[Succ]
    ) {
        override def toString = "block(%s)".format(args.mkString(", "))
    }
    
    sealed case class Succ(
        b: Int,
        ps: List[ir.Path]
    ) extends Positional {
        override def toString = "succ %s(%s);".format(b, ps)
    }
    
    sealed abstract class Stmt extends Positional
    sealed case class StmtSuperCall(vd: LvDecl, m: MethodName, qs: List[Path]) extends Stmt {
        override def toString = "%s = super->%s(%s)".format(vd, m, qs)
    }
    sealed case class StmtSuperCtor(qs: List[Path]) extends Stmt {
        override def toString = "super(%s)".format(qs)
    }
    sealed case class StmtCall(vd: LvDecl, p: Path, m: MethodName, qs: List[Path]) extends Stmt {
        override def toString = "%s = %s->%s(%s)".format(vd, p, m, qs)        
    }
    sealed case class StmtGetField(vd: LvDecl, p: Path, f: FieldName) extends Stmt {
        override def toString = "%s = %s->%s".format(vd, p, f)
    }
    sealed case class StmtNew(x: VarName, t: TypeRef, qs: List[Path]) extends Stmt {
        override def toString = "%s = new %s(%s);".format(x, t, qs.mkString(", "))
    }
    sealed case class StmtNull(vd: LvDecl) extends Stmt {
        override def toString = "%s = null;".format(vd)
    }
    sealed case class StmtReturn(p: Path) extends Stmt {
        override def toString = "return %s;".format(p)        
    }
    sealed case class StmtSetField(p: Path, f: FieldName, q: Path) extends Stmt {
        override def toString = "%s->%s = %s;".format(p, f, q)
    }
    sealed case class StmtSubinterval(x: VarName, lp_locks: List[Path], stmts: List[Stmt]) extends Stmt {
        override def toString = "subinterval %s locks %s {...}".format(x, lp_locks.mkString(", "))
    }
    sealed case class StmtHb(p: Path, q: Path) extends Stmt {
        override def toString = "%s hb %s;".format(p, q)        
    }
    sealed case class StmtLocks(p: Path, q: Path) extends Stmt {
        override def toString = "%s locks %s;".format(p, q)        
    }
    
    sealed case class WcTypeRef(
        c: ClassName,
        wpaths: List[WcPath],
        as: Attrs
    ) {
        override def toString = "%s<%s>%s".format(c, wpaths.mkString(", "), as)
        def withAttrs(as: Attrs) = ir.WcTypeRef(c, wpaths, as)
        def dependentPaths =
            wpaths.foldLeft(Set.empty[Path]) { (s, wp) => wp.addDependentPaths(s) }
    }
    
    sealed case class TypeRef(
        override val c: ClassName,
        paths: List[Path],
        override val as: Attrs
    ) extends WcTypeRef(c, paths, as) {
        override def withAttrs(as: Attrs) = ir.TypeRef(c, paths, as)
        def ctor = TypeRef(c, paths, as.withCtor)
    }
    
    sealed abstract class WcPath {
        def addDependentPaths(s: Set[Path]): Set[Path]
        
        def dependentOn(p: Path): Boolean =
            addDependentPaths(Set.empty).exists(_.hasPrefix(p))
    }
    sealed case class WcReadableBy(lp: List[Path]) extends WcPath {
        def addDependentPaths(s: Set[Path]) = s ++ lp

        override def toString = lp.mkString(", ") + " readable"
    }
    sealed case class WcWritableBy(lp: List[Path]) extends WcPath {
        def addDependentPaths(s: Set[Path]) = s ++ lp

        override def toString = lp.mkString(", ") + " writable"
    }
    sealed case class WcHb(lp: List[Path], lq: List[Path]) extends WcPath {
        def addDependentPaths(s: Set[Path]) = s ++ lp ++ lq

        override def toString = {
            (lp match { case List() => ""; case _ => lp.mkString(", ") + " " }) +
            "hb" + 
            (lq match { case List() => ""; case _ => " " + lq.mkString(", ") })
        }
    }
    sealed case class WcLocks(lp: List[Path]) extends WcPath {
        def addDependentPaths(s: Set[Path]) = s ++ lp

        override def toString = "locks %s".format(lp.mkString(", "))
    }
    sealed case class WcLockedBy(lp: List[Path]) extends WcPath {
        def addDependentPaths(s: Set[Path]) = s ++ lp

        override def toString = "%s locks".format(lp.mkString(", "))
    }
        
    sealed case class Path(
        lv: VarName, rev_fs: List[FieldName] // Fields stored in reverse order!
    ) extends WcPath {
        def fs = rev_fs.reverse
        def +(f: ir.FieldName) = Path(lv, f :: rev_fs)
        def ++(fs: List[ir.FieldName]) = fs.foldLeft(this)(_ + _)
        
        def addDependentPaths(s: Set[ir.Path]) = s + this
        
        def hasPrefix(p: ir.Path) =
            lv == p.lv && rev_fs.endsWith(p.rev_fs)
        
        def start = this + f_start
        def end = this + f_end        
        override def toString = (lv :: fs).mkString(".")
    }
    
    /// A TeePee is a typed path.
    sealed case class TeePee(
        wt: ir.WcTypeRef, p: ir.Path, as: Attrs
    ) {
        def isConstant: Boolean = !as.mutable
        
        override def toString = "[%s: %s %s]".format(p, wt, as)
    }
    
    sealed abstract class Req extends Positional
    sealed case class ReqWritableBy(lp: List[Path], lq: List[Path]) extends Req {
        override def toString = "requires %s writable by %s".format(lp.mkString(", "), lq.mkString(", "))
    }
    sealed case class ReqReadableBy(lp: List[Path], lq: List[Path]) extends Req {
        override def toString = "requires %s readable by %s".format(lp.mkString(", "), lq.mkString(", "))
    }
    sealed case class ReqSubintervalOf(lp: List[Path], lq: List[Path]) extends Req {
        override def toString = "requires %s subinterval of %s".format(lp.mkString(", "), lq.mkString(", "))
    }
    sealed case class ReqHb(lp: List[Path], lq: List[Path]) extends Req {
        override def toString = "requires %s hb %s".format(lp.mkString(", "), lq.mkString(", "))
    }
    
    sealed case class TcEnv(
        p_cur: ir.Path,                        // current interval
        wt_ret: ir.WcTypeRef,                  // return type of current method
        perm: Map[ir.Path, ir.TeePee],         // permanent equivalences, hold for duration of method
        temp: Map[ir.Path, ir.Path],           // temporary equivalences, cleared on method call
        lp_invalidated: Set[ir.Path],          // p is current invalid and must be reassigned        
        readable: IntransitiveRelation[Path],  // (p, q) means guard p is readable by interval q
        writable: IntransitiveRelation[Path],  // (p, q) means guard p is writable by interval q
        hb: TransitiveRelation[Path],          // (p, q) means interval p hb interval q
        subinterval: TransitiveRelation[Path], // (p, q) means interval p is a subinterval of interval q
        locks: IntransitiveRelation[Path]      // (p, q) means interval p locks lock q
    ) {
        def +(env: TcEnv) = {
            TcEnv(
                p_cur,
                wt_ret,
                perm,
                temp,
                lp_invalidated,
                readable + env.readable,
                writable + env.writable,
                hb + env.hb,
                subinterval + env.subinterval,
                locks + env.locks
            )
        }
    }
    
    case class IrError(msg: String, args: Any*) 
    extends RuntimeException {
        override def toString = "%s(%s)".format(msg, args.mkString(", "))        
    }
    
    val lv_this = ir.VarName("this")
    val lv_new = ir.VarName("new")
    val lv_end = ir.VarName("end")
    val lv_root = ir.VarName("root")
    val lv_mthd = ir.VarName("method")
    val lv_cur = ir.VarName("current")
    
    val p_this = lv_this.path
    val p_new = lv_new.path
    val p_root = lv_root.path
    val p_mthd = lv_mthd.path
    val p_cur = lv_cur.path

    val f_creator = ir.FieldName("creator")    
    val f_start = ir.FieldName("start")
    val f_ctor = ir.FieldName("constructor")
    val f_end = ir.FieldName("end")
    val f_super = ir.FieldName("super")
    
    val m_ctor = ir.MethodName("constructor")
    val m_toString = ir.MethodName("toString")
    val m_run = ir.MethodName("run")
    
    val c_object = ir.ClassName("Object")
    val c_void = ir.ClassName("Void")
    val c_interval = ir.ClassName("Interval")
    val c_point = ir.ClassName("Point")
    val c_lock = ir.ClassName("Lock")    
    val c_string = ir.ClassName("String")
    
    val t_void = ir.TypeRef(c_void, List(), ir.noAttrs)
    val t_string = ir.TypeRef(c_string, List(), ir.noAttrs)
    val t_interval = ir.TypeRef(c_interval, List(), ir.noAttrs)
    val t_point = ir.TypeRef(c_point, List(), ir.noAttrs)
    val t_lock = ir.TypeRef(c_lock, List(), ir.noAttrs)
    
    val gd_creator = GhostDecl(t_interval, f_creator)
    val gd_ctor = GhostDecl(t_interval, f_ctor)
    val p_ctor = gd_ctor.thisPath
    val gd_super = GhostDecl(t_interval, f_super)
    val p_super = gd_super.thisPath
    val t_objectCreator = ir.TypeRef(c_object, List(gd_creator.thisPath), ir.noAttrs)
    val t_objectCtor = ir.TypeRef(c_object, List(gd_ctor.thisPath), ir.noAttrs)
    
    val md_ctor_interface = 
        MethodDecl(
            /* attrs:  */ ctorAttrs,
            /* wt_ret: */ t_void, 
            /* name:   */ m_ctor, 
            /* args:   */ List(),
            /* reqs:   */ List(),
            /* blocks: */ Array(
                Block(
                    /* args:  */ List(),
                    /* stmts: */ List(ir.StmtSuperCtor(List())),
                    /* succ:  */ List()
                )
            )
        )
    
    val cds_default = List(
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_object,
            /* Ghosts:  */  List(gd_creator),
            /* Extends: */  List(),
            /* Reqs:    */  List(),
            /* Ctor:    */  MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_ctor, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(List())),
                            /* succ:  */ List()
                        )
                    )
                ),
            /* Fields:  */  List(),
            /* Methods: */  List(
                MethodDecl(
                    /* attrs:  */ noAttrs,
                    /* wt_ret: */ t_string, 
                    /* name:   */ m_toString, 
                    /* args:   */ List(),
                    /* reqs:   */ List(ir.ReqReadableBy(List(gd_creator.thisPath), List(p_mthd))),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(),
                            /* succ:  */ List()
                        )
                    )
                )
            )
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_void,
            /* Ghosts:  */  List(),
            /* Extends: */  List(t_objectCtor),
            /* Reqs:    */  List(),
            /* Ctor:    */  MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_ctor, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(List())),
                            /* succ:  */ List()
                        )
                    )
                ),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_string,
            /* Ghosts:  */  List(),
            /* Extends: */  List(t_objectCtor),
            /* Reqs:    */  List(),
            /* Ctor:    */  MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_ctor, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(List())),
                            /* succ:  */ List()
                        )
                    )
                ),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_interval,
            /* Ghosts:  */  List(),
            /* Extends: */  List(t_objectCtor),
            /* Reqs:    */  List(),
            /* Ctor:    */  MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_ctor, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(List())),
                            /* succ:  */ List()
                        )
                    )
                ),
            /* Fields:  */  List(
                FieldDecl(noAttrs, t_point, f_start, gd_ctor.thisPath),
                FieldDecl(noAttrs, t_point, f_end, gd_ctor.thisPath)),
            /* Methods: */  List(
                MethodDecl(
                    /* attrs:  */ noAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_run, 
                    /* args:   */ List(),
                    /* reqs:   */ List(ir.ReqSubintervalOf(List(p_mthd), List(p_this))),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(),
                            /* succ:  */ List()
                        )
                    )
                )
            )
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_point,
            /* Ghosts:  */  List(),
            /* Extends: */  List(t_objectCtor),
            /* Reqs:    */  List(),
            /* Ctor:    */  MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_ctor, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(List())),
                            /* succ:  */ List()
                        )
                    )
                ),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_lock,
            /* Ghosts:  */  List(),
            /* Extends: */  List(t_objectCtor),
            /* Reqs:    */  List(),
            /* Ctor:    */  MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_ctor, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(List())),
                            /* succ:  */ List()
                        )
                    )
                ),
            /* Fields:  */  List(),
            /* Methods: */  List()
        )
    )
   
}