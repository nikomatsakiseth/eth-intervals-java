package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.collection.immutable.Stack
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import Util._

    
/*

______________________________________________________________________
Naming Conventions

Naming conventions generally follow the formal typing rules,
although in some cases I elected to use longer names in the code.
Short names begin with a letter indicating the type of the
variable or field:

b -> block id
lv -> local variable name
p,q -> path
tp,tq -> typed path (TeePee)
wp,wq -> wildcard path
f -> field name
m -> method name
c -> class name
cd, md, fd, rfd, gfd -> class, method, field decl, reified field decl, ghost field decl
blk -> block definition
t -> type (TypeRef)
wt -> wildcard type (WcTypeRef)

The type is then optionally followed by an underscore
with a more descriptive tag, like p_r for the receiver path.

______________________________________________________________________
Assumptions

We currently assume but do not check:

- The method CFG is dominated by block 0 
- Every subinterval push dominates and is post dominated by a corresponding subinterval pop
- Every path through a CFG leads to a return or throw statement (unless the method is void in 
    the Java code)
- The fact that return (and to some extent throw) does not have normal successors is encoded in the CFG 
- Super constructors are invoked exactly once, and from another constructor.  The 'this' pointer
  is not referenced before the super constructor.
- Acyclic supertypes
  
Note that all of these properties are either enforced by Java or by the CFG construction
code.

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
    val mutableAttrs = Attrs(ListSet(AttrMutable))
    val interfaceAttrs = Attrs(ListSet(AttrInterface))
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
        pos: Position,
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
        superClasses: List[ClassName],
        ghosts: List[Ghost],
        reqs: List[Req],
        ctors: List[MethodDecl],
        fields: List[FieldDecl],
        methods: List[MethodDecl]
    ) extends Positional {
        override def toString =
            "class %s%s extends %s".format(
                name, "".join(" ", reqs), superClasses.mkString(", "))
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

    sealed abstract class FieldDecl extends Positional {
        val wt: WcTypeRef
        val name: FieldName
        
        def thisPath = name.thisPath
    }
    
    sealed case class GhostFieldDecl(
        wt: WcTypeRef,
        name: FieldName
    ) extends FieldDecl {
        override def toString = "ghost %s %s".format(wt, name)
        
        def ghost = Ghost(name, thisPath)
    }
    
    sealed case class ReifiedFieldDecl(
        as: Attrs,       
        wt: WcTypeRef,
        name: FieldName,
        p_guard: Path
    ) extends FieldDecl {
        override def toString = "%s %s %s requires %s".format(as, wt, name, p_guard)
    }
    
    sealed case class Block(
        args: List[LvDecl],
        stmts: List[Stmt],
        gotos: List[Goto]
    ) {
        override def toString = "block(%s)".format(args.mkString(", "))
    }
    
    sealed case class Goto(
        b: Int,
        ps: List[ir.Path]
    ) extends Positional {
        override def toString = "goto %s(%s);".format(b, ps)
    }
    
    sealed abstract class Stmt extends Positional
    sealed case class StmtSuperCtor(m: ir.MethodName, qs: List[Path]) extends Stmt {
        override def toString = "super %s(%s)".format(m, qs)
    }
    sealed case class StmtSuperCall(x: VarName, m: MethodName, qs: List[Path]) extends Stmt {
        override def toString = "%s = super->%s(%s)".format(x, m, qs)
    }
    sealed case class StmtCall(x: VarName, p: Path, m: MethodName, qs: List[Path]) extends Stmt {
        override def toString = "%s = %s->%s(%s)".format(x, p, m, qs)        
    }
    sealed case class StmtGetField(x: VarName, p: Path, f: FieldName) extends Stmt {
        override def toString = "%s = %s->%s".format(x, p, f)
    }
    sealed case class StmtNew(x: VarName, t: TypeRef, m: ir.MethodName, qs: List[Path]) extends Stmt {
        override def toString = "%s = new %s %s(%s);".format(x, t, m, qs.mkString(", "))
    }
    sealed case class StmtCast(x: VarName, wt: WcTypeRef, q: Path) extends Stmt {
        override def toString = "%s = (%s)%s;".format(x, wt, q)
    }
    sealed case class StmtNull(x: VarName, wt: WcTypeRef) extends Stmt {
        override def toString = "%s = (%s)null;".format(x, wt)
    }
    sealed case class StmtReturn(p: Path) extends Stmt {
        override def toString = "return %s;".format(p)        
    }
    sealed case class StmtSetField(p: Path, f: FieldName, q: Path) extends Stmt {
        override def toString = "%s->%s = %s;".format(p, f, q)
    }
    sealed case class StmtSubintervalPush(x: VarName, addCheckedArglocks: List[Path]) extends Stmt {
        override def toString = "subinterval push %s locks %s".format(x, addCheckedArglocks.mkString(", "))
    }
    sealed case class StmtSubintervalPop(x: VarName) extends Stmt {
        override def toString = "subinterval pop %s".format(x)
    }
    sealed case class StmtHb(p: Path, q: Path) extends Stmt {
        override def toString = "%s hb %s;".format(p, q)        
    }
    sealed case class StmtLocks(p: Path, q: Path) extends Stmt {
        override def toString = "%s locks %s;".format(p, q)        
    }

    def isSuperCtor(s: Stmt) = s match {
        case StmtSuperCtor(_, _) => true
        case _ => false
    }
    
    sealed case class WcGhost(f: FieldName, wp: ir.WcPath) {
        override def toString = "<%s: %s>".format(f, wp)        
    } 
    sealed case class Ghost(
        override val f: FieldName,
        p: Path
    ) extends WcGhost(f, p)
    
    sealed case class WcTypeRef(
        c: ClassName,
        wghosts: List[WcGhost],
        as: Attrs
    ) {
        override def toString = "%s%s%s".format(c, wghosts.mkString(""), as)
        def withAttrs(as: Attrs) = ir.WcTypeRef(c, wghosts, as)
        def owghost(f: FieldName) = wghosts.find(_.f == f).map(_.wp)
        def dependentPaths =
            wghosts.foldLeft(Set.empty[Path]) { (s, g) => g.wp.addDependentPaths(s) }
    }
    
    sealed case class TypeRef(
        override val c: ClassName,
        val ghosts: List[Ghost],
        override val as: Attrs
    ) extends WcTypeRef(c, ghosts, as) {
        override def withAttrs(as: Attrs) = ir.TypeRef(c, ghosts, as)
        def oghost(f: FieldName) = ghosts.find(_.f == f).map(_.p)
        def ctor = withAttrs(as.withCtor)
    }
    
    sealed abstract class WcPath {
        def addDependentPaths(s: Set[Path]): Set[Path]
        
        def dependentOn(p: Path): Boolean =
            addDependentPaths(Set.empty).exists(_.hasPrefix(p))
    }
    sealed case class WcReadableBy(lp: List[Path]) extends WcPath {
        def addDependentPaths(s: Set[Path]) = s ++ lp

        override def toString = "readableBy " + lp.mkString(", ")
    }
    sealed case class WcWritableBy(lp: List[Path]) extends WcPath {
        def addDependentPaths(s: Set[Path]) = s ++ lp

        override def toString = "writableBy " + lp.mkString(", ")
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
        ps_cur: List[ir.Path],                  // current interval
        wt_ret: ir.WcTypeRef,                   // return type of current method
        perm: Map[ir.VarName, ir.TeePee],       // permanent equivalences, hold for duration of method
        temp: Map[ir.Path, ir.Path],            // temporary equivalences, cleared on method call
        ps_invalidated: Set[ir.Path],           // p is current invalid and must be reassigned                
        readable: IntransitiveRelation[Path],   // (p, q) means guard p is readable by interval q
        writable: IntransitiveRelation[Path],   // (p, q) means guard p is writable by interval q
        hb: TransitiveRelation[Path],           // (p, q) means interval p hb interval q
        subinterval: TransitiveRelation[Path],  // (p, q) means interval p is a subinterval of interval q
        locks: IntransitiveRelation[Path]       // (p, q) means interval p locks lock q        
    ) {
        def withCurrent(ps_cur: List[ir.Path]) = TcEnv(ps_cur, wt_ret, perm, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
        def withRet(wt_ret: ir.WcTypeRef) = TcEnv(ps_cur, wt_ret, perm, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
        def withPerm(perm: Map[ir.VarName, ir.TeePee]) = TcEnv(ps_cur, wt_ret, perm, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
        def withTemp(temp: Map[ir.Path, ir.Path]) = TcEnv(ps_cur, wt_ret, perm, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
        def withInvalidated(ps_invalidated: Set[ir.Path]) = TcEnv(ps_cur, wt_ret, perm, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
        def withReadable(readable: IntransitiveRelation[Path]) = TcEnv(ps_cur, wt_ret, perm, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
        def withWritable(writable: IntransitiveRelation[Path]) = TcEnv(ps_cur, wt_ret, perm, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
        def withHb(hb: TransitiveRelation[Path]) = TcEnv(ps_cur, wt_ret, perm, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
        def withSubinterval(subinterval: TransitiveRelation[Path]) = TcEnv(ps_cur, wt_ret, perm, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
        def withLocks(locks: IntransitiveRelation[Path]) = TcEnv(ps_cur, wt_ret, perm, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
        
        def +(env: TcEnv) =
            withTemp(temp ++ env.temp).
            withReadable(readable + env.readable).
            withWritable(writable + env.writable).
            withHb(hb + env.hb).
            withSubinterval(subinterval + env.subinterval).
            withLocks(locks + env.locks)
        
        // Intersecting two environments produces an environment with the
        // "worst-case" assumptions of both.
        def **(env2: TcEnv) =
            this
                .withTemp(temp ** env2.temp)
                .withInvalidated(ps_invalidated ++ env2.ps_invalidated)
                .withReadable(readable ** env2.readable)
                .withWritable(writable ** env2.writable)
                .withHb(hb ** env2.hb)
                .withSubinterval(subinterval ** env2.subinterval)
                .withLocks(locks ** env2.locks)        
    }
    
    object Env {
        val empty = ir.TcEnv(
            List(),
            t_void,
            Map(),
            Map(),
            ListSet.empty,
            IntransitiveRelation.empty,
            IntransitiveRelation.empty,
            TransitiveRelation.empty,
            TransitiveRelation.empty,
            IntransitiveRelation.empty
        )
        
        def intersect(envs: List[ir.TcEnv]) = envs match {
            case List() => empty
            case hd :: tl => tl.foldLeft(hd)(_ ** _)
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
    val lv_ghost = ir.VarName("ghost")
    
    val p_this = lv_this.path
    val p_new = lv_new.path
    val p_root = lv_root.path
    val p_mthd = lv_mthd.path
    val p_cur = lv_cur.path
    val p_ghost = lv_ghost.path

    val f_creator = ir.FieldName("creator")    
    val f_start = ir.FieldName("start")
    val f_ctor = ir.FieldName("constructor")
    val f_end = ir.FieldName("end")
    val f_super = ir.FieldName("super")
    
    val m_init = ir.MethodName("<init>")
    val m_toString = ir.MethodName("toString")
    val m_run = ir.MethodName("run")
    
    val c_object = ir.ClassName("Object")
    val c_void = ir.ClassName("Void")
    val c_interval = ir.ClassName("Interval")
    val c_guard = ir.ClassName("Guard")
    val c_point = ir.ClassName("Point")
    val c_lock = ir.ClassName("Lock")    
    val c_string = ir.ClassName("String")
    
    val t_void = ir.TypeRef(c_void, List(), ir.noAttrs)
    val t_string = ir.TypeRef(c_string, List(), ir.noAttrs)
    val t_interval = ir.TypeRef(c_interval, List(), ir.noAttrs)
    val t_point = ir.TypeRef(c_point, List(), ir.noAttrs)
    val t_lock = ir.TypeRef(c_lock, List(), ir.noAttrs)

    // Synthetic ghost fields .ctor and .super:
    val gfd_ctor = GhostFieldDecl(t_interval, f_ctor)
    val p_ctor = gfd_ctor.thisPath
    val gfd_super = GhostFieldDecl(t_interval, f_super)
    val p_super = gfd_super.thisPath
    
    // Ghost field creator, declared on root type Object:
    val gfd_creator = GhostFieldDecl(t_interval, f_creator)
    val p_this_creator = gfd_creator.thisPath
    val t_objectCreator = ir.TypeRef(c_object, List(gfd_creator.ghost), ir.noAttrs)
    val t_objectCtor = ir.TypeRef(c_object, List(gfd_ctor.ghost), ir.noAttrs)
    val t_object = ir.TypeRef(c_object, List(), ir.noAttrs)
    val t_guard = ir.TypeRef(c_guard, List(), ir.noAttrs)
    
    val md_ctor_interface = 
        MethodDecl(
            /* attrs:  */ ctorAttrs,
            /* wt_ret: */ t_void, 
            /* name:   */ m_init, 
            /* args:   */ List(),
            /* reqs:   */ List(),
            /* blocks: */ Array(
                Block(
                    /* args:  */ List(),
                    /* stmts: */ List(ir.StmtSuperCtor(m_init, List())),
                    /* goto:  */ List()
                )
            )
        )
    
    val cds_default = List(
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_object,
            /* Extends: */  List(),
            /* Ghosts:  */  List(),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_init, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(m_init, List())),
                            /* goto:  */ List()
                        )
                    )
                )),
            /* Fields:  */  List(
                    ir.gfd_creator
                ),
            /* Methods: */  List(
                MethodDecl(
                    /* attrs:  */ noAttrs,
                    /* wt_ret: */ t_string, 
                    /* name:   */ m_toString, 
                    /* args:   */ List(),
                    /* reqs:   */ List(ir.ReqReadableBy(List(gfd_creator.thisPath), List(p_mthd))),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(),
                            /* goto:  */ List()
                        )
                    )
                )
            )
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_void,
            /* Extends: */  List(c_object),
            /* Ghosts:  */  List(Ghost(f_creator, p_ctor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_init, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(m_init, List())),
                            /* goto:  */ List()
                        )
                    )
                )),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_string,
            /* Extends: */  List(c_object),
            /* Ghosts:  */  List(Ghost(f_creator, p_ctor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_init, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(m_init, List())),
                            /* goto:  */ List()
                        )
                    )
                )),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  interfaceAttrs,
            /* Name:    */  c_guard,
            /* Extends: */  List(c_object),
            /* Ghosts:  */  List(),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_init, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(m_init, List())),
                            /* goto:  */ List()
                        )
                    )
                )),
            /* Fields:  */  List(
                ReifiedFieldDecl(noAttrs, t_point, f_start, gfd_ctor.thisPath),
                ReifiedFieldDecl(noAttrs, t_point, f_end, gfd_ctor.thisPath)),
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
                            /* goto:  */ List()
                        )
                    )
                )
            )            
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_interval,
            /* Extends: */  List(c_object, c_guard),
            /* Ghosts:  */  List(Ghost(f_creator, p_ctor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_init, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(m_init, List())),
                            /* goto:  */ List()
                        )
                    )
                )),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_point,
            /* Extends: */  List(c_object),
            /* Ghosts:  */  List(Ghost(f_creator, p_ctor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_init, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(m_init, List())),
                            /* goto:  */ List()
                        )
                    )
                )),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_lock,
            /* Extends: */  List(c_object, c_guard),
            /* Ghosts:  */  List(Ghost(f_creator, p_ctor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_init, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(m_init, List())),
                            /* goto:  */ List()
                        )
                    )
                )),
            /* Fields:  */  List(),
            /* Methods: */  List()
        )
    )
   
}