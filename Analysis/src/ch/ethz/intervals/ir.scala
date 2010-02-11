package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import Util._

    
/*
___ Naming Conventions _______________________________________________

Naming conventions generally follow the formal typing rules,
although in some cases I elected to use longer names in the code.
Short names begin with a letter indicating the type of the
variable or field:

b -> block id
lv -> local variable name
p,q -> path
tp,tq -> typed path (ir.CanonPath)
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

___ Assumptions ______________________________________________________

We currently assume but do not check:

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

    // ___ Positional AST Nodes _____________________________________________
    
    abstract class PositionalAst extends Positional {
        def setDefaultPos(p: Position) {
            if(!this.hasPos)
                setPos(p)
            setDefaultPosOnChildren()
        }
        
        def setDefaultPosOnChildren(): Unit
    }
    
    // ___ Attributes attached to paths, types, methods _____________________
    
    sealed abstract class Attr(c: String, val adj: String) {
        override def toString = c
    }
    
    // Attrs for class definitions:
    case object AttrInterface extends Attr("i", "interface")

    // Attrs for types:
    case object AttrCtor extends Attr("c", "constructor")
    
    sealed case class Attrs(private val s: Set[Attr]) {
        def +(a: Attr) = Attrs(s + a)
        
        def interface = s.contains(AttrInterface)
        def withInterface = this + AttrInterface
        
        def ctor = s.contains(AttrCtor)
        def withCtor = this + AttrCtor
        
        def diff(as: Attrs): Set[Attr] = s -- as.s
        
        def preWords = "".join("", s.map(_.adj), " ")
        def postWords = "".join(" ", s.map(_.adj), "")
        
        override def toString = "{%s}".format(s.mkString(""))
    }
    val noAttrs = Attrs(ListSet.empty)
    val ctorAttrs = Attrs(ListSet(AttrCtor))
    val interfaceAttrs = Attrs(ListSet(AttrInterface))
    val allPathAttrs = Attrs(ListSet(AttrGhost, AttrMutable))

    // ___ Names of variables, fields, methods, classes _____________________   
    
    val quoteRegex = "[^a-zA-Z0-9_]".r
    sealed abstract class Name(name: String) {
        def toIdent =
            quoteRegex.findFirstIn(name) match {
                case None => name
                case Some(_) => "`%s`".format(name)
            }
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
    
    // ___ Error reporting __________________________________________________
    
    sealed case class Error(
        pos: Position,
        msg: String,
        args: Array[String]
    ) {
        override def toString = "%s(%s)".format(msg, args.mkString(", "))
    }
    
    // ___ Declarations in the AST __________________________________________
    
    sealed case class ClassDecl(
        attrs: Attrs,       
        name: ClassName,
        superClasses: List[ClassName],
        ghosts: List[Ghost],
        reqs: List[Req],
        ctors: List[MethodDecl],
        fields: List[FieldDecl],
        methods: List[MethodDecl]
    ) extends PositionalAst {
        def gfds = fields.flatMap {
            case gfd: GhostFieldDecl => List(gfd)
            case _ => List()
        }
        
        def rfds = fields.flatMap {
            case rfd: ReifiedFieldDecl => List(rfd)
            case _ => List()
        }
        
        def setDefaultPosOnChildren() {
            reqs.foreach(_.setDefaultPos(pos))
            ctors.foreach(_.setDefaultPos(pos))
            fields.foreach(_.setDefaultPos(pos))
            methods.foreach(_.setDefaultPos(pos))
        }        
        
        override def toString =
            "%sclass %s%s extends %s%s%s".format(
                attrs.preWords, name, "".join(gfds), superClasses.mkString(", "), "".join(" ", ghosts), "".join(" ", reqs))
    }
    
    sealed case class MethodDecl(
        attrs: Attrs,       
        wt_ret: WcTypeRef,
        name: MethodName,
        args: List[LvDecl],
        reqs: List[Req],
        body: StmtSeq
    ) extends PositionalAst {
        def msig(t_rcvr: TypeRef) = MethodSig(t_rcvr, attrs, args, reqs, wt_ret)
        
        def setDefaultPosOnChildren() {
            reqs.foreach(_.setDefaultPos(pos))
            body.setDefaultPos(pos)
        }
        
        override def toString =
            "%s%s %s(%s)%s".format(
                attrs.preWords, wt_ret, name, args.mkString(", "), "".join(" ", reqs))
    }
    
    sealed case class MethodSig(
        t_rcvr: TypeRef,
        as: Attrs,
        args: List[LvDecl],
        reqs: List[Req],
        wt_ret: WcTypeRef
    ) {
        override def toString = "(%s%s (%s)::_(%s)%s)".format(
            as.preWords, wt_ret, t_rcvr, args.mkString(", "), reqs.mkString(""))            
    }
    
    sealed case class LvDecl(
        name: VarName,
        wt: WcTypeRef
    ) {
        override def toString = "%s %s".format(wt, name)
    }

    sealed abstract class FieldDecl extends PositionalAst {
        val wt: WcTypeRef
        val name: FieldName
        
        def thisPath = name.thisPath
        
        def setDefaultPosOnChildren() {
        }        
    }
    
    sealed case class GhostFieldDecl(
        wt: WcTypeRef,
        name: FieldName
    ) extends FieldDecl {
        override def toString = "<%s %s>".format(wt, name)
        
        def ghost = Ghost(name, thisPath)
    }
    
    sealed case class ReifiedFieldDecl(
        as: Attrs,       
        wt: WcTypeRef,
        name: FieldName,
        p_guard: Path
    ) extends FieldDecl {
        override def toString = "%s%s %s requires %s".format(as.preWords, wt, name, p_guard)
    }
    
    // ______ Leaf Statements _______________________________________________
    sealed abstract class Stmt extends PositionalAst {
        def setDefaultPosOnChildren() { }        
    }
    sealed case class StmtSuperCtor(m: ir.MethodName, qs: List[Path]) extends Stmt {
        override def toString = "super %s(%s)".format(m, qs)
    }
    sealed case class StmtSuperCall(x: VarName, m: MethodName, qs: List[Path]) extends Stmt {
        override def toString = "%s = super->%s(%s)".format(x.toIdent, m, qs)
    }
    sealed case class StmtCheckType(p: ir.Path, wt: ir.WcTypeRef) extends Stmt {
        override def toString = "%s <: %s".format(p, wt)
    }
    sealed case class StmtCall(x: VarName, p: Path, m: MethodName, qs: List[Path]) extends Stmt {
        override def toString = "%s = %s->%s(%s)".format(x.toIdent, p, m, qs)        
    }
    sealed case class StmtGetField(x: VarName, p: Path, f: FieldName) extends Stmt {
        override def toString = "%s = %s->%s".format(x.toIdent, p, f)
    }
    sealed case class StmtNew(x: VarName, t: TypeRef, m: ir.MethodName, qs: List[Path]) extends Stmt {
        override def toString = "%s = new %s %s(%s);".format(x.toIdent, t, m, qs.mkString(", "))
    }
    sealed case class StmtCast(x: VarName, wt: WcTypeRef, q: Path) extends Stmt {
        override def toString = "%s = (%s)%s;".format(x.toIdent, wt, q)
    }
    sealed case class StmtNull(x: VarName, wt: WcTypeRef) extends Stmt {
        override def toString = "%s = (%s)null;".format(x.toIdent, wt)
    }
    sealed case class StmtReturn(p: Option[Path]) extends Stmt {
        override def toString = "return %s;".format(p)        
    }
    sealed case class StmtSetField(p: Path, f: FieldName, q: Path) extends Stmt {
        override def toString = "%s->%s = %s;".format(p, f, q)
    }
    sealed case class StmtHb(p: Path, q: Path) extends Stmt {
        override def toString = "%s hb %s;".format(p, q)        
    }
    sealed case class StmtLocks(p: Path, q: Path) extends Stmt {
        override def toString = "%s locks %s;".format(p, q)        
    }
    
    // ______ Branching _____________________________________________________
    //
    // StmtBreak and StmtContinue are used to indicate a branch 
    // in or out of a compound statement.  StmtCondBreak indicates
    // a conditional break.  It does not identify the conditions in
    // which the break occurs, as it's not really relevant to our 
    // type check.
    //
    // All three statements use an integer to identify 
    // their target: the number indicates how far up the parent stack we 
    // must go (0 == immediate parent).
    //
    // Our breaks and continues are somewhat different from in Java:
    // (1) They take arguments which indicate the values supplied to the
    //     SSA arguments on the target block.
    // (2) They are fully explicit: for example, loop bodies always end
    //     in CONTINUE.  
    
    sealed case class StmtCondBreak(i: Int, ps: List[Path]) extends Stmt {
        override def toString = "condBreak %d(%s);".format(i, ", ".join(ps))
    }
    sealed case class StmtBreak(i: Int, ps: List[Path]) extends Stmt {
        override def toString = "break %d(%s);".format(i, ", ".join(ps))
    }
    sealed case class StmtContinue(i: Int, ps: List[Path]) extends Stmt {
        override def toString = "continue %d(%s);".format(i, ", ".join(ps))
    }
    
    // ______ Flow Control and Compound Statements __________________________
    //
    // Compound statements define internal control flow.  When a compound 
    // statement terminates, it will define a set of variables called defines,
    // which in traditional SSA would be the phi-nodes at the start of the 
    // next compound node.  Since all exits from a compound node are made
    // explicit via a StmtBreak statement, the arguments to the StmtBreak
    // match up with these defines variables.
    //
    // In addition, the Loop compound statement defines a set of arguments
    // and their initial values.  These are loop variables whose value is 
    // set on every iteration to the arguments of the StmtContinue.
    
    sealed case class StmtSeq(stmts: List[Stmt]) {
        def setDefaultPos(pos: Position) = stmts.foreach(_.setDefaultPos(pos))
        override def toString = "StmtSeq(length=%d)".format(stmts.length)
    }
    
    sealed case class StmtCompound(kind: CompoundKind, defines: List[LvDecl]) extends Stmt {
        override def setDefaultPosOnChildren() {
            kind.subseqs.foreach(_.setDefaultPos(pos))
        }
        override def toString = "%s => (...)".format(kind, ", ".join(defines))
    }
    sealed abstract class CompoundKind {
        def subseqs: List[StmtSeq]
    }
    sealed case class Block(seq: StmtSeq) extends CompoundKind {
        override def toString = "Block"
        def subseqs = List(seq)
    }
    sealed case class Switch(seqs: List[StmtSeq]) extends CompoundKind {
        override def toString = "Switch"
        def subseqs = seqs
    }
    sealed case class Loop(args: List[LvDecl], ps_initial: List[ir.Path], seq: StmtSeq) extends CompoundKind {
        override def toString = "Loop"
        def subseqs = List(seq)
    }
    sealed case class Subinterval(x: VarName, ps_locks: List[Path], seq: StmtSeq) extends CompoundKind {
        override def toString = "Subinterval[%s locks %s]".format(x, ", ".join(ps_locks))
        def subseqs = List(seq)
    }
    sealed case class TryCatch(seq_try: StmtSeq, seq_catch: StmtSeq) extends CompoundKind {
        override def toString = "TryCatch"
        def subseqs = List(seq_try, seq_catch)
    }
    
    val empty_method_body = StmtSeq(List())

    // ___ Types and Paths __________________________________________________
    
    sealed case class WcGhost(f: FieldName, wp: ir.WcPath) {
        override def toString = "@%s(%s)".format(f.name, wp)
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
        override def toString = "%s%s%s".format("".join("", wghosts, " "), c, as.postWords)
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
        override def toString = (lv :: fs).map(_.toIdent).mkString(".")
    }
    
    // ___ Canonical Paths __________________________________________________
    
    sealed abstract class CanonPath { // canon. path
        val p: ir.Path        
        val wt: ir.WcTypeRef
        
        override def toString = "cp(%s: %s)".format(p, wt)
    }
    
    sealed case class CpLv(lv: ir.VarName, wt: ir.WcTypeRef, isGhost: Boolean) extends CanonPath {
        val p = ir.Path(lv, List())
        
        override def toString = super.toString
    }
    
    sealed case class CpField(cp: CanonPath, fd: ir.FieldDecl) extends CanonPath {
        val p = cp.p + fd.name
        val wt = fd.wt
        
        override def toString = super.toString
    }
    
    sealed case class CpCtor(cp: CanonPath) extends CanonPath {
        val p = cp.p + ir.f_ctor
        
        override def toString = super.toString
    }
    
    sealed case class CpSuper(cp: CanonPath) extends CanonPath {
        val p = cp.p + ir.f_ctor
        
        override def toString = super.toString
    }
    
    // ___ Requirements _____________________________________________________
    
    sealed abstract class Req extends PositionalAst {
        def setDefaultPosOnChildren() { }        
    }
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

    val f_creator = ir.FieldName("ch.ethz.intervals.quals.Creator")
    val f_start = ir.FieldName("start")
    val f_ctor = ir.FieldName("constructor")
    val f_end = ir.FieldName("end")
    val f_super = ir.FieldName("super")
    
    val m_init = ir.MethodName("<init>")
    val m_toScalar = ir.MethodName("toScalar")
    val m_run = ir.MethodName("run")
    
    val m_arrayGet = ir.MethodName("arrayGet")
    val m_arraySet = ir.MethodName("arraySet")
    val m_toString = ir.MethodName("toString") // used only in unit testing
    
    // Special types understood by the system:
    //    (During testing, the definitions in cds_default are used)
    val c_object = ir.ClassName("java.lang.Object")
    val c_interval = ir.ClassName("ch.ethz.intervals.Interval")
    val c_guard = ir.ClassName("ch.ethz.intervals.guard.Guard")
    val c_point = ir.ClassName("ch.ethz.intervals.Point")
    val c_lock = ir.ClassName("ch.ethz.intervals.Lock")
    val c_string = ir.ClassName("java.lang.String") // used only in unit testing

    // Types used to translate Java constructs into classes:
    //    These types are not treated specially by the type system,
    //    but we provide default, synthetic definitions.
    val c_scalar = ir.ClassName("scalar")  // Represents any scalar value.
    val c_void = ir.ClassName("void")      // Represents void values.
    val c_array = ir.ClassName("array")    // Represents arrays.    
    
    val t_void = ir.TypeRef(c_void, List(), ir.noAttrs)
    val t_scalar = ir.TypeRef(c_scalar, List(), ir.noAttrs)
    val t_interval = ir.TypeRef(c_interval, List(), ir.noAttrs)
    val t_point = ir.TypeRef(c_point, List(), ir.noAttrs)
    val t_lock = ir.TypeRef(c_lock, List(), ir.noAttrs)
    val t_string = ir.TypeRef(c_string, List(), ir.noAttrs)

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
            /* body:   */ ir.StmtSeq(
                List(
                    ir.StmtSuperCtor(m_init, List()),
                    ir.StmtReturn(None)
                )
            )
        )
    
    // Two "classes" used to represent the type void and scalar data.
    val cds_special = List(
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_void,
            /* Extends: */  List(),
            /* Ghosts:  */  List(),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_scalar,
            /* Extends: */  List(),
            /* Ghosts:  */  List(),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_array,
            /* Extends: */  List(),
            /* Ghosts:  */  List(),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(),
            /* Fields:  */  List(),
            /* Methods: */  List()
        )        
    )
    
    // Simplified versions of built-in classes used in our unit tests.
    val cds_unit_test = List(
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
                    /* body:   */ empty_method_body)),
            /* Fields:  */  List(ir.gfd_creator),
            /* Methods: */  List(MethodDecl(
                    /* attrs:  */ noAttrs,
                    /* wt_ret: */ t_string, 
                    /* name:   */ m_toString, 
                    /* args:   */ List(),
                    /* reqs:   */ List(ir.ReqReadableBy(List(gfd_creator.thisPath), List(p_mthd))),
                    /* body:   */ empty_method_body))
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_string,
            /* Extends: */  List(c_object),
            /* Ghosts:  */  List(Ghost(f_creator, p_ctor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(md_ctor_interface),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  interfaceAttrs,
            /* Name:    */  c_guard,
            /* Extends: */  List(c_object),
            /* Ghosts:  */  List(),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(md_ctor_interface),
            /* Fields:  */  List(
                ReifiedFieldDecl(noAttrs, t_point, f_start, gfd_ctor.thisPath),
                ReifiedFieldDecl(noAttrs, t_point, f_end, gfd_ctor.thisPath)),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_interval,
            /* Extends: */  List(c_object, c_guard),
            /* Ghosts:  */  List(Ghost(f_creator, p_ctor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(md_ctor_interface),
            /* Fields:  */  List(),
            /* Methods: */  List(MethodDecl(
                    /* attrs:  */ noAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_run, 
                    /* args:   */ List(),
                    /* reqs:   */ List(ir.ReqSubintervalOf(List(p_mthd), List(p_this))),
                    /* body:   */ empty_method_body))
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_point,
            /* Extends: */  List(c_object),
            /* Ghosts:  */  List(Ghost(f_creator, p_ctor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(md_ctor_interface),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_lock,
            /* Extends: */  List(c_object, c_guard),
            /* Ghosts:  */  List(Ghost(f_creator, p_ctor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(md_ctor_interface),
            /* Fields:  */  List(),
            /* Methods: */  List()
        )
    )
    
    val builtinPosition = new Position {
        override def <(that: Position) = this != that
        def column = 1
        def line = 1
        def lineContents = "<built-in>"
        override def toString = "<built-in>"
    }
    md_ctor_interface.setDefaultPos(builtinPosition)
    cds_special.foreach(_.setDefaultPos(builtinPosition))
    cds_unit_test.foreach(_.setDefaultPos(builtinPosition))
   
}