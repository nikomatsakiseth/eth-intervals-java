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

    sealed case class Attrs(private val s: Set[Attr]) {
        def +(a: Attr) = Attrs(s + a)
        
        def interface = s.contains(AttrInterface)
        def withInterface = this + AttrInterface
        
        def diff(as: Attrs): Set[Attr] = s -- as.s
        
        def preWords = "".join("", s.map(_.adj), " ")
        def postWords = "".join(" ", s.map(_.adj), "")
        
        override def toString = "{%s}".format(s.mkString(""))
    }
    val noAttrs = Attrs(ListSet.empty)
    val interfaceAttrs = Attrs(ListSet(AttrInterface))

    // ___ Names of variables, fields, methods, classes _____________________   
    
    val quoteRegex = "[^a-zA-Z0-9._]".r
    sealed abstract class Name(name: String) {
        override def toString = 
            if(name.contains(".")) "(%s)".format(name)
            else name
    }
    
    sealed case class VarName(name: String) extends Name(name) {
        def path = ir.Path(this, List())
        def +(f: FieldName) = path + f
        def ++(fs: List[FieldName]) = path ++ fs
    }
    
    sealed case class TypeVarName(name: String) extends Name(name)
    
    sealed case class MethodName(name: String) extends Name(name)
    
    sealed case class ClassName(name: String) extends Name(name)

    sealed abstract class FieldName(name: String) extends Name(name) {
        def thisPath = p_this + this
    }
    
    sealed case class PlainFieldName(name: String) extends FieldName(name)
    
    val ctor = "Constructor"
    sealed case class ClassCtorFieldName(c: ir.ClassName) extends {
        val name = "%s[%s]".format(ctor, c) // wacky syntax to initialize a field before super ctor:
    } with FieldName(name) {
        override def toString = name 
    }
    
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
        typeVars: List[TypeVarDecl],
        typeArgs: List[TypeArg],
        superClasses: List[ClassName],
        ghosts: List[Ghost],
        reqs: List[Req],
        ctors: List[MethodDecl],
        fields: List[FieldDecl],
        methods: List[MethodDecl]
    ) extends PositionalAst {
        def isNamed(n: ClassName) = (name == n)
        
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
    
    sealed case class TypeVarDecl(
        name: TypeVarName,
        wts_lb: List[ir.WcTypeRef]
    ) extends PositionalAst {
        def isNamed(tv: TypeVarName) = (name == tv)
        def typeArgOf(p: ir.Path) = ir.TypeArg(name, ir.PathType(p, name))
        
        def setDefaultPosOnChildren() { }
    }
    
    sealed case class MethodDecl(
        wt_ret: ir.WcTypeRef,
        name: ir.MethodName,
        args: List[ir.LvDecl],
        reqs: List[ir.Req],
        body: ir.StmtSeq
    ) extends PositionalAst {
        def isNamed(n: MethodName) = (name == n)
        
        def msig(wct_rcvr: ir.WcClassType) = 
            ir.MethodSig(
                wct_rcvr,
                args, 
                reqs, 
                wt_ret
            )
        
        def setDefaultPosOnChildren() {
            reqs.foreach(_.setDefaultPos(pos))
            body.setDefaultPos(pos)
        }
        
        override def toString =
            "%s %s(%s)%s".format(
                wt_ret, name, args.mkString(", "), "".join(" ", reqs))
    }
    
    sealed case class MethodSig(
        wct_rcvr: ir.WcClassType,
        args: List[ir.LvDecl],
        reqs: List[ir.Req],
        wt_ret: ir.WcTypeRef
    ) {
        override def toString = "(%s _(%s this, %s)%s)".format(
            wt_ret, wct_rcvr, args.mkString(", "), "".join(" ", reqs))
    }
    
    sealed case class LvDecl(
        name: ir.VarName,
        wt: ir.WcTypeRef
    ) {
        override def toString = "%s %s".format(wt, name)
    }

    sealed abstract class FieldDecl extends PositionalAst {
        val wt: ir.WcTypeRef
        val name: ir.FieldName
        
        def isNamed(n: FieldName) = (name == n)
        
        def thisPath = name.thisPath
        
        def setDefaultPosOnChildren() {
        }        
    }
    
    sealed case class GhostFieldDecl(
        wt: ir.WcTypeRef,
        name: ir.FieldName
    ) extends FieldDecl {
        override def toString = "<%s %s>".format(wt, name)
        
        def ghostOf(p: ir.Path) = Ghost(name, p + name)
    }
    
    sealed case class ReifiedFieldDecl(
        as: ir.Attrs,       
        wt: ir.WcTypeRef,
        name: ir.FieldName,
        p_guard: ir.Path
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
        override def toString = "%s = super->%s(%s)".format(x, m, qs)
    }
    sealed case class StmtCheckType(p: ir.Path, wt: ir.WcTypeRef) extends Stmt {
        override def toString = "%s <: %s".format(p, wt)
    }
    sealed case class StmtCall(x: VarName, p: Path, m: MethodName, qs: List[Path]) extends Stmt {
        override def toString = "%s = %s->%s(%s)".format(x, p, m, qs)        
    }
    sealed case class StmtGetField(x: VarName, p: Path, f: FieldName) extends Stmt {
        override def toString = "%s = %s->%s".format(x, p, f)
    }
    sealed case class StmtNew(x: VarName, ct: ClassType, m: ir.MethodName, qs: List[Path]) extends Stmt {
        override def toString = "%s = new %s %s(%s);".format(x, ct, m, qs.mkString(", "))
    }
    sealed case class StmtCast(x: VarName, wt: WcTypeRef, q: Path) extends Stmt {
        override def toString = "%s = (%s)%s;".format(x, wt, q)
    }
    sealed case class StmtNull(x: VarName, wt: WcTypeRef) extends Stmt {
        override def toString = "%s = (%s)null;".format(x, wt)
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

    // ___ Unconstructed Sets _______________________________________________
    //
    // Tracks which constructors have completed. 
    
    sealed abstract class Unconstructed {
        def toSuffixString: String        
    }
    
    // All constructors completed (the default in normal Java code).
    case object FullyConstructed extends Unconstructed {
        override def toString = "FullyConstructed"
        override def toSuffixString = ""
    }
    
    // No constructors completed.
    case object FullyUnconstructed extends Unconstructed {
        override def toString = "{}"
        override def toSuffixString = " " + toString
    }
    
    // Supertypes of 'c' are constructed but not 'c' itself.
    case class PartiallyConstructed(c: ir.ClassName) extends Unconstructed {
        override def toString = "{%s}".format(c)
        override def toSuffixString = " " + toString
    }
    
    // ___ Types ____________________________________________________________
    //
    // The type hierarchy:
    //

    sealed abstract class WcTypeRef
    
    sealed case class PathType(
        p: ir.Path,
        tv: ir.TypeVarName
    ) extends WcTypeRef
    
    sealed case class WcClassType(
        c: ir.ClassName,                            // Name of class being referenced
        wghosts: List[ir.WcGhost],                  // Ghost arguments 
        wtargs: List[ir.WcTypeArg]                  // Generic type arguments
    ) extends WcTypeRef {
        override def toString = 
            "%s%s%s".format(
                "".join("", wghosts, " "), 
                c, 
                "".join(" ", wtargs, "")
            )
            
        def withDefaultWghosts(wgs_additional: List[ir.WcGhost]) = {
            val wgs_new = wgs_additional.filter(wg => !wghosts.exists(_.isNamed(wg.f)))
            copy(wghosts = wgs_new ++ wghosts)
        }
    }
    
    sealed class ClassType(
        c: ir.ClassName,
        val ghosts: List[ir.Ghost],
        val targs: List[ir.TypeArg] 
    ) extends WcClassType(c, ghosts, targs) {
        def withDefaultGhosts(gs_additional: List[ir.Ghost]) = {
            val gs_new = gs_additional.filter(g => !ghosts.exists(_.isNamed(g.f)))
            ir.ClassType(c, gs_new ++ ghosts, targs)
        }        
    }
    
    object ClassType {
        def apply(c: ir.ClassName, ghosts: List[ir.Ghost], targs: List[ir.TypeArg]) = {
            new ClassType(c, ghosts, targs)
        }
    }
    
    // ______ Type Bounds ___________________________________________________
    
    sealed case class TypeBounds(
        wts_lb: List[ir.WcTypeRef],             // lower bounds (must be at least 1)
        owts_ub: Option[List[ir.WcTypeRef]]     // upper bounds, or None if not upper-bounded
    )
    
    // ______ Type Args _____________________________________________________
    //
    // A TypeArg <tv: wt> maps the type variable 'tv' to the type 'wt'.
    // A WcTypeArg maps the type variable 'tv' to a bounded range of types.
    
    sealed abstract class WcTypeArg {
        def tv: ir.TypeVarName                  // bound type variable 
        def bounds: TypeBounds
        def toOptionTypeArg: Option[ir.TypeArg]
        
        def isNamed(n: TypeVarName) = (tv == n)        
    }
    
    sealed case class BoundedTypeArg(
        tv: ir.TypeVarName,
        bounds: TypeBounds
    ) extends WcTypeArg {
        def toOptionTypeArg = None
    }
    
    sealed case class TypeArg(
        tv: ir.TypeVarName,
        wt: ir.WcTypeRef
    ) extends WcTypeArg {
        def bounds = TypeBounds(List(wt), Some(List(wt)))
        def toOptionTypeArg = Some(this)
    }
    
    // ______ Ghosts ________________________________________________________
    //
    // A Ghost @f(p) associates a fixed path with a given ghost field.  
    // A WcGhost is the same but may associate a wildcard instead.
    
    sealed case class WcGhost(f: ir.FieldName, wp: ir.WcPath) {
        override def toString = "@%s(%s)".format(f, wp)
        
        def isNamed(n: FieldName) = (f == n)        
    } 
    
    sealed class Ghost(
        f: FieldName,
        val p: Path
    ) extends WcGhost(f, p)
    
    object Ghost {
        def apply(f: FieldName, p: Path) = new Ghost(f, p)
    }
    
    // ______ Paths and Wildcard Paths ______________________________________
    //
    // A path p is a local variable followed by a sequence of fields.
    // A wildcard path wp may also include existential references
    // like (? readableBy p).
    
    sealed abstract class WcPath {
        def addDependentPaths(s: Set[Path]): Set[Path]
        
        def isDependentOn(p: Path): Boolean =
            addDependentPaths(Set.empty).exists(_.hasPrefix(p))
    }
    sealed abstract class WcWildcardPath(ps: List[Path]) extends WcPath {
        def addDependentPaths(s: Set[Path]) = s ++ ps
        protected def toString(tag: String) = ps match {
            case List() => tag
            case _ => tag + " " + ps.mkString(", ")
        }
    }
    sealed case class WcReadableBy(ps: List[Path]) extends WcWildcardPath(ps) {
        override def toString = toString("readableBy")
    }
    sealed case class WcWritableBy(ps: List[Path]) extends WcWildcardPath(ps) {
        override def toString = toString("writableBy")
    }
    sealed case class WcHbNow(ps: List[Path]) extends WcWildcardPath(ps) {
        override def toString = toString("hbNow")
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
        override def toString = ".".join(lv :: fs)
    }
    
    // ___ Canonical Paths __________________________________________________
    //
    // Canonical paths are created by the environment.  If, at a given
    // point in execution, any two paths are mapped to the same canonical 
    // path, then those two paths represent the same object.
    //
    // A TeeCeePee is a "Typed Canonical Path".  It allows a canonical path
    // to be associated with alternate typings beyond the default "wt".  
    
    sealed abstract class CanonPath { // canon. path
        val p: ir.Path        
        val wt: ir.WcTypeRef
        
        def thisSubst = PathSubst.vp(ir.lv_this, p)
        
        def toTcp = TeeCeePee(this, wt)
        
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
    
    sealed case class CpObjCtor(cp: CanonPath) extends CanonPath {
        val p = cp.p + ir.f_objCtor
        val wt = ir.wt_constructedInterval
        
        override def toString = super.toString
    }
    
    sealed case class CpClassCtor(cp: CanonPath, c: ir.ClassName) extends CanonPath {
        val p = cp.p + ClassCtorFieldName(c)
        val wt = ir.wt_constructedInterval
        
        override def toString = super.toString
    }
    
    sealed case class TeeCeePee[+T <: WcTypeRef](cp: CanonPath, ty: T) {
        def p = cp.p
        
        def withTy[S <: WcTypeRef](ty: S) = ir.TeeCeePee(cp, ty)
        
        override def toString = "tcp(%s: %s)".format(p, ty)
    }
    
    // ___ Requirements _____________________________________________________
    //
    // Requirements are specified on method headers and allow the callee to
    // dictate conditions to the caller which must hold on method entry.
    
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
    
    // ___ Pre-defined Types and Constants __________________________________
    
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
    
    val f_creator = ir.PlainFieldName("ch.ethz.intervals.quals.Creator")
    val f_objCtor = ir.PlainFieldName("ch.ethz.intervals.quals.Constructor")
    val f_start = ir.PlainFieldName("start")
    val f_end = ir.PlainFieldName("end")
    val f_super = ir.PlainFieldName("super")
    
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
    
    // Ghost field creator, declared on root type Object:
    val p_this_objCtor = f_objCtor.thisPath
    val p_this_creator = f_creator.thisPath
    val p_this_intervalCtor = ClassCtorFieldName(c_interval).thisPath
    
    val t_void = ir.ClassType(c_void, List(), List())
    val t_interval = ir.ClassType(c_interval, List(), List())
    
    val wg_objCtorHbNow = ir.WcGhost(ir.f_objCtor, ir.WcHbNow(List()))
    val wg_objCtorHbNowThisObjCtor = ir.WcGhost(ir.f_objCtor, ir.WcHbNow(List(ir.p_this_objCtor)))

    val wt_constructedPoint = ir.WcClassType(c_point, List(wg_objCtorHbNow), List())
    val wt_constructedInterval = ir.WcClassType(c_interval, List(wg_objCtorHbNow), List())    
    
    val md_emptyCtor = 
        ir.MethodDecl(
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
            /* TyVars:  */  List(),
            /* TyArgs:  */  List(),
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
            /* TyVars:  */  List(),
            /* TyArgs:  */  List(),
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
            /* TyVars:  */  List(),
            /* TyArgs:  */  List(),
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
            /* TyVars:  */  List(),
            /* TyArgs:  */  List(),
            /* Extends: */  List(),
            /* Ghosts:  */  List(),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(MethodDecl(
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_init, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* body:   */ empty_method_body)),
            /* Fields:  */  List(GhostFieldDecl(wt_constructedInterval, f_creator)),
            /* Methods: */  List(MethodDecl(
                    /* wt_ret: */ ir.WcClassType(c_string, List(wg_objCtorHbNow), List()), 
                    /* name:   */ m_toString, 
                    /* args:   */ List(),
                    /* reqs:   */ List(
                        ir.ReqReadableBy(List(p_this_creator), List(p_mthd)),
                        ir.ReqHb(List(p_this_intervalCtor), List(p_mthd))),
                    /* body:     */ empty_method_body))
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_string,
            /* TyVars:  */  List(),
            /* TyArgs:  */  List(),
            /* Extends: */  List(c_object),
            /* Ghosts:  */  List(Ghost(f_creator, p_this_objCtor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(md_emptyCtor),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  interfaceAttrs,
            /* Name:    */  c_guard,
            /* TyVars:  */  List(),
            /* TyArgs:  */  List(),
            /* Extends: */  List(c_object),
            /* Ghosts:  */  List(),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(md_emptyCtor),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_interval,
            /* TyVars:  */  List(),
            /* TyArgs:  */  List(),
            /* Extends: */  List(c_object, c_guard),
            /* Ghosts:  */  List(Ghost(f_creator, p_this_objCtor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(md_emptyCtor),
            /* Fields:  */  List(
                ReifiedFieldDecl(noAttrs, wt_constructedPoint, f_start, p_this_intervalCtor),
                ReifiedFieldDecl(noAttrs, wt_constructedPoint, f_end, p_this_intervalCtor)),
            /* Methods: */  List(MethodDecl(
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_run, 
                    /* args:   */ List(),
                    /* reqs:   */ List(
                        ir.ReqSubintervalOf(List(p_mthd), List(p_this)),
                        ir.ReqHb(List(p_this_objCtor), List(p_mthd))),
                    /* body:   */ empty_method_body))
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_point,
            /* TyVars:  */  List(),
            /* TyArgs:  */  List(),
            /* Extends: */  List(c_object),
            /* Ghosts:  */  List(Ghost(f_creator, p_this_objCtor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(md_emptyCtor),
            /* Fields:  */  List(),
            /* Methods: */  List()
        ),
        ClassDecl(
            /* Attrs:   */  noAttrs,
            /* Name:    */  c_lock,
            /* TyVars:  */  List(),
            /* TyArgs:  */  List(),
            /* Extends: */  List(c_object, c_guard),
            /* Ghosts:  */  List(Ghost(f_creator, p_this_objCtor)),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(md_emptyCtor),
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
    md_emptyCtor.setDefaultPos(builtinPosition)
    cds_special.foreach(_.setDefaultPos(builtinPosition))
    cds_unit_test.foreach(_.setDefaultPos(builtinPosition))
   
}