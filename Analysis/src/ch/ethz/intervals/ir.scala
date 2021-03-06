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
    sealed abstract class Name {
        val name: String
        
        override def toString = 
            if(name.contains(".")) "(%s)".format(name)
            else name
            
        /** True if this is a name we generated internally */
        def isGenerated = name.startsWith("[")
            
        /** This allows names to be used in patterns.  Convenient. */
        def unapply(ar: AnyRef): Boolean = (ar == this)
    }
    
    sealed case class VarName(name: String) extends Name {
        def path = ir.PathLv(this)
        def /(f: FieldName) = path / f
        def /+(fs: List[FieldName]) = path /+ fs
    }
    
    sealed case class TypeVarName(name: String) extends Name {
        def thisPtype = ir.PathType(ir.p_this, this)
    }
    
    sealed case class MethodName(name: String) extends Name
    
    sealed abstract class AnyClassName extends Name {
        def ct = ir.ClassType(this, List(), List())
    }
    
    sealed case class ClassName(name: String) extends AnyClassName

    sealed case class StaticClassName(c: ClassName) extends AnyClassName {
        val name = "static[%s]".format(c)
    }

    sealed abstract class FieldName extends Name {
        def thisPath = p_this / this
    }
    
    sealed case class PlainFieldName(name: String) extends FieldName
    
    val ctor = "Constructor"
    sealed case class ClassCtorFieldName(c: ir.AnyClassName) extends FieldName {
        val name = "%s[%s]".format(ctor, c) // wacky syntax to initialize a field before super ctor:
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
        name: AnyClassName,
        ghostFieldDecls: List[ir.GhostFieldDecl],
        typeVarDecls: List[TypeVarDecl],
        superClasses: List[ClassName],
        ghosts: List[Ghost],
        typeArgs: List[TypeArg],
        reqs: List[Req],
        ctors: List[MethodDecl],
        reifiedFieldDecls: List[ReifiedFieldDecl],
        methods: List[MethodDecl]
    ) extends PositionalAst {
        def isNamed(n: AnyClassName) = (name == n)
        
        def setDefaultPosOnChildren() {
            ghostFieldDecls.foreach(_.setDefaultPos(pos))
            typeVarDecls.foreach(_.setDefaultPos(pos))
            reqs.foreach(_.setDefaultPos(pos))
            ctors.foreach(_.setDefaultPos(pos))
            reifiedFieldDecls.foreach(_.setDefaultPos(pos))
            methods.foreach(_.setDefaultPos(pos))
        } 
        
        override def toString = "(Class %s%s)".format(name, attrs)
    }
    
    sealed case class GhostFieldDecl(
        name: ir.FieldName,
        c: ir.ClassName
    ) extends PositionalAst {
        def isNamed(f: ir.FieldName) = (name == f)
        def thisPath = name.thisPath                        
        def ghostOf(p: ir.Path) = ir.Ghost(name, p / name)
        def setDefaultPosOnChildren() { }
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
        name: ir.MethodName,
        args: List[ir.LvDecl],
        reqs: List[ir.Req],
        wt_ret: ir.WcTypeRef,
        wps_identity: List[ir.WcPath],
        body: ir.StmtSeq
    ) extends PositionalAst {
        def isNamed(n: MethodName) = (name == n)
        
        def msig(
            lv_cur: ir.VarName,
            lv_rcvr: ir.VarName,
            lvs_args: List[ir.VarName]
        ) = {
            PathSubst.vv(
                ir.lv_mthd  :: ir.lv_this   :: args.map(_.name),
                lv_cur      :: lv_rcvr      :: lvs_args
            ).methodSig(            
                ir.MethodSig(args.map(_.wt), args.map(_.wps_identity), name, reqs, wt_ret, wps_identity)
            )
        }
        
        def setDefaultPosOnChildren() {
            reqs.foreach(_.setDefaultPos(pos))
            body.setDefaultPos(pos)
        }
        
        override def toString = "(Method %s())".format(name)
    }
    
    sealed case class MethodSig(
        wts_args: List[ir.WcTypeRef],
        argIdentities: List[List[ir.WcPath]],
        name: ir.MethodName,
        reqs: List[ir.Req],
        wt_ret: ir.WcTypeRef,
        retIdentity: List[ir.WcPath]
    ) {
        override def toString = "(%s %s(%s)%s)".format(
            wt_ret, name, ", ".join(wts_args), "".join(" ", reqs))
    }
    
    sealed case class LvDecl(
        name: ir.VarName,
        wt: ir.WcTypeRef,
        wps_identity: List[ir.WcPath]
    ) {
        override def toString = "%s: %s".format(name, wt)
    }

    sealed case class ReifiedFieldDecl(
        wt: ir.WcTypeRef,
        name: ir.FieldName,
        p_guard: ir.Path,
        wps_identity: List[ir.WcPath]
    ) extends PositionalAst {
        def isNamed(n: FieldName) = (name == n)
        def thisPath = name.thisPath                        
        override def toString = "(Field %s: %s)".format(name, wt)
        def setDefaultPosOnChildren() {}        
    }
    
    // ______ Leaf Statements _______________________________________________
    sealed abstract class Stmt extends PositionalAst {
        def setDefaultPosOnChildren() { }
        def lvs_def: List[ir.VarName]  
    }
    sealed case class StmtSuperCtor(m: ir.MethodName, lvs_arg: List[VarName]) extends Stmt {
        override def toString = "super %s(%s)".format(m, lvs_arg)
        def lvs_def = List()
    }
    sealed case class StmtSuperCall(lv_def: VarName, m: MethodName, lvs_arg: List[VarName]) extends Stmt {
        override def toString = "%s = super->%s(%s)".format(lv_def, m, lvs_arg)
        def lvs_def = List(lv_def)
    }
    sealed case class StmtCheckType(lv: ir.VarName, wt: ir.WcTypeRef) extends Stmt {
        override def toString = "%s <: %s".format(lv, wt)
        def lvs_def = List()
    }
    sealed case class StmtGetStatic(lv_def: ir.VarName, c: ir.ClassName) extends Stmt {
        override def toString = "%s = static[%s]".format(lv_def, c)
        def lvs_def = List(lv_def)
    }
    sealed case class StmtCall(lv_def: VarName, lv_rcvr: VarName, m: MethodName, lvs_arg: List[VarName]) extends Stmt {
        override def toString = "%s = %s->%s(%s)".format(lv_def, lv_rcvr, m, ", ".join(lvs_arg))        
        def lvs_def = List(lv_def)
    }
    sealed case class StmtGetField(lv_def: VarName, lv_owner: VarName, f: FieldName) extends Stmt {
        override def toString = "%s = %s->%s".format(lv_def, lv_owner, f)
        def lvs_def = List(lv_def)
    }
    sealed case class StmtNew(lv_def: VarName, ct: ClassType, m: ir.MethodName, lvs_arg: List[VarName]) extends Stmt {
        override def toString = "%s = new %s %s(%s);".format(lv_def, ct, m, lvs_arg.mkString(", "))
        def lvs_def = List(lv_def)
    }
    sealed case class StmtCast(lv_def: VarName, wt: WcTypeRef, y: VarName) extends Stmt {
        override def toString = "%s = (%s)%s;".format(lv_def, wt, y)
        def lvs_def = List(lv_def)
    }
    sealed case class StmtNull(lv_def: VarName, wt: WcTypeRef) extends Stmt {
        override def toString = "%s = (%s)null;".format(lv_def, wt)
        def lvs_def = List(lv_def)
    }
    sealed case class StmtReturn(lv_value: Option[VarName]) extends Stmt {
        override def toString = "return %s;".format(lv_value)        
        def lvs_def = List()
    }
    sealed case class StmtSetField(lv_owner: VarName, f: FieldName, lv_value: VarName) extends Stmt {
        override def toString = "%s->%s = %s;".format(lv_owner, f, lv_value)
        def lvs_def = List()
    }
    sealed case class StmtHb(lv_from: VarName, lv_to: VarName) extends Stmt {
        override def toString = "%s hb %s;".format(lv_from, lv_to)        
        def lvs_def = List()
    }
    sealed case class StmtLocks(lv_inter: VarName, lv_lock: VarName) extends Stmt {
        override def toString = "%s locks %s;".format(lv_inter, lv_lock)        
        def lvs_def = List()
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
    
    sealed case class StmtCondBreak(i: Int, lvs: List[VarName]) extends Stmt {
        override def toString = "condBreak %d(%s);".format(i, ", ".join(lvs))
        def lvs_def = List()
    }
    sealed case class StmtBreak(i: Int, lvs: List[VarName]) extends Stmt {
        override def toString = "break %d(%s);".format(i, ", ".join(lvs))
        def lvs_def = List()
    }
    sealed case class StmtContinue(i: Int, lvs: List[VarName]) extends Stmt {
        override def toString = "continue %d(%s);".format(i, ", ".join(lvs))
        def lvs_def = List()
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
    
    
    /** `tag`: purely for debugging
      * `kind`: the kind of compound statement
      * `defines`: the set of variables defined by an applicable break()
      */
    sealed case class StmtCompound(tag: String, kind: CompoundKind, defines: List[LvDecl]) extends Stmt {
        override def setDefaultPosOnChildren() {
            kind.subseqs.foreach(_.setDefaultPos(pos))
        }
        override def toString = "%s: %s => (...)".format(tag, kind, ", ".join(defines))
        def lvs_def = defines.map(_.name)
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
    sealed case class Loop(args: List[LvDecl], lvs_initial: List[VarName], seq: StmtSeq) extends CompoundKind {
        override def toString = "Loop"
        def subseqs = List(seq)
    }
    sealed case class InlineInterval(lv_inter: VarName, init: StmtSeq, run: StmtSeq) extends CompoundKind {
        override def toString = "InlineInterval[%s]".format(lv_inter)
        def subseqs = List(init, run)
    }
    sealed case class TryCatch(seq_try: StmtSeq, seq_catch: StmtSeq) extends CompoundKind {
        override def toString = "TryCatch"
        def subseqs = List(seq_try, seq_catch)
    }
    
    val empty_method_body = StmtSeq(List())

    // ___ Types ____________________________________________________________
    //
    // The type hierarchy:
    //

    sealed abstract class WcTypeRef {
        def java: String
        
        def toUserString: String
        
        def withDefaultWghosts(wgs_additional: List[ir.WcGhost]): WcTypeRef
    }
    
    sealed case class PathType(
        p: ir.Path,
        tv: ir.TypeVarName
    ) extends WcTypeRef {
        override def java = toString
        override def toString = "%s:%s".format(p, tv)
        override def toUserString = toString
        
        def withDefaultWghosts(wgs_additional: List[ir.WcGhost]) = this
    }
    
    sealed case class WcClassType(
        c: ir.AnyClassName,                         // Name of class being referenced
        wghosts: List[ir.WcGhost],                  // Ghost arguments 
        wtargs: List[ir.WcTypeArg]                  // Generic type arguments
    ) extends WcTypeRef {
        override def java = c.toString        
        override def toString = "%s%s%s".format(
            "".join("", wghosts, " "), 
            c, 
            "".join(" ", wtargs, "")
        )
        override def toUserString = {
            val wgs = wghosts.filter(wg => !wg.wp.isGenerated).sortWith(_ < _)
            val wtas = wtargs.sortWith(_ < _)
            "%s%s%s".format(
                "".join("", wgs, " "), 
                c, 
                "".join(" ", wtas, "")
            )
        }   
            
        def withDefaultWghosts(wgs_additional: List[ir.WcGhost]) = {
            val wgs_new = wgs_additional.filter(wg => !wghosts.exists(_.isNamed(wg.f)))
            copy(wghosts = wgs_new ++ wghosts)
        }
    }
    
    sealed class ClassType(
        c: ir.AnyClassName,
        val ghosts: List[ir.Ghost],
        val targs: List[ir.TypeArg] 
    ) extends WcClassType(c, ghosts, targs) {
        def withDefaultGhosts(gs_additional: List[ir.Ghost]) = {
            val gs_new = gs_additional.filter(g => !ghosts.exists(_.isNamed(g.f)))
            ir.ClassType(c, gs_new ++ ghosts, targs)
        }        
    }
    
    object ClassType {
        def apply(c: ir.AnyClassName, ghosts: List[ir.Ghost], targs: List[ir.TypeArg]) = {
            new ClassType(c, ghosts, targs)
        }
    }
    
    // ______ Type Bounds ___________________________________________________
    
    sealed case class TypeBounds(
        wts_lb: List[ir.WcTypeRef],             // lower bounds (must be at least 1)
        wts_ub: List[ir.WcTypeRef]              // upper bounds (may be empty)
    ) {
        override def toString = {
            val str = 
                if(wts_ub.isEmpty) ""
                else ", ".join(wts_ub) + " <: "
            str + "? <: " + ", ".join(wts_lb)
        }
    }
    
    // ______ Type Args _____________________________________________________
    //
    // A TypeArg <tv: wt> maps the type variable 'tv' to the type 'wt'.
    // A WcTypeArg maps the type variable 'tv' to a bounded range of types.
    
    sealed abstract class WcTypeArg {
        def tv: ir.TypeVarName                  // bound type variable 
        def bounds: TypeBounds
        def toOptionTypeArg: Option[ir.TypeArg]
        
        def <(wta: ir.WcTypeArg) = (tv.toString < wta.tv.toString)
        
        def isNamed(n: TypeVarName) = (tv == n)
    }
    
    sealed case class BoundedTypeArg(
        tv: ir.TypeVarName,
        bounds: TypeBounds
    ) extends WcTypeArg {
        def toOptionTypeArg = None
        
        override def toString = "<%s: %s>".format(tv, bounds)
    }
    
    sealed case class TypeArg(
        tv: ir.TypeVarName,
        wt: ir.WcTypeRef
    ) extends WcTypeArg {
        def bounds = TypeBounds(List(wt), List(wt))
        def toOptionTypeArg = Some(this)
        
        override def toString = "<%s: %s>".format(tv, wt)
    }
    
    // ______ Ghosts ________________________________________________________
    //
    // A Ghost @f(p) associates a fixed path with a given ghost field.  
    // A WcGhost is the same but may associate a wildcard instead.
    
    sealed case class WcGhost(f: ir.FieldName, wp: ir.WcPath) {
        override def toString = "@%s(%s)".format(f, wp)
        def <(wcg: ir.WcGhost) = (f.toString < wcg.f.toString)
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
        def isGenerated: Boolean
        def isDependentOn(p: Path): Boolean =
            addDependentPaths(Set.empty).exists(_.hasPrefix(p))
    }
    
    sealed abstract class WcWildcardPath(ps: List[Path]) extends WcPath {
        def isGenerated = false
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
    
    sealed abstract class Path extends WcPath {
        def isGenerated: Boolean
        def addDependentPaths(s: Set[ir.Path]) = s + this   
        
        def /(f: ir.FieldName): Path = PathField(this, f)
        def /+(fs: List[ir.FieldName]) = fs.foldLeft(this)(_ / _)
        def start = this / ir.f_start
        def end = this / ir.f_end
             
        def hasPrefix(p: ir.Path): Boolean
        def basedOnVar(lv1: ir.VarName): Boolean
    }

    sealed case class PathLv(lv: ir.VarName) extends Path {
        def isGenerated = lv.isGenerated
        override def toString = lv.toString
        def hasPrefix(p: ir.Path) = (this == p)
        def basedOnVar(lv1: ir.VarName) = (lv == lv1)
    }
    
    sealed case class PathStatic(c: ClassName) extends Path {
        def isGenerated = false
        override def toString = "static[%s]".format(c)
        def hasPrefix(p: ir.Path) = (this == p)
        def basedOnVar(lv1: ir.VarName) = false
    }
    
    sealed case class PathField(p_base: ir.Path, f: ir.FieldName) extends Path {
        def isGenerated = false
        override def toString = p_base match {
            case PathStatic(c) => "%s#%s".format(c, f)
            case _ => p_base.toString + "." + f
        }
        def hasPrefix(p: ir.Path) = (this == p) || (p_base == p)
        def basedOnVar(lv1: ir.VarName) = p_base.basedOnVar(lv1)
    }
    
    // Used for reverse pattern matching on paths:
    object / {
        def unapply(pf: PathField) = Some((pf.p_base, pf.f))
    }
    
    // ___ Canonical Paths __________________________________________________
    //
    // Canonical paths are created by the environment.  They contain all the
    // information the environment knows about the object found at a particular
    // path.  
    //
    // A TeeCeePee is a "Typed Canonical Path".  It allows a canonical path
    // to be associated with alternate typings beyond the default types.  This
    // is commonly used to refer to supertypes.
    
    sealed abstract class CanonPathComponent {
        /** Path to this component. */
        def p: ir.Path
        
        /** List of WcPaths which apply to this component.  May include p. */
        def wps_identity: List[ir.WcPath]
        
        /** Will the path `p` exist at runtime? */
        def isReified: Boolean
        
        /** Substitution of `p` for `this` */
        def thisSubst = PathSubst.vp(ir.lv_this, p)
    } 
    
    /** Identifiers a variable that would exist but an error
      * occurred while checking the statement which defines it. */
    sealed case class CpcErrorLv(lv: ir.VarName) 
    extends CanonPathComponent {
        def p = lv.path
        def isReified = false
        def wps_identity = List()
    }
    
    sealed abstract class CpcGhost extends CanonPathComponent {
        def isReified = false
        def c: ir.ClassName
    }
    
    object CpcGhost {
        def unapply(comp: CpcGhost) = Some((comp.p, comp.c))
    }
    
    sealed case class CpcGhostLv(
        lv: ir.VarName,
        c: ir.ClassName,
        wps_identity: List[ir.WcPath]
    ) extends CpcGhost {
        def p = lv.path        
        override def toString = p.toString
    }
    
    sealed case class CpcGhostField(
        cpc_base: ir.CanonPathComponent,
        f: ir.FieldName,
        c: ir.ClassName,
        wps_identity: List[ir.WcPath]
    ) extends CpcGhost {
        def p = cpc_base.p / f
        override def toString = p.toString
    }
    
    sealed abstract class CpcReified extends CanonPathComponent {
        def isReified = true
        def wt: ir.WcTypeRef
    }
    
    object CpcReified {
        def unapply(comp: CpcReified) = Some((comp.p, comp.wt))
    }
    
    sealed case class CpcReifiedLv(
        lv: ir.VarName,
        wt: ir.WcTypeRef,
        wps_identity: List[ir.WcPath]
    ) extends CpcReified {
        def p = lv.path
        override def toString = lv.toString
    }
    
    sealed case class CpcReifiedStatic(
        c: ir.ClassName
    ) extends CpcReified {
        val staticC = ir.StaticClassName(c)
        val wt = staticC.ct.withDefaultWghosts(wgs_constructed)
        val p = ir.PathStatic(c)
        val wps_identity = List()
        override def toString = "static[%s]".format(c)
    }
    
    sealed case class CpcReifiedField(
        cpc_base: CanonPathComponent,
        rfd: ir.ReifiedFieldDecl
    ) extends CpcReified {
        def p = cpc_base.p / rfd.name
        def wt = rfd.wt
        def wps_identity = rfd.wps_identity
        override def toString = p.toString
    }
    
    class CanonPath(
        /** Information about equivalent paths we uncovered: */
        val components: List[ir.CanonPathComponent]
    ) {
        assert(!components.isEmpty)
        
        def isReified = components.exists(_.isReified)
        def isGhost = !isReified
        
        // The "representative" component is a good one for error messages:
        def reprComp = {
            val sorted = components.sortWith((c1, c2) => c1.p.toString.length > c2.p.toString.length)
            // First choose a reified one without redirections:
            sorted.find(comp => comp.isReified && comp.wps_identity.isEmpty).getOrElse {
                // Next take any reified path:
                sorted.find(_.isReified).getOrElse {
                    // Next take a ghost path without redirections:
                    sorted.find(comp => comp.wps_identity.isEmpty).getOrElse {
                        // Finally take anything at all:
                        sorted(0)
                    }
                }                
            }
        }        
        def reprPath = reprComp.p
        def reprWt = reprComp match {
            case CpcErrorLv(lv) => throw new DependentFailure(lv)
            case CpcReified(_, wt) => wt
            case CpcGhost(_, c) => c.ct
        }
        
        def reifiedComponents = components.flatMap {
            case rcomp @ ir.CpcReified(_, _) => Some(rcomp)
            case _ => None
        }
        
        def paths = components.map(_.p)
        def wps_identity = components.flatMap(_.wps_identity)
        def wts = reifiedComponents.map(_.wt)
        
        override def toString = "cp(%s)".format(reprPath)
        override def equals(a: Any) = a match {
            case cp: CanonPath => components.equals(cp.components)
            case _ => false
        }
        override def hashCode = components.hashCode
    }
    
    object CanonPath {
        def apply(components: List[ir.CanonPathComponent]) =
            new CanonPath(components)
    }
    
    /** A subtype of `CanonPath` whose components have been found to be 
      * immutable in some interval. */
    sealed class ImmutableCanonPath(
        components: List[ir.CanonPathComponent]
    ) extends CanonPath(components) {
        override def toString = "immcp(%s)".format(reprPath)        
    }
    
    object ImmutableCanonPath {
        def apply(components: List[ir.CanonPathComponent]) =
            new ImmutableCanonPath(components)
    }
    
    sealed case class TeeCeePee[+T <: WcTypeRef](cp: CanonPath, ty: T) {
        def reprPath = cp.reprPath
        def withTy[S <: WcTypeRef](ty: S) = ir.TeeCeePee(cp, ty)
        override def toString = "tcp(%s: %s)".format(reprPath, ty)
    }    
    
    object TeeCeePee {
        object pathType {
            def unapply(tcp: ir.TeeCeePee[_]) = tcp.ty match {
                case pt: ir.PathType => Some(ir.TeeCeePee(tcp.cp, pt))
                case _ => None
            }
        }
        
        object wcClassType {            
            def unapply(tcp: ir.TeeCeePee[_]) = tcp.ty match {
                case wct: ir.WcClassType => Some(ir.TeeCeePee(tcp.cp, wct))
                case _ => None
            }
        }
    }
    
    // ___ Requirements _____________________________________________________
    //
    // Requirements are specified on method headers and allow the callee to
    // dictate conditions to the caller which must hold on method entry.
    
    sealed abstract class Req extends PositionalAst {
        def setDefaultPosOnChildren() { }        
    }
    sealed case class ReqWritableBy(lp: List[Path], lq: List[Path]) extends Req {
        override def toString = "requires %s writableBy %s".format(lp.mkString(", "), lq.mkString(", "))
    }
    sealed case class ReqReadableBy(lp: List[Path], lq: List[Path]) extends Req {
        override def toString = "requires %s readableBy %s".format(lp.mkString(", "), lq.mkString(", "))
    }
    sealed case class ReqSuspends(lp: List[Path], lq: List[Path]) extends Req {
        override def toString = "requires %s suspends %s".format(lp.mkString(", "), lq.mkString(", "))
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
    val f_start = ir.PlainFieldName("start")
    val f_end = ir.PlainFieldName("end")
    val f_super = ir.PlainFieldName("super")
    val f_objCtor = ir.PlainFieldName("ch.ethz.intervals.quals.Constructor")
    val f_parent = ir.PlainFieldName("ch.ethz.intervals.Parent")
    val f_length = ir.PlainFieldName("length")
    
    val m_init = ir.MethodName("<init>")
    val m_toScalar = ir.MethodName("toScalar")
    val m_run = ir.MethodName("run")
    
    val tv_arrayElem = ir.TypeVarName("E")
    
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
    val c_RacyGuard = ir.ClassName("ch.ethz.intervals.guard.RacyGuard")

    // Types used to translate Java constructs into classes:
    //    These types are not treated specially by the type system,
    //    but we provide default, synthetic definitions (see cds_special below).
    val c_any = ir.ClassName("any")        // Universal root class.
    val c_scalar = ir.ClassName("scalar")  // Represents any scalar value.
    val c_void = ir.ClassName("void")      // Represents void values.
    val c_array = ir.ClassName("array")    // Represents arrays.    
    
    // Ghost field creator, declared on root type Object:
    val p_this_objCtor = f_objCtor.thisPath
    val p_this_creator = f_creator.thisPath
    val p_this_intervalCtor = ClassCtorFieldName(c_interval).thisPath
    val p_this_arrayCtor = ClassCtorFieldName(c_array).thisPath
    
    val t_void = ir.ClassType(c_void, List(), List())
    val t_interval = ir.ClassType(c_interval, List(), List())
    
    val wg_objCtorHbNow = ir.WcGhost(ir.f_objCtor, ir.WcHbNow(List()))
    val wg_objCtorHbNowThisObjCtor = ir.WcGhost(ir.f_objCtor, ir.WcHbNow(List(ir.p_this_objCtor)))

    val wt_constructedPoint = ir.WcClassType(c_point, List(wg_objCtorHbNow), List())
    val wt_constructedInterval = ir.WcClassType(c_interval, List(wg_objCtorHbNow), List())    
    
    val req_constructed = ir.ReqHb(List(p_this_objCtor), List(p_mthd))
    
    val wgs_constructed = List(ir.wg_objCtorHbNow)
    val wgs_fieldsDefault = List(ir.WcGhost(ir.f_objCtor, ir.WcHbNow(List(ir.p_this_objCtor))))
    
    val f_racy = ir.PlainFieldName("racy")
    val wt_constructedRacyGuard = c_RacyGuard.ct.withDefaultWghosts(wgs_constructed)
    
    val md_baseCtor = ir.MethodDecl(
        name   = m_init, 
        args   = List(),
        reqs   = List(),
        wt_ret = t_void, 
        wps_identity = List(),
        body   = empty_method_body
    )
    
    val md_emptyCtor = ir.MethodDecl(
        name   = m_init,
        args   = List(),
        reqs   = List(),
        wt_ret = t_void,
        wps_identity = List(),
        body   = ir.StmtSeq(
            List(
                ir.StmtSuperCtor(m_init, List()),
                ir.StmtReturn(None)
            )
        )
    )
    
    // Special classes defined by us to represent some of the oddities of
    // the Java type system:
    val cds_special = List(
        ClassDecl( // Universal root class.
            attrs             = noAttrs,
            name              = c_any,
            ghostFieldDecls   = List(),
            typeVarDecls      = List(),
            superClasses      = List(),
            ghosts            = List(),
            typeArgs          = List(),
            reqs              = List(),
            ctors             = List(md_baseCtor),
            reifiedFieldDecls = List(),
            methods           = List()
        ),
        ClassDecl( // void data.
            attrs             = noAttrs,
            name              = c_void,
            ghostFieldDecls   = List(),
            typeVarDecls      = List(),
            superClasses      = List(c_any),
            ghosts            = List(),
            typeArgs          = List(),
            reqs              = List(),
            ctors             = List(),
            reifiedFieldDecls = List(),
            methods           = List()
        ),
        ClassDecl( // scala data (ints, bytes, floats, etc)
            attrs             = noAttrs,
            name              = c_scalar,
            ghostFieldDecls   = List(),
            typeVarDecls      = List(),
            superClasses      = List(c_any),
            ghosts            = List(),
            typeArgs          = List(),
            reqs              = List(),
            ctors             = List(),
            reifiedFieldDecls = List(),
            methods           = List()
        ),
        ClassDecl( // arrays
            attrs             = noAttrs,
            name              = c_array,
            ghostFieldDecls   = List(),
            typeVarDecls      = List(ir.TypeVarDecl(ir.tv_arrayElem, List(c_any.ct))),
            superClasses      = List(c_object),
            ghosts            = List(),
            typeArgs          = List(),
            reqs              = List(),
            ctors             = List(),
            reifiedFieldDecls = List(
                ReifiedFieldDecl(c_scalar.ct, f_length, p_this_arrayCtor, List())),
            methods           = List(
                MethodDecl(
                    name   = m_arrayGet, 
                    args   = List(
                        ir.LvDecl(ir.VarName("index"), ir.c_scalar.ct, List())),
                    reqs   = List(
                        ir.ReqReadableBy(List(p_this_creator), List(p_mthd))),
                    wt_ret = ir.tv_arrayElem.thisPtype, 
                    wps_identity = List(),
                    body   = empty_method_body),
                MethodDecl(
                    name   = m_arraySet, 
                    args   = List(
                        ir.LvDecl(ir.VarName("index"), ir.c_scalar.ct, List()),
                        ir.LvDecl(ir.VarName("value"), ir.tv_arrayElem.thisPtype, List())),
                    reqs   = List(
                        ir.ReqWritableBy(List(p_this_creator), List(p_mthd))),
                    wt_ret = c_void.ct, 
                    wps_identity = List(),
                    body   = empty_method_body)
            )
        )        
    )
    
    // Simplified versions of built-in classes used in our unit tests.
    val cds_unit_test = List(
        ClassDecl(
            attrs             = noAttrs,
            name              = c_object,
            ghostFieldDecls   = List(GhostFieldDecl(ir.f_creator, ir.c_guard)),
            typeVarDecls      = List(),
            superClasses      = List(),
            ghosts            = List(),
            typeArgs          = List(),
            reqs              = List(),
            ctors             = List(md_emptyCtor),
            reifiedFieldDecls = List(),
            methods           = List(
                MethodDecl(
                    name   = m_toString, 
                    args   = List(),
                    reqs   = List(
                        ir.ReqReadableBy(List(p_this_creator), List(p_mthd)),
                        req_constructed),
                    wt_ret = c_string.ct, 
                    wps_identity = List(),
                    body   = empty_method_body))
        ),
        ClassDecl(
            attrs             = noAttrs,
            name              = c_string,
            ghostFieldDecls   = List(),
            typeVarDecls      = List(),
            superClasses      = List(c_object),
            ghosts            = List(Ghost(f_creator, p_this_objCtor)),
            typeArgs          = List(),
            reqs              = List(),
            ctors             = List(md_emptyCtor),
            reifiedFieldDecls = List(),
            methods           = List()
        ),
        ClassDecl(
            attrs             = interfaceAttrs,
            name              = c_guard,
            ghostFieldDecls   = List(),
            typeVarDecls      = List(),
            superClasses      = List(c_object),
            ghosts            = List(),
            typeArgs          = List(),
            reqs              = List(),
            ctors             = List(md_emptyCtor),
            reifiedFieldDecls = List(),
            methods           = List()
        ),
        ClassDecl(
            attrs             = noAttrs,
            name              = c_RacyGuard,
            ghostFieldDecls   = List(),
            typeVarDecls      = List(),
            superClasses      = List(c_guard),
            ghosts            = List(),
            typeArgs          = List(),
            reqs              = List(),
            ctors             = List(md_emptyCtor),
            reifiedFieldDecls = List(),
            methods           = List()
        ),
        ClassDecl(
            attrs             = noAttrs,
            name              = ir.StaticClassName(c_RacyGuard),
            ghostFieldDecls   = List(),
            typeVarDecls      = List(),
            superClasses      = List(c_any),
            ghosts            = List(),
            typeArgs          = List(),
            reqs              = List(),
            ctors             = List(md_emptyCtor),
            reifiedFieldDecls = List(
                ReifiedFieldDecl(wt_constructedRacyGuard, f_racy, p_this_objCtor, List())),
            methods           = List()
        ),
        ClassDecl( 
            // Note: In this version, there is nothing linking @Parent to anything else.
            // In the real code, however, the Dependency argument is linked to @Parent.
            attrs             = noAttrs,
            name              = c_interval,
            ghostFieldDecls   = List(GhostFieldDecl(ir.f_parent, ir.c_interval)),
            typeVarDecls      = List(),
            superClasses      = List(c_object, c_guard),
            ghosts            = List(Ghost(f_creator, p_this_objCtor)),
            typeArgs          = List(),
            reqs              = List(),
            ctors             = List(md_emptyCtor),
            reifiedFieldDecls = List(
                ReifiedFieldDecl(wt_constructedPoint, f_start, p_this_intervalCtor, List()),
                ReifiedFieldDecl(wt_constructedPoint, f_end, p_this_intervalCtor, List())),
            methods           = List(
                MethodDecl(
                    name   = m_run, 
                    args   = List(),
                    reqs   = List(
                        ir.ReqSuspends(List(p_mthd), List(p_this)),
                        ir.ReqHb(List(p_this_objCtor), List(p_mthd))),
                    wt_ret = t_void, 
                    wps_identity = List(),
                    body   = empty_method_body))
        ),
        ClassDecl(
            attrs             = noAttrs,
            name              = c_point,
            ghostFieldDecls   = List(),
            typeVarDecls      = List(),
            superClasses      = List(c_object),
            ghosts            = List(Ghost(f_creator, p_this_objCtor)),
            typeArgs          = List(),
            reqs              = List(),
            ctors             = List(md_emptyCtor),
            reifiedFieldDecls = List(),
            methods           = List()
        ),
        ClassDecl(
            attrs             = noAttrs,
            name              = c_lock,
            ghostFieldDecls   = List(),
            typeVarDecls      = List(),
            superClasses      = List(c_object, c_guard),
            ghosts            = List(Ghost(f_creator, p_this_objCtor)),
            typeArgs          = List(),
            reqs              = List(),
            ctors             = List(md_emptyCtor),
            reifiedFieldDecls = List(),
            methods           = List()
        )
    )
    
    def emptyStaticCounterpart(c: ir.ClassName) = ir.ClassDecl(
        attrs             = noAttrs,
        name              = StaticClassName(c),
        ghostFieldDecls   = List(),
        typeVarDecls      = List(),
        superClasses      = List(c_any),
        ghosts            = List(),
        typeArgs          = List(),
        reqs              = List(),
        ctors             = List(md_emptyCtor),
        reifiedFieldDecls = List(),
        methods           = List()
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