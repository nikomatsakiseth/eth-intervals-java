package harmonic.compiler

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import Util._

abstract class Ast {
    
    import Ast.Node    
    import Ast.QualName
    import Ast.ClassName
    import Ast.PackageName
    import Ast.SimpleName
    import Ast.MemberName
    import Ast.SimpleOrMemberName
    import Ast.VarName
        
    /** Names which get changed during resolve */
    type QN <: QualName     // Name of a class
    type LN <: Ast.Name     // Name of a local variable
    type FN <: Ast.Name     // Name of a field
    type VN <: Ast.Name     // Either local or member name
    
    /** References to types */
    type OTR <: Node
    type TR <: Node
    
    /** The form of statements */
    type Stmt <: ParseStmt
    
    /** The form of expressions */
    type Expr <: ParseTlExpr
    
    /** The form of expressions */
    type AstPath <: Node
    
    /** Nested expressions eventually become restricted */
    type NE <: ParseTlExpr
    
    /** Method receiver expressions eventually become restricted */
    type Rcvr <: ParseRcvr
    
    /** Field owner expressions eventually become restricted */
    type Owner <: ParseOwner
    
    /** Type of a template's parameter: becomes a Param */
    type TmplLv <: AstPattern
    
    /** Type of an expression */
    type Ty
    
    /** Type of a class */
    type TyClass <: Ty
    
    /** Type of a tuple */
    type TyTuple <: Ty
    
    /** Data attached to class declarations */
    type CSym
    
    /** Data attached to field declarations, access */
    type VSym
    
    /** Data attached to method declarations, calls */
    type MSym
    
    /** Data attached to method calls */
    type MCallData
    
    def errTy: Ty
    def symTy(vsym: VSym): Ty
    def tupleTy(tys: List[Ty]): TyTuple
    def mthdSym(data: MCallData): MSym
    def returnTy(data: MCallData): Ty
    
    private def printSepFunc(out: PrettyPrinter, asts: List[Node], sepfunc: (() => Unit)) {
        asts.dropRight(1).foreach { ast =>
            ast.print(out)
            sepfunc()
        }
        asts.takeRight(1).foreach(_.print(out))
    }
    
    private def printSep(out: PrettyPrinter, asts: List[Node], sep: String) {
        printSepFunc(out, asts, () => out.write(sep))
    }
    
    // ___ Pre-resolve Type Names ___________________________________________
    //
    // Immediately after parsing, most names have not yet been resolved, 
    // and thus we use the following classes.  RelNames will 
    // be converted to a ClassName.
    
    sealed abstract class RelName extends QualName {
        def /(nm: String) = RelDot(this, nm)   
        
        def component: String
        
        def toClass(pkg: Name.Package): Name.Class
        def toPackage(pkg: Name.Package): Name.Package
    }
    
    case class RelBase(nm: String) extends RelName {
        override def toString = nm
        def component = nm
        
        def toClass(pkg: Name.Package) = Name.Class(pkg, nm)
        def toPackage(pkg: Name.Package) = Name.Subpackage(pkg, nm)
    }
    
    case class RelDot(context: RelName, component: String) extends RelName {
        override def toString = "%s.%s".format(context, component)
        
        def toClass(pkg: Name.Package) = Name.Class(context.toPackage(pkg), component)
        def toPackage(pkg: Name.Package) = Name.Subpackage(context.toPackage(pkg), component)
    }
    
    // ___ Declarations _____________________________________________________
    
    case class CompUnit(
        pkg: PackageName,
        imports: List[ImportDecl],
        classes: List[ClassDecl]
    ) extends Node {
        
        override def print(out: PrettyPrinter) {
            out.writeln("package %s;", pkg)
            imports.foreach(_.println(out))
            printSepFunc(out, classes, () => out.writeln(""))
        }
        
    }
    
    sealed abstract class MemberDecl extends Node {
        def annotations: List[Annotation]
        
        def asMethodNamed(name: Name.Method): Option[MethodDecl] = None
        def asFieldNamed(name: Name.Var): Option[FieldDecl] = None
    }
    
    sealed abstract class ImportDecl extends Node
    
    case class ImportOne(
        fromName: RelName,
        toName: RelBase
    ) extends ImportDecl {
        override def toString = "import %s(%s)".format(fromName, toName)
    }
    
    case class ImportAll(
        fromName: RelName
    ) extends ImportDecl {
        override def toString = "import %s.*".format(fromName)
    }
    
    case class ClassDecl(
        name: QN,
        annotations: List[Annotation],
        superClasses: List[QN],
        pattern: Param,
        members: List[MemberDecl],
        sym: CSym
    ) extends Node { // TODO Inner classes
        override def toString = "[class %s%s]".format(
            name, pattern
        )
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.println(out))
            out.write("class %s", name)
            pattern.println(out)
            if(!superClasses.isEmpty) {
                out.write("extends ")
                printSep(out, superClasses, ", ")
            }
            out.indented("{", "}") {
                members.foreach(_.println(out))
            }
        }
    }
    
    private def printOptBody(out: PrettyPrinter, optBody: Option[Body]) = optBody match {
        case Some(b) => b.print(out)
        case None => out.write(";")
    }
    
    case class IntervalDecl(
        annotations: List[Annotation],
        name: VarName,
        optParent: Option[AstPath],
        optBody: Option[Body]
    ) extends MemberDecl {
        override def toString = "[interval %s(%s)]".format(name, optParent)
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.println(out))
            optParent match {
                case None => out.writeln("interval %s", name)
                case Some(parent) => out.writeln("interval %s(%s)", name, parent)
            }
            printOptBody(out, optBody)
        }
    }
    
    case class MethodDecl(
        annotations: List[Annotation],
        name: Name.Method,
        receiverSym: VSym,
        params: List[Param],
        returnTref: OTR,
        requirements: List[PathRequirement],
        optBody: Option[Body]
    ) extends MemberDecl {
        override def toString = "[method %s]".format(name)
        override def asMethodNamed(mthdName: Name.Method) = {
            if(mthdName == name) Some(this)
            else None
        }
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.println(out))
            returnTref.printsp(out)
            name.parts.zip(params).foreach { case (part, param) =>
                out.write(part)
                param.printsp(out)
            }
            out.writeln("")
            requirements.foreach(_.println(out))
            printOptBody(out, optBody)
        }
    }
    
    case class PathRequirement(
        left: AstPath, rel: PcRel, right: AstPath
    ) extends Node {
        override def toString = "%s %s %s".format(left, rel, right)
        
        override def print(out: PrettyPrinter) {
            left.printsp(out)
            out.write("%s ", rel)
            right.print(out)
        }
    }
    
    case class FieldDecl(
        annotations: List[Annotation],
        name: VarName,
        tref: OTR,
        optBody: Option[Body]
    ) extends MemberDecl {
        override def asFieldNamed(fldName: Name.Var) = {
            if(name == fldName) Some(this)
            else None
        }
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.println(out))
            tref.printsp(out)
            name.print(out)
            optBody match {
                case None =>
                case Some(body) => 
                    out.write(" = ")
                    body.print(out)
            }
            out.write(";")
        }
    }
    
    case class RelDecl(
        annotations: List[Annotation],        
        left: AstPath, 
        kind: PcRel,
        right: AstPath
    ) extends MemberDecl {
        override def toString = "%s %s %s".format(left, kind, right)
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.println(out))
            left.print(out)
            out.write(" %s ", kind)
            right.print(out)
            out.write(";")
        }
    }
    
    case class Annotation(
        name: QN
    ) extends Node {
        override def toString = "[%s]".format(name)
        
        override def print(out: PrettyPrinter) {
            out.write("[")
            name.print(out)
            out.write("]")
        }
    }
    
    // ___ Params and Lvalues _______________________________________________
    //
    // In general patterns are used for "assignable" things.  There are several
    // variations:
    // 
    // - Param for method and class parameters
    // - Lvalue for the left-hand-side of an assignment
    // - AstPattern is a base class for Param and Lvalue
    // - ParseLvalue is the version of Lvalue generated by the parser
    
    sealed abstract class AstPattern extends Node {
        def symbols: List[VSym]
        def ty: Ty
    }
    sealed abstract trait TupleAstPattern extends AstPattern {
        def subpatterns: List[AstPattern]
        def ty: TyTuple = tupleTy(subpatterns.map(_.ty))
        
        def symbols = subpatterns.flatMap(_.symbols)

        override def toString = "(%s)".format(subpatterns.mkString(", "))
        
        override def print(out: PrettyPrinter) {
            out.write("(")
            printSep(out, subpatterns, ", ")
            out.write(")")
        }
    }
    sealed abstract trait VarAstPattern extends AstPattern {
        def sym: VSym
        def ty = symTy(sym)
        
        def symbols = List(sym)
    }
    
    sealed abstract class Param extends AstPattern
    
    case class TupleParam(
        params: List[Param]
    ) extends Param with TupleAstPattern {
        def subpatterns = params
    }
    
    case class VarParam(
        annotations: List[Annotation], 
        tref: TR,
        name: LN,
        sym: VSym
    ) extends Param with VarAstPattern {
        override def toString = "%s %s: %s".format(annotations.mkString(" "), tref, name)
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.printsp(out))
            name.print(out)
            out.write(": ")
            tref.printsp(out)
        }        
    }
    
    sealed abstract class Lvalue extends AstPattern
    
    case class TupleLvalue(
        lvalues: List[Lvalue]
    ) extends Lvalue with TupleAstPattern {
        def subpatterns = lvalues
    }
    
    // After parsing, we have only DeclareVarLvalues.
    // During resolve phase, they are converted as needed.
    case class DeclareVarLvalue(
        annotations: List[Annotation], 
        tref: OTR,
        name: LN,
        sym: VSym
    ) extends Lvalue with VarAstPattern {
        override def toString = "%s %s: %s".format(annotations.mkString(" "), tref, name)
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.printsp(out))
            name.print(out)
            out.write(": ")
            tref.printsp(out)
        }        
    }
    
    case class ReassignVarLvalue(
        name: VarName,
        sym: VSym
    ) extends Lvalue with VarAstPattern {
        override def toString = name.toString
        
        override def print(out: PrettyPrinter) {
            name.print(out)
        }        
    }
    
    case class FieldLvalue(
        name: MemberName,
        sym: VSym
    ) extends Lvalue with VarAstPattern {
        override def toString = name.toString
        
        override def print(out: PrettyPrinter) {
            name.print(out)
        }        
    }
    
    // ___ Type References __________________________________________________
    //
    // T.R. go through several phases.  After parsing they are basically
    // just a parse tree.  After resolution they have a bit more structure.
    // After lowering they are just a wrapper around a `Type.Ref`.

    // ______ Types after lowering __________________________________________
    
    /** Symbolic ref. to a type, used after lowering */
    case class TypeRef(ty: Type.Ref) extends Node
    
    // ______ Inferable types _______________________________________________
    //
    // During Parse and Resolve, some type refs may be omitted and 
    // InferredTypeRef() used in their place.
    
    /** Base class for optional (i.e., inferable) type references. */
    sealed abstract class OptionalParseTypeRef extends Node
    
    /** Base class for optional (i.e., inferable) type references. */
    sealed abstract trait OptionalResolveTypeRef extends Node
    
    /** A type the user omitted.  After lowering will be made explicit. */
    case class InferredTypeRef() extends OptionalParseTypeRef with OptionalResolveTypeRef {
        override def toString = "<infer>"
    }
    
    // ______ Types after parse _____________________________________________
    
    sealed abstract trait ParseTypeRef extends OptionalParseTypeRef
    
    case class PathType(path: AstPath) extends ParseTypeRef {
        override def toString = path.toString        
    }
    
    case class ConstrainedType(path: AstPath, typeArgs: List[TypeArg]) extends ParseTypeRef {
        override def toString = "%s[%s]".format(path, typeArgs.mkString(", "))        
    }
    
    // ______ Types after resolve ___________________________________________
    
    sealed abstract trait ResolveTypeRef extends OptionalResolveTypeRef
    
    case class TypeVar(path: AstPath, typeVar: SimpleOrMemberName) extends ResolveTypeRef {
        override def toString = "%s.%s".format(path, typeVar)
    }
    
    case class ClassType(className: ClassName, typeArgs: List[TypeArg]) extends ResolveTypeRef {
        override def toString = "%s[%s]".format(className, typeArgs.mkString(", "))
    }
    
    // ______ Parse and Resolve Both_________________________________________
    
    case class NullType() extends ParseTypeRef with ResolveTypeRef {
        override def toString = Type.Null.toString
    }
    
    case class TupleType(types: List[TR]) extends ParseTypeRef with ResolveTypeRef {
        override def toString = "(%s)".format(types.mkString(", "))
        
        override def print(out: PrettyPrinter) {
            out.write("(")
            printSep(out, types, ", ")
            out.write(")")
        }
    }

    sealed abstract class TypeArg extends Node {
        def forTypeVar(name: VarName): Option[TypeTypeArg]
        def forGhost(name: VarName): Option[PathTypeArg]
    }
    
    case class TypeTypeArg(name: SimpleOrMemberName, rel: TcRel, typeRef: TR) extends TypeArg {
        override def toString = "%s %s %s".format(name, rel, typeRef)
        
        def forTypeVar(aName: VarName) = if(name == aName) Some(this) else None
        def forGhost(aName: VarName) = None
    }
    
    case class PathTypeArg(name: SimpleOrMemberName, rel: PcRel, path: AstPath) extends TypeArg {
        override def toString = "%s %s %s".format(name, rel, path)
        
        def forTypeVar(aName: VarName) = None
        def forGhost(aName: VarName) = if(name == aName) Some(this) else None
    }
    
    // ___ Paths ____________________________________________________________
    // 
    // Paths are like mini-expressions that always have the form `a.b.c`
    
    sealed abstract trait ParsePath extends Node {
        def ty: Ty
    }
    
    case class PathErr(name: String) extends ParsePath {
        def ty = errTy
        override def toString = "<err:%s>".format(name)
    }
    
    case class PathBase(name: VN, sym: VSym) extends ParsePath {
        override def toString = name.toString
        def ty = symTy(sym)
    }
    
    case class PathDot(owner: AstPath, name: FN, sym: VSym, ty: Ty) extends ParsePath {
        override def toString = owner + "." + name
    }
    
    case class TypedPath(path: Path.Typed) extends Node
    
    // ___ Statements and Expressions _______________________________________
    
    case class Body(stmts: List[Stmt]) extends Node {
        override def toString = "{...}"
        override def print(out: PrettyPrinter) {
            out.indented("{", "}") {
                stmts.foreach(_.printsemiln(out))
            }            
        }
    }
    
    /** All kinds of statements which users can directly enter.
      * Includes some syntactic sugar. */
    sealed abstract trait ParseStmt extends Node {
        def ty: Ty
        
        def printsemiln(out: PrettyPrinter) {
            print(out)
            out.writeln(";")
        }
    }
    
    /** Those statements that are still used after lowering. */
    sealed abstract trait LowerStmt extends ParseStmt

    /** Any expression at all. */
    sealed abstract trait AnyExpr extends Node {
        def ty: Ty        
    }
    
    /** Method receivers after parsing. */
    sealed abstract trait ParseRcvr extends Node
    
    /** Field owners after parsing. */
    sealed abstract trait ParseOwner extends ParseRcvr
    
    /** Top-level expressions after parsing. */
    sealed abstract trait ParseTlExpr extends ParseRcvr with ParseOwner with ParseStmt
    
    /** Top-level expressions after resolve. */
    sealed abstract trait ResolveTlExpr extends ParseTlExpr
    
    /** Method receivers after lowering. */
    sealed abstract trait LowerRcvr extends ParseRcvr
    
    /** Field owners after lowering. */
    sealed abstract trait LowerOwner extends LowerRcvr with ParseOwner
    
    /** Top-level expressions after lowering. */
    sealed abstract trait LowerTlExpr extends ParseTlExpr with LowerStmt

    /** Expressions that are always safe to evaluate without the
      * possibility of a race condition. After lowering, only 
      * `AtomicExpr` may be nested within other expressions. */
    sealed abstract trait AtomicExpr extends LowerTlExpr with LowerOwner
    
    case class Tuple(exprs: List[NE]) extends AtomicExpr {
        override def toString = "(%s)".format(exprs.mkString(", "))
        
        def ty = tupleTy(exprs.map(_.ty))
        
        override def print(out: PrettyPrinter) {
            out.write("(")
            printSep(out, exprs, ", ")
            out.write(")")
        }        
    }
    
    case class Block(
        async: Boolean, 
        returnTref: OTR, 
        param: TmplLv, 
        stmts: List[Stmt], 
        ty: Ty
    ) extends LowerTlExpr {
        def className = if(async) Name.AsyncBlockQual else Name.BlockQual

        private[this] def sep = if(async) ("{{", "}}") else ("{", "}")
        
        override def toString = {
            val (l, r) = sep
            "%s %s %s -> ... %s".format(l, returnTref, param, r)
        }
        
        override def print(out: PrettyPrinter) {
            val (l, r) = sep
            out.indented(l, r) { 
                returnTref.printsp(out)
                param.printsp(out)
                out.writeln(" ->")
                stmts.foreach(_.printsemiln(out)) 
            }
        }
    }
    
    case class Cast(expr: NE, typeRef: TR) extends AtomicExpr {
        override def toString = "(%s)(%s)".format(typeRef, expr)
        
        override def print(out: PrettyPrinter) {
            out.write("(")
            typeRef.print(out)
            out.write(")")
            out.write("(")
            expr.print(out)
            out.write(")")
        }
        
    }
    
    case class Literal(obj: Object, ty: Ty) extends LowerTlExpr {
        override def toString = obj.toString
    }
    
    case class Assign(lvalue: Lvalue, rvalue: Expr) 
    extends LowerStmt {
        def ty = rvalue.ty
        override def toString = "%s = %s".format(lvalue, rvalue)
        
        override def print(out: PrettyPrinter) {
            lvalue.print(out)
            out.write(" = ")
            rvalue.print(out)
        }        
    }
    
    // After parsing, paths like `a.b.c` are represented
    // using one of these nodes rather than Var and Field
    // nodes.  This helps to reduce duplicate code during
    // the reduction phase.
    case class PathExpr(path: AstPath) extends ParseTlExpr {
        override def toString = path.toString
        override def print(out: PrettyPrinter) {
            path.print(out)
        }
    }
    
    // n.b.: A Var could refer to any sort of variable, not only a 
    // local variable.  For example, a field of the current class or
    // one of its enclosing classes.
    case class Var(name: LN, sym: VSym) extends AtomicExpr {
        override def toString = name.toString
        def ty = symTy(sym)
        
        override def print(out: PrettyPrinter) {
            out.write(name.toString)
            out.write(" /*%s*/".format(sym))
        }
    }
    
    case class Field(owner: NE, name: FN, sym: VSym, ty: Ty) extends LowerTlExpr {
        override def toString = "%s.%s".format(owner, name)
        
        override def print(out: PrettyPrinter) {
            owner.printdot(out)
            name.print(out)
            out.write(" /*%s;%s*/".format(sym, ty))
        }
    }
    
    case class Super(ty: Ty) extends LowerRcvr {
        override def toString = "super"
    }
    
    case class Static(name: Name.Class) extends LowerOwner {
        override def toString = name.toString
    }
    
    case class CallPart(ident: String, arg: NE) extends Node {
        override def toString = "%s %s".format(ident, arg)
        
        override def print(out: PrettyPrinter) {
            out.write("%s ", ident)
            arg.print(out)
        }
    }
    case class MethodCall(rcvr: Rcvr, parts: List[CallPart], data: MCallData)
    extends LowerTlExpr {
        def name = Name.Method(parts.map(_.ident))
        def args = parts.map(_.arg)
        def ty = returnTy(data)
        override def toString = "%s.%s".format(rcvr, parts.mkString(" "))
        
        override def print(out: PrettyPrinter) {
            rcvr.printdot(out)
            printSep(out, parts, " ")
            out.write(" /*%s;%s*/".format(mthdSym(data), returnTy(data)))
        }        
    }
    
    /** Used to create new instances of classes. */
    case class NewCtor(tref: TR, arg: NE, msym: MSym, ty: TyClass) extends LowerTlExpr {
        override def toString = "new %s%s".format(tref, arg)
        
        override def print(out: PrettyPrinter) {
            out.write("new ")
            tref.print(out)
            arg.print(out)
        }        
    }
    
    /** Used to create new instances of classes. */
    case class NewAnon(tref: TR, arg: NE, members: List[MemberDecl], csym: CSym, msym: MSym, ty: TyClass) 
    extends LowerTlExpr {
        override def toString = "new %s%s { ... }".format(tref, arg)
        
        override def print(out: PrettyPrinter) {
            out.write("new ")
            tref.print(out)
            arg.print(out)
            out.indented("{", "}") {
                members.foreach(_.println(out))
            }
        }        
    }
    
    case class Null(ty: Ty)
    extends LowerTlExpr {
        override def toString = "null"
    }
    
    /** ImpVoid is inserted when there is an empty set of statements like "{ }". */
    case class ImpVoid(ty: Ty)
    extends ResolveTlExpr {
        override def toString = "<(Void)null>"
    }
    
    /** Inserted as the default receiver for methods etc */
    case class ImpThis(ty: Ty)
    extends ParseTlExpr {
        override def toString = "<this>"
    }
   
    case class Labeled(name: VarName, body: Body)
    extends LowerStmt {
        def ty = body.stmts.last.ty
        override def toString = "%s: %s".format(name, body)
        
        override def print(out: PrettyPrinter) {
            out.write("%s: ", name)
            body.print(out)
        }
    }
    
}

object Ast {
    
    // ___ Invariant Parts __________________________________________________
    
    sealed abstract class Node extends Positional with Product {
        def print(out: PrettyPrinter) {
            out.write(toString)
        }
        
        def printsp(out: PrettyPrinter) {
            print(out)
            out.write(" ")
        }
        
        def printdot(out: PrettyPrinter) {
            print(out)
            out.write(".")
        }
        
        def printc(out: PrettyPrinter) {
            print(out)
            out.write(", ")
        }
        
        def println(out: PrettyPrinter) {
            print(out)
            out.writeln("")
        }
    }
    
    // ______ Names _________________________________________________________
    
    sealed abstract class Name extends Node
    
    // Field names immediately after parsing (and after resolve)
    sealed abstract trait SimpleOrMemberName extends Name

    // An unqualified name which will eventually be converted to
    // a LocalName or MemberName.
    case class SimpleName(text: String) extends SimpleOrMemberName {
        override def toString = text
    }
    
    sealed abstract class QualName extends Name

    sealed case class PackageName(name: Name.Package) extends Name {
        override def toString = name.toString
    }
    
    sealed case class ClassName(name: Name.Class) extends QualName {
        override def toString = name.toString
    }
    
    sealed abstract class VarName extends Name {
        def name: Name.Var
    }
    
    sealed case class MemberName(name: Name.MemberVar) extends VarName with SimpleOrMemberName {
        override def toString = name.toString
    }
    
    sealed case class LocalName(name: Name.LocalVar) extends VarName {
        override def toString = name.toString
    }
    
    // ___ Phases ___________________________________________________________

    /** Parse phase: relative names unresolved and
      * types uninferred. */
    object Parse extends Ast {
        type QN = RelName
        type LN = SimpleName
        type FN = SimpleOrMemberName 
        type OTR = OptionalParseTypeRef
        type TR = ParseTypeRef
        type NE = ParseTlExpr
        type Stmt = ParseStmt
        type Expr = ParseTlExpr
        type AstPath = ParsePath
        type Rcvr = ParseRcvr
        type Owner = ParseOwner
        type TmplLv = TupleLvalue
        type CSym = Unit
        type VSym = Unit
        type MSym = Unit
        type MCallData = Unit
        type Ty = Unit
        type TyClass = Unit
        type TyTuple = Unit
        
        def errTy = ()
        def symTy(unit: Unit) = ()
        def tupleTy(tys: List[Ty]) = ()
        def mthdSym(unit: Unit) = ()
        def returnTy(unit: Unit) = ()
        
        def definedClasses(cunit: CompUnit) =
            cunit.classes.map(cdecl => (cdecl.name.toClass(cunit.pkg.name), cdecl))
    }
    
    /** After Resolve(): relative names resolved. */
    object Resolve extends Ast {
        type QN = ClassName
        type LN = LocalName
        type FN = SimpleOrMemberName 
        type OTR = OptionalResolveTypeRef
        type TR = ResolveTypeRef
        type NE = ParseTlExpr
        type Stmt = ParseStmt
        type Expr = ResolveTlExpr
        type AstPath = ParsePath
        type Rcvr = ParseRcvr
        type Owner = ParseOwner
        type TmplLv = Lvalue
        type CSym = Unit
        type VSym = Unit
        type MSym = Unit
        type MCallData = Unit
        type Ty = Unit
        type TyClass = Unit
        type TyTuple = Unit

        def errTy = ()
        def symTy(unit: Unit) = ()
        def tupleTy(tys: List[Ty]) = ()
        def mthdSym(unit: Unit) = ()
        def returnTy(unit: Unit) = ()
    }

    /** After Lower(): types inferred (but not checked!) and 
      * expressions broken apart into statements. */
    object Lower extends Ast {
        type QN = ClassName
        type LN = LocalName
        type FN = MemberName 
        type OTR = TypeRef
        type TR = TypeRef
        type NE = AtomicExpr
        type Stmt = LowerStmt
        type Expr = LowerTlExpr
        type AstPath = TypedPath
        type Rcvr = LowerRcvr
        type Owner = LowerOwner
        type TmplLv = Param
        type CSym = Symbol.Class
        type VSym = Symbol.Var
        type MSym = Symbol.Method
        type MCallData = (Symbol.Method, Symbol.MethodSignature[Pattern.Anon])
        type Ty = Type.Ref
        type TyClass = Type.Class
        type TyTuple = Type.Tuple

        def errTy = Type.Object
        def symTy(vsym: Symbol.Var) = vsym.ty
        def tupleTy(tys: List[Type.Ref]) = Type.Tuple(tys)
        def mthdSym(data: MCallData) = data._1
        def returnTy(data: MCallData) = data._2.returnTy
        
        def toPatternRef(pat: Param): Pattern.Ref = pat match {
            case TupleParam(params) => Pattern.Tuple(params.map(toPatternRef))
            case VarParam(_, _, _, sym) => Pattern.Var(sym.name, sym.ty)
        }
        
        def methodId(clsName: Name.Qual, mdecl: MethodDecl) = {
            Symbol.MethodId(clsName, mdecl.name, mdecl.params.map(toPatternRef))
        }
            
    }

}
