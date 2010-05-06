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
    import Ast.MemberName
    import Ast.LocalName
    import Ast.VarName
        
    // ___ Type variables ___________________________________________________
    //
    // The shape of the final AST that is used in byte-code
    // generation is quite different from the version that is produced by
    // the parser: all names are fully qualified, for example, and all 
    // expressions are typed.  
    //
    // These type variables specify those changes.  Each of them represents
    // one kind of reference which changes type between phases.  In some cases,
    // such as symbols, the relevant types are simply Unit until lowering, at
    // which point they become actual symbols.  In others, such as names, the
    // types go through a number of small changes in each phase.
    //
    // The overall goal is to specify the tree as precisely as possible so
    // that match expressions need never consider impossible cases.
    
    type CN <: QualName     // Eventually ClassName
    type CND <: Ast.Name    // Name of class in declaration (starts out unqual, becomes qual)
    type MN <: Ast.Name     // The potentially classless name of a member (e.g. "bar" in "foo.bar")
    type MNC <: MN          // Member name with class (e.g., "foo.(String.bar)")
    type MND <: Ast.Name    // Name of member in declaration
    type VN <: Ast.Name     // Eventually VarName
    
    type OTR <: Node        // Optional type reference (i.e., inferrable)
    type TR <: Node         // Mandatory type reference
    
    type Stmt <: ParseStmt  
    type Expr <: ParseTlExpr
    type AstPath <: TypedNode
    
    type NE <: ParseTlExpr
    type Rcvr <: ParseRcvr
    type Owner <: ParseOwner
    
    type Ty
    type TyClass <: Ty
    type TyTuple <: Ty
    
    type CSym
    type VSym
    type LVSym <: VSym
    type FSym <: VSym
    type MSym
    
    type MCallData
    
    def errTy: Ty
    def toTy(ty: Type.Ref): Ty
    def vsymTy(vsym: VSym): Ty
    def trefTy(tref: TR): Ty
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
    
    sealed abstract trait TypedNode extends Node {
        def ty: Ty
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
        override def toString = "import %s -> %s".format(fromName, toName)
    }
    
    case class ImportAll(
        fromName: RelName
    ) extends ImportDecl {
        override def toString = "import %s.*".format(fromName)
    }
    
    case class ClassDecl(
        name: CND,
        annotations: List[Annotation],
        superClasses: List[CN],
        pattern: Param[MSym],
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
        name: MND,
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
        receiverSym: LVSym,
        params: List[Param[LVSym]],
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
        name: MND,
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
        name: CN
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
    
    sealed abstract class AstPattern[+S <: VSym] extends Node {
        def symbols: List[S]
        def ty: Ty
    }
    sealed abstract trait TupleAstPattern[+S <: VSym] extends AstPattern[S] {
        def subpatterns: List[AstPattern[S]]
        def ty: TyTuple = tupleTy(subpatterns.map(_.ty))
        
        def symbols = subpatterns.flatMap(_.symbols)

        override def toString = "(%s)".format(subpatterns.mkString(", "))
        
        override def print(out: PrettyPrinter) {
            out.write("(")
            printSep(out, subpatterns, ", ")
            out.write(")")
        }
    }
    sealed abstract trait VarAstPattern[+S <: VSym] extends AstPattern[S] {
        def sym: S
        def ty = vsymTy(sym)
        
        def symbols = List(sym)
    }
    
    sealed abstract class Param[+S <: VSym] extends AstPattern {
        def sym: S
    }
    
    case class TupleParam[+S <: VSym](
        params: List[Param[S]]
    ) extends Param with TupleAstPattern {
        def subpatterns = params
    }
    
    case class VarParam[+S <: VSym](
        annotations: List[Annotation], 
        tref: OTR,
        name: LocalName,
        sym: S
    ) extends Param with VarAstPattern {
        override def toString = "%s %s: %s".format(annotations.mkString(" "), tref, name)
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.printsp(out))
            name.print(out)
            out.write(": ")
            tref.printsp(out)
        }        
    }
    
    sealed abstract class Lvalue extends AstPattern[LVSym]
    
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
        name: LocalName,
        sym: LVSym
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
        name: LocalName,
        sym: LVSym
    ) extends Lvalue with VarAstPattern {
        override def toString = name.toString
        
        override def print(out: PrettyPrinter) {
            name.print(out)
        }        
    }
    
    case class FieldLvalue(
        name: MNC,
        sym: FSym
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
    
    case class TypeVar(path: AstPath, typeVar: MN) extends ResolveTypeRef {
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
    
    case class TypeTypeArg(name: MN, rel: TcRel, typeRef: TR) extends TypeArg {
        override def toString = "%s %s %s".format(name, rel, typeRef)
        
        def forTypeVar(aName: VarName) = if(name == aName) Some(this) else None
        def forGhost(aName: VarName) = None
    }
    
    case class PathTypeArg(name: MN, rel: PcRel, path: AstPath) extends TypeArg {
        override def toString = "%s %s %s".format(name, rel, path)
        
        def forTypeVar(aName: VarName) = None
        def forGhost(aName: VarName) = if(name == aName) Some(this) else None
    }
    
    // ___ Paths ____________________________________________________________
    // 
    // Paths are like mini-expressions that always have the form `a.b.c`

    sealed abstract trait ResolvePath extends TypedNode
    
    sealed abstract trait ParsePath extends ResolvePath
    
    case class PathErr(name: String) extends ResolvePath {
        def ty = errTy
        override def toString = "<err:%s>".format(name)
    }
    
    case class PathBase(name: VN, sym: VSym) extends ParsePath {
        override def toString = name.toString
        def ty = vsymTy(sym)
    }
    
    case class PathDot(owner: AstPath, name: MN, sym: FSym, ty: Ty) extends ParsePath {
        override def toString = owner + "." + name
    }
    
    case class TypedPath(path: Path.Typed) extends TypedNode {
        def ty = toTy(path.ty)
    }
    
    // ___ Statements and Expressions _______________________________________
    
    case class Body(stmts: List[Stmt]) extends Node {
        override def toString = "{...}"
        override def print(out: PrettyPrinter) {
            out.indented("{", "}") {
                stmts.foreach(_.printsemiln(out))
            }            
        }
    }
    
    sealed abstract trait ParseStmt extends TypedNode {
        def printsemiln(out: PrettyPrinter) {
            print(out)
            out.writeln(";")
        }
    }
    
    sealed abstract trait ResolveStmt extends ParseStmt
    
    sealed abstract trait LowerStmt extends ResolveStmt

    sealed abstract trait ParseRcvr extends Node
    sealed abstract trait ParseOwner extends ParseRcvr
    sealed abstract trait ParseTlExpr extends ParseRcvr with ParseOwner with ParseStmt
    
    sealed abstract trait ResolveRcvr extends ParseRcvr
    sealed abstract trait ResolveOwner extends ParseOwner with ResolveRcvr
    sealed abstract trait ResolveTlExpr extends ParseTlExpr with ResolveOwner with ResolveStmt
    
    sealed abstract trait LowerRcvr extends ResolveRcvr
    sealed abstract trait LowerOwner extends LowerRcvr with ResolveOwner
    sealed abstract trait LowerTlExpr extends ResolveTlExpr with LowerStmt

    /** Atomic expressions are variables and tuples of variables.
      * After lowering, only AtomicExpr may be nested within 
      * other expressions. */
    sealed abstract trait AtomicExpr extends LowerTlExpr
    
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
        param: Param[LVSym], 
        stmts: List[Stmt], 
        ty: Ty
    ) extends LowerTlExpr {
        def className = if(async) Name.AsyncBlockClass else Name.BlockClass

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
        def ty = trefTy(typeRef)
        
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
        def ty = path.ty
        override def toString = path.toString
        override def print(out: PrettyPrinter) {
            path.print(out)
        }
    }
    
    case class Var(name: LocalName, sym: LVSym) extends AtomicExpr with LowerOwner {
        override def toString = name.toString
        def ty = vsymTy(sym)
        
        override def print(out: PrettyPrinter) {
            out.write(name.toString)
            out.write(" /*%s*/".format(sym))
        }
    }
    
    case class Field(owner: Owner, name: MN, sym: FSym, ty: Ty) extends LowerTlExpr {
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
    
    case class MethodCall(rcvr: Rcvr, name: Name.Method, args: List[NE], data: MCallData)
    extends LowerTlExpr {
        def ty = returnTy(data)
        override def toString = {
            val strs = name.parts.zip(args).map { case (p, a) =>
                "%s(%s)".format(p, a)
            }
            "%s.%s".format(rcvr, strs.mkString(" "))
        }
        
        override def print(out: PrettyPrinter) {
            var first = true
            rcvr.printdot(out)
            name.parts.zip(args).foreach { case (p, a) =>
                if(!first) out.write(" ")
                first = false
                
                out.write(p)
                out.write(" ")
                a.print(out)
            }
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
   
    case class Labeled(name: LocalName, body: Body)
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
    
    sealed abstract class QualName extends Name

    sealed case class PackageName(name: Name.Package) extends Name {
        override def toString = name.toString
    }
    
    sealed case class ClassName(name: Name.Class) extends QualName {
        override def toString = name.toString
    }
    
    sealed abstract trait VarName extends Name {
        def name: Name.Var
    }
    
    sealed abstract trait UnloweredMemberName extends Name {
        def name: Name.UnloweredMember
        override def toString = name.toString
    }
    
    sealed case class MemberName(name: Name.Member) extends VarName with UnloweredMemberName {
        override def toString = name.toString
    }
    
    sealed case class ClasslessMemberName(name: Name.ClasslessMember) extends UnloweredMemberName {
        override def toString = name.toString
    }
    
    sealed case class LocalName(name: Name.LocalVar) extends VarName {
        override def toString = name.toString
    }
    
    // ___ Phases ___________________________________________________________

    /** Parse phase: relative names unresolved and
      * types uninferred. */
    object Parse extends Ast {
        type CN = RelName
        type CND = RelBase
        type MN = RelName 
        type MNC = RelDot
        type MND = RelBase
        type VN = RelName
        type OTR = OptionalParseTypeRef
        type TR = ParseTypeRef
        type NE = ParseTlExpr
        type Stmt = ParseStmt
        type Expr = ParseTlExpr
        type AstPath = ParsePath
        type Rcvr = ParseRcvr
        type Owner = ParseOwner
        type CSym = Unit
        type VSym = Unit
        type LVSym = Unit
        type FSym = Unit
        type MSym = Unit
        type MCallData = Unit
        type Ty = Unit
        type TyClass = Unit
        type TyTuple = Unit
        
        def errTy = ()
        def toTy(ty: Type.Ref) = ()
        def trefTy(tref: TR) = ()
        def vsymTy(unit: VSym) = ()
        def tupleTy(tys: List[Ty]) = ()
        def mthdSym(unit: MSym) = ()
        def returnTy(unit: MCallData) = ()
        
        def definedClasses(cunit: CompUnit) =
            cunit.classes.map(cdecl => (cdecl.name.toClass(cunit.pkg.name), cdecl))
    }
    
    /** After Resolve(): relative names resolved. */
    object Resolve extends Ast {
        type CN = ClassName
        type CND = ClassName
        type MN = UnloweredMemberName 
        type MNC = MemberName
        type MND = MemberName
        type VN = VarName
        type OTR = OptionalResolveTypeRef
        type TR = ResolveTypeRef
        type NE = ResolveTlExpr
        type Stmt = ResolveStmt
        type Expr = ResolveTlExpr
        type AstPath = ResolvePath
        type Rcvr = ResolveRcvr
        type Owner = ResolveOwner
        type CSym = Unit
        type VSym = Unit
        type LVSym = Unit
        type FSym = Unit
        type MSym = Unit
        type MCallData = Unit
        type Ty = Unit
        type TyClass = Unit
        type TyTuple = Unit

        def errTy = ()
        def toTy(ty: Type.Ref) = ()
        def trefTy(tref: TR) = ()
        def vsymTy(unit: VSym) = ()
        def tupleTy(tys: List[Ty]) = ()
        def mthdSym(unit: MSym) = ()
        def returnTy(unit: MCallData) = ()
    }

    /** After Lower(): types inferred (but not checked!) and 
      * expressions broken apart into statements. */
    object Lower extends Ast {
        type CN = ClassName
        type CND = ClassName
        type MN = MemberName 
        type MNC = MemberName
        type MND = MemberName
        type VN = VarName
        type OTR = TypeRef
        type TR = TypeRef
        type NE = AtomicExpr
        type Stmt = LowerStmt
        type Expr = LowerTlExpr
        type AstPath = TypedPath
        type Rcvr = LowerRcvr
        type Owner = LowerOwner
        type CSym = ClassSymbol
        type VSym = VarSymbol.Any
        type LVSym = VarSymbol.Local
        type FSym = VarSymbol.Field
        type MSym = MethodSymbol
        type MCallData = (MethodSymbol, MethodSignature[Pattern.Anon])
        type Ty = Type.Ref
        type TyClass = Type.Class
        type TyTuple = Type.Tuple

        def errTy = Type.Object
        def toTy(ty: Type.Ref) = ty
        def trefTy(tref: TR) = tref.ty
        def vsymTy(vsym: VSym) = vsym.ty
        def tupleTy(tys: List[Ty]) = Type.Tuple(tys)
        def mthdSym(data: MCallData) = data._1
        def returnTy(data: MCallData) = data._2.returnTy
        
        def toPatternRef(pat: Param[VSym]): Pattern.Ref = pat match {
            case TupleParam(params) => Pattern.Tuple(params.map(toPatternRef))
            case VarParam(_, _, _, sym) => Pattern.Var(sym.name, sym.ty)
        }
        
        def toPatternAnon(pat: AstPattern): Pattern.Anon = pat match {
            case pat: TupleAstPattern => Pattern.SubstdTuple(pat.subpatterns.map(toPatternAnon))
            case pat: VarAstPattern => Pattern.SubstdVar(pat.sym.ty)
        }
        
    }

}
