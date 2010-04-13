package inter.compiler

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import Util._

class Ast {
    
    import Ast.Node    
    import Ast.PkgName
    import Ast.AbsName
    import Ast.VarName
        
    /** Potentially relative names later become Absolute Names */
    type PN <: PkgName     
    
    /** Optional types later become specified */
    type OT <: OptionalTypeRef
    
    /** Nested expressions eventually become restricted */
    type NE <: Expr
    
    /** Lvalue in an assignment: initially may omit types */
    type LV <: Node
    
    /** Type of an expression */
    type Ty
    
    /** Data attached to class declarations */
    type CSym
    
    /** Data attached to field declarations, access */
    type VSym
    
    /** Data attached to method declarations, calls */
    type MSym 
    
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
    
    // ___ Names ____________________________________________________________
    
    // ______ Potentially relative names ____________________________________
    sealed abstract class RelName extends PkgName {
        def toAbs(pkg: AbsName) = withPosOf(this, Ast.AbsName(toQual(pkg.qualName)))
        def toQual(pkg: Name.Qual): Name.Qual
        def /(nm: String) = RelDot(this, nm)   
    }
    case class RelBase(nm: String) extends RelName {
        def toQual(pkg: Name.Qual) = pkg / nm
        override def toString = nm
    }
    case class RelDot(context: RelName, component: String) extends RelName {
        def toQual(pkg: Name.Qual) = context.toQual(pkg) / component
        override def toString = "%s.%s".format(context, component)
    }
    
    // ___ Declarations _____________________________________________________
    
    case class CompUnit(
        pkg: AbsName,
        imports: List[ImportDecl],
        classes: List[ClassDecl]
    ) extends Node {
        
        override def print(out: PrettyPrinter) {
            out.writeln("package %s;", pkg)
            imports.foreach(_.println(out))
            printSepFunc(out, classes, () => out.writeln(""))
        }
        
        def definedClasses = {
            val pkgQualName = pkg.qualName
            classes.map(cdecl => (cdecl.name.toQual(pkgQualName), cdecl))
        }
        
    }
    
    sealed abstract class MemberDecl extends Node {
        def annotations: List[Annotation]
        
        def asMethodNamed(name: Name.Method): Option[MethodDecl] = None
        def asFieldNamed(name: Name.Var): Option[FieldDecl] = None
    }
    
    sealed abstract class ImportDecl extends Node
    
    case class ImportOne(
        fromName: AbsName,
        toName: RelBase
    ) extends ImportDecl {
        override def toString = "import %s(%s)".format(fromName, toName)
    }
    
    case class ImportAll(
        fromName: AbsName
    ) extends ImportDecl {
        override def toString = "import %s.*".format(fromName)
    }
    
    case class ClassDecl(
        name: PN,
        annotations: List[Annotation],
        superClasses: List[PN],
        pattern: TuplePattern,
        members: List[MemberDecl],
        sym: CSym
    ) extends MemberDecl {
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
    
    case class DeclPart(ident: String, pattern: TuplePattern) extends Node {
        override def toString = "%s%s".format(ident, pattern)
        override def print(out: PrettyPrinter) {
            out.write("%s", ident)
            pattern.print(out)
        }
    }
    case class MethodDecl(
        annotations: List[Annotation],
        parts: List[DeclPart],
        returnTref: OT,
        returnTy: Ty,
        requirements: List[PathRequirement],
        optBody: Option[Body]
    ) extends MemberDecl {
        def name = Name.Method(parts.map(_.ident))
        def patterns = parts.map(_.pattern)
        override def toString = "[method %s]".format(name)
        override def asMethodNamed(mthdName: Name.Method) = {
            if(mthdName == name) Some(this)
            else None
        }
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.println(out))
            returnTref.printsp(out)
            printSep(out, parts, " ")
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
        tref: OT,
        ty: Ty,
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
        name: PN
    ) extends Node {
        override def toString = "[%s]".format(name)
        
        override def print(out: PrettyPrinter) {
            out.write("[")
            name.print(out)
            out.write("]")
        }
    }
    
    // ___ Patterns _________________________________________________________
    //
    // In a pattern, all types are specified.
    
    abstract class Pattern extends Node
    
    case class TuplePattern(
        patterns: List[Pattern]
    ) extends Pattern {
        override def toString = "(%s)".format(patterns.mkString(", "))
        
        override def print(out: PrettyPrinter) {
            out.write("(")
            printSep(out, patterns, ", ")
            out.write(")")
        }
    }
    
    case class VarPattern(
        annotations: List[Annotation], 
        tref: TypeRef, 
        name: VarName,
        sym: VSym) 
    extends Pattern {
        override def toString = "%s %s %s".format(annotations, tref, name)
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.printsp(out))
            tref.printsp(out)
            name.print(out)
        }
    }
    
    // ___ Lvalues __________________________________________________________
    //
    // An lvalue is like a pattern but it may omit types.
    
    sealed abstract trait Lvalue extends Node
    
    case class TupleLvalue(
        lvalues: List[Lvalue]
    ) extends Lvalue {
        override def toString = "(%s)".format(lvalues.mkString(", "))
        
        override def print(out: PrettyPrinter) {
            out.write("(")
            printSep(out, lvalues, ", ")
            out.write(")")
        }
    }
    
    case class VarLvalue(
        annotations: List[Annotation], 
        tref: OT, 
        name: VarName,
        sym: VSym) 
    extends Lvalue {
        override def toString = "%s %s %s".format(annotations, tref, name)
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.printsp(out))
            tref.printsp(out)
            name.print(out)
        }
    }
    
    // ___ Type References __________________________________________________
    
    sealed abstract class OptionalTypeRef extends Node
    
    case class InferredTypeRef() extends OptionalTypeRef {
        override def toString = "<infer>"
    }
    
    abstract class TypeRef extends OptionalTypeRef
    
    case class NullType() extends TypeRef {
        override def toString = "null"
    }
    
    case class TupleType(types: List[TypeRef]) extends TypeRef {
        override def toString = "(%s)".format(types.mkString(", "))
        
        override def print(out: PrettyPrinter) {
            out.write("(")
            printSep(out, types, ", ")
            out.write(")")
        }
    }
    
    case class VarType(path: AstPath, typeVar: VarName) extends TypeRef {
        override def toString = "%s:%s".format(path, typeVar)
        
        override def print(out: PrettyPrinter) {
            path.print(out)
            out.write(":")
            typeVar.print(out)
        }
    }
    
    case class ClassType(className: PN, typeArgs: List[TypeArg]) extends TypeRef {
        override def toString = {
            if(typeArgs.isEmpty) className.toString
            else "%s[%s]".format(className, typeArgs.mkString(", "))
        }
        
        override def print(out: PrettyPrinter) {
            className.print(out)
            if(!typeArgs.isEmpty) {
                out.write("[")
                printSep(out, typeArgs, ", ")
                out.write("]")
            }
        }
    }
    
    sealed abstract class TypeArg extends Node
    
    case class TypeTypeArg(name: VarName, rel: TcRel, typeRef: TypeRef) extends TypeArg {
        override def toString = "%s %s %s".format(name, rel, typeRef)
    }
    
    case class PathTypeArg(name: VarName, rel: PcRel, path: AstPath) extends TypeArg {
        override def toString = "%s %s %s".format(name, rel, path)
    }
    
    // ___ Paths ____________________________________________________________
    
    sealed abstract trait AstPath extends Node {
        def ty: Ty
    }
    
    case class PathField(owner: AstPath, name: VarName, sym: VSym, ty: Ty) extends AstPath {
        override def toString = owner + " " + name
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
    
    sealed abstract trait Stmt extends Node {
        def ty: Ty
        
        def printsemiln(out: PrettyPrinter) {
            print(out)
            out.writeln(";")
        }
    }
    
    sealed abstract trait Expr extends Stmt {
        def ty: Ty
    }
    
    sealed abstract trait LoweredExpr extends Expr
    
    case class Tuple(exprs: List[NE], ty: Ty) extends LoweredExpr {
        override def toString = "(%s)".format(exprs.mkString(", "))
        
        override def print(out: PrettyPrinter) {
            out.write("(")
            printSep(out, exprs, ", ")
            out.write(")")
        }        
    }
    
    abstract class Tmpl(l: String, r: String) extends Expr {
        def stmts: List[Stmt]
        
        override def toString = "%s...%s".format(l, r)
        
        override def print(out: PrettyPrinter) {
            out.indented(l, r) { stmts.foreach(_.printsemiln(out)) }
        }
    }
    
    case class InlineTmpl(stmts: List[Stmt], ty: Ty)
    extends Tmpl("{", "}")
    
    case class AsyncTmpl(stmts: List[Stmt], ty: Ty)
    extends Tmpl("{{", "}}")
    
    case class Literal(obj: Object, ty: Ty) extends Expr {
        override def toString = obj.toString
    }
    
    case class Assign(lvalue: LV, rvalue: Expr) 
    extends Stmt {
        def ty = rvalue.ty
        override def toString = "%s = %s".format(lvalue, rvalue)
        
        override def print(out: PrettyPrinter) {
            lvalue.print(out)
            out.write(" = ")
            rvalue.print(out)
        }        
    }

    // n.b.: A Var could refer to any sort of variable, not only a 
    // local variable.  For example, a field of the current class or
    // one of its enclosing classes.
    case class Var(name: VarName, sym: VSym, ty: Ty) extends LoweredExpr with AstPath {
        override def toString = name.toString
    }
    
    case class Field(owner: NE, name: VarName, sym: VSym, ty: Ty) extends Expr {
        override def toString = "%s %s".format(owner, name)
        
        override def print(out: PrettyPrinter) {
            owner.printsp(out)
            name.print(out)
        }
    }
    
    case class CallPart(ident: String, arg: NE) extends Node {
        override def toString = "%s %s".format(ident, arg)
        
        override def print(out: PrettyPrinter) {
            out.write("%s ", ident)
            arg.print(out)
        }        
    }
    case class MethodCall(rcvr: NE, parts: List[CallPart], sym: MSym, ty: Ty)
    extends Expr {
        def name = Name.Method(parts.map(_.ident))
        def args = parts.map(_.arg)
        override def toString = "%s %s".format(rcvr, parts.mkString(" "))
        
        override def print(out: PrettyPrinter) {
            rcvr.printsp(out)
            printSep(out, parts, " ")
        }        
    }
    
    /** Used to create new instances of Java classes. */
    case class NewJava(tref: TypeRef, arg: Tuple, ty: Ty) extends Expr {
        override def toString = "new %s%s".format(tref, arg)
        
        override def print(out: PrettyPrinter) {
            out.write("new ")
            tref.print(out)
            arg.print(out)
        }        
    }
    
    case class Null(ty: Ty)
    extends Expr {
        override def toString = "null"
    }
    
    /** ImpVoid is inserted when there is an empty 
      * set of statements like "{ }". */
    case class ImpVoid(ty: Ty)
    extends Expr {
        override def toString = "<(Void)null>"
    }
   
    case class ImpThis(ty: Ty)
    extends Expr {
        override def toString = "<this>"
    }
   
    case class Labeled(name: VarName, body: Body)
    extends Stmt {
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
    
    abstract class Node extends Positional {
        def print(out: PrettyPrinter) {
            out.write(toString)
        }
        
        def printsp(out: PrettyPrinter) {
            print(out)
            out.write(" ")
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
    
    sealed abstract class PkgName extends Node {
        def toQual(pkg: Name.Qual): Name.Qual
    }

    // ______ Names known to be absolute ____________________________________
    sealed case class AbsName(qualName: Name.Qual) extends PkgName {
        override def toString = qualName.toString
        def toQual(pkg: Name.Qual) = qualName
        def component = qualName.rev_components.head
    }
    
    // ______ Non-qualified names ___________________________________________
    case class VarName(text: String) extends Node {
        def name = Name.Var(text)
        override def toString = text
    }
    
    // ___ Requirement Kinds ________________________________________________
    
    // ___ Phases ___________________________________________________________

    /** Parse phase: relative names unresolved and
      * types uninferred. */
    object Parse extends Ast {
        type PN = RelName
        type OT = OptionalTypeRef
        type NE = Expr
        type LV = Lvalue
        type CSym = Unit
        type VSym = Unit
        type MSym = Unit
        type Ty = Unit
    }
    
    /** After Resolve(): relative names resolved. */
    object Resolve extends Ast {
        type PN = Ast.AbsName
        type OT = OptionalTypeRef
        type NE = Expr
        type LV = Lvalue
        type CSym = Unit
        type VSym = Unit
        type MSym = Unit
        type Ty = Unit
    }

    /** After Lower(): types inferred (but not checked!) and 
      * expressions broken apart into statements. */
    object Lower extends Ast {
        type PN = Ast.AbsName
        type OT = TypeRef
        type NE = LoweredExpr
        type LV = Pattern
        type CSym = Symbol.Class
        type VSym = Symbol.Var
        type MSym = Symbol.Method
        type Ty = Type.Ref
    }
    
}
