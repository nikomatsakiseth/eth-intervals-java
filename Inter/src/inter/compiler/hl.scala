package inter.compiler

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import Util._

class Hl {
    
    import Hl.Ast
    import Hl.PkgName
    import Hl.AbsName
    import Hl.VarName
        
    /** Potentially relative names later become Absolute Names */
    type PN <: PkgName     
    
    /** Optional types later become specified */
    type OT <: OptionalTypeRef
    
    /** Type of an expression */
    type Ty
    
    /** Data attached to class declarations */
    type CSym
    
    /** Data attached to field declarations, access */
    type VSym
    
    /** Data attached to method declarations, calls */
    type MSym 
    
    private def printSepFunc(out: PrettyPrinter, asts: List[Ast], sepfunc: (() => Unit)) {
        asts.dropRight(1).foreach { ast =>
            ast.print(out)
            sepfunc()
        }
        asts.takeRight(1).foreach(_.print(out))
    }
    
    private def printSep(out: PrettyPrinter, asts: List[Ast], sep: String) {
        printSepFunc(out, asts, () => out.write(sep))
    }
    
    // ___ Names ____________________________________________________________
    
    // ______ Potentially relative names ____________________________________
    sealed abstract class RelName extends PkgName {
        def toAbs(pkg: AbsName): AbsName
        def toQual(pkg: Name.Qual): Name.Qual
        def /(nm: String) = RelDot(this, nm)   
    }
    case class RelBase(nm: String) extends RelName {
        def toAbs(pkg: AbsName) = withPosOf(this, Hl.AbsDot(pkg, nm))
        def toQual(pkg: Name.Qual) = pkg / nm
        override def toString = nm
    }
    case class RelDot(context: RelName, component: String) extends RelName {
        def toAbs(pkg: AbsName) = withPosOf(this, Hl.AbsDot(context.toAbs(pkg), component))
        def toQual(pkg: Name.Qual) = context.toQual(pkg) / component
        override def toString = "%s.%s".format(context, component)
    }
    
    // ___ Declarations _____________________________________________________
    
    case class CompUnit(
        pkg: AbsName,
        imports: List[ImportDecl],
        classes: List[ClassDecl]
    ) extends Ast {
        
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
    
    sealed abstract class MemberDecl extends Ast {
        def annotations: List[Annotation]
        
        def asMethodNamed(name: Name.Method): Option[MethodDecl] = None
        def asFieldNamed(name: Name.Var): Option[FieldDecl] = None
    }
    
    sealed abstract class ImportDecl extends Ast
    
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
    
    private def printOptBody(out: PrettyPrinter, optBody: Option[InlineTmpl]) = optBody match {
        case Some(b) => b.print(out)
        case None => out.write(";")
    }
    
    case class IntervalDecl(
        annotations: List[Annotation],
        name: VarName,
        optParent: Option[Path],
        optBody: Option[InlineTmpl],
        sym: VSym      
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
    
    case class DeclPart(ident: String, pattern: TuplePattern) extends Ast {
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
        requirements: List[PathRequirement],
        optBody: Option[InlineTmpl],
        sym: MSym
    ) extends MemberDecl {
        def name = Name.Method(parts.map(_.ident))
        def patterns = parts.map(_.pattern)
        override def toString = "[method %s]".format(name)
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.println(out))
            retTref.printsp(out)
            printSep(out, parts, " ")
            out.writeln("")
            requirements.foreach(_.println(out))
            printOptBody(out, optBody)
        }
    }
    
    case class PathRequirement(
        left: Path, rel: PcRel, right: Path
    ) extends Ast {
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
        value: Option[Expr],
        sym: VSym
    ) extends MemberDecl {
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.println(out))
            tref.printsp(out)
            name.print(out)
            value match {
                case None =>
                case Some(expr) => 
                    out.write(" = ")
                    expr.print(out)
            }
            out.write(";")
        }
    }
    
    case class RelDecl(
        annotations: List[Annotation],        
        left: Path, 
        kind: PcRel,
        right: Path
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
    ) extends Ast {
        override def toString = "[%s]".format(name)
        
        override def print(out: PrettyPrinter) {
            out.write("[")
            name.print(out)
            out.write("]")
        }
    }
    
    // ___ Patterns _________________________________________________________
    
    abstract class Pattern extends Ast
    
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
    
    // ___ Untyped Patterns _________________________________________________
    
    sealed abstract trait Lvalue extends Ast
    
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
    
    sealed abstract class OptionalTypeRef extends Ast
    
    case object InferredTypeRef extends OptionalTypeRef {
        override def toString = "<infer>"
    }
    
    abstract class TypeRef extends OptionalTypeRef
    
    case class TupleType(types: List[TypeRef]) extends TypeRef {
        override def toString = "(%s)".format(types.mkString(", "))
        
        override def print(out: PrettyPrinter) {
            out.write("(")
            printSep(out, types, ", ")
            out.write(")")
        }
    }
    
    case class PathType(path: Path, typeVar: VarName) extends TypeRef {
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
    
    sealed abstract class TypeArg extends Ast
    
    case class TypeTypeArg(name: VarName, rel: TcRel, typeRef: TypeRef) extends TypeArg {
        override def toString = "%s %s %s".format(name, rel, typeRef)
    }
    
    case class PathTypeArg(name: VarName, rel: PcRel, path: Path) extends TypeArg {
        override def toString = "%s %s %s".format(name, rel, path)
    }
    
    // ___ Paths ____________________________________________________________
    
    sealed abstract trait Path extends Ast
    
    case class PathField(p: Path, f: VarName) extends Path {
        override def toString = p + " " + f
    }
    
    // ___ Statements and Expressions _______________________________________
    
    sealed abstract trait Stmt extends Ast {
        def printsemiln(out: PrettyPrinter) {
            print(out)
            out.writeln(";")
        }
    }
    
    sealed abstract trait Expr extends Stmt {
        def ty: Ty
    }
    
    case class Tuple(exprs: List[Expr], ty: Ty) extends Expr {
        override def toString = "(%s)".format(exprs.mkString(", "))
        
        override def print(out: PrettyPrinter) {
            out.write("(")
            printSep(out, exprs, ", ")
            out.write(")")
        }        
    }
    
    abstract class Tmpl(l: String, r: String) extends Expr {
        def stmts: List[Stmt]
        
        override def toString = "%s %s %s".format(l, stmts.mkString("", "; ", "; "), r)
        
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
    
    case class Assign(lvalue: Lvalue, rvalue: Expr) 
    extends Stmt {
        override def toString = "%s = %s".format(lvalue, rvalue)
        
        override def print(out: PrettyPrinter) {
            lvalue.print(out)
            out.write(" = ")
            rvalue.print(out)
        }        
    }

    case class Var(name: VarName, sym: VSym, ty: Ty) extends Expr with Path {
        override def toString = name.toString
    }
    
    case class Field(owner: Expr, name: VarName, sym: MSym, ty: Ty) extends Expr {
        override def toString = "%s %s".format(owner, name)
        
        override def print(out: PrettyPrinter) {
            owner.printsp(out)
            name.print(out)
        }        
    }
    
    case class CallPart(ident: String, arg: Expr) extends Ast {
        override def toString = "%s %s".format(ident, arg)
        
        override def print(out: PrettyPrinter) {
            out.write("%s ", ident)
            arg.print(out)
        }        
    }
    case class MethodCall(rcvr: Expr, parts: List[CallPart], sym: MSym, ty: Ty)
    extends Expr {
        def name = Name.Method(parts.map(_.ident))
        def args = parts.map(_.arg)
        override def toString = "%s %s".format(rcvr, parts.mkString(" "))
        
        override def print(out: PrettyPrinter) {
            rcvr.printsp(out)
            printSep(out, parts, " ")
        }        
    }
    
    case class New(tref: TypeRef, arg: Tuple, ty: Ty) extends Expr {
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
    
    case object ImpVoid
    extends Expr {
        override def toString = "<(Void)null>"
    }
   
    case object ImpThis
    extends Expr {
        override def toString = "<this>"
    }
   
    case class Labeled(name: VarName, block: InlineTmpl)
    extends Stmt {
        override def toString = "%s: %s".format(name, block)
        
        override def print(out: PrettyPrinter) {
            out.write("%s: ", name)
            block.print(out)
        }
    }
    
}

object Hl {
    
    // ___ Invariant Parts __________________________________________________
    
    abstract class Ast extends Positional {
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
    
    sealed abstract class PkgName extends Ast {
        def toQual(pkg: Name.Qual): Name.Qual
    }

    // ______ Names known to be absolute ____________________________________
    sealed abstract class AbsName extends PkgName {
        def /(nm: String) = AbsDot(this, nm)
        def toQual(pkg: Name.Qual) = qualName
        def qualName: Name.Qual
    }
    case object AbsRoot extends AbsName {
        override def toString = "<root>"
        def qualName = Name.Qual(List())        
    }
    case class AbsDot(context: AbsName, component: String) extends AbsName {
        override def toString = context match {
            case AbsRoot => component
            case abs => abs.toString + "." + component
        }
        def qualName = context.qualName / component
    }
    def abs(name: String) = name.split('.').foldLeft[AbsName](AbsRoot)(_ / _)
    
    // ______ Non-qualified names ___________________________________________
    case class VarName(text: String) extends Ast {
        def name = Name.Var(text)
        override def toString = text
    }
    
    // ___ Requirement Kinds ________________________________________________
    
    // ___ Phases ___________________________________________________________

    /** Parse phase: relative names unresolved and
      * types uninferred. */
    object P extends Hl {
        type PN = RelName
        type OT = OptionalTypeRef
        type CSym = Unit
        type VSym = Unit
        type MSym = Unit
        type Ty = Unit
    }
    
    /** After ResolveName(): relative names resolved. */
    object RN extends Hl {
        type PN = Hl.AbsName
        type OT = OptionalTypeRef
        type CSym = Unit
        type VSym = Unit
        type MSym = Unit
        type Ty = Unit
    }
    
    /** After CheckTypes(): types inferred. */
    object CT extends Hl {
        type PN = Hl.AbsName
        type OT = TypeRef
        type CSym = Symbol.Class
        type VSym = Symbol.Var
        type MSym = Symbol.Method
        type Ty = Symbol.Type
    }
    
}