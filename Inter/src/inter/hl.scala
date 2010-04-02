package inter

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import Util._

class Hl {
    
    /** Potentially relative names later become Absolute Names */
    type QN <: QualName     
    
    /** Optional types later become specified */
    type OT <: OptionalTypeRef
    
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
    
    abstract class QualName extends Ast
    abstract class AbsName extends QualName {
        def /(nm: String) = AbsDot(this, nm)
    }
    case object AbsRoot extends AbsName
    case class AbsDot(context: QualName, component: String) extends AbsName {
        override def toString = "%s.%s".format(context, component)
    }
    case class RelName(nm: String) extends QualName {
        override def toString = nm
    }
    
    case class VarName(name: String) extends Ast {
        override def toString = name
    }
    
    case class MethodName(parts: List[String]) {
        override def toString = parts.mkString("", "(_)", " _")
    }
    
    // ___ Declarations _____________________________________________________
    
    case class CompUnit(
        pkg: Option[AbsName],
        imports: List[ImportDecl],
        classes: List[ClassDecl]
    ) extends Ast {
        
        override def print(out: PrettyPrinter) {
            out.writeln("package %s;", pkg)
            imports.foreach(_.println(out))
            printSepFunc(out, classes, () => out.writeln(""))
        }
        
    }
    
    abstract class MemberDecl extends Ast {
        def annotations: List[Annotation]
    }
    
    abstract class ImportDecl extends Ast
    
    case class ImportOne(
        fromName: AbsName,
        toName: Option[RelName]
    ) extends ImportDecl {
        override def toString = "import %s(%s)".format(fromName, toName)
    }
    
    case class ImportAll(
        fromName: AbsName
    ) extends ImportDecl {
        override def toString = "import %s.*".format(fromName)
    }
    
    case class ClassDecl(
        name: String,
        annotations: List[Annotation],
        superClasses: List[QN],
        pattern: TuplePattern,
        members: List[MemberDecl]
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
        parent: Option[Path],
        optBody: Option[InlineTmpl]        
    ) extends MemberDecl {
        override def toString = "[interval %s(%s)]".format(name, parent)
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.println(out))
            parent match {
                case None => out.writeln("interval %s", name)
                case Some(par) => out.writeln("interval %s(%s)", name, par)
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
        retTref: OT,
        requirements: List[Requirement],
        optBody: Option[InlineTmpl]
    ) extends MemberDecl {
        def name = MethodName(parts.map(_.ident))
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
    
    case class Requirement(
        left: Path, rhs: ReqRhs
    ) extends Ast {
        override def toString = "%s %s".format(left, rhs)
        
        override def print(out: PrettyPrinter) {
            left.printsp(out)
            rhs.print(out)
        }
    }
    
    sealed abstract class ReqKind
    case object ReqHb extends ReqKind {
        override def toString = "->"
    }
    case object ReqLocks extends ReqKind {
        override def toString = "locks"
    }
    case object ReqSubOf extends ReqKind {
        override def toString = "subOf"
    }
    case object ReqInlineSubOf extends ReqKind {
        override def toString = "inlineSubOf"
    }
    
    case class ReqRhs(
        kind: ReqKind,
        right: Path
    ) extends Ast {
        override def toString = "%s %s".format(kind, right)
        
        override def print(out: PrettyPrinter) {
            out.write(" %s ", kind)
            right.print(out)
        }
    }
    
    case class FieldDecl(
        annotations: List[Annotation],
        name: VarName,
        tref: OT,
        value: Option[Expr]
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
        kind: ReqKind,
        right: Path
    ) extends MemberDecl {
        override def toString = "%s %s %s".format(left, kind, right)
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.println(out))
            left.print(out)
            out.write(" %s ", kind)
            right.print(out)
        }
    }
    
    case class Annotation(
        name: QN
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
        name: VarName) 
    extends Pattern {
        override def toString = "%s %s %s".format(annotations, tref, name)
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.printsp(out))
            tref.printsp(out)
            name.print(out)
        }
    }
    
    // ___ Untyped Patterns _________________________________________________
    
    abstract trait Lvalue extends Ast
    
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
        name: VarName) 
    extends Lvalue {
        override def toString = "%s %s %s".format(annotations, tref, name)
        
        override def print(out: PrettyPrinter) {
            annotations.foreach(_.printsp(out))
            tref.printsp(out)
            name.print(out)
        }
    }
    
    // ___ Type References __________________________________________________
    
    abstract class OptionalTypeRef extends Ast
    
    case object InferredTypeRef extends OptionalTypeRef {
        override def toString = "<infer>"
    }
    
    abstract class TypeRef extends OptionalTypeRef
    
    case class PathType(path: Path, typeVar: VarName) extends TypeRef {
        override def toString = "%s:%s".format(path, typeVar)
        
        override def print(out: PrettyPrinter) {
            path.print(out)
            out.write(":")
            typeVar.print(out)
        }
    }
    
    case class ClassType(className: QN, typeArgs: List[TypeArg]) extends TypeRef {
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
    
    case class TypeArg(fieldName: VarName, reqRhs: ReqRhs) extends Ast {
        override def toString = "%s %s".format(fieldName, reqRhs)
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
    
    sealed abstract trait Expr extends Stmt
    
    case class Tuple(exprs: List[Expr]) extends Expr {
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
    
    case class InlineTmpl(stmts: List[Stmt])
    extends Tmpl("{", "}")
    
    case class AsyncTmpl(stmts: List[Stmt])
    extends Tmpl("{{", "}}")
    
    case class Literal(obj: Object) extends Expr {
        override def toString = obj.toString
    }
    
    case class Assign(lvalue: Lvalue, rvalue: Expr) 
    extends Expr {
        override def toString = "%s = %s".format(lvalue, rvalue)
        
        override def print(out: PrettyPrinter) {
            lvalue.print(out)
            out.write(" = ")
            rvalue.print(out)
        }        
    }

    case class Var(name: VarName) extends Expr with Lvalue with Path {
        override def toString = name.toString
    }
    
    case class Field(owner: Expr, name: VarName) extends Expr with Lvalue {
        override def toString = "%s %s".format(owner, name)
        
        override def print(out: PrettyPrinter) {
            owner.printsp(out)
            name.print(out)
        }        
    }
    
    case class CallPart(ident: String, arg: Expr) extends Ast {
        override def toString = "%s%s".format(ident, arg)
        
        override def print(out: PrettyPrinter) {
            out.write("%s", ident)
            arg.print(out)
        }        
    }
    case class MethodCall(rcvr: Expr, parts: List[CallPart])
    extends Expr {
        def name = MethodName(parts.map(_.ident))
        def args = parts.map(_.arg)
        override def toString = "%s.%s".format(rcvr, parts.mkString(" "))
        
        override def print(out: PrettyPrinter) {
            rcvr.printsp(out)
            printSep(out, parts, " ")
        }        
    }
    
    case class New(tref: TypeRef, arg: Tuple) extends Expr {
        override def toString = "new %s%s".format(tref, arg)
        
        override def print(out: PrettyPrinter) {
            out.write("new ")
            tref.print(out)
            arg.print(out)
        }        
    }
    
    case class Null()
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
    
    // Phases of High-Level IR:
    
    object P extends Hl {
        type QN = QualName
        type OT = OptionalTypeRef
    }
    
    object AN extends Hl {
        type QN = AbsName
        type OT = OptionalTypeRef
    }
    
    object TI extends Hl {
        type QN = AbsName
        type OT = TypeRef
    }
    
}