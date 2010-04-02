package inter

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import Util._

object hl {
    
    abstract class Ast extends Positional
    
    // ___ Names ____________________________________________________________
    
    abstract class QualName extends Positional {
        def /(nm: String) = DotName(this, nm)
    }
    case class BaseName(component: String) extends QualName {
        override def toString = component
    }
    case class DotName(context: QualName, component: String) extends QualName {
        override def toString = "%s.%s".format(context, component)
    }
    
    case class VarName(name: String) extends Positional {
        override def toString = name
    }
    
    case class MethodName(parts: List[String]) extends Positional {
        override def toString = parts.mkString("", "(_)", " _")
    }
    
    // ___ Declarations _____________________________________________________
    
    case class CompUnit(
        pkg: Option[QualName],
        imports: List[ImportDecl],
        classes: List[ClassDecl]
    ) extends Ast
    
    abstract class MemberDecl extends Ast {
        def annotations: List[Annotation]
    }
    
    abstract class ImportDecl extends Ast
    
    case class ImportOne(
        fromName: QualName,
        toName: Option[BaseName]
    ) extends ImportDecl {
        override def toString = "import %s(%s)".format(fromName, toName)
    }
    
    case class ImportAll(
        fromName: QualName
    ) extends ImportDecl {
        override def toString = "import %s.*".format(fromName)
    }
    
    case class ClassDecl(
        name: String,
        annotations: List[Annotation],
        superClasses: List[QualName],
        pattern: TuplePattern,
        members: List[MemberDecl]
    ) extends MemberDecl {
        override def toString = "[class %s%s]".format(
            name, pattern
        )
    }
    
    case class IntervalDecl(
        annotations: List[Annotation],
        name: VarName,
        parent: Option[QualName],
        optBody: Option[InlineTmpl]        
    ) extends MemberDecl {
        override def toString = "[interval %s(%s)]".format(name, parent)
    }
    
    case class DeclPart(ident: String, pattern: TuplePattern) extends Ast {
        override def toString = "%s%s".format(ident, pattern)
    }
    case class MethodDecl(
        annotations: List[Annotation],
        parts: List[DeclPart],
        retTref: TypeRef,
        requirements: List[Requirement],
        optBody: Option[InlineTmpl]
    ) extends MemberDecl {
        def name = MethodName(parts.map(_.ident))
        def patterns = parts.map(_.pattern)
        override def toString = "[method %s]".format(name)
    }
    
    case class Requirement(
        left: QualName, rhs: ReqRhs
    ) extends Ast {
        override def toString = "%s %s".format(left, rhs)
    }
    
    case class ReqRhs(
        op: String,
        right: QualName
    ) extends Ast {
        override def toString = "%s %s".format(op, right)
    }
    
    case class FieldDecl(
        annotations: List[Annotation],
        name: VarName,
        tref: Option[TypeRef],
        value: Option[Expr]
    ) extends MemberDecl
    
    case class HbDecl(
        annotations: List[Annotation],        
        from: QualName, 
        to: QualName
    ) extends MemberDecl {
    }
    
    case class LockDecl(
        annotations: List[Annotation],        
        interval: QualName, 
        lock: QualName
    ) extends MemberDecl
    
    case class Annotation(
        name: QualName
    ) extends Ast {
        override def toString = "[%s]".format(name)
    }
    
    // ___ Patterns _________________________________________________________
    
    abstract class Pattern extends Ast
    
    case class TuplePattern(
        patterns: List[Pattern]
    ) extends Pattern {
        override def toString = "(%s)".format(patterns.mkString(", "))
    }
    
    case class VarPattern(
        annotations: List[Annotation], 
        tref: TypeRef, 
        name: VarName) 
    extends Pattern {
        override def toString = "%s %s %s".format(annotations, tref, name)
    }
    
    // ___ Untyped Patterns _________________________________________________
    
    abstract trait Lvalue extends Ast
    
    case class TupleLvalue(
        lvalues: List[Lvalue]
    ) extends Lvalue {
        override def toString = "(%s)".format(lvalues.mkString(", "))
    }
    
    case class VarLvalue(
        annotations: List[Annotation], 
        tref: Option[TypeRef], 
        name: VarName) 
    extends Lvalue {
        override def toString = "%s %s %s".format(annotations, tref, name)
    }
    
    // ___ Type References __________________________________________________
    
    abstract class TypeRef extends Ast
    
    case class PathType(path: QualName, typeVar: VarName) extends TypeRef {
        override def toString = "%s:%s".format(path, typeVar)
    }
    
    case class ClassType(className: QualName, typeArgs: List[TypeArg]) extends TypeRef {
        override def toString = {
            if(typeArgs.isEmpty) className.toString
            else "%s[%s]".format(className, typeArgs.mkString(", "))
        }
    }
    
    case class TypeArg(fieldName: VarName, reqRhs: ReqRhs) extends Ast {
        override def toString = "%s %s".format(fieldName, reqRhs)
    }
    
    // ___ Statements and Expressions _______________________________________
    
    abstract trait Stmt extends Ast
    
    abstract trait Expr extends Stmt
    
    case class Tuple(exprs: List[Expr]) extends Expr {
        override def toString = "(%s)".format(exprs.mkString(", "))
    }
    
    abstract class Tmpl extends Expr {
        def stmts: List[Stmt]
    }
    
    case class InlineTmpl(stmts: List[Stmt])
    extends Tmpl {
        override def toString = "{ %s }".format(stmts.mkString("", "; ", "; "))
    }
    
    case class AsyncTmpl(stmts: List[Stmt])
    extends Tmpl {
        override def toString = "{{ %s }}".format(stmts.mkString("", "; ", "; "))
    }
    
    case class Literal(obj: Object) extends Expr {
        override def toString = obj.toString
    }
    
    case class Assign(lvalue: Lvalue, rvalue: Expr) 
    extends Expr {
        override def toString = "%s = %s".format(lvalue, rvalue)
    }

    case class Var(name: VarName) extends Expr with Lvalue {
        override def toString = name.toString
    }
    
    case class Field(owner: Expr, name: VarName) extends Expr with Lvalue {
        override def toString = "%s %s".format(owner, name)
    }
    
    case class CallPart(ident: String, arg: Expr) extends Ast {
        override def toString = "%s%s".format(ident, arg)
    }
    case class MethodCall(rcvr: Expr, parts: List[CallPart])
    extends Expr {
        def name = MethodName(parts.map(_.ident))
        def args = parts.map(_.arg)
        override def toString = "%s.%s".format(rcvr, parts.mkString(" "))
    }
    
    case class New(tref: TypeRef, arg: Tuple) extends Expr {
        override def toString = "new %s%s".format(tref, arg)
    }
    
    case class Null()
    extends Expr {
        override def toString = "null"
    }
    
    case object ImpVoid
    extends Expr {
        override def toString = "[(Void)null]"
    }
   
    case object ImpThis
    extends Expr {
        override def toString = "[this]"
    }
   
    case class Empty()
    extends Stmt {
        override def toString = ";"
    }
    
    case class Labeled(name: VarName, block: InlineTmpl)
    extends Stmt {
        override def toString = "%s: %s".format(name, block)
    }
    
}