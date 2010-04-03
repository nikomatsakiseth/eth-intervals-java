package inter

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.NoPosition
import scala.util.parsing.syntax.Tokens
import scala.util.parsing.syntax.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

import Util._

class HlParser extends StandardTokenParsers with PackratParsers {
    
    import Hl.{P => hl}
    
    lexical.delimiters += (
        "{{", "}}", "{", "}", "[", "]", "(", ")", "=", "->", ",", "@", ":", ".", ";"
    )
    lexical.reserved += (
        "class", "extends", "import", "package", "interval", "requires",
        "throw", "locks", "new", "locks"
    )
    
    def parseCompUnitFromJavaReader(javaReader: java.io.Reader) = {
        val reader = scala.util.parsing.input.StreamReader(javaReader)
        val tokens = new lexical.Scanner(reader)
        phrase(compUnit)(tokens)
    }
    
    def comma[A](p: PackratParser[A]) = repsep(p, ",")<~opt(",")
    def comma1[A](p: PackratParser[A]) = rep1sep(p, ",")<~opt(",")
    
    // ___ Names ____________________________________________________________
    
    lazy val absDotName = positioned(
        ident~rep1("."~>ident) ^^ { 
            case f~fs => (f :: fs).foldLeft[hl.AbsName](hl.AbsRoot) { _ / _ } }
    )
    
    lazy val absName = absDotName | positioned(
        ident ^^ (hl.AbsRoot / _)
    )
    
    lazy val relBase = positioned(ident ^^ hl.RelBase)
    
    lazy val relDot = positioned(
        relBase~rep1("."~>ident) ^^ {
            case b~fs => fs.foldLeft[hl.RelName](b) { _ / _ } }
    )
    
    lazy val relName = relDot | relBase
    
    lazy val varName = positioned(
        ident ^^ hl.VarName
    )
    
    // ___ Declarations _____________________________________________________
    
    lazy val compUnit = positioned(
        packageDecl~rep(importDecl)~rep(classDecl) ^^ {
            case p~i~c => hl.CompUnit(p, i, c)
        }
    )
    
    lazy val packageDecl = positioned(
        opt("package"~>absName<~";") ^^ { case Some(a) => a; case None => hl.AbsRoot }
    )
    
    lazy val importDecl = positioned(
        "import"~>absName~opt("("~>relBase<~")")~";"  ^^ { case q~n~_ => hl.ImportOne(q, n) }
    |   "import"~>absName<~"."<~"*"<~";"               ^^ hl.ImportAll
    )
    
    lazy val superClasses = opt("extends"~>comma1(relName)) ^^ {
        case Some(names) => names
        case None => List()
    }
    
    lazy val annotation = positioned(
        "["~>relName<~"]" ^^ hl.Annotation
    )
    
    lazy val annotations = rep(annotation) 
    
    lazy val classDecl = positioned(
        annotations~"class"~ident~tuplePattern~superClasses~"{"~
            rep(member)~
        "}" ^^ {
            case a~_~n~p~sups~"{"~mems~"}" => hl.ClassDecl(n, a, sups, p, mems)
        }
    )
    
    lazy val member: PackratParser[hl.MemberDecl] = (
        classDecl
    |   methodDecl
    |   fieldDecl
    |   intervalDecl
    |   relDecl
    )
    
    lazy val methodDecl = positioned(
        annotations~typeRef~rep1(declPart)~rep(requirement)~optBody ^^ {
            case ann~ret~parts~reqs~optBody => hl.MethodDecl(ann, parts, ret, reqs, optBody)
        }
    |   annotations~rep1(declPart)~rep(requirement)~optBody ^^ {
            case ann~parts~reqs~optBody => hl.MethodDecl(ann, parts, hl.InferredTypeRef, reqs, optBody)
        }
    )
    
    lazy val declPart = positioned(
        ident~tuplePattern ^^ { case i~p => hl.DeclPart(i, p) }
    )
    
    lazy val requirement = positioned(
        "requires"~>path~reqRhs ^^ { case l~r => hl.Requirement(l, r) }
    )
    
    lazy val declOp = (
        "->"            ^^^ hl.ReqHb
    |   "locks"         ^^^ hl.ReqLocks
    )
    
    lazy val reqOp = (
        declOp
    |   "subOf"         ^^^ hl.ReqSubOf
    |   "inlineSubOf"   ^^^ hl.ReqInlineSubOf
    )
    
    lazy val reqRhs = positioned(
        reqOp~path ^^ { case o~r => hl.ReqRhs(o, r) }      
    )
    
    lazy val intervalDecl = positioned(
        annotations~"interval"~varName~";" ^^ { 
            case ann~_~nm~";" => hl.IntervalDecl(ann, nm, None, None) }
    |   annotations~"interval"~varName~"("~path~")"~optBody ^^ { 
            case ann~_~nm~"("~qn~")"~optBody => hl.IntervalDecl(ann, nm, Some(qn), optBody) }
    )
    
    lazy val optBody = (
        itmpl   ^^ { case b => Some(b) }
    |   ";"     ^^^ None
    )
    
    lazy val fieldDecl = positioned(
        annotations~typeRef~varName~opt("="~>expr)~";" ^^ {
            case a~t~n~v~";" => hl.FieldDecl(a, n, t, v) }
    |   annotations~varName~opt("="~>expr)~";" ^^ {
            case a~n~v~";" => hl.FieldDecl(a, n, hl.InferredTypeRef, v) }
    )
    
    lazy val relDecl = positioned(
        annotations~path~declOp~path~";" ^^ { case a~l~k~r~";" => hl.RelDecl(a, l, k, r) }
    )
    
    // ___ Argument Patterns and Lvalues ____________________________________
    
    lazy val tuplePattern = positioned(
        "("~>comma(pattern)<~")" ^^ hl.TuplePattern
    )
    lazy val varPattern = positioned(
        annotations~typeRef~varName ^^ {
            case a~t~n => hl.VarPattern(a, t, n) }
    )
    lazy val pattern: PackratParser[hl.Pattern] = tuplePattern | varPattern
    
    lazy val tupleLvalue= positioned(
        "("~>comma(lvalue)<~")" ^^ hl.TupleLvalue
    )
    lazy val varLvalue = positioned(
        annotations~typeRef~varName ^^ {
            case a~t~n => hl.VarLvalue(a, t, n) }
    |   annotations~varName ^^ {
            case a~n => hl.VarLvalue(a, hl.InferredTypeRef, n) }
    )
    lazy val lvalue: PackratParser[hl.Lvalue] = tupleLvalue | varLvalue
    
    // ___ Type References __________________________________________________
    
    lazy val typeRef = pathType | classType
    
    lazy val pathType = positioned(
        path~":"~varName ^^ { case b~":"~v => hl.PathType(b, v) }
    )
    
    lazy val classType = positioned(
        relName~"["~comma(typeArg)~"]" ^^ { case c~"["~a~"]" => hl.ClassType(c, a) }
    |   relName ^^ { case c => hl.ClassType(c, List()) }
    )
    
    lazy val typeArg = positioned(
        varName~reqRhs ^^ { case v~r => hl.TypeArg(v, r) }
    )
    
    // ___ Paths ____________________________________________________________
    
    lazy val path = positioned(
        varName~rep(varName) ^^ {
            case v~fs => fs.foldLeft[hl.Path](hl.Var(v))(hl.PathField(_, _)) }
    )
    
    // ___ Expressions ______________________________________________________
    
    lazy val itmpl = positioned(
        "{"~>stmts<~"}"                 ^^ hl.InlineTmpl
    )
    
    lazy val atmpl = positioned(
        "{{"~>stmts<~"}}"               ^^ hl.AsyncTmpl
    )
    
    lazy val tuple = positioned(
        "("~>comma(expr)<~")"               ^^ hl.Tuple
    )
    
    lazy val arg: PackratParser[hl.Expr] = tuple | itmpl | atmpl
    
    lazy val callPart = positioned(
        ident~arg                           ^^ { case i~a => hl.CallPart(i, a) }
    )
    
    lazy val rcvr: PackratParser[hl.Expr] = positioned(
        arg
    |   varName                             ^^ hl.Var
    |   numericLit                          ^^ hl.Literal // XXX
    |   stringLit                           ^^ hl.Literal
    |   "null"                              ^^ { case _ => hl.Null() }
    |   "new"~typeRef~tuple                 ^^ { case _~t~a => hl.New(t, a) }
    )
    
    lazy val field = positioned(
        rcvr~varName~rep(varName)           ^^ { case r~f~fs => fs.foldLeft(hl.Field(r, f))(hl.Field(_, _)) }
    )
    
    lazy val mthdCall = positioned(
        rcvr~rep1(callPart)                 ^^ { case r~cp => hl.MethodCall(r, cp) }
    |   rep1(callPart)                      ^^ { case cp => hl.MethodCall(hl.ImpThis, cp) }
    )
    
    lazy val assign = positioned(
        lvalue~"="~expr                     ^^ { case l~"="~e => hl.Assign(l, e) }
    )
    
    lazy val expr: PackratParser[hl.Expr] = mthdCall | field | assign | rcvr
    
    lazy val stmt: PackratParser[hl.Stmt] = positioned(
        varName~":"~itmpl                   ^^ { case l~":"~b => hl.Labeled(l, b) }
    |   expr
    )
    
    lazy val stmts = repsep(stmt, ";")<~opt(";")
    
}

object HlParser {
    
    def main(files: Array[String]) {
        val parser = new HlParser()
        for(file <- files) {
            val javaReader = Util.javaReaderFromPath(file)
            parser.parseCompUnitFromJavaReader(javaReader) match {
                case n: parser.NoSuccess => System.err.println(n.toString)
                case parser.Success(compUnit, _) => compUnit.println(PrettyPrinter.stdout)
            }
        }
    }
    
}