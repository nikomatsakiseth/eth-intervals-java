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
    
    lexical.delimiters += (
        "{", "}", "[", "]", "(", ")", "=", "->", ",", ";", "@", ":"
    )
    lexical.reserved += (
        "class", "extends", "import", "package", "interval", "requires", "for", "while", "if",
        "else", "throw", "locks", "new", "break", "return", "continue"
    )
    
    def parseCompUnitFromJavaReader(javaReader: java.io.Reader) = {
        val reader = scala.util.parsing.input.StreamReader(javaReader)
        val tokens = new lexical.Scanner(reader)
        phrase(compUnit)(tokens)
    }
    
    def comma[A](p: PackratParser[A]) = repsep(p, ",")
    def comma1[A](p: PackratParser[A]) = rep1sep(p, ",")
    
    // ___ Names ____________________________________________________________
    
    lazy val baseName = positioned(
        ident ^^ hl.BaseName
    )
    
    lazy val qualName = positioned(
        baseName~rep("."~>ident) ^^ { 
            case b~fs => fs.foldLeft[hl.QualName](b) { _ / _ } }
    )
    
    lazy val varName = positioned(
        ident ^^ hl.VarName
    )
    
    // ___ Declarations _____________________________________________________
    
    lazy val compUnit = positioned(
        opt(packageDecl)~rep(importDecl)~rep(classDecl) ^^ {
            case p~i~c => hl.CompUnit(p, i, c)
        }
    )
    
    lazy val packageDecl = positioned(
        "package"~>qualName<~";"
    )
    
    lazy val importDecl = positioned(
        "import"~>qualName~opt("("~>baseName<~")")~";"  ^^ { case q~n~_ => hl.ImportOne(q, n) }
    |   "import"~>qualName<~"."<~"*"<~";"               ^^ hl.ImportAll
    )
    
    lazy val superClasses = opt("extends"~>comma1(qualName)) ^^ {
        case Some(names) => names
        case None => List()
    }
    
    lazy val annotation = positioned(
        "["~>qualName<~"]" ^^ hl.Annotation
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
    |   hbDecl
    |   lockDecl
    )
    
    lazy val methodDecl = positioned(
        annotations~typeRef~rep1(declPart)~rep(requirement)~optBody ^^ {
            case ann~ret~parts~reqs~optBody => hl.MethodDecl(ann, parts, ret, reqs, optBody)
        }
    )
    
    lazy val declPart = positioned(
        ident~tuplePattern ^^ { case i~p => hl.DeclPart(i, p) }
    )
    
    lazy val requirement = positioned(
        "requires"~>qualName~reqRhs ^^ { case l~r => hl.Requirement(l, r) }
    )
    
    lazy val reqRhs = positioned(
        ident~qualName ^^ { case o~r => hl.ReqRhs(o, r) }      
    )
    
    lazy val intervalDecl = positioned(
        annotations~"interval"~varName~";" ^^ { 
            case ann~_~nm~";" => hl.IntervalDecl(ann, nm, None, None) }
    |   annotations~"interval"~varName~"("~qualName~")"~optBody ^^ { 
            case ann~_~nm~"("~qn~")"~optBody => hl.IntervalDecl(ann, nm, Some(qn), optBody) }
    )
    
    lazy val optBody = (
        block   ^^ { case b => Some(b) }
    |   ";"     ^^^ None
    )
    
    lazy val fieldDecl = positioned(
        annotations~opt(typeRef)~varName~opt("="~>expr)~";" ^^ {
            case a~t~n~v~";" => hl.FieldDecl(a, n, t, v) }
    )
    
    lazy val hbDecl = positioned(
        annotations~qualName~"->"~qualName~";" ^^ { case a~l~_~r~";" => hl.HbDecl(a, l, r) }
    )
    
    lazy val lockDecl = positioned(
        annotations~qualName~"locks"~qualName~";" ^^ { case a~l~_~r~";" => hl.LockDecl(a, l, r) }
    )
    
    // ___ Argument Patterns ________________________________________________
    
    lazy val tuplePattern: PackratParser[hl.TuplePattern] = positioned(
        "("~>comma(pattern)<~")" ^^ hl.TuplePattern
    )
    lazy val varPattern = positioned(
        annotations~typeRef~varName ^^ {
            case a~t~n => hl.VarPattern(a, t, n) }
    )
    lazy val pattern = tuplePattern | varPattern
    
    // ___ Type References __________________________________________________
    
    lazy val typeRef = pathType | classType
    
    lazy val pathType = positioned(
        qualName~":"~varName ^^ { case b~":"~v => hl.PathType(b, v) }
    )
    
    lazy val classType = positioned(
        qualName~"["~comma(typeArg)~"]" ^^ { case c~"["~a~"]" => hl.ClassType(c, a) }
    |   qualName ^^ { case c => hl.ClassType(c, List()) }
    )
    
    lazy val typeArg = positioned(
        varName~reqRhs ^^ { case v~r => hl.TypeArg(v, r) }
    )
    
    // ___ Expressions ______________________________________________________
    
    lazy val block = positioned(
        "{"~>rep(stmt)<~"}"                 ^^ hl.Block
    )
    
    lazy val tuple = positioned(
        "("~>comma(expr)<~")"               ^^ hl.Tuple
    )
    
    lazy val callPart = positioned(
        ident~tuple                         ^^ { case i~t => hl.CallPart(i, t) }
    )
    
    lazy val rootExpr: PackratParser[hl.Expr] = positioned(
        tuple
    |   rep1(callPart)                      ^^ { case cp => hl.MethodCall(hl.ImpThis, cp) }
    |   varName                             ^^ hl.Var
    |   numericLit                          ^^ hl.Literal // XXX
    |   stringLit                           ^^ hl.Literal
    |   "new"~typeRef~tuple                 ^^ { case _~t~a => hl.New(t, a) }
    )
    
    lazy val field = positioned(
        rootExpr~"."~varName~rep("."~>varName) ^^ { 
            case e~"."~f~fs => fs.foldLeft(hl.Field(e, f))(hl.Field(_, _)) }
    )
    
    lazy val mthdCall = positioned(
        rootExpr~rep1("."~>rep1(callPart))  ^^ { case e~cs => cs.foldLeft(e)(hl.MethodCall(_, _)) }
    )
    
    lazy val assign = positioned(
        coreLvalue~"="~expr                 ^^ { case l~"="~e => hl.Assign(l, e) }
    )
    
    lazy val coreExpr: PackratParser[hl.Expr] = field | mthdCall | assign | rootExpr
    
    lazy val coreLvalue: PackratParser[hl.Lvalue] = positioned(
        field
    |   varName                             ^^ hl.Var
    )
    
    lazy val lvalue: PackratParser[hl.Lvalue] = coreLvalue | pattern
    
    lazy val stmtExpr: PackratParser[hl.Expr] = positioned(
        block
    |   "if"~"("~expr~")"~expr~"else"~expr  ^^ { case _~"("~c~")"~t~"else"~f => hl.IfElse(c, t, f) }
    |   "for"~"("~lvalue~":"~expr~")"~expr  ^^ { case _~"("~l~":"~s~")"~b => hl.For(l, s, b) }
    |   "while"~"("~expr~")"~expr           ^^ { case _~"("~c~")"~b => hl.While(c, b) }
    |   pattern~"="~expr~";"                ^^ { case l~"="~v~";" => hl.Assign(l, v) }
    |   pattern~";"                         ^^ { case l~";" => hl.Assign(l, hl.ImpVoid) }
    |   coreExpr<~";"
    )
    
    lazy val expr: PackratParser[hl.Expr] = coreExpr | stmtExpr
    
    lazy val stmt: PackratParser[hl.Stmt] = positioned(
        ";"                                 ^^ { case _ => hl.Empty() }
    |   block
    |   varName~":"~block                   ^^ { case l~":"~b => hl.Labeled(l, b) }
    |   "break"~opt(varName)~";"            ^^ { case _~n~";" => hl.Break(n) }
    |   "return"~expr~";"                   ^^ { case _~n~";" => hl.Return(n) }
    |   "return"~";"                        ^^ { case _ => hl.Return(hl.ImpVoid) }
    |   "continue"~opt(varName)~";"         ^^ { case _~n~";" => hl.Continue(n) }
    |   "if"~"("~expr~")"~stmt~"else"~stmt  ^^ { case _~"("~c~")"~t~"else"~f => hl.IfElse(c, t, f) }
    |   "if"~"("~expr~")"~stmt              ^^ { case _~"("~c~")"~t => hl.IfElse(c, t, hl.ImpVoid) }
    |   "for"~"("~lvalue~":"~expr~")"~stmt  ^^ { case _~"("~l~":"~s~")"~b => hl.For(l, s, b) }
    |   "while"~"("~expr~")"~stmt           ^^ { case _~"("~c~")"~b => hl.While(c, b) }
    |   "throw"~expr                        ^^ { case _~e => hl.Throw(e) }
    |   stmtExpr
    )
    
}

object HlParser {
    
    def main(files: Array[String]) {
        val parser = new HlParser()
        for(file <- files) {
            val javaReader = Util.javaReaderFromPath(file)
            parser.parseCompUnitFromJavaReader(javaReader) match {
                case n: parser.NoSuccess => System.err.println(n.toString)
                case parser.Success(compUnit, _) => HlPretty.stdout.println(compUnit)
            }
        }
    }
    
}