package intervals

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.NoPosition
import scala.util.parsing.syntax.Tokens
import scala.util.parsing.syntax.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical

import Util._

class HlParser extends StandardTokenParsers {
    
    lexical.delimiters += (
        "{", "}", "[", "]", "(", ")", "=", "->", ",", ";", "@", ":"
    )
    lexical.reserved += (
        "class", "extends", "import", "package", "interval", "requires", "for", "while", "if",
        "else", "throw", "locks", "new"
    )
    
    def parseCompUnitFromJavaReader(javaReader: java.io.Reader) = {
        val reader = scala.util.parsing.input.StreamReader(javaReader)
        val tokens = new lexical.Scanner(reader)
        phrase(compUnit)(tokens)
    }
    
    def comma[A](p: Parser[A]) = repsep(p, ",")
    def comma1[A](p: Parser[A]) = rep1sep(p, ",")
    
    // ___ Names ____________________________________________________________
    
    def baseName = positioned(
        ident ^^ hl.BaseName
    )
    
    def qualName = positioned(
        baseName~rep("."~>ident) ^^ { 
            case b~fs => fs.foldLeft[hl.QualName](b) { _ / _ } }
    )
    
    def varName = positioned(
        ident ^^ hl.VarName
    )
    
    // ___ Declarations _____________________________________________________
    
    def compUnit = positioned(
        opt(packageDecl)~rep(importDecl)~rep(classDecl) ^^ {
            case p~i~c => hl.CompUnit(p, i, c)
        }
    )
    
    def packageDecl = positioned(
        "package"~>qualName<~";"
    )
    
    def importDecl = positioned(
        "import"~>qualName~opt("("~>baseName<~")")~";"  ^^ { case q~n~_ => hl.ImportOne(q, n) }
    |   "import"~>qualName<~"."<~"*"<~";"               ^^ hl.ImportAll
    )
    
    def superClasses = opt("extends"~>comma1(qualName)) ^^ {
        case Some(names) => names
        case None => List()
    }
    
    def annotation = positioned(
        "["~>qualName<~"]" ^^ hl.Annotation
    )
    
    def annotations = rep(annotation) 
    
    def classDecl = positioned(
        annotations~"class"~ident~tuplePattern~superClasses~"{"~
            rep(member)~
        "}" ^^ {
            case a~_~n~p~sups~"{"~mems~"}" => hl.ClassDecl(n, a, sups, p, mems)
        }
    )
    
    def member: Parser[hl.MemberDecl] = (
        classDecl
    |   methodDecl
    |   fieldDecl
    |   intervalDecl
    |   hbDecl
    |   lockDecl
    )
    
    def methodDecl = positioned(
        annotations~typeRef~rep1(declPart)~rep(requirement)~optBody ^^ {
            case ann~ret~parts~reqs~optBody => hl.MethodDecl(ann, parts, ret, reqs, optBody)
        }
    )
    
    def declPart = positioned(
        ident~tuplePattern ^^ { case i~p => hl.DeclPart(i, p) }
    )
    
    def requirement = positioned(
        "requires"~>qualName~reqRhs ^^ { case l~r => hl.Requirement(l, r) }
    )
    
    def reqRhs = positioned(
        ident~qualName ^^ { case o~r => hl.ReqRhs(o, r) }      
    )
    
    def intervalDecl = positioned(
        annotations~"interval"~varName~";" ^^ { 
            case ann~_~nm~";" => hl.IntervalDecl(ann, nm, None, None) }
    |   annotations~"interval"~varName~"("~qualName~")"~optBody ^^ { 
            case ann~_~nm~"("~qn~")"~optBody => hl.IntervalDecl(ann, nm, Some(qn), optBody) }
    )
    
    def optBody = (
        block   ^^ { case b => Some(b) }
    |   ";"     ^^^ None
    )
    
    def fieldDecl = positioned(
        annotations~opt(typeRef)~varName~opt("="~>expr)~";" ^^ {
            case a~t~n~v~";" => hl.FieldDecl(a, n, t, v) }
    )
    
    def hbDecl = positioned(
        annotations~qualName~"->"~qualName ^^ { case a~l~_~r => hl.HbDecl(a, l, r) }
    )
    
    def lockDecl = positioned(
        annotations~qualName~"locks"~qualName ^^ { case a~l~_~r => hl.LockDecl(a, l, r) }
    )
    
    // ___ Argument Patterns ________________________________________________
    
    def tuplePattern: Parser[hl.TuplePattern] = positioned(
        "("~>comma(pattern)<~")" ^^ hl.TuplePattern
    )
    def varPattern = positioned(
        annotations~typeRef~varName ^^ {
            case a~t~n => hl.VarPattern(a, t, n) }
    )
    def pattern = tuplePattern | varPattern
    
    // ___ Type References __________________________________________________
    
    def typeRef = pathType | classType
    
    def pathType = positioned(
        qualName~":"~varName ^^ { case b~":"~v => hl.PathType(b, v) }
    )
    
    def classType = positioned(
        qualName~"["~comma(typeArg)~"]" ^^ { case c~"["~a~"]" => hl.ClassType(c, a) }
    |   qualName ^^ { case c => hl.ClassType(c, List()) }
    )
    
    def typeArg = positioned(
        varName~reqRhs ^^ { case v~r => hl.TypeArg(v, r) }
    )
    
    // ___ Expressions ______________________________________________________
    
    def block = positioned(
        "{"~>rep(stmt)<~"}"                 ^^ hl.Block
    )
    
    def tuple = positioned(
        "("~>comma(expr)<~")"               ^^ hl.Tuple
    )
    
    def callPart = positioned(
        ident~tuple                         ^^ { case i~t => hl.CallPart(i, t) }
    )
    
    def coreExpr: Parser[hl.Expr] = positioned(
        tuple
    |   coreLvalue~"="~expr                 ^^ { case l~"="~e => hl.Assign(l, e) }
    |   varName                             ^^ hl.Var
    |   rep1(callPart)                      ^^ { case cp => hl.MethodCall(hl.ImpThis, cp) }
    |   "new"~typeRef~tuple                 ^^ { case _~t~a => hl.New(t, a) }
    |   field
    |   expr~"."~rep1(callPart)             ^^ { case o~"."~cp => hl.MethodCall(o, cp) }
    )
    
    def coreLvalue: Parser[hl.Lvalue] = positioned(
        varName                             ^^ hl.Var
    |   field
    )
    
    def lvalue: Parser[hl.Lvalue] = coreLvalue | pattern
    
    def field = positioned(
        expr~"."~varName                    ^^ { case o~"."~f => hl.Field(o, f) }
    )
    
    def stmtExpr: Parser[hl.Expr] = positioned(
        "{"~>rep(stmt)<~"}"                 ^^ hl.Block
    |   "if"~"("~expr~")"~expr~"else"~expr  ^^ { case _~"("~c~")"~t~"else"~f => hl.IfElse(c, t, f) }
    |   "for"~"("~lvalue~":"~expr~")"~expr  ^^ { case _~"("~l~":"~s~")"~b => hl.For(l, s, b) }
    |   "while"~"("~expr~")"~expr           ^^ { case _~"("~c~")"~b => hl.While(c, b) }
    |   pattern~"="~expr~";"                ^^ { case l~"="~v~";" => hl.Assign(l, v) }
    |   pattern~";"                         ^^ { case l~";" => hl.Assign(l, hl.ImpVoid) }
    |   coreExpr<~";"
    )
    
    def expr: Parser[hl.Expr] = coreExpr | stmtExpr
    
    def stmt: Parser[hl.Stmt] = positioned(
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