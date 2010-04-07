package inter.compiler

import scala.util.parsing.input.Reader
import scala.util.parsing.input.PagedSeqReader
import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.syntax.Tokens
import scala.util.parsing.syntax.StdTokens
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers
import scala.collection.immutable.PagedSeq

import java.io.File

import Ast.{Parse => out}
import Util._

class Parse extends StdTokenParsers with PackratParsers {
    
    trait HlTokens extends StdTokens {
        case class Operator(chars: String) extends Token {
            override def toString = "operator '%s'".format(chars)
        }
    }
    
    class HlLexical extends StdLexical with HlTokens {
        def esc = elem("escape", c => c == '`')
        def notEsc = elem("escape", c => c != '`' && c != EofCh && c != '\n')
        
        val sep = "()[]{};\"\'."
        def isOperCont(c: Char) = c != EofCh && !c.isWhitespace && !sep.contains(c)
        def isOperStart(c: Char) = isOperCont(c) && !c.isLetter && !c.isDigit
        
        def operStart = elem("operStart", isOperStart)            
        def operCont = elem("operCont", isOperCont)

        override def token: Parser[Token] = ( 
            // This isn't quite right: it won't work for "//" or "/*", for example.
            operStart~rep(operCont) ^^ { case c~cs => processOper((c::cs).mkString("")) }
        |   esc~rep1(notEsc)~esc    ^^ { case _~cs~_ => Keyword(cs.mkString("")) }
        |   super.token
        )
        
        protected def processOper(name: String) = {
            if (reserved.contains(name) || delimiters.contains(name)) Keyword(name) 
            else Operator(name)
        }
    }
    
    type Tokens = StdTokens
    val lexical = new HlLexical
    
    lexical.delimiters += (
        "{{", "}}", "{", "}", "[", "]", "(", ")", "=", "->", ",", ":", ".", ";", "*"
    )
    lexical.reserved += (
        "class", "extends", "import", "package", 
        "interval", "requires", "locks", "new"
    )
    
    def comma[A](p: PackratParser[A]) = repsep(p, ",")<~opt(",")
    def comma1[A](p: PackratParser[A]) = rep1sep(p, ",")<~opt(",")
    
    lazy val oper = (
        elem("operator", _.isInstanceOf[lexical.Operator]) ^^ (_.chars)
    )
    
    // ___ Names ____________________________________________________________
    
    lazy val absName = positioned(
        ident~rep("."~>ident) ^^ { case b~fs => Ast.AbsName(Name.Qual((b :: fs).reverse)) }
    )
    
    lazy val relBase = positioned(ident ^^ out.RelBase)
    
    lazy val relDot = positioned(
        relBase~rep1("."~>ident) ^^ {
            case b~fs => fs.foldLeft[out.RelName](b) { _ / _ } }
    )
    
    lazy val relName = relDot | relBase
    
    lazy val varName = positioned(
        ident ^^ Ast.VarName
    )
    
    // ___ Declarations _____________________________________________________
    
    lazy val compUnit = positioned(
        packageDecl~rep(importDecl)~rep(classDecl) ^^ {
            case p~i~c => out.CompUnit(p, i, c)
        }
    )
    
    lazy val packageDecl = positioned(
        opt("package"~>absName<~";") ^^ { case Some(a) => a; case None => Ast.AbsName(Name.QualRoot) }
    )
    
    lazy val importDecl = positioned(
        "import"~>absName~("->"~>relBase)<~";"  ^^ { case q~n => out.ImportOne(q, n) }
    |   "import"~>absName<~";"                  ^^ { case q => out.ImportOne(q, out.RelBase(q.component)) }
    |   "import"~>absName<~"."<~"*"<~";"        ^^ out.ImportAll
    )
    
    lazy val superClasses = opt("extends"~>comma1(relName)) ^^ {
        case Some(names) => names
        case None => List()
    }
    
    lazy val annotation = positioned(
        "["~>relName<~"]" ^^ out.Annotation
    )
    
    lazy val annotations = rep(annotation) 
    
    lazy val optTuplePattern = positioned(
        opt(tuplePattern) ^^ { 
            case Some(tp) => tp
            case None => out.TuplePattern(List())
        }
    )
    
    lazy val classDecl = positioned(
        annotations~"class"~relBase~optTuplePattern~superClasses~"{"~
            rep(member)~
        "}" ^^ {
            case a~_~n~p~sups~"{"~mems~"}" => out.ClassDecl(n, a, sups, p, mems, ())
        }
    )
    
    lazy val member: PackratParser[out.MemberDecl] = (
        classDecl
    |   methodDecl
    |   fieldDecl
    |   intervalDecl
    |   relDecl
    )
    
    lazy val methodDecl = positioned(
        annotations~typeRef~rep1(declPart)~rep(requirement)~optBody ^^ {
            case ann~ret~parts~reqs~optBody => out.MethodDecl(ann, parts, ret, reqs, optBody, ())
        }
    |   annotations~rep1(declPart)~rep(requirement)~optBody ^^ {
            case ann~parts~reqs~optBody => out.MethodDecl(ann, parts, out.InferredTypeRef, reqs, optBody, ())
        }
    )
    
    lazy val declPart = positioned(
        ident~tuplePattern ^^ { case i~p => out.DeclPart(i, p) }
    )
    
    lazy val requirement = positioned(
        "requires"~>path~pcRel~path ^^ { case l~p~r => out.PathRequirement(l, p, r) }
    )
    
    lazy val declOp = (
        "->"            ^^^ PcHb
    |   "locks"         ^^^ PcLocks
    )
    
    lazy val pcRel = (
        declOp
    |   "subOf"         ^^^ PcSubOf
    |   "inlineSubOf"   ^^^ PcInlineSubOf
    )
    
    lazy val wcRel = (
        "="             ^^^ PcEq
    |   "permitsWr"     ^^^ PcPermitsWr
    |   "permitsRd"     ^^^ PcPermitsRd
    )
    
    lazy val tcRel = (
        ":"             ^^^ TcEq
    |   "<:"            ^^^ TcSub
    |   ":>"            ^^^ TcSup
    )
    
    lazy val intervalDecl = positioned(
        annotations~"interval"~varName~";" ^^ { 
            case ann~_~nm~";" => out.IntervalDecl(ann, nm, None, None, ()) }
    |   annotations~"interval"~varName~"("~path~")"~optBody ^^ { 
            case ann~_~nm~"("~qn~")"~optBody => out.IntervalDecl(ann, nm, Some(qn), optBody, ()) }
    )
    
    lazy val optBody = (
        itmpl   ^^ { case b => Some(b) }
    |   ";"     ^^^ None
    )
    
    lazy val fieldDecl = positioned(
        annotations~typeRef~varName~opt("="~>expr)~";" ^^ {
            case a~t~n~v~";" => out.FieldDecl(a, n, t, v, ()) }
    |   annotations~varName~opt("="~>expr)~";" ^^ {
            case a~n~v~";" => out.FieldDecl(a, n, out.InferredTypeRef, v, ()) }
    )
    
    lazy val relDecl = positioned(
        annotations~path~declOp~path~";" ^^ { case a~l~k~r~";" => out.RelDecl(a, l, k, r) }
    )
    
    // ___ Argument Patterns and Lvalues ____________________________________
    
    lazy val tuplePattern = positioned(
        "("~>comma(pattern)<~")" ^^ out.TuplePattern
    )
    lazy val varPattern = positioned(
        annotations~typeRef~varName ^^ {
            case a~t~n => out.VarPattern(a, t, n, ()) }
    )
    lazy val pattern: PackratParser[out.Pattern] = tuplePattern | varPattern
    
    lazy val tupleLvalue= positioned(
        "("~>comma(lvalue)<~")" ^^ out.TupleLvalue
    )
    lazy val varLvalue = positioned(
        annotations~typeRef~varName ^^ {
            case a~t~n => out.VarLvalue(a, t, n, ()) }
    |   annotations~varName ^^ {
            case a~n => out.VarLvalue(a, out.InferredTypeRef, n, ()) }
    )
    lazy val lvalue: PackratParser[out.Lvalue] = tupleLvalue | varLvalue
    
    // ___ Type References __________________________________________________
    
    lazy val typeRef: PackratParser[out.TypeRef] = pathType | classType
    
    lazy val pathType = positioned(
        path~":"~varName ^^ { case b~":"~v => out.PathType(b, v) }
    )
    
    lazy val classType = positioned(
        relName~"["~comma(typeArg)~"]" ^^ { case c~"["~a~"]" => out.ClassType(c, a) }
    |   relName ^^ { case c => out.ClassType(c, List()) }
    )
    
    lazy val typeArg: PackratParser[out.TypeArg] = typeTypeArg | pathTypeArg
    
    lazy val typeTypeArg = positioned(
        varName~tcRel~typeRef ^^ { case x~o~t => out.TypeTypeArg(x, o, t) }
    )
    
    lazy val pathTypeArg = positioned(
        varName~wcRel~path ^^ { case x~o~p => out.PathTypeArg(x, o, p) }
    )
    
    // ___ Paths ____________________________________________________________
    
    lazy val path = positioned(
        varName~rep(varName) ^^ {
            case v~fs => fs.foldLeft[out.Path](out.Var(v, ()))(out.PathField(_, _)) }
    )
    
    // ___ Expressions ______________________________________________________
    
    lazy val itmpl = positioned(
        "{"~>stmts<~"}"                 ^^ out.InlineTmpl
    )
    
    lazy val atmpl = positioned(
        "{{"~>stmts<~"}}"               ^^ out.AsyncTmpl
    )
    
    lazy val tuple = positioned(
        "("~>comma(expr)<~")"               ^^ out.Tuple
    )
    
    lazy val arg: PackratParser[out.Expr] = tuple | itmpl | atmpl
    
    lazy val callPart = positioned(
        ident~arg                           ^^ { case i~a => out.CallPart(i, a) }
    )
    
    lazy val rcvr: PackratParser[out.Expr] = positioned(
        arg
    |   varName                             ^^ { case n => out.Var(n, ()) }
    |   numericLit                          ^^ out.Literal // XXX
    |   stringLit                           ^^ out.Literal
    |   "null"                              ^^ { case _ => out.Null() }
    |   "new"~typeRef~tuple                 ^^ { case _~t~a => out.New(t, a) }
    )
    
    lazy val field = positioned(
        rcvr~varName~rep(varName)           ^^ { case r~f~fs => fs.foldLeft(out.Field(r, f, ()))(out.Field(_, _, ())) }
    )
    
    lazy val mthdCall = positioned(
        rcvr~rep1(callPart)                 ^^ { case r~cp => out.MethodCall(r, cp, ()) }
    |   rep1(callPart)                      ^^ { case cp => out.MethodCall(out.ImpThis, cp, ()) }
    )
    
    lazy val unary = mthdCall | field | rcvr
    
    lazy val expr: PackratParser[out.Expr] = (
        unary~rep(oper~unary) ^^ { case l~rs => 
            val parts = rs.map { case o~r => out.CallPart(o, r) }
            out.MethodCall(l, parts, ())
        }
    )
    
    lazy val stmt: PackratParser[out.Stmt] = positioned(
        varName~":"~itmpl                   ^^ { case l~":"~b => out.Labeled(l, b) }
    |   lvalue~"="~expr                     ^^ { case l~"="~e => out.Assign(l, e) }
    |   expr
    )
    
    lazy val stmts = repsep(stmt, ";")<~opt(";")
    
}

object Parse {
    
    class FileReader(interFile: File, seq: PagedSeq[Char], off: Int) extends PagedSeqReader(seq, off) {
        
        override def rest: FileReader =
            if (seq.isDefinedAt(offset)) new FileReader(interFile, seq, offset + 1)
            else this
            
        override def drop(n: Int): FileReader = 
            new FileReader(interFile, seq, offset + n)
            
        override def pos = new OffsetPosition(source, offset) with InterPosition {
            def file = interFile
        }
        
    }

    def apply(state: CompilationState, interFile: File) = {
        val javaReader = Util.javaReaderFromFile(interFile)
        val parser = new Parse()
        
        val reader = new FileReader(interFile, PagedSeq.fromReader(javaReader), 0)
        val tokens = new parser.lexical.Scanner(reader)
        parser.phrase(parser.compUnit)(tokens) match {
            case n: parser.NoSuccess => {
                state.reporter.report(n.next.pos, "parse.error", n.msg)
                None
            }
            case parser.Success(compUnit, _) => {
                if(state.config.dumpParsedTrees) {
                    compUnit.println(PrettyPrinter.stdout)
                }
                
                Some(compUnit)
            }
        }                
    }
      
}