package harmonic.compiler

import scala.util.parsing.input.Reader
import scala.util.parsing.input.PagedSeqReader
import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.OffsetPosition
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.syntax.Tokens
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers
import scala.collection.immutable.PagedSeq

import com.smallcultfollowing.lathos.Context

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
        
        val sep = "()[]{};@\"\'."
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
        "{{", "}}", "{", "}", "[", "]", "(", ")", "=", "->", ",", ":", ".", ";", "*", "@"
    )
    lexical.reserved += (
        "class", "extends", "import", "package", 
        "interval", "requires", "ensures", "locks", "new",
        "type", "ghost", "true", "false", "this",
        "super", "null", "return", "break", "continue",
        
        "permitsWr", "permitsRd", "ensuresFinal"
    )
    
    def comma[A](p: PackratParser[A]) = repsep(p, ",")<~opt(",")
    def comma1[A](p: PackratParser[A]) = rep1sep(p, ",")<~opt(",")
    def optl[A](p: PackratParser[List[A]]) = opt(p) ^^ { _.getOrElse(List()) }
    
    lazy val oper = (
        elem("operator", _.isInstanceOf[lexical.Operator]) ^^ (_.chars)
    |   "*" ^^^ "*"
    )
    
    // ___ Names ____________________________________________________________
    
    lazy val varIdent = ident | "this"
    
    lazy val relBase = positioned(varIdent ^^ out.RelBase)
    
    lazy val relDot = positioned(
        relBase~"."~ident~rep1("."~>ident) ^^ {
            case b~"."~f~fs => fs.foldLeft(b / f) { _ / _ } }
    )
    
    lazy val relName = relDot | relBase
    
    lazy val localName = positioned(varIdent ^^ { s => Ast.LocalName(Name.LocalVar(s)) })    
    lazy val varName = relBase | "("~>relName<~")"
    lazy val memberName = varName
    
    lazy val packageName = positioned(
        rep1sep(ident, ".") ^^ { idents => 
            Ast.PackageName(idents.foldLeft[Name.Package](Name.Root)(Name.Subpackage(_, _)))
        }
    )
    
    // ___ Declarations _____________________________________________________
    
    lazy val compUnit = positioned(
        packageDecl~rep(importDecl)~rep(classDecl) ^^ {
            case p~i~c => out.CompUnit(p, i, c)
        }
    )
    
    lazy val packageDecl = positioned(
        opt("package"~>packageName<~";") ^^ { 
            case Some(a) => a
            case None => Ast.PackageName(Name.Root) 
        }
    )
    
    lazy val importDecl = positioned(
        "import"~>relName~("->"~>relBase)<~";"  ^^ { case q~n => out.ImportOne(q, n) }
    |   "import"~>relName<~";"                  ^^ { case q => out.ImportOne(q, out.RelBase(q.component)) }
    |   "import"~>relName<~"."<~"*"<~";"        ^^ out.ImportAll
    )
    
    lazy val extendsArg: PackratParser[out.ExtendsArg] = (
        tupleExtendsArg | pathExtendsArg
    )
    
    lazy val tupleExtendsArg = positioned(
        "("~>comma(extendsArg)<~")"             ^^ out.TupleExtendsArg
    )
    
    lazy val pathExtendsArg = positioned(
        path                                    ^^ out.PathExtendsArg
    )
    
    lazy val noExtendsArg = positioned(
        success(())                             ^^ { _ => out.TupleExtendsArg(List()) }
    )
    
    lazy val extendsDecl = positioned(
        relName~(tupleExtendsArg|noExtendsArg)  ^^ { case n~ea => out.ExtendsDecl(n, List(ea), ()) }
    )
    
    lazy val superClasses = opt("extends"~>comma1(extendsDecl)) ^^ {
        case Some(names) => names
        case None => List()
    }
    
    lazy val annotation = positioned(
        "@"~>relName ^^ out.Annotation
    )
    
    lazy val annotations = rep(annotation) 
    
    lazy val optClassTupleParam = positioned(
        opt(tupleMthdParam) ^^ { 
            case Some(tp) => tp
            case None => out.TupleParam(List())
        }
    )
    
    lazy val classDecl = positioned(
        annotations~"class"~relBase~optClassTupleParam~superClasses~"{"~
            rep(member)~
        "}" ^^ {
            case a~_~n~p~sups~"{"~mems~"}" => out.ClassDecl(n, a, sups, p, mems, (), ())
        }
    )
    
    lazy val member: PackratParser[out.MemberDecl] = (
        methodDecl
    |   fieldDecl
    |   intervalDecl
    |   relDecl
    )
    
    lazy val methodDecl = positioned(
        annotations~rep1(declPart)~optTypeRef~rep(requirement)~rep(ensures)~optBody ^^ {
            case ann~parts~ret~reqs~ens~optBody => 
                val name = Name.Method(parts.map(_._1))
                val params = parts.map(_._2)
                out.MethodDecl(ann, name, params, ret, reqs, ens, optBody)
        }
    )
    
    lazy val declPart = ident~tupleMthdParam ^^ { case i~p => (i, p) }
    
    lazy val reqRelation = positioned(
        typeRef~tcRel~typeRef   ^^ { case l~op~r => out.TypeRequirement(l, op, r) }
    |   path~pcRel~path         ^^ { case l~op~r => out.PathRequirement(l, op, r) }
    )
    
    lazy val requirement = "requires"~>reqRelation
    lazy val ensures = "ensures"~>reqRelation
    
    lazy val declOp = (
        "->"            ^^^ PcHb
    |   "locks"         ^^^ PcLocks
    )
    
    lazy val pcRel = (
        declOp
    |   "subOf"         ^^^ PcSubOf
    |   "inlineSubOf"   ^^^ PcInlineSubOf
    )
    
    lazy val wcRel: PackratParser[PcWcRel] = (
        "="             ^^^ PcEq
    |   guardRel
    )
    
    lazy val guardRel: PackratParser[PcWcRel] = (
        "permitsWr"     ^^^ PcPermitsWr
    |   "permitsRd"     ^^^ PcPermitsRd
    |   "ensuresFinal"  ^^^ PcEnsuresFinal
    )
    
    lazy val tcRel = (
        ":"             ^^^ TcEq
    |   "<:"            ^^^ TcSub
    |   ":>"            ^^^ TcSup
    )
    
    lazy val intervalDecl = positioned(
        annotations~"interval"~relBase~"("~path~")"~body ^^ { 
            case a~_~n~"("~p~")"~b => out.IntervalDecl(a, n, p, b) 
        }
    )
    
    lazy val optBody = (
        body
    |   ";"     ^^ { case _ => out.AbstractBody() }
    )
    
    lazy val fieldValue = positioned(
        "="~>expr ^^ { case e => out.Body(List(e)) }
    |   "="~>body
    )
    
    lazy val fieldDecl = positioned(
        annotations~relBase~optTypeRef~fieldValue~";" ^^ {
            case a~n~t~v~";" => out.FieldDecl(a, n, t, v) 
        }
    )
    
    lazy val relDecl = positioned(
        annotations~path~declOp~path~";" ^^ { case a~l~k~r~";" => out.RelDecl(a, l, k, r) }
    )
    
    // ___ Lvalues: Parameters and Locals ___________________________________
    
    lazy val tupleMthdParam = positioned(
        "("~>comma(mthdParam)<~")" ^^ { case ps => out.TupleParam(ps) }
    )
    lazy val varMthdParam = positioned(
        annotations~localName~reqTypeRef ^^ {
            case a~n~t => out.VarParam(a, t, n, ()) }
    )
    lazy val mthdParam: PackratParser[out.Param[Unit]] = tupleMthdParam | varMthdParam
    
    lazy val tupleBlkParam = positioned(
        "("~>comma(blkParam)<~")" ^^ { case ps => out.TupleParam(ps) }
    )
    lazy val varBlkParam = positioned(
        annotations~localName~optTypeRef ^^ {
            case a~n~t => out.VarParam(a, t, n, ()) }
    )
    lazy val blkParam: PackratParser[out.Param[Unit]] = tupleBlkParam | varBlkParam
    
    lazy val tupleLvalue = positioned(
        "("~>comma(lvalue)<~")" ^^ { case ls => out.TupleLvalue(ls) }
    )
    lazy val reassignLvalue = positioned(
        localName ^^ { case n => out.ReassignVarLvalue(n, ()) }
    )
    lazy val fieldLvalue = positioned(
        relDot ^^ { case n => out.FieldLvalue(n, ()) }
    )
    lazy val declLvalue = positioned(
        annotations~localName~reqTypeRef ^^ { 
            case a~n~t => out.DeclareVarLvalue(a, t, n, ())
        }
    )
    lazy val lvalue: PackratParser[out.Lvalue] = tupleLvalue | declLvalue | reassignLvalue | fieldLvalue
    
    // ___ Type References __________________________________________________
    
    lazy val optTypeRef: PackratParser[out.OTR] = reqTypeRef | infTypeRef
    
    lazy val infTypeRef = positioned(
        success(()) ^^ { case () => out.InferredTypeRef() }
    )
    
    lazy val reqTypeRef = ":"~>typeRef
    
    lazy val typeRef: PackratParser[out.TR] = constrainedType | pathType | tupleType
    
    lazy val tupleType = positioned(
        "("~comma(typeRef)~")" ^^ { case _~tys~_ => out.TupleType(tys) }
    )
    
    lazy val pathType = positioned(
        path ^^ out.PathType
    )
    
    lazy val constrainedType = positioned(
        path~"["~comma(typeArg)~"]" ^^ { case p~"["~a~"]" => out.ConstrainedType(p, a) }
    )
    
    lazy val typeArg: PackratParser[out.TypeArg] = typeTypeArg | pathTypeArg
    
    lazy val typeTypeArg = positioned(
        varName~tcRel~typeRef ^^ { case x~o~t => out.TypeTypeArg(x, o, t) }
    )
    
    lazy val pathTypeArg = positioned(
        varName~wcRel~path ^^ { case x~o~p => out.PathTypeArg(x, o, p) }
    )
    
    // ___ Paths ____________________________________________________________
    //
    // A sequence like `a.b.c`.  We use a guard to prevent `a.b.c()` from
    // being parsed as the path `a.b.c` followed by `()`.  Instead it would
    // be parsed as `a.b` and then `.c()`.
    
    lazy val path = positioned(
        varName~rep("."~>memberName) ^^ {
            case v~fs => fs.foldLeft[out.PathNode](out.PathBase(v, ()))(out.PathDot(_, _, ()))
        }
    )
    
    lazy val notArg = not(guard("(" | "{" | "{{"))
    
    lazy val pathAsExpr = positioned(
        varName~notArg~rep("."~>memberName<~notArg) ^^ {
            case v~_~fs => fs.foldLeft[out.PathNode](out.PathBase(v, ()))(out.PathDot(_, _, ()))
        }
    )
    
    // ___ Expressions ______________________________________________________
    
    lazy val body = positioned(
        "{"~>stmts<~"}"                 ^^ out.Body
    )
    
    lazy val noBlkParam = positioned(
        success(())                             ^^ { case () => out.TupleParam(List()) }
    )
    
    lazy val blkBody: PackratParser[(out.OptionalParseTypeRef, out.Param[Unit], List[out.Stmt])] = (
        tupleBlkParam~reqTypeRef~"->"~stmts     ^^ { case (p~r~"->"~s) => (r, p, s) }
    |   blkParam~infTypeRef~"->"~stmts          ^^ { case (p~r~"->"~s) => (r, p, s) }
    |   noBlkParam~infTypeRef~stmts             ^^ { case (p~r~s) => (r, p, s) }
    )
    
    lazy val blk = positioned(
        "{"~>blkBody<~"}"                       ^^ { case (r, p, s) => out.Block(false, r, p, s, ()) }
    )
    
    lazy val asyncBlk = positioned(
        "{{"~>blkBody<~"}}"                     ^^ { case (r, p, s) => out.Block(true, r, p, s, ()) }
    )
    
    lazy val tuple = positioned(
        "("~>comma(expr)<~")"                   ^^ { e => out.Tuple(e) }
    )
    
    lazy val arg: PackratParser[out.Expr] = tuple | blk | asyncBlk
    
    lazy val callPart = ident~arg                           ^^ { case i~a => (i, a) }
    
    def outMethodCall(r: out.Rcvr, cps: List[(String, out.Expr)]) = {
        out.MethodCall(r, Name.Method(cps.map(_._1)), cps.map(_._2), ())
    }
    def args(cps: List[(String, out.Expr)]) = cps.map(_._2)
    
    lazy val newExpr = positioned(
        "new"~typeRef~tuple ^^ {
            case "new"~t~a => out.NewCtor(t, List(a), (), ())
        }
    )
    
    lazy val rcvr: PackratParser[out.Rcvr] = positioned(
        expr0
    |   "super"                             ^^ { case _ => out.Super(()) }
    )
    
    lazy val expr0: PackratParser[out.Expr] = positioned(
        rcvr~"."~rep1(callPart)             ^^ { case r~"."~cps => outMethodCall(r, cps) }
    |   impThis~rep1(callPart)              ^^ { case r~cps => outMethodCall(r, cps) }
    |   pathAsExpr
    |   expr0~"."~memberName                ^^ { case r~"."~f => out.Field(r, f, ()) }
    |   arg
    |   numericLit                          ^^ { l => out.Literal(Integer.valueOf(l), ()) }
    |   stringLit                           ^^ { l => out.Literal(l, ()) }
    |   "true"                              ^^ { _ => out.Literal(java.lang.Boolean.TRUE, ()) }
    |   "false"                             ^^ { _ => out.Literal(java.lang.Boolean.FALSE, ()) }
    |   "null"                              ^^ { _ => out.Null() }
    |   newExpr
    )
    
    lazy val impThis = positioned(
        success(()) ^^ { case () => out.ImpThis(()) }
    )
    
    lazy val operPart = oper~expr0 ^^ { case o~r => (o, r) }
    
    lazy val expr: PackratParser[out.Expr] = positioned(
        expr0~rep1(operPart) ^^ { case l~cps => outMethodCall(l, cps) }
    |   expr0 
    )
    
    lazy val stmt: PackratParser[out.Stmt] = positioned(
        "interval"~localName~body           ^^ { case _~l~b => out.InlineInterval(l, b, ()) }
    |   lvalue~"="~expr                     ^^ { case l~"="~e => out.Assign(List(l), List(e)) }
    |   "return"~expr                       ^^ { case _~e => out.MethodReturn(e) }
    |   "return"~impVoid                    ^^ { case _~e => out.MethodReturn(e) }
    |   expr
    )
    
    lazy val impVoid = (
        positioned(success(()) ^^ { case () => out.ImpVoid(()) })        
    )
    
    lazy val stmts: PackratParser[List[out.Stmt]] = (
        rep1sep(stmt, ";")<~opt(";")
    |   impVoid ^^ { s => List(s) }
    )
    
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

    def apply(global: Global, interFile: File) = {
        val log = global.debugServer.contextForPageTitled("Parse", interFile)
        val javaReader = Util.javaReaderFromFile(interFile)
        val parser = new Parse()
        
        val reader = new FileReader(interFile, PagedSeq.fromReader(javaReader), 0)
        val tokens = new parser.lexical.Scanner(reader)
        parser.phrase(parser.compUnit)(tokens) match {
            case n: parser.NoSuccess => {
                Error.ParseError(n.msg).report(global, n.next.pos)
                None
            }
            case parser.Success(compUnit, _) => {
                if(global.config.dumpParsedTrees) {
                    compUnit.print(PrettyPrinter.debug(log))
                }
                
                Some(compUnit)
            }
        }                
    }
      
}