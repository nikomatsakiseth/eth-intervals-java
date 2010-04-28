package harmonic.compiler

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Map

import Ast.{Parse => in}
import Ast.{Resolve => out}
import Util._

/** `Resolve` resolves relative names into absolute ones, making
  * use of the imports as well as the source/classpaths. */
case class Resolve(state: CompilationState, compUnit: in.CompUnit) {
    
    // ___ Class Name Resolution ____________________________________________
    //
    // When the user names a class, they can of course refer to classes by
    // a shortened name that is resolved against the imports defined in the
    // compilation unit.  Imports are considered in reverse order.  Certain
    // default imports are added to the head of the list.
    
    val allImports = (
        in.ImportAll(Ast.AbsName(Name.QualRoot)) ::
        in.ImportAll(Ast.AbsName(Name.Package("java.lang"))) ::
        in.ImportAll(Ast.AbsName(Name.Package("harmonic.lang"))) ::
        in.ImportAll(compUnit.pkg) ::
        compUnit.imports
    ).reverse
    
    def requireLoadedOrLoadable(node: Ast.Node, qname: Name.Qual) = {
        state.requireLoadedOrLoadable(node.pos, qname)
    }
    
    /** Resolves `Rn` to a list of possible expansions.
      * Any class names included in the list have been loaded. 
      * The expansions are ordered in list of preference. */
    def resolveRelName(relName: in.RelName): List[Name.Qual] = {
        def resolveAgainst(ctxName: Name.Qual, nm: String) = {
            val className = Name.Class(ctxName, nm)
            if(loadedOrLoadable(className)) 
                Some(className)
            else {
                ctxName match {
                    case pkgName: Name.Package => 
                        Some(Name.Subpackage(pkgName, nm))
                    case _ => 
                        None
                }                    
            }            
        }
        
        relName match {
            case in.RelDot(ctx, nm) => {
                val expansions = resolveRelName(ctx)
                expansions.flatMap(resolveAgainst(_, nm))
            }
            
            case Rb @ in.RelBase(nm) => {
                allImports.flatMap { 
                    case in.ImportOne(from, Rb) => Some(from)
                    case in.ImportOne(_, _) => None
                    case in.ImportAll(Ast.PackageName(pkg)) => resolveAgainst(pkg, nm)
                }
            }
        }
    }
    
    def resolveToClass(relName: in.RelName): Name.Class = {
        val expansions = resolveRelName(relName)
        val optResult = expansions.firstSome {
            case className: Name.Class => className
            case _: Name.Package => None
        }
        optResult.getOrElse {
            state.reporter.report(relName.pos, "cannot.resolve", relName.toString)
            Name.ObjectQual
        }
    }

    def resolveName(relName: in.RelName) = 
        withPosOf(relName, Ast.ClassName(resolveToClass(relName)))

    // ___ Header Resolution ________________________________________________
    //
    // Before we can resolve a class body, we must first ensure that its
    // superclasses (transitively) have been loaded.
    
    def resolveClassHeader(
        csym: Symbol.ClassFromSource,
        cdecl: in.ClassDecl
    ) {
        val superClassNames = cdecl.superClasses.map(resolveName)
        val allFieldNames = cdecl.
    }
    
    // ___ Symbol Tables ____________________________________________________
    //
    // The symbol tables we use during Resolve simply track what variables
    // are in scope.

    type SymTab = Map[String, Name.Var]

    // ___ Walk tree and resolve names ______________________________________
    
    def resolveClassDecl(cdecl: in.ClassDecl) = withPosOf(cdecl, out.ClassDecl(
        name = cdecl.name.toAbs(compUnit.pkg),
        annotations = cdecl.annotations.map(resolveAnnotation),
        superClasses = cdecl.superClasses.map(resolveName),
        pattern = resolveTupleParam(cdecl.pattern),
        members = cdecl.members.map(resolveMember),
        sym = cdecl.sym
    ))
    
    def resolveAnnotation(ann: in.Annotation) = withPosOf(ann, out.Annotation(
        name = resolveName(ann.name)
    ))
    
    def resolveParam(param: in.Param): out.Param = withPosOf(param, param match {
        case tupleParam: in.TupleParam => resolveTupleParam(tupleParam)
        case varParam: in.VarParam => resolveVarParam(varParam)
    })
    
    def resolveTupleParam(tupleParam: in.TupleParam) = withPosOf(tupleParam, out.TupleParam(
        params = tupleParam.params.map(resolveParam)
    ))
    
    def resolveVarParam(varParam: in.VarParam) = withPosOf(varParam, out.VarParam(
        annotations = varParam.annotations.map(resolveAnnotation),
        tref = resolveTypeRef(varParam.tref),
        name = varParam.name,
        sym = ()
    ))
    
    def resolveMember(mem: in.MemberDecl): out.MemberDecl = withPosOf(mem, mem match {
        case decl: in.ClassDecl => resolveClassDecl(decl)
        case decl: in.IntervalDecl => resolveIntervalDecl(decl)
        case decl: in.MethodDecl => resolveMethodDecl(decl)
        case decl: in.FieldDecl => resolveFieldDecl(decl)
        case decl: in.RelDecl => resolveRelDecl(decl)
    })
    
    def resolveIntervalDecl(decl: in.IntervalDecl) = withPosOf(decl, out.IntervalDecl(
        annotations = decl.annotations.map(resolveAnnotation),
        name = decl.name,
        optParent = decl.optParent.map(resolvePath),
        optBody = decl.optBody.map(resolveBody)
    ))
    
    def resolveMethodDecl(decl: in.MethodDecl) = withPosOf(decl, out.MethodDecl(
        annotations = decl.annotations.map(resolveAnnotation),
        receiverSym = (),
        parts = decl.parts.map(resolveDeclPart),
        returnTref = resolveOptionalTypeRef(decl.returnTref),
        returnTy = (),
        requirements = decl.requirements.map(resolveRequirement),
        optBody = decl.optBody.map(resolveBody)
    ))
    
    def resolveDeclPart(decl: in.DeclPart) = withPosOf(decl, out.DeclPart(
        ident = decl.ident,
        param = resolveTupleParam(decl.param)
    ))
    
    def resolveRequirement(requirement: in.PathRequirement) = withPosOf(requirement, out.PathRequirement(
        left = resolvePath(requirement.left),
        rel = requirement.rel,
        right = resolvePath(requirement.right)
    ))
    
    def resolveFieldDecl(decl: in.FieldDecl) = withPosOf(decl, out.FieldDecl(
        annotations = decl.annotations.map(resolveAnnotation),
        name = decl.name,
        tref = resolveOptionalTypeRef(decl.tref),
        ty = (),
        optBody = decl.optBody.map(resolveBody)
    ))
    
    def resolveRelDecl(decl: in.RelDecl) = withPosOf(decl, out.RelDecl(
        annotations = decl.annotations.map(resolveAnnotation),
        left = resolvePath(decl.left),
        kind = decl.kind,
        right = resolvePath(decl.right)
    ))
    
    def resolvePath(path: in.AstPath): out.AstPath = withPosOf(path, path match {
        case in.Var(name, ()) => out.Var(name, ())
        case in.PathField(p, f, (), ()) => out.PathField(resolvePath(p), f, (), ())
    })
    
    def resolveOptionalTypeRef(otref: in.OptionalTypeRef): out.OptionalTypeRef = withPosOf(otref, otref match {
        case in.InferredTypeRef() => withPosOf(otref, out.InferredTypeRef())
        case tref: in.TypeRef => resolveTypeRef(tref)
    })
    
    def resolveTypeRef(tref: in.TypeRef): out.TypeRef = withPosOf(tref, tref match {
        case in.NullType() => out.NullType()
        case in.VarType(path, tvar) => out.VarType(resolvePath(path), tvar)
        case in.ClassType(cn, targs) => out.ClassType(resolveName(cn), targs.map(resolveTypeArg))
        case in.TupleType(trefs) => out.TupleType(trefs.map(resolveTypeRef))
    })

    def resolveTypeArg(targ: in.TypeArg): out.TypeArg = withPosOf(targ, targ match {
        case ttarg: in.TypeTypeArg => resolveTypeTypeArg(ttarg)
        case ptarg: in.PathTypeArg => resolvePathTypeArg(ptarg)
    })
    
    def resolveTypeTypeArg(targ: in.TypeTypeArg): out.TypeTypeArg = withPosOf(targ, out.TypeTypeArg(
        name = targ.name, rel = targ.rel, typeRef = resolveTypeRef(targ.typeRef)
    ))
    
    def resolvePathTypeArg(targ: in.PathTypeArg): out.PathTypeArg = withPosOf(targ, out.PathTypeArg(
        name = targ.name, rel = targ.rel, path = resolvePath(targ.path)
    ))
    
    def resolveStmts(stmts: List[in.Stmt]) = stmts.map(resolveStmt)
    
    def resolveStmt(stmt: in.Stmt): out.Stmt = withPosOf(stmt, stmt match {
        case expr: in.Expr => resolveExpr(expr)
        case in.Assign(lv, rv) => out.Assign(resolveLocal(lv), resolveExpr(rv))
        case in.Labeled(name, body) => out.Labeled(name, resolveBody(body))
    })
    
    def resolveTupleLocal(tupLocal: in.TupleLocal): out.TupleLocal = withPosOf(tupLocal, {
        out.TupleLocal(tupLocal.locals.map(resolveLocal))
    })
    
    def resolveLocal(local: in.Local): out.Local = withPosOf(local, local match {
        case tupLocal: in.TupleLocal => resolveTupleLocal(tupLocal)
        case in.VarLocal(annotations, tref, name, ()) => out.VarLocal(
            annotations = annotations.map(resolveAnnotation),
            tref = resolveOptionalTypeRef(tref),
            name = name,
            sym = ()
        )
    })
    
    def resolveNewCtor(expr: in.NewCtor): out.NewCtor = withPosOf(expr, {
        out.NewCtor(
            tref = resolveTypeRef(expr.tref),
            arg = resolveExpr(expr.arg),
            msym = (),
            ty = ()
        )
    })
    
    def resolveNewAnon(expr: in.NewAnon): out.NewAnon = withPosOf(expr, {
        out.NewAnon(
            tref = resolveTypeRef(expr.tref),
            arg = resolveExpr(expr.arg),
            members = expr.members.map(resolveMember),
            csym = (),
            msym = (),
            ty = ()
        )
    })
    
    def resolveRcvr(rcvr: in.Rcvr): out.Rcvr = withPosOf(rcvr, rcvr match {
        case in.Super(()) => out.Super(())
        case e: in.Expr => resolveExpr(e)
    })
    
    def resolveExpr(expr: in.Expr): out.Expr = withPosOf(expr, expr match {
        case tuple: in.Tuple => resolveTuple(tuple)
        case tmpl: in.Block => resolveBlock(tmpl)
        case in.Cast(v, t, ()) => out.Cast(resolveExpr(v), resolveTypeRef(t), ())
        case e: in.Literal => resolveLiteral(e)
        case in.Var(name, ()) => out.Var(name, ())
        case in.Field(owner, name, (), ()) => out.Field(resolveExpr(owner), name, (), ())
        case in.MethodCall(rcvr, parts, ()) => out.MethodCall(resolveRcvr(rcvr), parts.map(resolvePart), ())
        case e: in.NewCtor => resolveNewCtor(e)
        case e: in.NewAnon => resolveNewAnon(e)
        case in.Null(()) => out.Null(())
        case in.ImpVoid(()) => out.ImpVoid(())
        case in.ImpThis(()) => out.ImpThis(())
    })
    
    def resolveLiteral(expr: in.Literal) = {
        requireLoadedOrLoadable(expr, Name.Qual(expr.obj.getClass))
        out.Literal(expr.obj, ())            
    }
    
    def resolvePart(part: in.CallPart) = withPosOf(part, out.CallPart(
        ident = part.ident,
        arg = resolveExpr(part.arg)
    ))
    
    def resolveTuple(tuple: in.Tuple) = withPosOf(tuple, out.Tuple(
        exprs = tuple.exprs.map(resolveExpr)
    ))
    
    def resolveBlock(tmpl: in.Block) = withPosOf(tmpl, out.Block(
        async = tmpl.async,
        returnTref = resolveOptionalTypeRef(tmpl.returnTref),
        returnTy = (),
        param = resolveTupleLocal(tmpl.param),
        stmts = resolveStmts(tmpl.stmts),
        ty = ()
    ))
    
    def resolveBody(body: in.Body) = withPosOf(body, out.Body(
        stmts = resolveStmts(body.stmts)
    ))
    
    val resolvedClassDecls = compUnit.classes.map(resolveClassDecl)
    
    if(state.config.dumpResolvedTrees) {
        resolvedClassDecls.foreach { cdecl =>
            cdecl.println(PrettyPrinter.stdout)
        }
    }
    
    resolvedClassDecls
}