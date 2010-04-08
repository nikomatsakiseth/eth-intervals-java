package inter.compiler

import java.io.File

import scala.collection.mutable.ListBuffer

import Ast.{Parse => in}
import Ast.{Resolve => out}
import Util._

/** `Resolve` resolves relative names into absolute ones, making
  * use of the imports as well as the source/classpaths. */
object Resolve {
    def apply(state: CompilationState, compUnit: in.CompUnit) = {
        
        // ___ Name Resolution __________________________________________________
        //
        // We consider the imports in reverse order.  Implicitly there are two
        // imports, "import java.lang.*" and "import current.package.*" at the
        // head of the file.
        
        val allImports = (
            in.ImportAll(Ast.AbsName(Name.QualRoot)) ::
            in.ImportAll(Ast.AbsName(Name.Qual("java.lang"))) ::
            in.ImportAll(Ast.AbsName(Name.Qual("inter.lang"))) ::
            in.ImportAll(compUnit.pkg) ::
            compUnit.imports
        ).reverse
        
        def resolveToQualName(rn: in.RelName): Name.Qual = rn match {
            case in.RelDot(ctx, nm) => resolveToQualName(ctx) / nm
            case in.RelBase(nm) => {
                val m = allImports.firstSome { 
                    case in.ImportOne(from, to) => {
                        if (to == rn) {
                            if(!state.loadedOrLoadable(from.qualName))
                                state.reporter.report(rn.pos, "cannot.find.class", from.toString)
                            Some(from.qualName)
                        } else None
                    }
                    case in.ImportAll(pkg) => {
                        val qualName = pkg.qualName / nm
                        if(state.loadedOrLoadable(qualName)) Some(qualName)
                        else None
                    }
                }
                m match {
                    case Some(qual) => qual
                    case None => {
                        state.reporter.report(rn.pos, "cannot.resolve", nm)
                        Name.Qual(nm)
                    }
                }
            }
        }
        
        def resolveName(rn: in.RelName) = withPosOf(rn, Ast.AbsName(resolveToQualName(rn)))

        // ___ Walk tree and resolve names ______________________________________
        
        def resolveClassDecl(cdecl: in.ClassDecl) = withPosOf(cdecl, out.ClassDecl(
            name = cdecl.name.toAbs(compUnit.pkg),
            annotations = cdecl.annotations.map(resolveAnnotation),
            superClasses = cdecl.superClasses.map(resolveName),
            pattern = resolveTuplePattern(cdecl.pattern),
            members = cdecl.members.map(resolveMember),
            sym = cdecl.sym
        ))
        
        def resolveAnnotation(ann: in.Annotation) = withPosOf(ann, out.Annotation(
            name = resolveName(ann.name)
        ))
        
        def resolvePattern(pattern: in.Pattern): out.Pattern = withPosOf(pattern, pattern match {
            case tuplePat: in.TuplePattern => resolveTuplePattern(tuplePat)
            case varPat: in.VarPattern => resolveVarPattern(varPat)
        })
        
        def resolveTuplePattern(tuplePat: in.TuplePattern) = withPosOf(tuplePat, out.TuplePattern(
            patterns = tuplePat.patterns.map(resolvePattern)
        ))
        
        def resolveVarPattern(varPat: in.VarPattern) = withPosOf(varPat, out.VarPattern(
            annotations = varPat.annotations.map(resolveAnnotation),
            tref = resolveTypeRef(varPat.tref),
            name = varPat.name,
            sym = varPat.sym            
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
            optBody = decl.optBody.map(resolveBody),
            sym = decl.sym            
        ))
        
        def resolveMethodDecl(decl: in.MethodDecl) = withPosOf(decl, out.MethodDecl(
            annotations = decl.annotations.map(resolveAnnotation),
            parts = decl.parts.map(resolveDeclPart),
            returnTref = resolveOptionalTypeRef(decl.returnTref),
            requirements = decl.requirements.map(resolveRequirement),
            optBody = decl.optBody.map(resolveBody),
            sym = decl.sym            
        ))
        
        def resolveDeclPart(decl: in.DeclPart) = withPosOf(decl, out.DeclPart(
            ident = decl.ident,
            pattern = resolveTuplePattern(decl.pattern)
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
            optBody = decl.optBody.map(resolveBody),
            sym = ()
        ))
        
        def resolveRelDecl(decl: in.RelDecl) = withPosOf(decl, out.RelDecl(
            annotations = decl.annotations.map(resolveAnnotation),
            left = resolvePath(decl.left),
            kind = decl.kind,
            right = resolvePath(decl.right)
        ))
        
        def resolvePath(path: in.Path): out.Path = withPosOf(path, path match {
            case in.Var(name, (), ()) => out.Var(name, (), ())
            case in.PathField(p, f, (), ()) => out.PathField(resolvePath(p), f, (), ())
        })
        
        def resolveOptionalTypeRef(otref: in.OptionalTypeRef): out.OptionalTypeRef = withPosOf(otref, otref match {
            case in.InferredTypeRef => out.InferredTypeRef
            case tref: in.TypeRef => resolveTypeRef(tref)
        })
        
        def resolveTypeRef(tref: in.TypeRef): out.TypeRef = withPosOf(tref, tref match {
            case in.PathType(path, tvar) => out.PathType(resolvePath(path), tvar)
            case in.ClassType(cn, targs) => out.ClassType(resolveName(cn), targs.map(resolveTypeArg))
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
            case in.Assign(lv, rv) => out.Assign(resolveLvalue(lv), resolveExpr(rv))
            case in.Labeled(name, blk) => out.Labeled(name, resolveInlineTmpl(blk))
        })
        
        def resolveLvalue(lvalue: in.Lvalue): out.Lvalue = withPosOf(lvalue, lvalue match {
            case in.TupleLvalue(lvalues) => out.TupleLvalue(lvalues.map(resolveLvalue))
            case in.VarLvalue(annotations, tref, name, ()) => out.VarLvalue(
                annotations = annotations.map(resolveAnnotation),
                tref = resolveOptionalTypeRef(tref),
                name = name,
                sym = ()
            )
        })
        
        def resolveExpr(expr: in.Expr): out.Expr = withPosOf(expr, expr match {
            case tuple: in.Tuple => resolveTuple(tuple)
            case tmpl: in.InlineTmpl => resolveInlineTmpl(tmpl)
            case in.AsyncTmpl(stmts, ()) => out.AsyncTmpl(resolveStmts(stmts), ())
            case in.Literal(obj, ()) => out.Literal(obj, ())
            case in.Var(name, (), ()) => out.Var(name, (), ())
            case in.Field(owner, name, (), ()) => out.Field(resolveExpr(owner), name, (), ())
            case in.MethodCall(rcvr, parts, (), ()) => out.MethodCall(resolveExpr(rcvr), parts.map(resolvePart), (), ())
            case in.New(tref, arg, ()) => out.New(resolveTypeRef(tref), resolveTuple(arg), ())
            case in.Null(()) => out.Null(())
            case in.ImpVoid => out.ImpVoid
            case in.ImpThis => out.ImpThis
        })
        
        def resolvePart(part: in.CallPart) = withPosOf(part, out.CallPart(
            ident = part.ident,
            arg = resolveExpr(part.arg)
        ))
        
        def resolveTuple(tuple: in.Tuple) = withPosOf(tuple, out.Tuple(
            exprs = tuple.exprs.map(resolveExpr),
            ty = ()
        ))
        
        def resolveInlineTmpl(tmpl: in.InlineTmpl) = withPosOf(tmpl, out.InlineTmpl(
            stmts = resolveStmts(tmpl.stmts),
            ty = ()
        ))
        
        def resolveBody(body: in.Body) = withPosOf(body, out.Body(
            stmts = resolveStmts(body.stmts)
        ))
        
        compUnit.classes.map(resolveClassDecl)
    }
}