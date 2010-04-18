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
        
        def requireLoadedOrLoadable(node: Ast.Node, qname: Name.Qual) {
            if(!state.loadedOrLoadable(qname))
                state.reporter.report(node.pos, "cannot.find.class", qname.toString)            
        }
        
        def resolveToQualName(rn: in.RelName): Name.Qual = rn match {
            case in.RelDot(ctx, nm) => resolveToQualName(ctx) / nm
            case in.RelBase(nm) => {
                val m = allImports.firstSome { 
                    case in.ImportOne(from, to) => {
                        if (to == rn) {
                            requireLoadedOrLoadable(from, from.qualName)
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
        
        def resolveLocal(local: in.Local): out.Local = withPosOf(local, local match {
            case in.TupleLocal(locals) => out.TupleLocal(locals.map(resolveLocal))
            case in.VarLocal(annotations, tref, name, ()) => out.VarLocal(
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
            case e: in.Literal => resolveLiteral(e)
            case in.Var(name, ()) => out.Var(name, ())
            case in.Field(owner, name, (), ()) => out.Field(resolveExpr(owner), name, (), ())
            case in.MethodCall(rcvr, parts, ()) => out.MethodCall(resolveExpr(rcvr), parts.map(resolvePart), ())
            case in.NewJava(tref, arg, ()) => out.NewJava(resolveTypeRef(tref), resolveTuple(arg), ())
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
        
        def resolveInlineTmpl(tmpl: in.InlineTmpl) = withPosOf(tmpl, out.InlineTmpl(
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
}