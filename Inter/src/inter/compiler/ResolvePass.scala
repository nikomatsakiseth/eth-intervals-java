package inter.compiler

import java.io.File

import scala.collection.mutable.ListBuffer

import Hl.{P => in}
import Hl.{RN => out}
import Util._

object ResolvePass {
    def apply(state: CompilationState, compUnit: in.CompUnit) = {
        
        // ___ Name Resolution __________________________________________________
        //
        // We consider the imports in reverse order.  Implicitly there are two
        // imports, "import java.lang.*" and "import current.package.*" at the
        // head of the file.
        
        val allImports = (
            in.ImportAll(Hl.AbsRoot) ::
            in.ImportAll(Hl.abs("java.lang")) ::
            in.ImportAll(Hl.abs("inter.lang")) ::
            in.ImportAll(compUnit.pkg) ::
            compUnit.imports
        ).reverse
        
        val toBeParsed = ListBuffer(state.toBeParsed: _*)
        val toBeLoaded = ListBuffer(state.toBeLoaded: _*)
        val toBeReflected = ListBuffer[Class[_]](state.toBeReflected: _*)
        
        def locate(absName: Hl.AbsName) = {
            val qualName = absName.qualName
            val sourceFiles = state.config.sourceFiles(qualName)
            val classFiles = state.config.classFiles(qualName)
            val reflClasses = state.config.reflectiveClasses(qualName)
            (sourceFiles, classFiles, reflClasses) match {
                case (List(), List(), None) => false
                case (List(), List(), Some(reflClass)) => {
                    toBeReflected += reflClass
                    true
                }
                case (sourceFile :: _, List(), _) => {
                    toBeParsed += Pair(sourceFile, Some(qualName))
                    true
                }
                case (List(), classFile :: _, _) => {
                    toBeLoaded += Pair(classFile, Some(qualName))
                    true
                }
                case (sourceFile :: _, classFile :: _, _) => {
                    if (sourceFile.lastModified > classFile.lastModified) {
                        toBeParsed += Pair(sourceFile, Some(qualName))
                        true
                    } else {
                        toBeLoaded += Pair(classFile, Some(qualName))
                        true
                    }
                }
            }            
        }
        
        def resolveName(rn: in.RelName): Hl.AbsName = rn match {
            case in.RelDot(ctx, nm) => resolveName(ctx) / nm
            case in.RelBase(nm) => {
                val m = allImports.firstSome { 
                    case in.ImportOne(from, to) => {
                        if (to == rn) {
                            if(!state.parsedClasses.isDefinedAt(from.qualName) && !locate(from))
                                state.reporter.report(rn.pos, "cannot.find.class", from.toString)
                            Some(from)
                        } else None
                    }
                    case in.ImportAll(pkg) => {
                        val absName = pkg / nm
                        if(state.parsedClasses.isDefinedAt(absName.qualName) || locate(absName))
                            Some(absName)
                        else None
                    }
                }
                m match {
                    case Some(abs) => abs
                    case None => {
                        state.reporter.report(rn.pos, "cannot.resolve", nm)
                        Hl.AbsRoot
                    }
                }
            }
        }

        // ___ Walk tree and resolve names ______________________________________
        
        def resolveClassDecl(cdecl: in.ClassDecl) = out.ClassDecl(
            name = cdecl.name,
            annotations = cdecl.annotations.map(resolveAnnotation),
            superClasses = cdecl.superClasses.map(resolveName),
            pattern = resolveTuplePattern(cdecl.pattern),
            members = cdecl.members.map(resolveMember)
        )
        
        def resolveAnnotation(ann: in.Annotation) = out.Annotation(
            name = resolveName(ann.name)
        )
        
        def resolvePattern(pattern: in.Pattern): out.Pattern = pattern match {
            case tuplePat: in.TuplePattern => resolveTuplePattern(tuplePat)
            case varPat: in.VarPattern => resolveVarPattern(varPat)
        }
        
        def resolveTuplePattern(tuplePat: in.TuplePattern) = out.TuplePattern(
            patterns = tuplePat.patterns.map(resolvePattern)
        )
        
        def resolveVarPattern(varPat: in.VarPattern) = out.VarPattern(
            annotations = varPat.annotations.map(resolveAnnotation),
            tref = resolveTypeRef(varPat.tref),
            name = varPat.name
        )
        
        def resolveMember(mem: in.MemberDecl): out.MemberDecl = mem match {
            case decl: in.ClassDecl => resolveClassDecl(decl)
            case decl: in.IntervalDecl => resolveIntervalDecl(decl)
            case decl: in.MethodDecl => resolveMethodDecl(decl)
            case decl: in.FieldDecl => resolveFieldDecl(decl)
            case decl: in.RelDecl => resolveRelDecl(decl)
        }
        
        def resolveIntervalDecl(decl: in.IntervalDecl) = out.IntervalDecl(
            annotations = decl.annotations.map(resolveAnnotation),
            name = decl.name,
            optParent = decl.optParent.map(resolvePath),
            optBody = decl.optBody.map(resolveInlineTmpl)
        )
        
        def resolveMethodDecl(decl: in.MethodDecl) = out.MethodDecl(
            annotations = decl.annotations.map(resolveAnnotation),
            parts = decl.parts.map(resolveDeclPart),
            retTref = resolveOptionalTypeRef(decl.retTref),
            requirements = decl.requirements.map(resolveRequirement),
            optBody = decl.optBody.map(resolveInlineTmpl)
        )
        
        def resolveDeclPart(decl: in.DeclPart) = out.DeclPart(
            ident = decl.ident,
            pattern = resolveTuplePattern(decl.pattern)
        )
        
        def resolveRequirement(requirement: in.Requirement) = out.Requirement(
            left = resolvePath(requirement.left),
            rhs = resolveReqRhs(requirement.rhs)
        )
        
        def resolveReqRhs(reqRhs: in.ReqRhs) = out.ReqRhs(
            kind = reqRhs.kind,
            right = resolvePath(reqRhs.right)
        )
        
        def resolveFieldDecl(decl: in.FieldDecl) = out.FieldDecl(
            annotations = decl.annotations.map(resolveAnnotation),
            name = decl.name,
            tref = resolveOptionalTypeRef(decl.tref),
            value = decl.value.map(resolveExpr)
        )
        
        def resolveRelDecl(decl: in.RelDecl) = out.RelDecl(
            annotations = decl.annotations.map(resolveAnnotation),
            left = resolvePath(decl.left),
            kind = decl.kind,
            right = resolvePath(decl.right)
        )
        
        def resolvePath(path: in.Path): out.Path = path match {
            case in.Var(name) => out.Var(name)
            case in.PathField(p, f) => out.PathField(resolvePath(p), f)
        }
        
        def resolveOptionalTypeRef(otref: in.OptionalTypeRef): out.OptionalTypeRef = otref match {
            case in.InferredTypeRef => out.InferredTypeRef
            case tref: in.TypeRef => resolveTypeRef(tref)
        }
        
        def resolveTypeRef(tref: in.TypeRef): out.TypeRef = tref match {
            case in.PathType(path, tvar) => out.PathType(resolvePath(path), tvar)
            case in.ClassType(cn, targs) => out.ClassType(resolveName(cn), targs.map(resolveTypeArg))
        }
        
        def resolveTypeArg(targ: in.TypeArg): out.TypeArg = out.TypeArg(
            fieldName = targ.fieldName,
            reqRhs = resolveReqRhs(targ.reqRhs)
        )
        
        def resolveStmt(stmt: in.Stmt): out.Stmt = stmt match {
            case expr: in.Expr => resolveExpr(expr)
            case in.Labeled(name, blk) => out.Labeled(name, resolveInlineTmpl(blk))
        }
        
        def resolveLvalue(lvalue: in.Lvalue): out.Lvalue = lvalue match {
            case in.TupleLvalue(lvalues) => out.TupleLvalue(lvalues.map(resolveLvalue))
            case in.VarLvalue(annotations, tref, name) => out.VarLvalue(
                annotations = annotations.map(resolveAnnotation),
                tref = resolveOptionalTypeRef(tref),
                name = name
            )
        }
        
        def resolveExpr(expr: in.Expr): out.Expr = expr match {
            case tuple: in.Tuple => resolveTuple(tuple)
            case tmpl: in.InlineTmpl => resolveInlineTmpl(tmpl)
            case in.AsyncTmpl(stmts) => out.AsyncTmpl(stmts.map(resolveStmt))
            case in.Literal(obj) => out.Literal(obj)
            case in.Assign(lv, rv) => out.Assign(resolveLvalue(lv), resolveExpr(rv))
            case in.Var(name) => out.Var(name)
            case in.Field(owner, name) => out.Field(resolveExpr(owner), name)
            case in.MethodCall(rcvr, parts) => out.MethodCall(resolveExpr(rcvr), parts.map(resolvePart))
            case in.New(tref, arg) => out.New(resolveTypeRef(tref), resolveTuple(arg))
            case in.Null() => out.Null()
            case in.ImpVoid => out.ImpVoid
            case in.ImpThis => out.ImpThis
        }
        
        def resolvePart(part: in.CallPart) = out.CallPart(
            ident = part.ident,
            arg = resolveExpr(part.arg)
        )
        
        def resolveTuple(tuple: in.Tuple) = out.Tuple(
            exprs = tuple.exprs.map(resolveExpr)
        )
        
        def resolveInlineTmpl(tmpl: in.InlineTmpl) = out.InlineTmpl(
            stmts = tmpl.stmts.map(resolveStmt)
        )
        
        val cdecls = compUnit.classes.map(resolveClassDecl)
        
        state.copy(
            toBeParsed = toBeParsed.toList,
            toBeLoaded = toBeLoaded.toList,
            toBeReflected = toBeReflected.toList,
            toBeTyped = cdecls ++ state.toBeTyped
        )
    }
}