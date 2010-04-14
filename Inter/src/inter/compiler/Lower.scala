package inter.compiler

import scala.collection.immutable.Map
import scala.collection.mutable.ListBuffer

import Ast.{Resolve => in}
import Ast.{Lower => out}
import Util._

/** Lowers the IR to what is expected by the type check.  This has two
  * main functions:
  * - Fills in any inferred types.  (Note that we don't perform a full type check!)
  * - Removes nested expressions into intermediate variables. */
case class Lower(state: CompilationState) {
    
    val emptyEnv = Env.empty(state)
    
    // ___ Translate from Ast to Symbol and back again ______________________
    
    def methodId(csym: Symbol.ClassFromInterFile, mdecl: in.MethodDecl) = {
        val parameterPatterns = mdecl.parts.map(p => symbolPattern(p.pattern))
        Symbol.MethodId(csym.name, mdecl.name, parameterPatterns)
    }

    def patternType(pattern: in.Pattern): Type.Ref = pattern match {
        case in.TuplePattern(patterns) => Type.Tuple(patterns.map(patternType))
        case in.VarPattern(_, tref, _, _) => symbolType(tref)
    }
    
    def symbolPattern(pattern: in.Pattern): Symbol.Pattern = pattern match {
        case in.TuplePattern(patterns) => Symbol.TuplePattern(patterns.map(symbolPattern))
        case in.VarPattern(_, tref, name, _) => Symbol.VarPattern(name.name, symbolType(tref))
    }
    
    def symbolType(tref: in.TypeRef): Type.Ref = tref match {
        case in.VarType(path, tvar) => Type.Var(namePath(path), tvar.name)
        case in.ClassType(name, targs) => Type.Class(name.qualName, targs.map(symbolTypeArg))
        case in.TupleType(types) => Type.Tuple(types.map(symbolType))
        case in.NullType() => Type.Null
    }
    
    def symbolTypeArg(targ: in.TypeArg): Type.Arg = targ match {
        case in.TypeTypeArg(name, rel, tref) => Type.TypeArg(name.name, rel, symbolType(tref))
        case in.PathTypeArg(name, rel, path) => Type.PathArg(name.name, rel, namePath(path))
    }
    
    def namePath(path: in.AstPath): Path.Ref = path match {
        case in.PathField(base, f, (), ()) => Path.Field(namePath(base), f.name)
        case in.Var(name, (), ()) => Path.Base(name.name)
    }
    
    def astVarName(from: Ast.Node, name: Name.Var) = withPosOf(from,
        Ast.VarName(name.text)
    )
    
    def astPathVar(env: Env)(from: Ast.Node, name: Name.Var) = withPosOf(from, {
        val sym = env.lookupLocalOrError(name, None)
        out.Var(astVarName(from, name), sym, sym.ty)
    })
    
    def astPathField(env: Env)(from: Ast.Node, base: Path.Ref, name: Name.Var) = {
        withPosOf(from, {
            val astOwner = astPath(env)(from, base)
            val sym = env.lookupFieldOrError(astOwner.ty, name, None)
            out.PathField(astOwner, astVarName(from, name), sym, sym.ty)
        })
    }
    
    def astPath(env: Env)(from: Ast.Node, path: Path.Ref): out.AstPath = path match {
        case Path.Base(name) => astPathVar(env)(from, name)
        case Path.Field(base, name) => astPathField(env)(from, base, name)
    }
    
    def astType(env: Env)(from: Ast.Node, ty: Type.Ref): out.TypeRef = {
        withPosOf(from, ty match {
            case Type.Tuple(tys) => 
                out.TupleType(tys.map(astType(env)(from, _)))
            case Type.Var(path, tvar) => 
                out.VarType(
                    astPath(env)(from, path),
                    astVarName(from, tvar)
                )
            case Type.Class(name, targs) => 
                out.ClassType(
                    withPosOf(from, Ast.AbsName(name)),
                    targs.map(astTypeArg(env)(from, _))
                )
            case Type.Null =>
                withPosOf(from, out.NullType())
        })        
    }
    
    def astTypeArg(env: Env)(from: Ast.Node, targ: Type.Arg) = {
        withPosOf(from, targ match {
            case Type.PathArg(name, rel, path) =>
                out.PathTypeArg(
                    astVarName(from, name),
                    rel,
                    astPath(env)(from, path)
                )

            case Type.TypeArg(name, rel, ty) =>
                out.TypeTypeArg(
                    astVarName(from, name),
                    rel,
                    astType(env)(from, ty)
                )
        })        
    }
    
    // ___ Subclassing ______________________________________________________
    
    // ___ Method Symbol Creation ___________________________________________
    
    def symbolsForMethodsNamed(
        sym: Symbol.ClassFromInterFile, 
        mthdName: Name.Method
    ) = {
        sym.methodSymbols.get(mthdName) match {
            case None => createSymbolsForMethodsNamed(sym, mthdName)
            case Some(res) => res
        }
    }
    
    def createSymbolsForMethodsNamed(
        csym: Symbol.ClassFromInterFile, 
        mthdName: Name.Method
    ) = {
        def forMethodDecl(mdecl: in.MethodDecl) = mdecl match {
            case in.MethodDecl(_, _, in.InferredTypeRef(), _, _, _) => {
                val memberId = methodId(csym, mdecl)

                if(state.inferStack(memberId)) {
                    // Cyclic inference: illegal.
                    if(!state.inferReported.contains(memberId)) {
                        state.inferReported += memberId
                        state.reporter.report(
                            mdecl.returnTref.pos,
                            "explicit.type.required.due.to.cycle",
                            mthdName.toString
                        )
                    }
                    Symbol.errorMethod(mthdName)
                } else {
                    val outMdecl = lowerMethodDecl(csym, mdecl)
                    new Symbol.Method(
                        name = mthdName,
                        returnTy = outMdecl.returnTy,
                        receiver = Symbol.VarPattern(Name.ThisVar, Type.Class(csym.name, List())),
                        parameterPatterns = memberId.parameterPatterns
                    )
                }
            }
            
            case in.MethodDecl(_, parts, returnTy: in.TypeRef, _, _, _) => {
                new Symbol.Method(
                    name = mthdName,
                    returnTy = symbolType(returnTy),
                    receiver = Symbol.VarPattern(Name.ThisVar, Type.Class(csym.name, List())),
                    parameterPatterns = parts.map(p => symbolPattern(p.pattern))
                )
            }
        }
        
        val mdecls = csym.resolvedSource.members.flatMap(_.asMethodNamed(mthdName))
        val msyms = mdecls.map(forMethodDecl)
        csym.methodSymbols(mthdName) = msyms
        msyms
    }
    
    def ThisEnv(csym: Symbol.ClassFromInterFile) = {
        emptyEnv + new Symbol.Var(Name.ThisVar, Type.Class(csym.name, List()))
    }
    
    def ThisScope(csym: Symbol.ClassFromInterFile) = {
        InScope(ThisEnv(csym))
    }
    
    def lowerClassDecl(
        cdecl: Ast.Resolve.ClassDecl
    ): out.ClassDecl = {
        val csym = state.symtab.classes(cdecl.name.qualName).asInstanceOf[Symbol.ClassFromInterFile]
        withPosOf(cdecl, out.ClassDecl(
            name = cdecl.name,
            annotations = cdecl.annotations.map(ThisScope(csym).lowerAnnotation),
            superClasses = cdecl.superClasses,
            pattern = ThisScope(csym).lowerTuplePattern(cdecl.pattern),
            members = cdecl.members.map(lowerMemberDecl(csym, _)),
            sym = csym
        ))
    }
    
    def lowerMemberDecl(
        csym: Symbol.ClassFromInterFile, 
        mem: in.MemberDecl
    ): out.MemberDecl = withPosOf(mem, mem match {
        case decl: in.ClassDecl => lowerClassDecl(decl)
        case decl: in.IntervalDecl => lowerIntervalDecl(csym, decl)
        case decl: in.MethodDecl => lowerMethodDecl(csym, decl)
        case decl: in.FieldDecl => lowerFieldDecl(csym, decl)
        case decl: in.RelDecl => lowerRelDecl(csym, decl)
    })
    
    def lowerRelDecl(
        csym: Symbol.ClassFromInterFile, 
        decl: in.RelDecl        
    ): out.RelDecl = withPosOf(decl, out.RelDecl(
        annotations = decl.annotations.map(ThisScope(csym).lowerAnnotation),
        left = ThisScope(csym).lowerPath(decl.left),
        kind = decl.kind,
        right = ThisScope(csym).lowerPath(decl.right)
    ))
    
    def lowerIntervalDecl(
        csym: Symbol.ClassFromInterFile, 
        decl: in.IntervalDecl
    ): out.IntervalDecl = withPosOf(decl, out.IntervalDecl(
        annotations = decl.annotations.map(ThisScope(csym).lowerAnnotation),
        name = decl.name,
        optParent = decl.optParent.map(ThisScope(csym).lowerPath),
        optBody = decl.optBody.map(lowerBody(ThisEnv(csym), _))
    ))
    
    def lowerMethodDecl(
        csym: Symbol.ClassFromInterFile, 
        mdecl: in.MethodDecl
    ): out.MethodDecl = {
        val memberId = methodId(csym, mdecl)
        csym.loweredMethods.get(memberId).getOrElse {
            val cdecl = csym.resolvedSource
            val clsName = cdecl.name.qualName
            assert(!state.inferStack(memberId))
            state.inferStack += memberId

            val receiver = new Symbol.Var(Name.ThisVar, Type.Class(clsName, List()))
            val parameterPatterns = mdecl.parts.map(p => symbolPattern(p.pattern))
            val parameterSymbols = parameterPatterns.flatMap(Symbol.createVarSymbols)
            val env = emptyEnv ++ (receiver :: parameterSymbols)
            val optBody = mdecl.optBody.map(lowerBody(env, _))

            val (returnTref, returnTy) = (mdecl.returnTref, optBody) match {
                case (in.InferredTypeRef(), None) => {
                    state.reporter.report(
                        mdecl.returnTref.pos, "explicit.return.type.reqd.if.abstract", mdecl.name.toString
                    )
                    (astType(env)(mdecl.returnTref, Type.Null), Type.Null)
                }

                case (in.InferredTypeRef(), Some(out.Body(stmts))) => {
                    val ty = stmts.last.ty
                    (astType(env)(stmts.last, ty), ty)
                }

                case (tref: in.TypeRef, _) => {
                    (InScope(env).lowerTypeRef(tref), symbolType(tref))
                }

            }

            val outMdecl = out.MethodDecl(
                annotations = mdecl.annotations.map(InScope(env).lowerAnnotation),
                parts = mdecl.parts.map(InScope(env).lowerDeclPart),
                returnTref = returnTref,
                returnTy = returnTy,
                requirements = mdecl.requirements.map(InScope(env).lowerRequirement),
                optBody = optBody
            )
            
            csym.loweredMethods(memberId) = outMdecl
            state.inferStack -= memberId
            outMdecl
        }
    }
    
    def lowerFieldDecl(
        csym: Symbol.ClassFromInterFile, 
        decl: in.FieldDecl
    ): out.FieldDecl = withPosOf(decl, {
        val optBody = decl.optBody.map(lowerBody(ThisEnv(csym), _))
        val env = ThisEnv(csym)
        val (tref, ty) = (decl.tref, optBody) match {
            case (tref: in.TypeRef, _) => // Explicit type.
                (InScope(env).lowerTypeRef(tref), symbolType(tref))
                
            case (in.InferredTypeRef(), Some(out.Body(stmts))) => { // Implicit type.
                val ty = stmts.last.ty
                (astType(env)(stmts.last, ty), ty)
            }
            
            case (in.InferredTypeRef(), None) => {
                state.reporter.report(
                    decl.tref.pos, "explicit.type.reqd.if.abstract", decl.name.toString
                )
                (astType(env)(decl.tref, Type.Null), Type.Null)                
            }
        }
        out.FieldDecl(
            annotations = decl.annotations.map(InScope(env).lowerAnnotation),
            name = decl.name,
            tref = tref,
            ty = ty,
            optBody = optBody
        )        
    })
    
    // ___ Lowering Types ___________________________________________________
    
    object InScope {
        def apply(env: Env) = new InScope(env)
    }
    
    class InScope(env: Env) {
        def lowerDeclPart(part: in.DeclPart): out.DeclPart = withPosOf(part, out.DeclPart(
            ident = part.ident,
            pattern = lowerTuplePattern(part.pattern)
        ))

        def lowerTuplePattern(pattern: in.TuplePattern) = withPosOf(pattern, out.TuplePattern(
            patterns = pattern.patterns.map(lowerPattern)
        ))
        
        def lowerVarPattern(pattern: in.VarPattern) = withPosOf(pattern, out.VarPattern(
            annotations = pattern.annotations.map(lowerAnnotation),
            tref = lowerTypeRef(pattern.tref),
            name = pattern.name,
            sym = env.lookupLocal(pattern.name.name).get // should be a variable already created
        ))
        
        def lowerPattern(pattern: in.Pattern): out.Pattern = withPosOf(pattern, pattern match {
            case p: in.TuplePattern => lowerTuplePattern(p)
            case p: in.VarPattern => lowerVarPattern(p)
        })
        
        def lowerRequirement(req: in.PathRequirement) = withPosOf(req, out.PathRequirement(
            left = lowerPath(req.left),
            rel = req.rel,
            right = lowerPath(req.right)
        ))

        def lowerVar(optExpTy: Option[Type.Ref])(v: in.Var) = withPosOf(v, {
            val sym = env.lookupLocal(v.name.name).getOrElse {
                state.reporter.report(v.pos, "no.such.var", v.name.toString)
                Symbol.errorVar(v.name.name, optExpTy)
            }
            out.Var(v.name, sym, sym.ty)
        })
        
        def lowerPathField(optExpTy: Option[Type.Ref])(path: in.PathField) = withPosOf(path, {
            // Note: very similar code to lowerField() below
            val owner = lowerPath(path.owner)
            val optSym = env.lookupField(owner.ty, path.name.name)
            val subst = Subst(Path.This -> Path.fromLoweredAst(owner))
            val sym = optSym.getOrElse {
                state.reporter.report(path.pos, "no.such.field", owner.ty.toString, path.name.toString)
                Symbol.errorVar(path.name.name, optExpTy)
            }
            out.PathField(owner, path.name, sym, subst.ty(sym.ty))
        })
    
        def lowerPath(path: in.AstPath): out.AstPath = withPosOf(path, path match {
            case p: in.Var => lowerVar(None)(p)
            case p: in.PathField => lowerPathField(None)(p)
        })
    
        def lowerOptionalTypeRef(otref: in.OptionalTypeRef): out.OptionalTypeRef = otref match {
            case in.InferredTypeRef() => withPosOf(otref, out.InferredTypeRef())
            case tref: in.TypeRef => lowerTypeRef(tref)
        }
    
        def lowerTypeRef(tref: in.TypeRef): out.TypeRef = tref match {
            case in.VarType(path, tvar) => out.VarType(lowerPath(path), tvar)
            case in.ClassType(cname, targs) => out.ClassType(cname, targs.map(lowerTypeArg))
        }

        def lowerTypeArg(targ: in.TypeArg): out.TypeArg = targ match {
            case ttarg: in.TypeTypeArg => lowerTypeTypeArg(ttarg)
            case ptarg: in.PathTypeArg => lowerPathTypeArg(ptarg)
        }
    
        def lowerTypeTypeArg(targ: in.TypeTypeArg): out.TypeTypeArg = out.TypeTypeArg(
            name = targ.name, rel = targ.rel, typeRef = lowerTypeRef(targ.typeRef)
        )
    
        def lowerPathTypeArg(targ: in.PathTypeArg): out.PathTypeArg = out.PathTypeArg(
            name = targ.name, rel = targ.rel, path = lowerPath(targ.path)
        )
        
        def lowerAnnotation(ann: in.Annotation) = withPosOf(ann,
            out.Annotation(name = ann.name)
        )
    }
    
    // ___ Lowering Statements ______________________________________________
    
    def tmpVarName(fromExpr: in.Expr) = {
        "(%s@%s)".format(fromExpr.toString, fromExpr.pos.toString)
    }
    
    def lowerBody(env: Env, body: in.Body): out.Body = {
        withPosOf(body, out.Body(lowerStmts(env, body.stmts)))
    }
    
    def lowerStmts(env0: Env, stmts: List[in.Stmt]): List[out.Stmt] = {
        var env = env0
        val result = new ListBuffer[out.Stmt]()
        
        stmts.foreach { stmt =>
            env = InScopeStmt(env, result).appendLoweredStmt(stmt)
        }
        
        result.toList
    }
    
    object InScopeStmt {
        def apply(env: Env, stmts: ListBuffer[out.Stmt]) = new InScopeStmt(env, stmts)
    }
    
    class InScopeStmt(env: Env, stmts: ListBuffer[out.Stmt]) extends InScope(env) {
        def extractSymbols(pattern: out.Pattern): List[Symbol.Var] = pattern match {
            case out.TuplePattern(patterns) => patterns.flatMap(extractSymbols)
            case out.VarPattern(_, _, _, sym) => List(sym)
        }
        
        def appendLoweredStmt(stmt: in.Stmt): Env = {
            stmt match {
                case in.Assign(lvalue, rvalue) => {
                    val optExpTy = optTypeFromLvalue(lvalue)
                    val outRvalue = lowerExpr(optExpTy)(rvalue)
                    val outPattern = lowerLvalue(outRvalue.ty, lvalue)
                    stmts += withPosOf(stmt, out.Assign(outPattern, outRvalue))
                    env ++ extractSymbols(outPattern)
                }
                
                case in.Labeled(name, body) => {
                    stmts += withPosOf(stmt, out.Labeled(name, lowerBody(env, body)))
                    env
                }
                
                case expr: in.Expr => {
                    stmts += lowerExpr(None)(expr)
                    env
                }
            }
        }
        
        /** Given an lvalue, attempts to extract the type which 
          * the lvalue expects to be assigned to it.  This may
          * not be possible if the user has not fully specified
          * the type. In that case, None is returned. */
        def optTypeFromLvalue(lvalue0: in.Lvalue): Option[Type.Ref] = {
            case class FailedException() extends Exception
            
            def theOldCollegeTry(lvalue: in.Lvalue): Type.Ref = lvalue match {
                case in.TupleLvalue(lvalues) => 
                    Type.Tuple(lvalues.map(theOldCollegeTry))
                case in.VarLvalue(_, in.InferredTypeRef(), _, _) =>
                    throw FailedException()
                case in.VarLvalue(_, tref: in.TypeRef, name, _) =>
                    symbolType(tref)
            }
            
            try {
                Some(theOldCollegeTry(lvalue0))
            } catch {
                case FailedException() => None
            }
        }
        
        /** Converts an Lvalue into a fully-typed Pattern, using hints from the
          * type of the RHS `rvalueTy` */
        def lowerLvalue(rvalueTy: Type.Ref, lvalue: in.Lvalue): out.Pattern = {
            withPosOf(lvalue, (rvalueTy, lvalue) match {
                // Unpack singleton tuples:
                case (ty, in.TupleLvalue(List(lv))) => lowerLvalue(ty, lv)
                case (Type.Tuple(List(ty)), lv) => lowerLvalue(ty, lv)
                
                // Unpack matching tuples:
                case (Type.Tuple(tys), in.TupleLvalue(lvalues)) if sameLength(tys, lvalues) => {
                    out.TuplePattern(
                        tys.zip(lvalues).map { case (t, l) => lowerLvalue(t, l) }
                    )
                }
                
                // If tuple sizes don't match, just infer NullType:
                case (_, in.TupleLvalue(lvalues)) => {
                    out.TuplePattern(
                        lvalues.map(lowerLvalue(Type.Null, _))
                    )
                }
                
                // Pattern w/o type specified, use type from RHS:
                case (ty, in.VarLvalue(anns, inTref @ in.InferredTypeRef(), name, ())) => {
                    val tref = astType(env)(inTref, ty)
                    out.VarPattern(
                        anns.map(lowerAnnotation),
                        tref,
                        name,
                        new Symbol.Var(name.name, ty)
                    )
                }
                    
                // Pattern w/ type specified, use type given:
                case (_, in.VarLvalue(anns, tref: in.TypeRef, name, sym)) => {
                    val ty = symbolType(tref)
                    out.VarPattern(
                        anns.map(lowerAnnotation),
                        lowerTypeRef(tref),
                        name,
                        new Symbol.Var(name.name, ty)
                    )                    
                }
                    
            })
        }
        
        def introduceVar(fromExpr: in.Expr, toExpr: out.Expr): out.Var = {
            val text = tmpVarName(fromExpr)
            val sym = new Symbol.Var(Name.Var(text), toExpr.ty)
            val nameAst = withPosOf(fromExpr, Ast.VarName(text))
            val tyAst = astType(env)(fromExpr, sym.ty)
            val lv = withPosOf(fromExpr, out.VarPattern(List(), tyAst, nameAst, sym))
            val assign = withPosOf(fromExpr, out.Assign(lv, withPosOf(fromExpr, toExpr)))
            stmts += assign
            withPosOf(fromExpr, out.Var(nameAst, sym, sym.ty))
        }
        
        def dummySubst(subst: Subst)(pat: Symbol.Pattern, text: String): Subst = pat match {
            case Symbol.VarPattern(name, _) =>
                subst + (name.toPath -> Path.Base(Name.Var(text)))
                
            case Symbol.TuplePattern(patterns) =>
                patterns.zipWithIndex.foldLeft(subst) { case (s, (p, i)) =>
                    dummySubst(s)(p, "%s_%d".format(text, i))
                }
        }
        
        def patSubst(subst: Subst)(pat: Symbol.Pattern, expr: in.Expr): Subst = {
            (pat, expr) match {
                case (Symbol.TuplePattern(patterns), in.Tuple(exprs, ())) if patterns.length == exprs.length =>
                    patterns.zip(exprs).foldLeft(subst) { case (s, (p, e)) => patSubst(s)(p, e) }
                
                case (Symbol.VarPattern(lname, _), in.Var(rname, (), ())) =>
                    subst + (lname.toPath -> rname.name.toPath)
                
                case (_, in.Tuple(List(subExpr), ())) =>
                    patSubst(subst)(pat, subExpr)
                    
                case (Symbol.TuplePattern(List(subPat)), _) =>
                    patSubst(subst)(subPat, expr)
                    
                case _ => 
                    dummySubst(subst)(pat, tmpVarName(expr))
            }
        }
        
        def patSubsts(allPatterns: List[Symbol.Pattern], allExprs: List[in.Expr]): Subst = {
            assert(allPatterns.length == allExprs.length) // Guaranteed syntactically.
            allPatterns.zip(allExprs).foldLeft(Subst.empty) { case (s, (p, e)) =>
                patSubst(s)(p, e)
            }
        }
    
        def lowerField(optExpTy: Option[Type.Ref])(expr: in.Field) = introduceVar(expr, { 
            // Note: very similar code to lowerPathField() above.
            // Big difference is that this version generates statements.
            val owner = lowerExprToVar(None)(expr.owner)
            val optSym = env.lookupField(owner.ty, expr.name.name)
            val subst = Subst(Path.This -> Path.fromLoweredAst(owner))
            val sym = optSym.getOrElse {
                state.reporter.report(expr.pos, "no.such.field", owner.ty.toString, expr.name.toString)
                Symbol.errorVar(expr.name.name, optExpTy)
            }
            out.Field(owner, expr.name, sym, subst.ty(sym.ty))
        })
        
        def lowerLiteralExpr(expr: in.Literal) = introduceVar(expr, {
            val ty = Type.Class(Name.Qual(expr.obj.getClass), List())
            out.Literal(expr.obj, ty)
        })
        
        def lowerPart(optExpTy: Option[Type.Ref])(part: in.CallPart) = withPosOf(part, {
            out.CallPart(part.ident, lowerExpr(optExpTy)(part.arg))
        })
        
        def lowerMethodCall(optExpTy: Option[Type.Ref])(mcall: in.MethodCall) = introduceVar(mcall, {
            val rcvr = lowerExpr(None)(mcall.rcvr)
            
            // Find all potential methods:
            val msyms = env.lookupMethods(rcvr.ty, mcall.name)
                
            // Identify the best method (if any):
            msyms match {
                case List() => {
                    println("lowerMethodCall(%s)", mcall)
                    state.reporter.report(mcall.pos, "no.such.method", rcvr.ty.toString, mcall.name.toString)
                    out.Null(optExpTy.getOrElse(Type.Null))
                }
                
                // Exactly one match: We can do more with inferencing
                // in this case, as we know the expected type.
                case List(msym) => {
                    val subst = patSubsts(
                        msym.receiver   ::  msym.parameterPatterns, 
                        mcall.rcvr      ::  mcall.parts.map(_.arg)
                    )
                    val optExpTys = msym.parameterPatterns.map(p => Some(subst.ty(p.ty)))
                    val parts = optExpTys.zip(mcall.parts).map { case (e,p) => lowerPart(e)(p) }
                    out.MethodCall(rcvr, parts, msym, subst.ty(msym.returnTy))
                }
                
                // Multiple matches: have to type the arguments without hints.
                case _ => {
                    val parts = mcall.parts.map(lowerPart(None))
                    
                    def potentiallyApplicable(msym: Symbol.Method) = {
                        msym.parameterPatterns
                    }
                    
                    throw new RuntimeException("TODO")
                }
            }
        })
        
        def lowerNewJava(expr: in.NewJava) = introduceVar(expr, {
            // XXX We could give an expected type for the arguments based on the constructor(s),
            // XXX just like in a method call.
            out.NewJava(lowerTypeRef(expr.tref), lowerTuple(None)(expr.arg), symbolType(expr.tref))
        })
        
        def lowerNull(optExpTy: Option[Type.Ref])(expr: in.Null) = introduceVar(expr, {
            val ty = optExpTy.getOrElse(Type.Null)
            out.Null(ty)
        })

        def lowerTuple(optExpTy: Option[Type.Ref])(tuple: in.Tuple) = withPosOf(tuple, {
            val exprs = tuple.exprs.map(lowerExpr(None)) // XXX deconstruct optExpTy
            out.Tuple(exprs, Type.Tuple(exprs.map(_.ty)))
        })
    
        def lowerInlineTmpl(tmpl: in.InlineTmpl) = introduceVar(tmpl, out.InlineTmpl(
            stmts = lowerStmts(env, tmpl.stmts),
            ty = Type.Class(Name.IntervalTmplQual, List(
                Type.PathArg(Name.IntervalTmplParent, PcEq, Path.Method)
            ))
        ))
        
        def lowerAsyncTmpl(tmpl: in.AsyncTmpl) = introduceVar(tmpl, out.AsyncTmpl(
            stmts = lowerStmts(env, tmpl.stmts),
            ty = Type.Class(Name.AsyncIntervalTmplQual, List(
                Type.PathArg(Name.IntervalTmplParent, PcEq, Path.Method)
            ))
        ))
        
        def lowerImpThis(expr: in.Expr) = withPosOf(expr, {
            val sym = env.lookupThis
            out.Var(astVarName(expr, Name.ThisVar), sym, sym.ty)
        })
        
        def lowerExpr(optExpTy: Option[Type.Ref])(expr: in.Expr): out.LoweredExpr = expr match {
            case tuple: in.Tuple => lowerTuple(optExpTy)(tuple)
            case tmpl: in.InlineTmpl => lowerInlineTmpl(tmpl)
            case tmpl: in.AsyncTmpl => lowerAsyncTmpl(tmpl)
            case lit: in.Literal => lowerLiteralExpr(lit)
            case e: in.Var => lowerVar(optExpTy)(e)
            case e: in.Field => lowerField(optExpTy)(e)
            case e: in.MethodCall => lowerMethodCall(optExpTy)(e)
            case e: in.NewJava => lowerNewJava(e)
            case e: in.Null => lowerNull(optExpTy)(e)
            case e: in.ImpVoid => introduceVar(expr, out.ImpVoid(Type.Void))
            case e: in.ImpThis => lowerImpThis(e)
        }
        
        def lowerExprToVar(optExpTy: Option[Type.Ref])(expr: in.Expr): out.Var = {
            lowerExpr(optExpTy)(expr) match {
                case v: out.Var => v
                case e => introduceVar(expr, e)
            }
        }
        
    }

}