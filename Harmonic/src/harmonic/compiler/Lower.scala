package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

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
        val parameterPatterns = mdecl.params.map(symbolPattern)
        Symbol.MethodId(csym.name, mdecl.name, parameterPatterns)
    }

    def patternType(param: in.Param): Type.Ref = param match {
        case in.TupleParam(params) => Type.Tuple(params.map(patternType))
        case in.VarParam(_, tref, _, ()) => symbolType(tref)
    }
    
    def symbolPattern(param: in.Param): Pattern.Ref = param match {
        case in.TupleParam(params) => Pattern.Tuple(params.map(symbolPattern))
        case in.VarParam(_, tref, name, ()) => Pattern.Var(name.name, symbolType(tref))
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
        case in.Var(name, ()) => Path.Base(name.name)
    }
    
    def astVarName(from: Ast.Node, name: Name.Var) = withPosOf(from,
        Ast.VarName(name.text)
    )
    
    def astPathVar(env: Env)(from: Ast.Node, name: Name.Var) = withPosOf(from, {
        val sym = env.lookupLocalOrError(name, None)
        out.Var(astVarName(from, name), sym)
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
            case in.MethodDecl(_, _, _, in.InferredTypeRef(), _, _, _) => {
                val memberId = methodId(csym, mdecl)

                if(state.inferStack(memberId)) {
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
                        kind = Symbol.Inter,
                        name = mthdName,
                        Symbol.MethodSignature(
                            returnTy = outMdecl.returnTy,
                            receiverTy = Type.Class(csym.name, List()),
                            parameterPatterns = memberId.parameterPatterns
                        )                        
                    )
                }
            }
            
            case in.MethodDecl(_, _, parts, returnTy: in.TypeRef, _, _, _) => {
                new Symbol.Method(
                    kind = Symbol.Inter,
                    name = mthdName,
                    Symbol.MethodSignature(
                        returnTy = symbolType(returnTy),
                        receiverTy = Type.Class(csym.name, List()),
                        parameterPatterns = parts.map(p => symbolPattern(p.param))
                    )
                )
            }
        }
        
        val mdecls = csym.resolvedSource.members.flatMap(_.asMethodNamed(mthdName))
        val msyms = mdecls.map(forMethodDecl)
        csym.methodSymbols(mthdName) = msyms
        msyms
    }
    
    def ThisEnv(csym: Symbol.ClassFromInterFile) = {
        emptyEnv.plusSym(new Symbol.Var(Name.ThisVar, Type.Class(csym.name, List())))
    }
    
    def ThisScope(csym: Symbol.ClassFromInterFile) = {
        InScope(ThisEnv(csym))
    }
    
    def lowerClassDecl(
        cdecl: Ast.Resolve.ClassDecl
    ): out.ClassDecl = {
        val csym = state.classes(cdecl.name.qualName).asInstanceOf[Symbol.ClassFromInterFile]
        withPosOf(cdecl, out.ClassDecl(
            name = cdecl.name,
            annotations = cdecl.annotations.map(ThisScope(csym).lowerAnnotation),
            superClasses = cdecl.superClasses,
            pattern = ThisScope(csym).lowerTupleParam(cdecl.pattern),
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

            val receiverSym = new Symbol.Var(Name.ThisVar, Type.Class(clsName, List()))
            val parameterPatterns = mdecl.params.map(symbolPattern)
            val parameterSyms = parameterPatterns.flatMap(Pattern.createVarSymbols)
            val env = emptyEnv.plusSyms(receiverSym :: parameterSyms)
            val optBody = mdecl.optBody.map(lowerBody(env, _))

            val (returnTref, returnTy) = (mdecl.returnTref, optBody) match {
                case (in.InferredTypeRef(), None) => {
                    state.reporter.report(
                        mdecl.returnTref.pos, "explicit.return.type.required.if.abstract", mdecl.name.toString
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
                receiverSym = receiverSym,
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
            param = lowerTupleParam(part.param)
        ))

        def lowerTupleParam(param: in.TupleParam) = withPosOf(param, {
            out.TupleParam(param.params.map(lowerParam))
        })
        
        def lowerVarParam(pattern: in.VarParam) = withPosOf(pattern, {
            val sym = env.lookupLocal(pattern.name.name).get // should be a variable already created
            out.VarParam(
                annotations = pattern.annotations.map(lowerAnnotation),
                tref = lowerTypeRef(pattern.tref),
                name = pattern.name,
                sym = sym
            )
        })
        
        def lowerParam(param: in.Param): out.Param = withPosOf(param, param match {
            case p: in.TupleParam => lowerTupleParam(p)
            case p: in.VarParam => lowerVarParam(p)
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
            out.Var(v.name, sym)
        })
        
        def lowerPathField(optExpTy: Option[Type.Ref])(path: in.PathField) = withPosOf(path, {
            // Note: very similar code to lowerField() below
            val owner = lowerPath(path.owner)
            val optSym = env.lookupField(owner.ty, path.name.name)
            val subst = Subst(Path.This -> owner.toPath)
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
    
        def lowerTypeRef(tref: in.TypeRef): out.TypeRef = withPosOf(tref, tref match {
            case in.NullType() => out.NullType()
            case in.TupleType(trefs) => out.TupleType(trefs.map(lowerTypeRef))
            case in.VarType(path, tvar) => out.VarType(lowerPath(path), tvar)
            case in.ClassType(cname, targs) => out.ClassType(cname, targs.map(lowerTypeArg))
        })

        def lowerTypeArg(targ: in.TypeArg): out.TypeArg = targ match {
            case ttarg: in.TypeTypeArg => lowerTypeTypeArg(ttarg)
            case ptarg: in.PathTypeArg => lowerPathTypeArg(ptarg)
        }
    
        def lowerTypeTypeArg(targ: in.TypeTypeArg): out.TypeTypeArg = withPosOf(targ, out.TypeTypeArg(
            name = targ.name, rel = targ.rel, typeRef = lowerTypeRef(targ.typeRef)
        ))
    
        def lowerPathTypeArg(targ: in.PathTypeArg): out.PathTypeArg = withPosOf(targ, out.PathTypeArg(
            name = targ.name, rel = targ.rel, path = lowerPath(targ.path)
        ))
        
        def lowerAnnotation(ann: in.Annotation) = withPosOf(ann,
            out.Annotation(name = ann.name)
        )
    }
    
    // ___ Lowering Statements ______________________________________________
    
    def tmpVarName(fromExpr: Ast#ParseExpr) = {
        //"(%s@%s)".format(fromExpr.toString, fromExpr.pos.toString)
        "(%s@%s)".format(fromExpr.getClass.getSimpleName, fromExpr.pos.toString)
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
        def appendLoweredStmt(stmt: in.Stmt): Env = {
            stmt match {
                case in.Assign(lvalue, rvalue) => {
                    val optExpTy = optTypeFromLocal(env, lvalue)
                    val outRvalue = lowerExpr(optExpTy)(rvalue)
                    val outLvalue = lowerLocal(outRvalue.ty, lvalue)
                    stmts += withPosOf(stmt, out.Assign(outLvalue, outRvalue))
                    env.plusSyms(outLvalue.symbols)
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
        def optTypeFromLocal(env: Env, local0: in.Local): Option[Type.Ref] = {
            case class FailedException() extends Exception
            
            def theOldCollegeTry(local: in.Local): Type.Ref = local match {
                case in.TupleLocal(locals) => 
                    Type.Tuple(locals.map(theOldCollegeTry))
                case in.VarLocal(_, in.InferredTypeRef(), name, _) =>
                    env.lookupLocal(name.name) match {
                        case Some(sym) => sym.ty
                        case None => throw FailedException()
                    }
                case in.VarLocal(_, tref: in.TypeRef, _, _) =>
                    symbolType(tref)
            }
            
            try {
                Some(theOldCollegeTry(local0))
            } catch {
                case FailedException() => None
            }
        }
        
        /** Lowers an Lvalue, using hints from the type of the RHS `rvalueTy`. */
        def lowerLocal(rvalueTy: Type.Ref, local: in.Local): out.Local = {
            withPosOf(local, (rvalueTy, local) match {
                // Unpack singleton tuples:
                case (ty, in.TupleLocal(List(lv))) => lowerLocal(ty, lv)
                case (Type.Tuple(List(ty)), lv) => lowerLocal(ty, lv)
                
                // Unpack matching tuples:
                case (Type.Tuple(tys), in.TupleLocal(locals)) if sameLength(tys, locals) => {
                    val outLocals = tys.zip(locals).map { case (t, l) => lowerLocal(t, l) }
                    out.TupleLocal(outLocals)
                }
                
                // If tuple sizes don't match, just infer NullType:
                case (_, in.TupleLocal(locals)) => {
                    val outLocals = locals.map(lowerLocal(Type.Null, _))
                    out.TupleLocal(outLocals)
                }
                
                // Pattern w/o type specified, either reassign pre-existing sym or use type from RHS:
                case (ty, in.VarLocal(anns, inTref @ in.InferredTypeRef(), name, ())) => {
                    env.lookupLocal(name.name) match {
                        case Some(sym) => {
                            out.VarLocal(
                                anns.map(lowerAnnotation),
                                withPosOf(inTref, out.InferredTypeRef()),
                                name, 
                                sym
                            )
                        }
                        
                        case None => {
                            out.VarLocal(
                                anns.map(lowerAnnotation),
                                astType(env)(inTref, ty),
                                name,
                                new Symbol.Var(name.name, ty)
                            )                            
                        }
                    }
                }
                    
                // Pattern w/ type specified, use type given:
                case (_, in.VarLocal(anns, tref: in.TypeRef, name, sym)) => {
                    if(env.localIsDefined(name.name)) {
                        state.reporter.report(
                            local.pos,
                            "shadowed.local.variable",
                            name.toString
                        )
                    }                        
                    
                    val ty = symbolType(tref)
                    out.VarLocal(
                        anns.map(lowerAnnotation),
                        lowerTypeRef(tref),
                        name,
                        new Symbol.Var(name.name, ty)
                    )                    
                }
                    
            })
        }
        
        def introduceVar(fromExpr: in.Expr, toExpr: out.LowerExpr): out.Var = {
            val text = tmpVarName(fromExpr)
            val sym = new Symbol.Var(Name.Var(text), toExpr.ty)
            val nameAst = withPosOf(fromExpr, Ast.VarName(text))
            val tyAst = astType(env)(fromExpr, sym.ty)
            val lv = withPosOf(fromExpr, out.VarLocal(List(), tyAst, nameAst, sym))
            val assign = withPosOf(fromExpr, out.Assign(lv, withPosOf(fromExpr, toExpr)))
            stmts += assign
            withPosOf(fromExpr, out.Var(nameAst, sym))
        }
        
        def dummySubst(subst: Subst)(pat: Pattern.Ref, text: String): Subst = pat match {
            case Pattern.Var(name, _) =>
                subst + (name.toPath -> Path.Base(Name.Var(text)))
                
            case Pattern.Tuple(patterns) =>
                patterns.zipWithIndex.foldLeft(subst) { case (s, (p, i)) =>
                    dummySubst(s)(p, "%s_%d".format(text, i))
                }
        }
        
        def patSubst(subst: Subst)(pat: Pattern.Ref, expr: Ast#ParseExpr): Subst = {
            (pat, expr) match {
                case (patTup: Pattern.Tuple, astTup: Ast#Tuple) if sameLength(patTup.patterns, astTup.exprs) =>
                    patTup.patterns.zip(astTup.exprs).foldLeft(subst) { 
                        case (s, (p, e)) => patSubst(s)(p, e) 
                    }
                
                case (patVar: Pattern.Var, astVar: Ast#Var) =>
                    subst + (patVar.name.toPath -> astVar.name.name.toPath)
                
                case (_, astTup: Ast#Tuple) if astTup.exprs.length == 1 =>
                    patSubst(subst)(pat, astTup.exprs.head)
                    
                case (patTup: Pattern.Tuple, _) if patTup.patterns.length == 1 =>
                    patSubst(subst)(patTup.patterns.head, expr)
                    
                case _ => 
                    dummySubst(subst)(pat, tmpVarName(expr))
            }
        }
        
        def patSubsts(allPatterns: List[Pattern.Ref], allExprs: List[Ast#ParseExpr]): Subst = {
            assert(allPatterns.length == allExprs.length) // Guaranteed syntactically.
            allPatterns.zip(allExprs).foldLeft(Subst.empty) { case (s, (p, e)) =>
                patSubst(s)(p, e)
            }
        }
        
        def mthdSubst(msym: Symbol.Method, rcvr: Ast#ParseExpr, args: List[Ast#ParseExpr]) = {
            val msig = msym.msig
            patSubsts(
                msig.thisPattern    ::  msig.parameterPatterns, 
                rcvr                ::  args
            )
        }
    
        def lowerField(optExpTy: Option[Type.Ref])(expr: in.Field) = introduceVar(expr, { 
            // Note: very similar code to lowerPathField() above.
            // Big difference is that this version generates statements.
            val owner = lowerExprToVar(None)(expr.owner)
            val optSym = env.lookupField(owner.ty, expr.name.name)
            val subst = Subst(Path.This -> owner.toPath)
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
        
        def identifyBestMethod(
            pos: Position,
            msyms: List[Symbol.Method],
            name: Name.Method,
            rcvrTy: Type.Ref,
            inRcvr: in.Expr,
            inArgs: List[in.Expr]
        ) = {
            // Identify the best method (if any):
            msyms match {
                case List() => {
                    state.reporter.report(pos, "no.such.method", rcvrTy.toString, name.toString)
                    None
                }
                
                // Exactly one match: We can do more with inferencing
                // in this case, as we know the expected type.
                case List(msym) => {
                    val subst = mthdSubst(msym, inRcvr, inArgs)
                    val optExpTys = msym.msig.parameterPatterns.map(p => Some(subst.ty(p.ty)))
                    val outArgs = optExpTys.zip(inArgs).map { case (t,a) => lowerExpr(t)(a) }
                    val msig = subst.methodSignature(msym.msig)
                    Some((outArgs, (msym, msig)))
                }
                
                // Multiple matches: have to type the arguments without hints.
                //   In theory, we could try to be smarter (i.e., if all options agree on the
                //   type of a particular argument, etc).
                case _ => {
                    val outArgs = inArgs.map(lowerExpr(None))
                    val argTys = outArgs.map(_.ty)
                    
                    // Find those symbols that are potentially applicable
                    // to the arguments provided:
                    def potentiallyApplicable(msym: Symbol.Method) = {
                        val subst = mthdSubst(msym, inRcvr, inArgs)
                        val parameterTys = msym.msig.parameterPatterns.map(p => subst.ty(p.ty))
                        // XXX Add suitable temps to the environment for the vars ref'd in subst.
                        argTys.zip(parameterTys).forall { case (p, a) => 
                            env.isSuitableArgument(p, a) 
                        }
                    }
                    val applicableMsyms = msyms.filter(potentiallyApplicable)
                    
                    // Try to find an unambiguously "best" choice:
                    def isBetterChoiceThan(msym_better: Symbol.Method, msym_worse: Symbol.Method) = {
                        Pattern.optSubst(
                            msym_better.msig.parameterPatterns,
                            msym_worse.msig.parameterPatterns
                        ) match {
                            case None => false
                            case Some(subst) => {
                                msym_better.msig.parameterPatterns.zip(msym_worse.msig.parameterPatterns).forall {
                                    case (pat_better, pat_worse) =>
                                        env.isSuitableArgument(subst.ty(pat_better.ty), pat_worse.ty)
                                }
                            }
                        }
                    }
                    def isBestChoice(msym: Symbol.Method) = {
                        applicableMsyms.forall { msym_other =>
                            msym == msym_other || isBetterChoiceThan(msym, msym_other)
                        }
                    }
                    val bestMsyms = applicableMsyms.filter(isBestChoice)
                    
                    (bestMsyms, applicableMsyms) match {
                        case (List(msym), _) => {
                            val subst = mthdSubst(msym, inRcvr, inArgs)
                            val msig = subst.methodSignature(msym.msig)
                            Some((outArgs, (msym, msig)))
                        }
                        
                        case (List(), List()) => {
                            state.reporter.report(
                                pos,
                                "no.applicable.methods",
                                argTys.map(_.toString).mkString(", ")
                            )
                            None
                        }
                        
                        case _ => {
                            state.reporter.report(
                                pos,
                                "ambiguous.method.call",
                                bestMsyms.length.toString
                            )
                            None
                        }
                    }
                }
            }            
        }
        
        def lowerMethodCall(optExpTy: Option[Type.Ref])(mcall: in.MethodCall) = introduceVar(mcall, {
            // Find all potential methods:
            val rcvr = lowerExpr(None)(mcall.rcvr)
            val msyms = env.lookupMethods(rcvr.ty, mcall.name)
            val best = identifyBestMethod(
                mcall.pos, msyms, mcall.name, 
                rcvr.ty, mcall.rcvr, mcall.parts.map(_.arg))
            best match {
                case Some((args, (msym, msig))) => {
                    def lowerPart(pair: (in.CallPart, out.AtomicExpr)) = withPosOf(pair._1,
                        out.CallPart(pair._1.ident, pair._2)
                    )
                    out.MethodCall(
                        rcvr = rcvr,
                        parts = mcall.parts.zip(args).map(lowerPart),
                        data = (msym, msig)
                    )                    
                }
                
                case None =>
                    out.Null(optExpTy.getOrElse(Type.Null))
            }
        })
        
        def lowerNewCtor(expr: in.NewCtor) = introduceVar(expr, {
            symbolType(expr.tref) match {
                case ty @ Type.Class(name, _) => {
                    val csym = state.classes(name)
                    val msyms = csym.constructors(state)
                    val tvar = tmpVarName(expr)
                    val rcvr = in.Var(Ast.VarName(tvar), ())
                    val best = identifyBestMethod(
                        expr.pos, msyms, Name.InitMethod,
                        ty, rcvr, List(expr.arg))
                    best match {
                        case Some((List(arg), (msym, msig))) => {
                            out.NewCtor(
                                tref = lowerTypeRef(expr.tref),
                                arg = arg,
                                msym = msym,
                                ty = ty
                            )
                        }
                            
                        
                        case _ =>
                            out.Null(ty)
                    }
                }
                
                case ty => {
                    state.reporter.report(
                        expr.pos, 
                        "can.only.create.classes"
                    )
                    out.Null(ty)
                }
            }
        })
        
        def lowerNewAnon(expr: in.NewAnon) = introduceVar(expr, {
            throw new RuntimeException("TODO")
        })
        
        def lowerNull(optExpTy: Option[Type.Ref])(expr: in.Null) = introduceVar(expr, {
            val ty = optExpTy.getOrElse(Type.Null)
            out.Null(ty)
        })

        def lowerTuple(optExpTy: Option[Type.Ref])(tuple: in.Tuple) = withPosOf(tuple, {
            optExpTy match {
                case Some(Type.Tuple(tys)) if sameLength(tys, tuple.exprs) => {
                    val outExprs = tys.zip(tuple.exprs).map { case (t, e) => 
                        lowerExpr(Some(t))(e)
                    }
                    out.Tuple(outExprs)
                }
                case _ => {
                    val exprs = tuple.exprs.map(lowerExpr(None))
                    out.Tuple(exprs)                    
                }
            }
        })
        
        def optTypeArg(TypeVarName: Name.Var, optExpTy: Option[Type.Ref]) = optExpTy match {
            case Some(Type.Class(_, typeArgs)) => {
                typeArgs.firstSome {
                    case Type.TypeArg(TypeVarName, TcEq, ty) => Some(ty)
                    case Type.TypeArg(TypeVarName, TcSub, ty) => Some(ty)
                    case _ => None
                }
            }
            case _ => None
        }
        
        def lowerBlockArgument(expArgumentTy: Type.Ref, local: in.Local): out.Param = {
            println("lowerBlockArgument(%s,%s)".format(expArgumentTy, local))
            withPosOf(local, (expArgumentTy, local) match {
                // Unpack singleton tuples:
                //    We keep the TupleParam() wrapper around a singleton entry just for pretty printing.
                case (ty, in.TupleLocal(List(lv))) => out.TupleParam(List(lowerBlockArgument(ty, lv)))
                case (Type.Tuple(List(ty)), lv) => lowerBlockArgument(ty, lv)
                
                // Unpack matching tuples:
                case (Type.Tuple(tys), in.TupleLocal(locals)) if sameLength(tys, locals) => {
                    val outParams = tys.zip(locals).map { case (t, l) => lowerBlockArgument(t, l) }
                    out.TupleParam(outParams)
                }
                
                // If tuple sizes don't match, try to find a bounding type, or just infer Object:
                case (_, in.TupleLocal(locals)) => {
                    // Try to find a suitable bounding type:
                    val optTy = env.upperBoundType(expArgumentTy).firstSome {
                        case ty @ Type.Tuple(tys) if sameLength(tys, locals) => Some(ty)
                        case _ => None
                    }
                    
                    optTy match {
                        case Some(Type.Tuple(tys)) => { // Found one.
                            val outParams = tys.zip(locals).map { case (t, l) => lowerBlockArgument(t, l) }
                            out.TupleParam(outParams)                            
                        }
                        
                        case _ => { // No match, just infer Object.
                            val outParams = locals.map(lowerBlockArgument(Type.Object, _))
                            out.TupleParam(outParams)
                        }
                    }
                }
                
                // Pattern w/o type specified, use expected type:
                case (ty, in.VarLocal(anns, inTref @ in.InferredTypeRef(), name, ())) => {
                    out.VarParam(
                        anns.map(lowerAnnotation),
                        astType(env)(inTref, ty),
                        name,
                        new Symbol.Var(name.name, ty)
                    )                            
                }
                    
                // Pattern w/ type specified, use type given:
                case (_, in.VarLocal(anns, tref: in.TypeRef, name, sym)) => {
                    if(env.localIsDefined(name.name)) {
                        state.reporter.report(
                            local.pos,
                            "shadowed.local.variable",
                            name.toString
                        )
                    }                        
                    
                    val ty = symbolType(tref)
                    out.VarParam(
                        anns.map(lowerAnnotation),
                        lowerTypeRef(tref),
                        name,
                        new Symbol.Var(name.name, ty)
                    )                    
                }
                    
            })
        }
        
        def lowerBlock(optExpTy: Option[Type.Ref])(tmpl: in.Block) = introduceVar(tmpl, {
            val expArgumentTy = optTypeArg(Name.AVar, optExpTy).getOrElse(Type.Void)
            
            val outParam = lowerBlockArgument(expArgumentTy, tmpl.param)
            
            val subenv = env.plusSyms(outParam.symbols)
            val outStmts = lowerStmts(subenv, tmpl.stmts)
            
            // This code extracts the expected return type from `optExpTy`: we decided
            // instead to extract the expected return type from `outStmts`.  As a side
            // effect, note that the `this` pointer inside an interval template does
            // not change, unlike an inner or anonymous class.
            //
            // val expReturnTy = optTypeArg(Name.RVar, optExpTy).getOrElse(Type.Void)
            // val (outReturnTypeRef, returnTy) = tmpl.returnTref match {
            //     case tref: in.TypeRef => (lowerTypeRef(tref), symbolType(tref))
            //     case in.InferredTypeRef() => (astType(env)(tmpl.returnTref, expReturnTy), expReturnTy)
            // }
            
            val (outReturnTypeRef, returnTy) = tmpl.returnTref match {
                case tref: in.TypeRef => (lowerTypeRef(tref), symbolType(tref))
                case in.InferredTypeRef() => {
                    val ty = outStmts.last.ty
                    (astType(env)(tmpl.returnTref, ty), ty)
                }
            }
            
            out.Block(
                async = tmpl.async,
                returnTref = outReturnTypeRef,
                returnTy = returnTy,
                param = outParam,
                stmts = outStmts,
                ty = Type.Class(tmpl.className, List(
                    Type.PathArg(Name.IntervalTmplParent, PcEq, Path.Method),
                    Type.TypeArg(Name.RVar, TcEq, returnTy),
                    Type.TypeArg(Name.AVar, TcEq, outParam.ty)
                ))
            )
        })
        
        def lowerImpThis(expr: in.Expr) = withPosOf(expr, {
            val sym = env.lookupThis
            out.Var(astVarName(expr, Name.ThisVar), sym)
        })
        
        def lowerCast(expr: in.Cast) = withPosOf(expr, {
            val ty = symbolType(expr.typeRef)
            out.Cast(
                lowerExpr(Some(ty))(expr),
                lowerTypeRef(expr.typeRef),
                ty
            )
        })
        
        def lowerExpr(optExpTy: Option[Type.Ref])(expr: in.Expr): out.AtomicExpr = expr match {
            case e: in.Tuple => lowerTuple(optExpTy)(e)
            case e: in.Block => lowerBlock(optExpTy)(e)
            case e: in.Cast => lowerCast(e)
            case e: in.Literal => lowerLiteralExpr(e)
            case e: in.Var => lowerVar(optExpTy)(e)
            case e: in.Field => lowerField(optExpTy)(e)
            case e: in.MethodCall => lowerMethodCall(optExpTy)(e)
            case e: in.NewCtor => lowerNewCtor(e)
            case e: in.NewAnon => lowerNewAnon(e)
            case e: in.Null => lowerNull(optExpTy)(e)
            case e: in.ImpVoid => introduceVar(expr, out.Null(Type.Void))
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