package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.util.parsing.input.Position

import Ast.{Resolve => in}
import Ast.{Lower => out}
import Util._

object Lower {
    class Data {
        /** Members whose type is currently being inferred. */
        val inferStack = new mutable.HashSet[Symbol.MemberId]()
    
        /** Members for which we have reported an inference error. */
        val inferReported = new mutable.HashSet[Symbol.MemberId]()
        
        /** Class parameter and environment */
        val classParamAndEnvs = new mutable.HashMap[Symbol.ClassFromSource, (out.Param, Env)]()
    }
}

/** Lowers the IR to what is expected by the type check.  This has two
  * main functions:
  * - Fills in any inferred types.  (Note that we don't perform a full type check!)
  * - Removes nested expressions into intermediate variables. */
case class Lower(state: CompilationState) {
    private[this] val data = state.data(classOf[Lower.Data])
    private[this] val emptyEnv = Env.empty(state)
    
    // ___ Method Symbol Creation ___________________________________________
    
    def classParamAndEnv(csym: Symbol.ClassFromSource): (out.Param, Env) = {
        data.classParamAndEnvs.get(csym).getOrElse {
            val thisTy = Type.Class(csym.name, List())
            val thisSym = new Symbol.LocalVar(Modifier.Set.empty, Name.ThisLocal, thisTy)
            val env0 = emptyEnv.plusThis(thisTy, thisSym)
            val cdecl = csym.resolvedSource
            val (List(outParam), env) = lowerParams(env0, List(cdecl.pattern))
            data.classParamAndEnvs(csym) = (outParam, env)
            (outParam, env)
        }
    }
    
    def symbolForConstructor(csym: Symbol.ClassFromSource) = {
        csym.optCtorSymbol.getOrElse {
            val cdecl = csym.resolvedSource
            val (outParam, env) = classParamAndEnv(csym)
            val ctorSymbol = new Symbol.Method(
                pos         = cdecl.pattern.pos,
                modifierSet = Modifier.Set.empty,
                kind        = Symbol.InterCtor,
                clsName     = csym.name,
                name        = Name.InitMethod,
                Symbol.MethodSignature(
                    returnTy = Type.Void,
                    receiverTy = csym.toType,
                    parameterPatterns = List(out.toPatternRef(outParam))
                )
            )
            csym.optCtorSymbol = Some(ctorSymbol)
            ctorSymbol
        }
    }
    
    def ThisEnv(csym: Symbol.ClassFromSource) = {
        classParamAndEnv(csym)._2
    }
    
    def ThisScope(csym: Symbol.ClassFromSource) = {
        InScope(ThisEnv(csym))
    }
    
    def symbolsForMethodsNamed(
        sym: Symbol.ClassFromSource, 
        mthdName: Name.Method
    ) = {
        sym.methodSymbols.get(mthdName) match {
            case None => createSymbolsForMethodsNamed(sym, mthdName)
            case Some(res) => res
        }
    }
    
    def createSymbolsForMethodsNamed(
        csym: Symbol.ClassFromSource, 
        mthdName: Name.Method
    ) = {
        def forMethodDecl(mdecl: in.MethodDecl) = mdecl match {
            case in.MethodDecl(annotations, _, _, _, in.InferredTypeRef(), _, _) => {
                val memberId = methodId(csym, mdecl)

                if(data.inferStack(memberId)) {
                    if(data.inferReported.addEntry(memberId)) {
                        state.reporter.report(
                            mdecl.returnTref.pos,
                            "explicit.type.required.due.to.cycle",
                            mthdName.toString
                        )
                    }
                    Symbol.errorMethod(mthdName, csym.name)
                } else {
                    val outMdecl = lowerMethodDecl(csym, mdecl)
                    new Symbol.Method(
                        pos         = mdecl.pos,
                        modifierSet = Modifier.forResolvedAnnotations(annotations),
                        kind        = Symbol.Inter,
                        clsName     = csym.name,
                        name        = mthdName,
                        Symbol.MethodSignature(
                            returnTy          = outMdecl.returnTref.ty,
                            receiverTy        = csym.toType,
                            parameterPatterns = outMdecl.params.map(out.toPatternRef)
                        )                        
                    )
                }
            }
            
            case in.MethodDecl(annotations, _, _, params, returnTref: in.ResolveTypeRef, _, _) => {
                new Symbol.Method(
                    pos         = mdecl.pos,
                    modifierSet = Modifier.forResolvedAnnotations(annotations),
                    kind        = Symbol.Inter,
                    clsName     = csym.name,
                    name        = mthdName,
                    msig        = extractMethodSignature(csym, params, returnTref)
                )
            }
        }
        
        val mdecls = csym.resolvedSource.members.flatMap(_.asMethodNamed(mthdName))
        val msyms = mdecls.map(forMethodDecl)
        csym.methodSymbols(mthdName) = msyms
        msyms
    }
    
    def lowerClass(
        csym: Symbol.ClassFromSource
    ): out.ClassDecl = {
        val cdecl = csym.resolvedSource
        val (outParam, env) = classParamAndEnv(csym)
        val result = withPosOf(cdecl, out.ClassDecl(
            name         = cdecl.name,
            annotations  = cdecl.annotations.map(InScope(emptyEnv).lowerAnnotation),
            superClasses = cdecl.superClasses,
            pattern      = outParam,
            members      = cdecl.members.map(lowerMemberDecl(csym, _)),
            sym          = csym
        ))
        
        // Make sure that we create all symbols:
        val methodNames = csym.loweredMethods.valuesIterator.map(_.name)
        methodNames.foreach(name => csym.methodsNamed(state)(name))
        
        result
    }
    
    def lowerMemberDecl(
        csym: Symbol.ClassFromSource, 
        mem: in.MemberDecl
    ): out.MemberDecl = withPosOf(mem, mem match {
        case decl: in.IntervalDecl => lowerIntervalDecl(csym, decl)
        case decl: in.MethodDecl => lowerMethodDecl(csym, decl)
        case decl: in.FieldDecl => lowerFieldDecl(csym, decl)
        case decl: in.RelDecl => lowerRelDecl(csym, decl)
    })
    
    def lowerRelDecl(
        csym: Symbol.ClassFromSource, 
        decl: in.RelDecl        
    ): out.RelDecl = withPosOf(decl, out.RelDecl(
        annotations = decl.annotations.map(ThisScope(csym).lowerAnnotation),
        left = ThisScope(csym).lowerPath(decl.left),
        kind = decl.kind,
        right = ThisScope(csym).lowerPath(decl.right)
    ))
    
    def lowerIntervalDecl(
        csym: Symbol.ClassFromSource, 
        decl: in.IntervalDecl
    ): out.IntervalDecl = withPosOf(decl, out.IntervalDecl(
        annotations = decl.annotations.map(ThisScope(csym).lowerAnnotation),
        name = decl.name,
        optParent = decl.optParent.map(ThisScope(csym).lowerPath),
        optBody = decl.optBody.map(lowerBody(ThisEnv(csym), _))
    ))
    
    def lowerMethodDecl(
        csym: Symbol.ClassFromSource, 
        mdecl: in.MethodDecl
    ): out.MethodDecl = {
        val memberId = methodId(csym, mdecl)
        csym.loweredMethods.get(memberId).getOrElse {
            assert(!data.inferStack(memberId))
            data.inferStack += memberId

            val (outParams, env) = lowerParams(ThisEnv(csym), mdecl.params)
            val optBody = mdecl.optBody.map(lowerBody(env, _))

            val returnTy = (mdecl.returnTref, optBody) match {
                case (in.InferredTypeRef(), None) => {
                    state.reporter.report(
                        mdecl.returnTref.pos, 
                        "explicit.return.type.required.if.abstract", 
                        mdecl.name.toString
                    )
                    Type.Void
                }

                case (in.InferredTypeRef(), Some(out.Body(stmts))) => {
                    stmts.last.ty // TODO Have to eliminate type vars and things that go out of scope
                }

                case (tref: in.ResolveTypeRef, _) => {
                    InScope(env).toTypeRef(tref)
                }
            }

            val outMdecl = out.MethodDecl(
                annotations  = mdecl.annotations.map(InScope(env).lowerAnnotation),
                name         = mdecl.name,
                receiverSym  = env.lookupThis,
                params       = outParams,
                returnTref   = out.TypeRef(returnTy),
                requirements = mdecl.requirements.map(InScope(env).lowerRequirement),
                optBody      = optBody
            )
            
            csym.loweredMethods(memberId) = outMdecl
            data.inferStack -= memberId
            outMdecl
        }
    }
    
    def lowerFieldDecl(
        csym: Symbol.ClassFromSource, 
        decl: in.FieldDecl
    ): out.FieldDecl = withPosOf(decl, {
        val optBody = decl.optBody.map(lowerBody(ThisEnv(csym), _))
        val env = ThisEnv(csym)
        val ty = (decl.tref, optBody) match {
            case (tref: in.ResolveTypeRef, _) => // Explicit type.
                InScope(env).toTypeRef(tref)
                
            case (in.InferredTypeRef(), Some(out.Body(stmts))) => { // Implicit type.
                stmts.last.ty
            }
            
            case (in.InferredTypeRef(), None) => {
                state.reporter.report(
                    decl.tref.pos, "explicit.type.reqd.if.abstract", decl.name.toString
                )
                Type.Object
            }
        }
        out.FieldDecl(
            annotations = decl.annotations.map(InScope(env).lowerAnnotation),
            name = decl.name,
            tref = out.TypeRef(ty),
            optBody = optBody
        )        
    })
    
    // ___ Parameters _______________________________________________________
    
    def lowerParams(env0: Env, inParams: List[in.Param]): (List[out.Param], Env) = {
        var env = env0
        
        def lowerParam(param: in.Param): out.Param = withPosOf(param, param match {
            case in.TupleParam(params) => out.TupleParam(params.map(lowerParam))
            case in.VarParam(annotations, tref, name, ()) => {
                val outAnnotations = annotations.map(InScope(env).lowerAnnotation)
                val outTypeRef = InScope(env).lowerTypeRef(tref)
                val modifierSet = Modifier.forLoweredAnnotations(outAnnotations)
                val sym = new Symbol.LocalVar(modifierSet, name.name, outTypeRef.ty)
                env = env.plusLocalVar(sym)
                out.VarParam(outAnnotations, outTypeRef, name, sym)
            }
        })
        
        (inParams.map(lowerParam), env)
    }
    
    def extractMethodSignature(
        csym: Symbol.ClassFromSource, 
        inParams: List[in.Param],
        inTypeRef: in.ResolveTypeRef
    ): Symbol.MethodSignature[Pattern.Ref] = {
        val (outParams, env) = lowerParams(ThisEnv(csym), inParams)
        Symbol.MethodSignature(
            returnTy          = InScope(env).toTypeRef(inTypeRef),
            receiverTy        = csym.toType,
            parameterPatterns = outParams.map(out.toPatternRef)
        )
    }
    
    // ___ Paths, Types _____________________________________________________
    
    object InScope {
        def apply(env: Env) = new InScope(env)
    }
    
    class InScope(env: Env) {
        
        // ___ Paths ____________________________________________________________
        
        def toTypedPath(path: in.AstPath): Path.Typed = {
            def errorPath(name: String) = {
                val sym = Symbol.errorLocalVar(name, None)
                Path.TypedBase(sym)
            }

            path match {
                case in.PathErr(name) =>
                    errorPath(name)

                case in.PathBase(Ast.LocalName(localName), ()) => {
                    val sym = env.locals(localName)
                    Path.TypedBase(sym)
                }

                case in.PathBase(Ast.MemberName(memberVar), ()) => {
                    val csym = state.classes(memberVar.className)
                    csym.fieldNamed(memberVar) match {
                        case None => {
                            state.reporter.report(
                                path.pos, "no.such.field", memberVar.className, memberVar.text
                            )
                            errorPath(path.toString)
                        }

                        case Some(fsym) if !fsym.modifierSet.isStatic => {
                            state.reporter.report(
                                path.pos, "expected.static", memberVar.className, memberVar.text
                            )
                            errorPath(path.toString)               
                        }

                        case Some(fsym) => {
                            Path.TypedBase(fsym)
                        }
                    }
                }

                case in.PathDot(owner, name, (), ()) => {
                    val ownerTypedPath = toTypedPath(owner)
                    val optSym = env.lookupField(ownerTypedPath.ty, name.name)
                    val fsym = optSym.getOrElse {
                        state.reporter.report(path.pos, "no.such.field", ownerTypedPath.ty.toString, name.toString)
                        Symbol.errorVar(path.name.name, optExpTy)
                    }

                    if(fsym.modifierSet.isStatic) {
                        state.reporter.report(path.pos, "qualified.static", fsym.name.toString)
                        Path.TypedBase(fsym)
                    } else {
                        Path.TypedField(ownerTypedPath, fsym)
                    }
                }
            }
        }
        
        def toPath(path: in.AstPath): Path.Ref = {
            toTypedPath(path).toPath
        }
        
        def lowerPath(path: in.AstPath): out.TypedPath = {
            withPosOf(path, out.TypedPath(toTypedPath(path)))
        }

        // ___ Types ____________________________________________________________
        
        def toTypeRef(tref: in.ResolveTypeRef): Type.Ref = {
            case in.TupleType(trefs) => Type.Tuple(trefs.map(toTypeRef))
            case in.NullType() => Type.Null
            case in.TypeVar(path, typeVar) => {
                val typedPath = toPath(path)
                val memberVar = env.lookupTypeVar(typedPath.ty, typeVar)
                val memberVar = Name.MemberVar(Name.ObjectQual, typeVar.text)
                Type.Var(typedPath, memberVar)
            }
            case in.ClassType(in.ClassName(className), inTypeArgs) => {
                val csym = state.classes(className)
                val typeArgs = inTypeArgs.flatMap(toTypeArgOf(csym))
                Type.Class(className, typeArgs)
            }
        }
        
        def toOptTypeArgOf(csym: Symbol.Class)(targ: in.TypeArg) = {
            targ match {
                case in.PathTypeArg(uName, rel, inPath) => {
                    env.findEntry(csym, uName) match {
                        case Right(entry) if entry.isConstrainableInPathArg =>
                            Some(Type.PathArg(entry.name, rel, toPath(inPath)))                                
                            
                        case Right(entry) => {
                            state.reporter.report(uName.pos, "not.in.path.arg", entry.name)
                            None                                
                        }
                            
                        case Left(err) => {
                            err.report(state, uName.pos)
                            None
                        }
                    }
                }

                case in.TypeTypeArg(name, rel, inTypeRef) => {
                    env.findEntry(csym, uName) match {
                        case Right(entry) if entry.isConstrainableInTypeArg =>
                            Some(Type.TypeArg(entry.name, rel, toTypeRef(inTypeRef)))
                            
                        case Right(entry) => {
                            state.reporter.report(uName.pos, "not.in.type.arg", entry.name)
                            None                                
                        }

                        case Left(err) => {
                            err.report(state, uName.pos)
                            None
                        }
                    }
                }
            }
        }
        
        def lowerTypeRef(tref: in.ResolveTypeRef): out.TypeRef = withPosOf(tref, {
            out.TypeRef(toTypeRef(ref))
        })
        
        // ___  _________________________________________________________________
        
        def lowerDeclPart(part: in.DeclPart): out.DeclPart = withPosOf(part, out.DeclPart(
            ident = part.ident,
            param = lowerTupleParam(part.param)
        ))

        def lowerRequirement(req: in.PathRequirement) = withPosOf(req, out.PathRequirement(
            left = lowerPath(req.left),
            rel = req.rel,
            right = lowerPath(req.right)
        ))

        def lowerAnnotation(ann: in.Annotation) = withPosOf(ann,
            out.Annotation(name = ann.name)
        )
    }
    
    // ___ Lowering Statements ______________________________________________
    
    def tmpVarName(fromExpr: Ast.Node) = {
        "(%s@%s)".format(fromExpr.getClass.getSimpleName, fromExpr.pos.toString)
    }
    
    def lowerBody(env: Env, body: in.Body): out.Body = {
        withPosOf(body, out.Body(lowerStmts(env, body.stmts)))
    }
    
    def lowerStmts(env0: Env, stmts: List[in.Stmt]): List[out.Stmt] = {
        var env = env0
        val result = new mutable.ListBuffer[out.Stmt]()
        
        stmts.foreach { stmt =>
            env = InScopeStmt(env, result).appendLoweredStmt(stmt)
        }
        
        result.toList
    }
    
    object InScopeStmt {
        def apply(env: Env, stmts: mutable.ListBuffer[out.Stmt]) = 
            new InScopeStmt(env, stmts)
    }
    
    class InScopeStmt(env: Env, stmts: mutable.ListBuffer[out.Stmt]) extends InScope(env) {
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
                                new Symbol.Var(Modifier.Set.empty, name.name, ty)
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
                        new Symbol.Var(Modifier.Set.empty, name.name, ty)
                    )                    
                }
                    
            })
        }
        
        def introduceVar(fromExpr: in.Expr, toExpr: out.Expr): out.Var = {
            val text = tmpVarName(fromExpr)
            val sym = new Symbol.Var(Modifier.Set.empty, Name.Var(text), toExpr.ty)
            val nameAst = withPosOf(fromExpr, Ast.VarName(text))
            val tyAst = astType(env)(fromExpr, sym.ty)
            val lv = withPosOf(fromExpr, out.VarLocal(List(), tyAst, nameAst, sym))
            val assign = withPosOf(fromExpr, out.Assign(lv, withPosOf(fromExpr, toExpr)))
            stmts += assign
            withPosOf(fromExpr, out.Var(nameAst, sym))
        }
        
        def dummySubst(subst: Subst)(pat: Pattern.Ref, text: String): Subst = pat match {
            case Pattern.Var(in.LocalName(name), _) =>
                subst + (name.toPath -> Name.LocalVar(text).toPath)
                
            case Pattern.Tuple(patterns) =>
                patterns.zipWithIndex.foldLeft(subst) { case (s, (p, i)) =>
                    dummySubst(s)(p, "%s[%d]".format(text, i))
                }
        }
        
        def patSubst(subst: Subst)(pat: Pattern.Ref, expr: Ast#ParseRcvrExpr): Subst = {
            (pat, expr) match {
                case (patTup: Pattern.Tuple, astTup: Ast#Tuple) if sameLength(patTup.patterns, astTup.exprs) =>
                    patTup.patterns.zip(astTup.exprs).foldLeft(subst) { 
                        case (s, (p, e)) => patSubst(s)(p, e) 
                    }
                
                case (patVar: Pattern.Var, astVar: Ast#Var) =>
                    subst + (patVar.name.toPath -> astVar.name.name.toPath)
                
                case (patVar: Pattern.Var, _: Ast#Super) =>
                    subst + (patVar.name.toPath -> Path.This)
                
                case (_, astTup: Ast#Tuple) if astTup.exprs.length == 1 =>
                    patSubst(subst)(pat, astTup.exprs.head)
                    
                case (patTup: Pattern.Tuple, _) if patTup.patterns.length == 1 =>
                    patSubst(subst)(patTup.patterns.head, expr)
                    
                case _ => 
                    dummySubst(subst)(pat, tmpVarName(expr))
            }
        }
        
        def patSubsts(allPatterns: List[Pattern.Ref], allExprs: List[Ast#ParseRcvrExpr]): Subst = {
            assert(allPatterns.length == allExprs.length) // Guaranteed syntactically.
            allPatterns.zip(allExprs).foldLeft(Subst.empty) { case (s, (p, e)) =>
                patSubst(s)(p, e)
            }
        }
        
        def mthdSubst(msym: Symbol.Method, rcvr: Ast#ParseRcvrExpr, args: List[Ast#ParseTlExpr]) = {
            val msig = msym.msig
            if(msym.modifierSet.isStatic) {
                patSubsts(msig.parameterPatterns, args)
            } else {
                patSubsts(msig.thisPattern :: msig.parameterPatterns, rcvr :: args)
            }
        }
    
        def lowerField(optExpTy: Option[Type.Ref])(expr: in.Field) = introduceVar(expr, { 
            val owner = lowerExprToVar(None)(expr.owner)
            val optSym = env.lookupField(owner.ty, expr.name.name)
            val fsym = optSym.getOrElse {
                state.reporter.report(expr.pos, "no.such.field", owner.ty.toString, expr.name.toString)
                Symbol.errorVar(expr.name.name, optExpTy)
            }
            if(fsym.modifierSet.isStatic) {
                state.reporter.report(path.pos, "qualified.static", sym.name.toString)
                out.Field(out.Static(fsym.name.className), fsym.name, fsym, fsym.ty)
            } else {
                val subst = Subst(Path.This -> owner.toPath)
                out.Field(owner, fsym.name, fsym, subst.ty(fsym.ty))                
            }
        })
        
        def lowerLiteralExpr(expr: in.Literal) = introduceVar(expr, {
            val ty = Type.Class(Name.Class(expr.obj.getClass), List())
            out.Literal(expr.obj, ty)
        })
        
        def identifyBestMethod(
            pos: Position,
            msyms: List[Symbol.Method],
            name: Name.Method,
            rcvrTy: Type.Ref,
            inRcvr: in.Rcvr,
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
                        // FIXME Add suitable temps to the environment for the vars ref'd in subst.
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
            val rcvr = lowerRcvr(mcall.rcvr, mcall.name)
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
                        new Symbol.Var(Modifier.Set.empty, name.name, ty)
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
                        new Symbol.Var(Modifier.Set.empty, name.name, ty)
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
        
        def lowerRcvr(rcvr: in.Rcvr, mthdName: Name.Method): out.Rcvr = withPosOf(rcvr, rcvr match {
            case e: in.Expr => lowerExpr(None)(e)
            case in.Super(()) => {
                // Find the next supertype in MRO that implements the method
                // `mthdName` (if any):
                val mro = MethodResolutionOrder(state).forSym(env.thisCsym)
                val optTy = mro.firstSome { 
                    case csym if !csym.methodsNamed(state)(mthdName).isEmpty => 
                        Some(csym.toType)
                    
                    case csym => 
                        None
                }
                optTy match {
                    case Some(ty) => out.Super(ty)
                    case None => {
                        state.reporter.report(
                            rcvr.pos,
                            "no.super.class.implements",
                            mthdName.toString
                        )
                        out.Super(Type.Object)
                    }
                }
            }
        })
        
        def lowerVar(optExpTy: Option[Type.Ref])(v: in.Var) = withPosOf(v, {
            // Resolve pass should ensure that this lookup succeeds:
            out.Var(v.name, env.locals(v.name.name))
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