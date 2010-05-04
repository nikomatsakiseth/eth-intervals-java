package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.util.parsing.input.Position

import Ast.{Resolve => in}
import Ast.{Lower => out}
import Util._

/** Lowers the IR to what is expected by the type check.  This has two
  * main functions:
  * - Fills in any inferred types.  (Note that we don't perform a full type check!)
  * - Removes nested expressions into intermediate variables. */
case class Lower(global: Global) {
    private[this] val data = globaldata(classOf[Lower.Data])
    private[this] val emptyEnv = Env.empty(global)
    
    def classParamAndEnv(csym: ClassFromSource): (out.Param, Env) = {
        val thisTy = Type.Class(csym.name, List())
        val thisSym = new VarSymbol.Local(Modifier.Set.empty, Name.ThisLocal, thisTy)
        val env0 = emptyEnv.plusThis(thisTy, thisSym)
        val cdecl = csym.resolvedSource
        val (List(outParam), env) = lowerMethodParams(env0, List(cdecl.pattern))
        data.classParamAndEnvs(csym) = (outParam, env)
        (outParam, env)
    }
    
    def createSymbolForConstructor(csym: ClassFromSource) = {
        val cdecl = csym.resolvedSource
        val (outParam, env) = classParamAndEnv(csym)
        new MethodSymbol(
            pos         = cdecl.pattern.pos,
            modifiers = Modifier.Set.empty,
            kind        = Symbol.InterCtor,
            clsName     = csym.name,
            name        = Name.InitMethod,
            MethodSignature(
                returnTy = Type.Void,
                receiverTy = csym.toType,
                parameterPatterns = List(out.toPatternRef(outParam))
            )
        )
    }
    
    def lowerMemberDecl(
        csym: ClassFromSource, 
        mem: in.MemberDecl
    ): out.MemberDecl = withPosOf(mem, mem match {
        case decl: in.IntervalDecl => lowerIntervalDecl(csym, decl)
        case decl: in.MethodDecl => lowerMethodDecl(csym, decl)
        case decl: in.FieldDecl => lowerFieldDecl(csym, decl)
        case decl: in.RelDecl => lowerRelDecl(csym, decl)
    })
    
    def lowerRelDecl(
        csym: ClassFromSource, 
        decl: in.RelDecl        
    ): out.RelDecl = withPosOf(decl, out.RelDecl(
        annotations = decl.annotations.map(ThisScope(csym).lowerAnnotation),
        left = ThisScope(csym).lowerPath(decl.left),
        kind = decl.kind,
        right = ThisScope(csym).lowerPath(decl.right)
    ))
    
    def lowerIntervalDecl(
        csym: ClassFromSource, 
        decl: in.IntervalDecl
    ): out.IntervalDecl = withPosOf(decl, out.IntervalDecl(
        annotations = decl.annotations.map(ThisScope(csym).lowerAnnotation),
        name = decl.name,
        optParent = decl.optParent.map(ThisScope(csym).lowerPath),
        optBody = decl.optBody.map(lowerBody(ThisEnv(csym), _))
    ))
    
    def lowerMethodDecl(
        csym: ClassFromSource, 
        mdecl: in.MethodDecl
    ): out.MethodDecl = {
        val (outParams, env) = lowerMethodParams(ThisEnv(csym), mdecl.params)
        val optBody = mdecl.optBody.map(lowerBody(env, _))

        val returnTy = (mdecl.returnTref, optBody) match {
            case (in.InferredTypeRef(), None) => {
                globalreport(
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
                InEnv(env).toTypeRef(tref)
            }
        }

        out.MethodDecl(
            annotations  = mdecl.annotations.map(InEnv(env).lowerAnnotation),
            name         = mdecl.name,
            receiverSym  = env.lookupThis,
            params       = outParams,
            returnTref   = out.TypeRef(returnTy),
            requirements = mdecl.requirements.map(InEnv(env).lowerRequirement),
            optBody      = optBody
        )
    }
    
    def lowerFieldDecl(
        csym: ClassFromSource, 
        decl: in.FieldDecl
    ): out.FieldDecl = withPosOf(decl, {
        val optBody = decl.optBody.map(lowerBody(ThisEnv(csym), _))
        val env = ThisEnv(csym)
        val ty = (decl.tref, optBody) match {
            case (tref: in.ResolveTypeRef, _) => // Explicit type.
                InEnv(env).toTypeRef(tref)
                
            case (in.InferredTypeRef(), Some(out.Body(stmts))) => { // Implicit type.
                stmts.last.ty
            }
            
            case (in.InferredTypeRef(), None) => {
                globalreport(
                    decl.tref.pos, "explicit.type.reqd.if.abstract", decl.name.toString
                )
                Type.Object
            }
        }
        out.FieldDecl(
            annotations = decl.annotations.map(InEnv(env).lowerAnnotation),
            name = decl.name,
            tref = out.TypeRef(ty),
            optBody = optBody
        )        
    })
    
    // ___ Parameters _______________________________________________________
    
    // Method parameters always have a specified type.  
    def lowerMethodParams(classEnv: Env, inParams: List[in.Param]) = {
        lowerAnyParams(classEnv, inParams.map(p => (Type.Object, p)))
    }
    
    // The type of block parameters can be inferred from context.
    def lowerBlockParam(env: Env, expTy: Type.Ref, inParam: in.Param) = {
        val (List(outParam), env1) = lowerAnyParams(env, List((expTy, inParam)))
        (outParam, env1)
    }
    
    def lowerAnyParams(env0: Env, inputs: List[(Type.Ref, in.Param)]): (List[out.Param], Env) = {
        var env = env0
        
        def lowerParam(expTy: Type.Ref, param: in.Param): out.Param = withPosOf(param, {
            (expTy, param) match {
                // Unpack singleton tuples:
                case (ty, in.TupleParam(List(p))) => out.TupleParam(List(lowerParam(ty, p)))
                case (Type.Tuple(List(ty)), p) => lowerParam(ty, p)
                
                // Unpack matching tuples:
                case (Type.Tuple(tys), in.TupleParam(params)) if sameLength(tys, params) => {
                    val outParams = tys.zip(params).map { case (t, l) => lowerParam(t, l) }
                    out.TupleParam(outParams)
                }
                
                // If not matching, try to find a bounding type, or just infer Object:
                case (_, in.TupleParam(params)) => {
                    val optTy = env.upperBoundType(expTy).firstSome {
                        case ty @ Type.Tuple(tys) if sameLength(tys, params) => Some(ty)
                        case _ => None
                    }
                    
                    optTy match {
                        case Some(Type.Tuple(tys)) => { // Found one.
                            val outParams = tys.zip(params).map { case (t, l) => lowerParam(t, l) }
                            out.TupleParam(outParams)                            
                        }
                        
                        case _ => { // No match, just infer Object.
                            val outParams = params.map(lowerParam(Type.Object, _))
                            out.TupleParam(outParams)
                        }
                    }
                }
                
                // Otherwise, create a new symbol, using `expTy` as its type if neccessary:
                case (_, in.VarParam(annotations, tref, name, ())) => {
                    val outAnnotations = annotations.map(InEnv(env).lowerAnnotation)
                    val ty = tref match {
                        case in.InferredTypeRef() => expTy
                        case tref: in.ResolveTypeRef => InEnv(env).toTypeRef(tref)
                    }
                    val modifiers = Modifier.forLoweredAnnotations(outAnnotations)
                    val sym = new VarSymbol.Local(modifiers, name.name, ty)
                    env = env.plusLocalVar(sym)
                    out.VarParam(outAnnotations, out.TypeRef(ty), name, sym)
                }
            }
        })
        
        (inputs.map { case (e, p) => lowerParam(e, p) }, env)
    }
    
    // ___ Paths, Types _____________________________________________________
    
    object InEnv {
        def apply(env: Env) = new InEnv(env)
    }
    
    class InEnv(env: Env) {
        
        // ___ Paths ____________________________________________________________
        
        def toTypedPath(path: in.AstPath): Path.Typed = {
            def errorPath(name: String) = {
                val sym = Symbol.errorLocalVar(Name.LocalVar(name), None)
                Path.TypedBase(sym)
            }

            path match {
                case in.PathErr(name) =>
                    errorPath(name)

                case in.PathBase(Ast.LocalName(localName), ()) => {
                    val sym = env.locals(localName)
                    Path.TypedBase(sym)
                }

                case in.PathBase(name @ Ast.MemberName(memberVar), ()) => {
                    val csym = global.csym(memberVar.className)
                    csym.fieldNamed(memberVar) match {
                        case None => {
                            Error.NoSuchMember(csym.toType, memberVar).report(global, name.pos)
                            errorPath(path.toString)
                        }

                        case Some(fsym) if !fsym.modifiers.isStatic => {
                            Error.ExpStatic(memberVar).report(global, path.pos)
                            errorPath(path.toString)               
                        }

                        case Some(fsym) => {
                            Path.TypedBase(fsym)
                        }
                    }
                }

                case in.PathDot(owner, name, (), ()) => {
                    val ownerTypedPath = toTypedPath(owner)
                    env.lookupField(ownerTypedPath.ty, name.name) match {
                        case Left(err) => {
                            err.report(global, name.pos)
                            errorPath(path.toString)
                        }
                        
                        case Right(fsym) if fsym.modifiers.isStatic => {
                            globalreport(path.pos, "qualified.static", fsym.name.toString)
                            Path.TypedBase(fsym)
                        }
                        
                        case Right(fsym) => {
                            Path.TypedField(ownerTypedPath, fsym)
                        }
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
            tref match {
                case in.TupleType(trefs) => Type.Tuple(trefs.map(toTypeRef))
                case in.NullType() => Type.Null
                case in.TypeVar(path, typeVar) => {
                    val typedPath = toTypedPath(path)
                    env.lookupTypeVar(typedPath.ty, typeVar.name) match {
                        case Left(err) => {
                            err.report(global, typeVar.pos)
                            Type.Null
                        }
                        case Right(memberVar) => {
                            Type.Var(typedPath.toPath, memberVar)                            
                        }
                    }
                }
                case in.ClassType(Ast.ClassName(className), inTypeArgs) => {
                    val csym = global.csym(className)
                    val typeArgs = inTypeArgs.flatMap(toOptTypeArgOf(csym))
                    Type.Class(className, typeArgs)
                }                
            }
        }
        
        def toOptTypeArgOf(csym: ClassSymbol)(targ: in.TypeArg) = {
            targ match {
                case in.PathTypeArg(name, rel, inPath) => {
                    env.lookupEntry(csym, name.name) match {
                        case Right(entry) if entry.isConstrainableInPathArg =>
                            Some(Type.PathArg(entry.name, rel, toPath(inPath)))                                
                            
                        case Right(entry) => {
                            globalreport(name.pos, "not.in.path.arg", entry.name.toString)
                            None                                
                        }
                            
                        case Left(err) => {
                            err.report(global, name.pos)
                            None
                        }
                    }
                }

                case in.TypeTypeArg(name, rel, inTypeRef) => {
                    env.lookupEntry(csym, name.name) match {
                        case Right(entry) if entry.isConstrainableInTypeArg =>
                            Some(Type.TypeArg(entry.name, rel, toTypeRef(inTypeRef)))
                            
                        case Right(entry) => {
                            globalreport(name.pos, "not.in.type.arg", entry.name.toString)
                            None                                
                        }

                        case Left(err) => {
                            err.report(global, name.pos)
                            None
                        }
                    }
                }
            }
        }
        
        def lowerTypeRef(tref: in.ResolveTypeRef): out.TypeRef = withPosOf(tref, {
            out.TypeRef(toTypeRef(tref))
        })
        
        // ___  _________________________________________________________________
        
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
            env = InEnvStmt(env, result).appendLoweredStmt(stmt)
        }
        
        result.toList
    }
    
    /** Lowers an Lvalue, using hints from the type of the RHS `rvalueTy`. */
    class LvalueLower(
        var env: Env
    ) {
        def lowerLvalue(rvalueTy: Type.Ref, lvalue: in.Lvalue): out.Lvalue = withPosOf(lvalue, {
            (rvalueTy, lvalue) match {
                // Unpack singleton tuples:
                case (ty, in.TupleLvalue(List(lv))) => lowerLvalue(ty, lv)
                case (Type.Tuple(List(ty)), lv) => lowerLvalue(ty, lv)

                // Unpack matching tuples:
                case (Type.Tuple(tys), in.TupleLvalue(lvalues)) if sameLength(tys, lvalues) => {
                    val outLvalues = tys.zip(lvalues).map { case (t, l) => lowerLvalue(t, l) }
                    out.TupleLvalue(outLvalues)
                }

                // If tuple sizes don't match, just infer NullType:
                case (_, in.TupleLvalue(lvalues)) => {
                    val outLvalues = lvalues.map(lowerLvalue(Type.Null, _))
                    out.TupleLvalue(outLvalues)
                }
                
                // New variable declaration, use type from rhs if none provided:
                case (rhsTy, in.DeclareVarLvalue(anns, tref, name, ())) => {
                    val outAnnotations = anns.map(InEnv(env).lowerAnnotation)
                    val mod = Modifier.forLoweredAnnotations(outAnnotations)
                    val ty = tref match {
                        case in.InferredTypeRef() => rhsTy
                        case inTref: in.ResolveTypeRef => InEnv(env).toTypeRef(inTref)
                    }
                    val sym = new VarSymbol.Local(mod, name.name, ty)
                    env = env.plusLocalVar(sym)
                    out.DeclareVarLvalue(outAnnotations, out.TypeRef(ty), name, sym)
                }
                
                // Reassign local variable:
                case (_, in.ReassignVarLvalue(localName @ Ast.LocalName(name), ())) => {
                    out.ReassignVarLvalue(localName, env.locals(name))
                }
                
                // Reassign field:
                case (_, in.FieldLvalue(memberName @ Ast.MemberName(name), ())) => {
                    val csym = global.csym(name.className)
                    val fsym = csym.fieldNamed(name) match {
                        case Some(fsym) => fsym
                        case None => {
                            Error.NoSuchMember(csym.toType, name).report(global, memberName.pos)
                            Symbol.errorField(name, None)
                        }
                    }
                    out.FieldLvalue(memberName, fsym)
                }
            }
        })
    }
    
    object InEnvStmt {
        def apply(env: Env, stmts: mutable.ListBuffer[out.Stmt]) = 
            new InEnvStmt(env, stmts)
    }
    
    class InEnvStmt(env: Env, stmts: mutable.ListBuffer[out.Stmt]) extends InEnv(env) {
        def appendLoweredStmt(stmt: in.Stmt): Env = {
            stmt match {
                case in.Assign(lvalue, rvalue) => {
                    val optExpTy = optTypeFromLocal(env, lvalue)
                    val outRvalue = lowerExpr(optExpTy)(rvalue)
                    val ll = new LvalueLower(env)
                    val outLvalue = ll.lowerLvalue(outRvalue.ty, lvalue)
                    stmts += withPosOf(stmt, out.Assign(outLvalue, outRvalue))
                    ll.env
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
        def optTypeFromLocal(env: Env, lvalue0: in.Lvalue): Option[Type.Ref] = {
            case class FailedException() extends Exception
            
            def theOldCollegeTry(lvalue: in.Lvalue): Type.Ref = lvalue match {
                case in.TupleLvalue(lvalues) => 
                    Type.Tuple(lvalues.map(theOldCollegeTry))
                case in.DeclareVarLvalue(_, in.InferredTypeRef(), _, ()) =>
                    throw FailedException()
                case in.DeclareVarLvalue(_, tref: in.ResolveTypeRef, _, ()) =>
                    toTypeRef(tref)
                case in.ReassignVarLvalue(Ast.LocalName(localName), ()) =>
                    env.locals(localName).ty
                case in.FieldLvalue(Ast.MemberName(memberName), ()) => {
                    val csym = global.csym(memberName.className)
                    csym.fieldNamed(memberName) match {
                        case Some(fsym) => fsym.ty
                        case None => throw FailedException()
                    }
                }
            }
            
            try {
                Some(theOldCollegeTry(lvalue0))
            } catch {
                case FailedException() => None
            }
        }
        
        def introduceVar(fromExpr: in.Expr, toExpr: out.Expr): out.Var = {
            val name = Name.LocalVar(tmpVarName(fromExpr))
            val sym = new VarSymbol.Local(Modifier.Set.empty, name, toExpr.ty)
            val assign = out.Assign(
                out.DeclareVarLvalue(List(), out.TypeRef(toExpr.ty), Ast.LocalName(name), sym), 
                toExpr
            )
            stmts += withPosOf(fromExpr, assign)
            withPosOf(fromExpr, out.Var(Ast.LocalName(name), sym))
        }
        
        def dummySubst(subst: Subst)(pat: Pattern.Ref, text: String): Subst = pat match {
            case Pattern.Var(name, _) =>
                subst + (name.toPath -> Name.LocalVar(text).toPath)
                
            case Pattern.Tuple(patterns) =>
                patterns.zipWithIndex.foldLeft(subst) { case (s, (p, i)) =>
                    dummySubst(s)(p, "%s[%d]".format(text, i))
                }
        }
        
        def patSubst(subst: Subst)(pat: Pattern.Ref, expr: in.ParseRcvr): Subst = {
            (pat, expr) match {
                case (patTup: Pattern.Tuple, astTup: in.Tuple) if sameLength(patTup.patterns, astTup.exprs) =>
                    patTup.patterns.zip(astTup.exprs).foldLeft(subst) { 
                        case (s, (p, e)) => patSubst(s)(p, e) 
                    }
                
                case (patVar: Pattern.Var, astVar: in.Var) =>
                    subst + (patVar.name.toPath -> astVar.name.name.toPath)
                
                case (patVar: Pattern.Var, _: in.Super) =>
                    subst + (patVar.name.toPath -> Path.This)
                
                case (_, astTup: in.Tuple) if astTup.exprs.length == 1 =>
                    patSubst(subst)(pat, astTup.exprs.head)
                    
                case (patTup: Pattern.Tuple, _) if patTup.patterns.length == 1 =>
                    patSubst(subst)(patTup.patterns.head, expr)
                    
                case _ => 
                    dummySubst(subst)(pat, tmpVarName(expr))
            }
        }
        
        def patSubsts(allPatterns: List[Pattern.Ref], allExprs: List[in.ParseRcvr]): Subst = {
            assert(allPatterns.length == allExprs.length) // Guaranteed syntactically.
            allPatterns.zip(allExprs).foldLeft(Subst.empty) { case (s, (p, e)) =>
                patSubst(s)(p, e)
            }
        }
        
        def mthdSubst(msym: MethodSymbol, rcvr: in.ParseRcvr, args: List[in.ParseTlExpr]) = {
            val msig = msym.msig
            rcvr match {
                case in.Static(_) | in.Super(_) => 
                    patSubsts(msig.parameterPatterns, args)
                case rcvr: in.ParseTlExpr =>
                    patSubsts(msig.thisPattern :: msig.parameterPatterns, rcvr :: args)
            }
        }
    
        def lowerOwner(owner: in.Owner): out.Owner = withPosOf(owner, owner match {
            case in.Static(className) => out.Static(className)
            case expr: in.Expr => lowerExprToVar(None)(expr)
        })
        
        def lowerField(optExpTy: Option[Type.Ref])(expr: in.Field) = introduceVar(expr, { 
            lowerOwner(expr.owner) match {
                // Static field ref. like System.out:
                case owner @ out.Static(className) => {
                    val memberVar = expr.name.name match {
                        case Name.ClasslessMember(text) => 
                            Name.Member(className, text)
                            
                        case Name.Member(className1, text) => {
                            if(className != className1) {
                                Error.DiffStaticClasses(className, className1).report(global, expr.name.pos)
                            }
                            Name.Member(className, text)
                        }
                    }
                    val csym = global.csym(className)
                    val fsym = csym.fieldNamed(memberVar) match {
                        case Some(fsym) if fsym.modifiers.isStatic => {
                            fsym                            
                        }
                        case Some(fsym) /* !Static */ => {
                            Error.ExpStatic(memberVar).report(global, expr.name.pos)
                            Symbol.errorField(memberVar, optExpTy)
                        }
                        case None => {
                            Error.NoSuchMember(csym.toType, expr.name.name).report(global, expr.name.pos)
                            Symbol.errorField(memberVar, optExpTy)
                        }
                    }
                    out.Field(owner, Ast.MemberName(fsym.name), fsym, fsym.ty)
                }

                // Instance field ref like foo.bar:
                case owner @ out.Var(_, sym) => {
                    val fsym = env.lookupField(sym.ty, expr.name.name) match {
                        case Right(fsym) if !fsym.modifiers.isStatic => {
                            fsym
                        }
                        case Right(fsym) /* Static */ => {
                            Error.QualStatic(fsym.name).report(global, expr.name.pos)
                            fsym
                        }
                        case Left(err) => {
                            err.report(global, expr.name.pos)
                            val memberVar = expr.name.name.inDefaultClass(Name.ObjectClass)
                            Symbol.errorField(memberVar, optExpTy)
                        }
                    }
                    val subst = Subst(Path.This -> sym.name.toPath)
                    out.Field(owner, Ast.MemberName(fsym.name), fsym, subst.ty(fsym.ty))
                }
            }
        })
        
        def lowerLiteralExpr(expr: in.Literal) = introduceVar(expr, {
            val ty = Type.Class(Name.Class(expr.obj.getClass), List())
            out.Literal(expr.obj, ty)
        })
        
        def identifyBestMethod(
            pos: Position,
            msyms: List[MethodSymbol],
            name: Name.Method,
            rcvrTy: Type.Ref,
            inRcvr: in.Rcvr,
            inArgs: List[in.Expr]
        ) = {
            // Identify the best method (if any):
            msyms match {
                case List() => {
                    Error.NoSuchMethod(rcvrTy, name).report(global, pos)
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
                    def potentiallyApplicable(msym: MethodSymbol) = {
                        val subst = mthdSubst(msym, inRcvr, inArgs)
                        val parameterTys = msym.msig.parameterPatterns.map(p => subst.ty(p.ty))
                        // FIXME Add suitable temps to the environment for the vars ref'd in subst.
                        argTys.zip(parameterTys).forall { case (p, a) => 
                            env.isSuitableArgument(p, a) 
                        }
                    }
                    val applicableMsyms = msyms.filter(potentiallyApplicable)
                    
                    // Try to find an unambiguously "best" choice:
                    def isBetterChoiceThan(msym_better: MethodSymbol, msym_worse: MethodSymbol) = {
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
                    def isBestChoice(msym: MethodSymbol) = {
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
                            globalreport(
                                pos,
                                "no.applicable.methods",
                                argTys.map(_.toString).mkString(", ")
                            )
                            None
                        }
                        
                        case _ => {
                            globalreport(
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
            val (rcvr, rcvrTy, msyms) = lowerRcvr(mcall.rcvr, mcall.name) match {
                case rcvr @ out.Static(className) => {
                    val csym = global.csym(className)
                    val msyms = csym.methodsNamed(mcall.name).filter(_.modifiers.isStatic)
                    (rcvr, csym.toType, msyms)
                }
                
                case rcvr @ out.Super(ty) => {
                    (rcvr, ty, env.lookupInstanceMethods(ty, mcall.name))
                }
                
                case rcvr @ out.Var(_, sym) => {
                    (rcvr, rcvr.ty, env.lookupInstanceMethods(sym.ty, mcall.name))                    
                }
            }
            val best = identifyBestMethod(
                mcall.pos, msyms, mcall.name, 
                rcvrTy, mcall.rcvr, mcall.args)
            best match {
                case Some((args, (msym, msig))) =>
                    out.MethodCall(rcvr, mcall.name, args, (msym, msig))
                case None =>
                    out.Null(optExpTy.getOrElse(Type.Null))
            }
        })
        
        def lowerNewCtor(expr: in.NewCtor) = introduceVar(expr, {
            toTypeRef(expr.tref) match {
                case ty @ Type.Class(name, _) => {
                    val csym = global.csym(name)
                    val msyms = csym.constructors
                    val tvar = Name.LocalVar(tmpVarName(expr))
                    val rcvr = in.Var(Ast.LocalName(tvar), ())
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
                    globalreport(
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
                
        def lowerBlock(optExpTy: Option[Type.Ref])(tmpl: in.Block) = introduceVar(tmpl, {
            val expArgumentTy = optTypeArg(Name.BlockA, optExpTy).getOrElse(Type.Void)
            
            val (outParam, subenv) = lowerBlockParam(env, expArgumentTy, tmpl.param)
            val outStmts = lowerStmts(subenv, tmpl.stmts)
            
            // This commented code extracts the expected return type from `optExpTy`: we decided
            // instead to extract the expected return type from `outStmts`.  As a side
            // comment, note that the `this` pointer inside an interval template does
            // not change, unlike an inner or anonymous class.
            //
            // val expReturnTy = optTypeArg(Name.BlockR, optExpTy).getOrElse(Type.Void)
            // val (outReturnTypeRef, returnTy) = tmpl.returnTref match {
            //     case tref: in.TypeRef => (lowerTypeRef(tref), symbolType(tref))
            //     case in.InferredTypeRef() => (astType(env)(tmpl.returnTref, expReturnTy), expReturnTy)
            // }
            
            val returnTy = tmpl.returnTref match {
                case in.InferredTypeRef() => outStmts.last.ty
                case tref: in.ResolveTypeRef => toTypeRef(tref)
            }
            
            out.Block(
                async = tmpl.async,
                returnTref = out.TypeRef(returnTy),
                param = outParam,
                stmts = outStmts,
                ty = Type.Class(tmpl.className, List(
                    Type.PathArg(Name.BlockParent, PcEq, Path.Method),
                    Type.TypeArg(Name.BlockR, TcEq, returnTy),
                    Type.TypeArg(Name.BlockA, TcEq, outParam.ty)
                ))
            )
        })
        
        def lowerImpThis(expr: in.Expr) = withPosOf(expr, {
            val sym = env.lookupThis
            out.Var(Ast.LocalName(Name.ThisLocal), sym)
        })
        
        def lowerCast(expr: in.Cast) = withPosOf(expr, {
            val ty = toTypeRef(expr.typeRef)
            out.Cast(
                lowerExpr(Some(ty))(expr),
                out.TypeRef(ty)
            )
        })
    
        def lowerRcvr(rcvr: in.Rcvr, mthdName: Name.Method): out.Rcvr = withPosOf(rcvr, rcvr match {
            case rcvr: in.Owner => lowerOwner(rcvr)
            case in.Super(()) => {
                // Find the next supertype in MRO that implements the method
                // `mthdName` (if any):
                val mro = MethodResolutionOrder(global).forSym(env.thisCsym)
                val optTy = mro.firstSome { 
                    case csym if !csym.methodsNamed(mthdName).isEmpty => 
                        Some(csym.toType)
                    
                    case csym => 
                        None
                }
                optTy match {
                    case Some(ty) => out.Super(ty)
                    case None => {
                        globalreport(
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