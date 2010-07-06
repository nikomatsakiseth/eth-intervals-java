package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.util.parsing.input.Position
import com.smallcultfollowing.lathos.model.Context

import Ast.{Resolve => in}
import Ast.{Lower => out}
import Ast.Lower.Extensions._
import Util._

/** Lowers the IR to what is expected by the type check.  This has two
  * main functions:
  * - Fills in any inferred types.  (Note that we don't perform a full type check!)
  * - Removes nested expressions into intermediate variables. */
case class Lower(global: Global) {
    val log = global.closestLog
    private[this] val emptyEnv = Env.empty(global)
    
    def classParamAndEnv(csym: ClassFromSource): (out.Param[VarSymbol.Field], Env) = {
        val thisTy = Type.Class(csym.name, List())
        val thisSym = new VarSymbol.Local(csym.pos, Modifier.Set.empty, Name.ThisLocal, thisTy)
        val env0 = emptyEnv.plusThis(thisTy, thisSym)
        val cdecl = csym.resolvedSource
        val (outParam, env) = lowerClassParam(csym, env0, cdecl.pattern)
        (outParam, env)
    }
    
    def createSymbolForConstructor(csym: ClassFromSource) = {
        val cdecl = csym.resolvedSource
        val msym = new MethodSymbol(
            pos       = cdecl.pattern.pos,
            modifiers = Modifier.Set.empty,
            kind      = MethodKind.HarmonicCtor,
            clsName   = csym.name,
            name      = Name.InitMethod,
            elaborate = csym.create,
            gather    = csym.gather,
            msig      = MethodSignature(
                returnTy = Type.Void,
                parameterPatterns = List(csym.classParam.toPatternRef)
            )
        )
        
        // TODO Add requirements and ensures from class
        // TODO Add ensures that equate each argument x to a field this.x
        msym.Requirements.v = Nil
        msym.Ensures.v = Nil
        
        msym
    }
    
    def lowerRelDecl(
        csym: ClassFromSource, 
        decl: in.RelDecl        
    ): out.RelDecl = withPosOf(decl, out.RelDecl(
        annotations = decl.annotations.map(InEnv(csym.classEnv).lowerAnnotation),
        left = InEnv(csym.classEnv).lowerPath(decl.left),
        kind = decl.kind,
        right = InEnv(csym.classEnv).lowerPath(decl.right)
    ))
    
    def lowerIntervalDecl(
        csym: ClassFromSource, 
        decl: in.IntervalDecl
    ): out.IntervalDecl = withPosOf(decl, out.IntervalDecl(
        annotations = decl.annotations.map(InEnv(csym.classEnv).lowerAnnotation),
        name = decl.name,
        parent = InEnv(csym.classEnv).lowerPath(decl.parent),
        body = lowerBody(csym.classEnv, decl.body)
    ))
    
    def addNullStmtToStmts(stmts: List[out.Stmt]) = {
        if(stmts.last.ty != Type.Void) {
            stmts :+ withPosOf(stmts.last, out.Null(Type.Void))
        } else {
            stmts
        }
    }
    
    def addNullStmtToBody(body: out.Body) = withPosOf(body, {
        out.Body(addNullStmtToStmts(body.stmts))
    })
    
    def inferredReturnType(env: Env, stmts: List[out.Stmt]) = {
        val retTypes = stmts.flatMap {
            case out.MethodReturn(path) => Some(path.ty)
            case _ => None
        }
        
        // TODO Have to eliminate type vars and things that go out of scope
        if(retTypes.isEmpty)
            Type.Void
        else
            env.mutualUpperBoundOfList(retTypes)
    }
    
    def lowerAbstractableBody(
        env: Env,
        body: in.AbstractableBody
    ) = withPosOf(body, {
        body match {
            case in.AbstractBody() => out.AbstractBody()
            case body: in.Body => lowerBody(env, body)
        }
    })
    
    def lowerMethodDecl(
        csym: ClassFromSource, 
        mdecl: in.MethodDecl,
        outParams: List[out.Param[VarSymbol.Local]],
        env: Env
    ): out.MethodDecl = {
        val absBody = lowerAbstractableBody(env, mdecl.body)

        val returnTy = (mdecl.returnTref, absBody) match {
            case (in.InferredTypeRef(), out.AbstractBody()) => {
                Error.ExplicitTypeRequiredIfAbstract(mdecl.name).report(global, mdecl.returnTref.pos)
                Type.Top
            }

            case (in.InferredTypeRef(), out.Body(stmts)) => {
                inferredReturnType(env, stmts)
            }

            case (tref: in.ResolveTypeRef, _) => {
                InEnv(env).toTypeRef(tref) 
            }
        }

        out.MethodDecl(
            annotations  = mdecl.annotations.map(InEnv(env).lowerAnnotation),
            name         = mdecl.name,
            params       = outParams,
            returnTref   = out.TypeRef(returnTy),
            requirements = mdecl.requirements.map(InEnv(env).lowerRequirement),
            ensures      = mdecl.ensures.map(InEnv(env).lowerRequirement),
            body         = absBody                
        )
    }
    
    def lowerFieldDecl(
        csym: ClassFromSource, 
        decl: in.FieldDecl
    ): out.FieldDecl = withPosOf(decl, {
        val env = csym.classEnv
        val body = lowerBody(env, decl.body)
        val ty = decl.tref match {
            case tref: in.ResolveTypeRef => InEnv(env).toTypeRef(tref)
            case in.InferredTypeRef() => body.stmts.last.ty
        }
        out.FieldDecl(
            annotations = decl.annotations.map(InEnv(env).lowerAnnotation),
            name = decl.name,
            tref = out.TypeRef(ty),
            body = body
        )
    })
    
    // ___ Parameters _______________________________________________________
    
    // Method parameters always have a specified type.  
    def lowerClassParam(csym: ClassSymbol, classEnv: Env, inParam: in.Param[Unit]) = {
        def newSym(pos: Position, modifiers: Modifier.Set, name: Name.LocalVar, ty: Type) = 
            new VarSymbol.Field(pos, modifiers, Name.Member(csym.name, name.text), ty, FieldKind.Harmonic)
        def addSym(env: Env, sym: VarSymbol.Field) = env
        val (List(outParam), env1) = lowerAnyParams(newSym, addSym)(classEnv, List((Type.Top, inParam)))
        (outParam, env1)
    }
    
    // Method parameters always have a specified type.  
    def lowerMethodParams(classEnv: Env, inParams: List[in.Param[Unit]]) = {
        def newSym(pos: Position, modifiers: Modifier.Set, name: Name.LocalVar, ty: Type) = 
            new VarSymbol.Local(pos, modifiers, name, ty)
        def addSym(env: Env, sym: VarSymbol.Local) = env.plusLocalVar(sym)
        lowerAnyParams(newSym, addSym)(classEnv, inParams.map(p => (Type.Top, p)))
    }
    
    // The type of block parameters can be inferred from context.
    def lowerBlockParam(env: Env, expTy: Type, inParam: in.Param[Unit]) = {
        def newSym(pos: Position, modifiers: Modifier.Set, name: Name.LocalVar, ty: Type) = 
            new VarSymbol.Local(pos, modifiers, name, ty)
        def addSym(env: Env, sym: VarSymbol.Local) = env.plusLocalVar(sym)
        val (List(outParam), env1) = lowerAnyParams(newSym, addSym)(env, List((expTy, inParam)))
        (outParam, env1)
    }
    
    def lowerAnyParams[S <: VarSymbol.Any](
        newSym: ((Position, Modifier.Set, Name.LocalVar, Type) => S),
        addSym: ((Env, S) => Env)
    )(
        env0: Env, 
        inputs: List[(Type, in.Param[Unit])]
    ): (List[out.Param[S]], Env) = {
        var env = env0
        
        def lowerParam(
            expTy: Type, 
            param: in.Param[Unit]
        ): out.Param[S] = withPosOf(param, {
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
                    val optTy = env.upperBounds(expTy).firstSome {
                        case ty @ Type.Tuple(tys) if sameLength(tys, params) => Some(ty)
                        case _ => None
                    }
                    
                    optTy match {
                        case Some(Type.Tuple(tys)) => { // Found one.
                            val outParams = tys.zip(params).map { case (t, l) => lowerParam(t, l) }
                            out.TupleParam(outParams)                            
                        }
                        
                        case _ => { // No match, just infer Object.
                            val outParams = params.map(lowerParam(Type.Top, _))
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
                    val sym = newSym(name.pos, modifiers, name.name, ty)
                    env = addSym(env, sym)
                    out.VarParam(outAnnotations, out.TypeRef(ty), name, sym)
                }
            }
        })
        
        (inputs.map { case (e, p) => lowerParam(e, p) }, env)
    }
    
    // ___ Substitutions ____________________________________________________
    
    def addPatPathToSubst(subst: Subst, pair: (Pattern.Ref, Path.Ref)): Subst = {
        pair match {
            case (Pattern.Tuple(List(pat)), path) => 
                addPatPathToSubst(subst, (pat, path))
                
            case (pat, Path.Tuple(List(path))) => 
                addPatPathToSubst(subst, (pat, path))
                
            case (Pattern.Tuple(pats), Path.Tuple(paths)) if sameLength(paths, paths) => 
                pats.zip(paths).foldLeft(subst)(addPatPathToSubst)
                
            case (Pattern.Tuple(pats), array) =>
                pats.zipWithIndex.foldLeft(subst) { case (s, (pat, idx)) =>
                    addPatPathToSubst(s, (pat, Path.Index(array, Path.Constant.integer(idx))))
                }
            
            case (Pattern.Var(name, _), path) =>
                subst + (name.toPath -> path)
        }
    }
    
    def addPatExprToSubst(subst: Subst, pair: (Pattern.Ref, in.Expr)): Subst = {
        pair match {
            case (pat, in.Tuple(List(expr))) =>
                addPatExprToSubst(subst, (pat, expr))
                
            case (Pattern.Tuple(List(pat)), expr) =>
                addPatExprToSubst(subst, (pat, expr))
                
            case (Pattern.Tuple(pats), in.Tuple(exprs)) if sameLength(pats, exprs) =>
                pats.zip(exprs).foldLeft(subst)(addPatExprToSubst)
            
            case (pat, expr) => {
                val path = expr match {
                    case in.PathBase(Ast.LocalName(name), ()) =>
                        Path.Local(name)
                    case _ => 
                        Path.Local(Name.LocalVar(tmpVarName(expr)))
                }
                addPatPathToSubst(subst, (pat, path))
            }
        }
    }
    
    def substFromPatExprs(pats: List[Pattern.Ref], asts: List[in.Expr]): Subst = {
        assert(pats.length == asts.length) // Guaranteed syntactically.
        pats.zip(asts).foldLeft(Subst.empty)(addPatExprToSubst)
    }
    
    def mthdSubst(msym: MethodSymbol, rcvr: in.Rcvr, args: List[in.Expr]) = {
        val msig = msym.msig
        rcvr match {
            case in.Static(_) | in.Super(_) => 
                substFromPatExprs(msig.parameterPatterns, args)
            case rcvr: in.Expr =>
                substFromPatExprs(
                    Pattern.Var(Name.ThisLocal, msym.clsName.toType) :: msig.parameterPatterns, 
                    rcvr :: args
                )
        }
    }
    
    // Flattens the method arguments into a list of final paths:
    def flattenAssignment(
        lvalues: List[Pattern.Anon], 
        rvalues: List[out.TypedPath]
    ): List[out.PathNode] = {
        def flattenPair(pos: out.TypedPath)(pair: (Pattern.Anon, Path.Typed)): List[out.TypedPath] = {
            pair match {
                case (Pattern.AnonTuple(List(pat)), path) => {
                    flattenPair(pos)((pat, path))
                }
                
                case (pat, Path.TypedTuple(List(path))) => {
                    flattenPair(pos)((pat, path))
                }
                
                case (Pattern.AnonTuple(pats), Path.TypedTuple(paths)) if sameLength(pats, paths) => {
                    pats.zip(paths).flatMap(flattenPair(pos))
                }
                    
                case (Pattern.AnonTuple(pats), path) => {
                    pats.indices.map({ idx =>
                        Path.TypedIndex(
                            path,
                            Path.TypedConstant.integer(idx)
                        ).toNodeWithPosOf(pos)
                    }).toList
                }
                
                case (Pattern.AnonVar(_), path) => {
                    List(path.toNodeWithPosOf(pos))
                }
            }
        }
        
        lvalues.zip(rvalues).flatMap {
            case (l, r) => flattenPair(r)(l, r.path)
        }
    }
    
    // ___ Paths, Types _____________________________________________________
    
    object InEnv {
        def apply(env: Env) = new InEnv(env)
    }
    
    class InEnv(env: Env) {
        
        // ___ Paths ____________________________________________________________
        
        def typedPathForPath(path: in.PathNode): Path.Typed = {
            def errorPath(name: String) = {
                val sym = VarSymbol.errorLocal(Name.LocalVar(name), None)
                Path.TypedLocal(sym)
            }
            
            def callData(
                name: Name.Method,
                msyms: List[MethodSymbol], 
                rcvrTy: Type, 
                inRcvr: in.Rcvr, 
                inArgs: List[in.PathNode],
                outArgNodes: List[out.TypedPath]
            ): Option[(List[Path.Typed], MethodSymbol)] = {
                val argTys = outArgNodes.map(_.ty)
                msyms match {
                    case List() => {
                        Error.NoSuchMethod(rcvrTy, name).report(global, path.pos)
                        None
                    }
                    
                    case msyms => {
                        def createSubst(msym: MethodSymbol) = mthdSubst(msym, inRcvr, inArgs)
                        resolveOverloading(path.pos, createSubst, msyms, argTys) map { case (msym, msig) =>
                            val flatArgNodes = flattenAssignment(msym.msig.parameterPatterns, outArgNodes)
                            (flatArgNodes.map(_.path), msym)
                        }
                    }
                }
            }

            path match {
                case in.PathErr(name) =>
                    errorPath(name)

                case in.PathBase(Ast.LocalName(localName), ()) => {
                    val sym = env.locals(localName)
                    Path.TypedLocal(sym)
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
                            Path.TypedField(Path.Static, fsym)
                        }
                    }
                }

                case in.PathDot(owner, name, ()) => {
                    val ownerTypedPath = typedPathForPath(owner)
                    env.lookupBean(ownerTypedPath.ty, name.name) match {
                        case Left(err) => {
                            err.report(global, name.pos)
                            errorPath(path.toString)
                        }
                        
                        case Right(Left(fsym)) if fsym.modifiers.isStatic => {
                            Error.QualStatic(fsym.name).report(global, path.pos)
                            Path.TypedField(Path.Static, fsym)
                        }
                        
                        case Right(Left(fsym)) => {
                            Path.TypedField(ownerTypedPath, fsym)
                        }
                        
                        case Right(Right(msym)) => {
                            Path.TypedCall(ownerTypedPath, msym, List())
                        }
                    }
                }
                
                case in.PathBaseCall(className, methodName, inArgs, ()) => {
                    val inRcvr = in.Static(className)
                    val outArgNodes = inArgs.map(lowerPath)
                    val classTy = Type.Class(className, List())
                    val msyms = global.staticMethods(className, methodName)
                    callData(methodName, msyms, classTy, inRcvr, inArgs, outArgNodes) match {
                        case Some((flatArgs, msym)) =>
                            Path.TypedCall(Path.Static, msym, flatArgs)
                        case None => 
                            errorPath(path.toString)
                    }
                }
                
                case in.PathCall(inRcvr, methodName, inArgs, ()) => {
                    val outRcvr = typedPathForPath(inRcvr)
                    val outArgNodes = inArgs.map(lowerPath)
                    val msyms = env.lookupInstanceMethods(outRcvr.ty, methodName)
                    callData(methodName, msyms, outRcvr.ty, inRcvr, inArgs, outArgNodes) match {
                        case Some((flatArgs, msym)) =>
                            Path.TypedCall(outRcvr, msym, flatArgs)
                        case None =>
                            errorPath(path.toString)
                    }                    
                }
            }
        }
        
        def pathForPath(path: in.PathNode): Path.Ref = {
            typedPathForPath(path).toPath
        }
        
        def lowerPath(path: in.PathNode): out.TypedPath = {
            typedPathForPath(path).toNodeWithPosOf(path)
        }

        // ___ Types ____________________________________________________________
        
        def toTypeRef(tref: in.ResolveTypeRef): Type = {
            tref match {
                case in.TupleType(trefs) => Type.Tuple(trefs.map(toTypeRef))
                case in.NullType() => Type.Null
                case in.TypeVar(path, typeVar) => {
                    val typedPath = typedPathForPath(path)
                    env.lookupTypeVar(typedPath.ty, typeVar.name) match {
                        case Left(err) => {
                            err.report(global, typeVar.pos)
                            Type.Null
                        }
                        case Right(memberVar) => {
                            Type.Member(typedPath.toPath, memberVar)                            
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
                            Some(Type.PathArg(entry.name, rel, pathForPath(inPath)))                                
                            
                        case Right(entry) => {
                            Error.NotLegalInPathArg(entry.name).report(global, name.pos)
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
                            Error.NotLegalInTypeArg(entry.name).report(global, name.pos)
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
        
        def lowerRequirement(req: in.Requirement) = withPosOf(req, 
            req match {
                case in.PathRequirement(left, rel, right) =>
                    out.PathRequirement(lowerPath(left), rel, lowerPath(right))
                case in.TypeRequirement(left, rel, right) => 
                    out.TypeRequirement(lowerTypeRef(left), rel, lowerTypeRef(right))
            }
        )

        def lowerAnnotation(ann: in.Annotation) = withPosOf(ann,
            out.Annotation(name = ann.name)
        )
        
        def lowerExtendsArg(arg: in.ExtendsArg): out.TypedPath = withPosOf(arg, {
            arg match {
                case in.PathExtendsArg(path) => lowerPath(path)
                case in.TupleExtendsArg(args) => {
                    val paths = args.map(n => lowerExtendsArg(n).path)
                    Path.TypedTuple(paths).toNode
                }
            }
        })
        
        def lowerExtendsDecl(extendsDecl: in.ExtendsDecl) = withPosOf(extendsDecl, {
            // Lower the argument paths 
            val args = extendsDecl.args.map(lowerExtendsArg)
            val argPaths = args.map(_.path.toPath) // list of untyped paths
            
            // Create a subst. by converting lowered paths to exprs
            val thisSym = env.lookupThis
            def createSubst(msym: MethodSymbol) = {
                val pats = msym.msig.parameterPatterns
                pats.zip(argPaths).foldLeft(Subst.empty)(addPatPathToSubst)
            }
            
            // Find the constructor being invoked (if any)
            val targetCsym = global.csym(extendsDecl.className.name)
            targetCsym.body.join()
            val msyms = targetCsym.constructors
            val (msym, msig) = resolveOverloading(extendsDecl.pos, createSubst, msyms, args.map(_.ty)).getOrElse {
                val errorSym = MethodSymbol.error(Name.InitMethod, targetCsym.name)
                (errorSym, errorSym.msig)
            }
            
            // Finally, convert the extends args to the flattened repr
            // used in the Lower code:
            val flatArgs = flattenAssignment(msym.msig.parameterPatterns, args)
            
            out.ExtendsDecl(
                className = extendsDecl.className,
                args      = flatArgs,
                data      = (msym, msig)
            )
        })
        
        // ___ Method call processing ___________________________________________
        
        /** Given a list of potential symbols, along with the types of the
          * arguments, finds the method which fits the types of the arguments
          * supplied (if any). */
        def resolveOverloading(
            pos: Position,
            createSubst: (MethodSymbol => Subst),
            msyms: List[MethodSymbol],
            argTys: List[Type]
        ): Option[out.MCallData] = {
            // Find those symbols that are potentially applicable
            // to the arguments provided:
            def potentiallyApplicable(msym: MethodSymbol) = {
                val subst = createSubst(msym)
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
                    val subst = createSubst(msym) // must succeed or else would not be appl.
                    val msig = subst.methodSignature(msym.msig)
                    Some((msym, msig))
                }
                
                case (List(), List()) => {
                    Error.NoApplicableMethods(argTys).report(global, pos)
                    None
                }
                
                case _ => {
                    Error.AmbiguousMethodCall(bestMsyms.length).report(global, pos)
                    None
                }
            }        
        }
        
    }
    
    // ___ Lowering Statements ______________________________________________
    
    def tmpVarName(from: Ast.Node) = {
        "(%s@%s:%s)".format(from.getClass.getSimpleName, from.pos.line, from.pos.column)
    }
    
    def lowerBody(env: Env, body: in.Body): out.Body = {
        withPosOf(body, out.Body(lowerStmts(env, body.stmts)))
    }
    
    def lowerStmts(env0: Env, stmts: List[in.Stmt]): List[out.Stmt] = {
        var env = env0
        val result = new mutable.ListBuffer[out.Stmt]()
        
        stmts.foreach { stmt =>
            env = InEnvStmt(env, Some(result)).appendLoweredStmt(stmt)
        }
        
        result.toList
    }
    
    object InEnvStmt {
        def apply(env: Env, optStmts: Option[mutable.ListBuffer[out.Stmt]]) = {
            new InEnvStmt(env, optStmts)            
        } 
    }
    
    class InEnvStmt(env: Env, optStmts: Option[mutable.ListBuffer[out.Stmt]]) extends InEnv(env) {
        def appendLoweredStmt(stmt: in.Stmt): Env = {
            stmt match {
                case stmt: in.Assign => {
                    appendAssign(stmt)
                }
                
                case in.InlineInterval(name, body, ()) => {
                    // TODO Inline intervals should be in scope from the beginning of the method!
                    val sym = new VarSymbol.Local(stmt.pos, Modifier.Set.empty, name.name, Type.InlineInterval)
                    optStmts.foreach(_ += withPosOf(stmt, 
                        out.InlineInterval(name, lowerBody(env, body), sym)
                    ))
                    env
                }
                
                case in.MethodReturn(expr) => {
                    // TODO: We could know the expected return type of the method if it was provided
                    // TODO: Check that return is allowed here (i.e., not field, not async block)
                    val path = lowerToTypedPathNode(None)(expr)
                    optStmts.foreach(_ += withPosOf(stmt,
                        out.MethodReturn(path)
                    ))
                    env
                }
                
                case expr: in.Expr => {
                    optStmts.foreach(_ += lowerExpr(None)(expr))
                    env
                }
            }
        }
        
        /** Given an lvalue, attempts to extract the type which 
          * the lvalue expects to be assigned to it.  This may
          * not be possible if the user has not fully specified
          * the type. In that case, None is returned. */
        def optTypeFromLocal(env: Env, lvalue0: in.Lvalue): Option[Type] = {
            case class FailedException() extends Exception
            
            def theOldCollegeTry(lvalue: in.Lvalue): Type = lvalue match {
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
        
        /** Helper for lowering assignment. Uses explicit or otherwise available types 
          * from the LHS as hints for lowering the RHS. Implicit types on the LHS are
          * derived from the RHS. Generates as a result a flattened set of symbols 
          * being assigned to and expressions being assigned from. */
        def appendAssign(inAssign: in.Assign): Env = {
            var outEnv = env
            val outLvalues = new mutable.ListBuffer[out.ElemLvalue]()
            val outExprs = new mutable.ListBuffer[out.Expr]()

            def fieldSymbol(memberName: Ast.MemberName) = {
                val name = memberName.name
                val csym = global.csym(name.className)
                csym.fieldNamed(name) match {
                    case Some(fsym) => fsym
                    case None => {
                        Error.NoSuchMember(csym.toType, name).report(global, memberName.pos)
                        VarSymbol.errorField(name, None)
                    }
                }                
            }
            
            def lowerDeclaredVar(lv: in.DeclareVarLvalue, ty: Type, rv: out.Expr) = {
                val in.DeclareVarLvalue(anns, _, name, ()) = lv
                val outAnnotations = anns.map(InEnv(outEnv).lowerAnnotation)
                val mod = Modifier.forLoweredAnnotations(outAnnotations)
                val sym = new VarSymbol.Local(name.pos, mod, name.name, ty)
                outEnv = outEnv.plusLocalVar(sym)

                outLvalues += withPosOf(lv, 
                    out.DeclareVarLvalue(outAnnotations, out.TypeRef(ty), name, sym)
                )
                
                outExprs += rv
            }
            
            def lowerReassignedVar(localName: Ast.LocalName, sym: VarSymbol.Local, rv: out.Expr) = {
                outLvalues += withPosOf(localName,
                    out.ReassignVarLvalue(localName, sym)
                )
                outExprs += rv
            }
            
            def lowerReassignedField(memberName: Ast.MemberName, fsym: VarSymbol.Field, rv: out.Expr) = {
                outLvalues += withPosOf(memberName,
                    out.FieldLvalue(memberName, fsym)
                )
                outExprs += rv                
            }
            
            def lowerFromOutExpr(pair: (in.Lvalue, out.Expr)): Unit = {
                pair match {
                    case (in.TupleLvalue(sublvs), rv) => {
                        val path = typedPathForExpr(rv)
                        val outRvs = sublvs.indices.map { idx =>
                            Path.TypedIndex(path, Path.TypedConstant.integer(idx)).toNodeWithPosOf(rv)
                        }
                        sublvs.zip(outRvs).foreach(lowerFromOutExpr)
                    }
                    
                    case (lv @ in.DeclareVarLvalue(_, inTref: in.ResolveTypeRef, _, _), rv) => {
                        val ty = InEnv(outEnv).toTypeRef(inTref)
                        lowerDeclaredVar(lv, ty, rv)
                    }
                    
                    case (lv @ in.DeclareVarLvalue(_, in.InferredTypeRef(), _, _), rv) => {
                        lowerDeclaredVar(lv, rv.ty, rv)
                    }
                    
                    case (lv @ in.ReassignVarLvalue(localName, ()), rv) => {
                        val sym = env.locals(localName.name)
                        lowerReassignedVar(localName, sym, rv)
                    }
                    
                    case (lv @ in.FieldLvalue(memberName, ()), rv) => {
                        val fsym = fieldSymbol(memberName)
                        lowerReassignedField(memberName, fsym, rv)
                    }
                }
            }

            def lowerFromInExpr(pair: (in.Lvalue, in.Expr)): Unit = {
                (pair) match {
                    case (in.TupleLvalue(List(lv)), rv) => lowerFromInExpr(lv, rv)
                    case (lv, in.Tuple(List(rv))) => lowerFromInExpr(lv, rv)

                    case (in.TupleLvalue(lvs), in.Tuple(rvs)) if sameLength(lvs, rvs) => 
                        lvs.zip(rvs).foreach(lowerFromInExpr)

                    case (lv @ in.TupleLvalue(_), rv) => {
                        val expTy = optTypeFromLocal(env, lv)
                        val outRv = lowerExpr(expTy)(rv)
                        lowerFromOutExpr((lv, outRv))
                    }

                    case (lv @ in.DeclareVarLvalue(_, inTref: in.ResolveTypeRef, _, _), rv) => {
                        val ty = InEnv(outEnv).toTypeRef(inTref)
                        val outRv = lowerExpr(Some(ty))(rv)
                        lowerDeclaredVar(lv, ty, outRv)
                    }

                    case (lv @ in.DeclareVarLvalue(_, in.InferredTypeRef(), _, _), rv) => {
                        val path = lowerToTypedPathNode(None)(rv)
                        lowerDeclaredVar(lv, path.ty, path)
                    }

                    case (lv @ in.ReassignVarLvalue(localName, ()), rv) => {
                        val sym = env.locals(localName.name)
                        val outRv = lowerExpr(Some(sym.ty))(rv)
                        lowerReassignedVar(localName, sym, outRv)
                    }

                    case (lv @ in.FieldLvalue(memberName, ()), rv) => {
                        val fsym = fieldSymbol(memberName)
                        val outRv = lowerExpr(Some(fsym.ty))(rv)
                        lowerReassignedField(memberName, fsym, outRv)
                    }
                }
            }

            inAssign.lvalues.zip(inAssign.rvalues).foreach(lowerFromInExpr)
            
            val outAssign = withPosOf(inAssign,
                out.Assign(outLvalues.toList, outExprs.toList)
            )
            
            optStmts.foreach(_ += outAssign)
            
            outEnv
        }

        def lowerField(optExpTy: Option[Type])(expr: in.Field) = withPosOf(expr, {
            expr.owner match {
                // Static field ref. like System.out:
                case in.Static(className) => {
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
                            VarSymbol.errorField(memberVar, optExpTy)
                        }
                        case None => {
                            Error.NoSuchMember(csym.toType, expr.name.name).report(global, expr.name.pos)
                            VarSymbol.errorField(memberVar, optExpTy)
                        }
                    }
                    Path.TypedField(Path.Static, fsym).toNode
                }
                
                // Instance field ref like foo.bar:
                case ownerExpr: in.Expr => {
                    val ownerPath = lowerToTypedPath(None)(ownerExpr)
                    env.lookupBean(ownerPath.ty, expr.name.name) match {
                        case Left(err) => {
                            err.report(global, expr.name.pos)
                            val memberVar = expr.name.name.inDefaultClass(Name.ObjectClass)
                            val fsym = VarSymbol.errorField(memberVar, optExpTy)
                            Path.TypedField(ownerPath, fsym).toNode
                        }
                        case Right(Left(fsym)) if fsym.modifiers.isStatic => {
                            Error.QualStatic(fsym.name).report(global, expr.name.pos)
                            Path.TypedField(ownerPath, fsym).toNode
                        }
                        case Right(Left(fsym)) => {
                            Path.TypedField(ownerPath, fsym).toNode
                        }
                        case Right(Right(msym)) => {
                            Path.TypedCall(ownerPath, msym, List()).toNode
                        }
                    }
                }              
            }
        })
        
        def lowerLiteralExpr(expr: in.Literal) = withPosOf(expr, {
            Path.TypedConstant(expr.obj).toNode
        })
        
        def identifyBestMethod(
            pos: Position,
            msyms: List[MethodSymbol],
            name: Name.Method,
            rcvrTy: Type,
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
                    val outArgs = optExpTys.zip(inArgs).map { case (t,a) => lowerToTypedPathNode(t)(a) }
                    val flatArgs = flattenAssignment(msym.msig.parameterPatterns, outArgs)
                    val msig = subst.methodSignature(msym.msig)
                    Some((flatArgs, msym, msig))
                }
                
                // Multiple matches: have to type the arguments without hints.
                //   In theory, we could try to be smarter (i.e., if all options agree on the
                //   type of a particular argument, etc).
                case _ => {
                    val outArgs = inArgs.map(lowerToTypedPathNode(None))
                    val argTys = outArgs.map(_.ty)
                    def createSubst(msym: MethodSymbol) = mthdSubst(msym, inRcvr, inArgs)
                    resolveOverloading(pos, createSubst, msyms, argTys).map { case (msym, msig) => 
                        val flatArgs = flattenAssignment(msym.msig.parameterPatterns, outArgs)
                        (flatArgs, msym, msig)
                    }
                }
            }            
        }
        
        def lowerMethodCall(optExpTy: Option[Type])(mcall: in.MethodCall) = withPosOf(mcall, {
            def identifyBestFromMcall(msyms: List[MethodSymbol], rcvrTy: Type) =
                identifyBestMethod(mcall.pos, msyms, mcall.name, rcvrTy, mcall.rcvr, mcall.args)
            
            val optRes = mcall.rcvr match {
                case in.Static(className) => {
                    val msyms = global.staticMethods(className, mcall.name)
                    identifyBestFromMcall(msyms, className.toType).map {
                        case (flatArgs, msym, msig) => {
                            val flatPaths = flatArgs.map(_.path)
                            out.TypedPath(
                                Path.TypedCall(Path.Static, msym, flatPaths)
                            )
                        }
                    }
                }
                
                case inRcvr: in.Expr => {
                    val outRcvr = lowerToTypedPath(None)(inRcvr)
                    val msyms = env.lookupInstanceMethods(outRcvr.ty, mcall.name)
                    identifyBestFromMcall(msyms, outRcvr.ty).map {
                        case (flatArgNodes, msym, _) => {
                            out.TypedPath(
                                Path.TypedCall(
                                    outRcvr, msym,
                                    flatArgNodes.map(_.path)
                                )
                            )
                        }
                    }
                }
                
                case in.Super(()) => {
                    val ty = firstSuperClassOfferingMethod(mcall.rcvr.pos, mcall.name)
                    val msyms = env.lookupInstanceMethods(ty, mcall.name)
                    identifyBestFromMcall(msyms, ty).map {
                        case (flatArgNodes, msym, msig) => {
                            out.MethodCall(
                                out.Super(ty),
                                mcall.name,
                                flatArgNodes,
                                (msym, msig)
                            )
                        }
                    }
                }
            }
            
            optRes.getOrElse {
                out.Null(optExpTy.getOrElse(Type.Null))
            }
        })
        
        def lowerNewCtor(expr: in.NewCtor) = withPosOf(expr, {
            toTypeRef(expr.tref) match {
                case ty @ Type.Class(name, _) => {
                    val csym = global.csym(name)
                    val msyms = csym.constructors
                    val tvar = Name.LocalVar(tmpVarName(expr))
                    val rcvr = in.varExpr(tvar)
                    val best = identifyBestMethod(
                        expr.pos, msyms, Name.InitMethod,
                        ty, rcvr, expr.args)
                    best match {
                        case Some((args, msym, msig)) => {
                            out.NewCtor(
                                tref = lowerTypeRef(expr.tref),
                                args = args,
                                data = (msym, msig),
                                ty = ty
                            )
                        }
                        
                        case None => {
                            out.Null(ty)                            
                        }
                    }
                }
                
                case ty => {
                    Error.CanOnlyCreateClasses(ty).report(global, expr.pos)
                    out.Null(ty)
                }
            }
        })
        
        def lowerNull(optExpTy: Option[Type])(expr: in.Null) = withPosOf(expr, {
            val ty = optExpTy.getOrElse(Type.Null)
            out.Null(ty)
        })

        def lowerTuple(optExpTy: Option[Type])(tuple: in.Tuple) = withPosOf(tuple, {
            val paths = optExpTy match {
                case Some(Type.Tuple(tys)) if sameLength(tys, tuple.exprs) => {
                    tys.zip(tuple.exprs).map { case (t, e) => 
                        lowerToTypedPath(Some(t))(e)
                    }
                }
                case _ => {
                    tuple.exprs.map(lowerToTypedPath(None))
                }
            }
            Path.TypedTuple(paths).toNode
        })
        
        def optTypeArg(TypeVarName: Name.Var, optExpTy: Option[Type]) = optExpTy match {
            case Some(Type.Class(_, typeArgs)) => {
                typeArgs.firstSome {
                    case Type.TypeArg(TypeVarName, TcEq, ty) => Some(ty)
                    case Type.TypeArg(TypeVarName, TcSub, ty) => Some(ty)
                    case _ => None
                }
            }
            case _ => None
        }
                
        def lowerBlock(optExpTy: Option[Type])(tmpl: in.Block) = withPosOf(tmpl, {
            val expArgumentTy = optTypeArg(Name.BlockA, optExpTy).getOrElse(Type.Void)
            val (outParam, subenv) = lowerBlockParam(env, expArgumentTy, tmpl.param)
            val outStmts0 = lowerStmts(subenv, tmpl.stmts)
            
            // Add a "null" statement to the end if the expected return type is Void.
            val outStmts = optTypeArg(Name.BlockR, optExpTy) match {
                case Some(Type.Void) => addNullStmtToStmts(outStmts0)
                case _ => outStmts0
            }

            // Infer return type based on the last statement in `outStmts`
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
            Path.TypedLocal(env.lookupThis).toNode
        })
        
        def lowerCast(expr: in.Cast) = withPosOf(expr, {
            Path.TypedCast(
                toTypeRef(expr.typeRef),
                lowerToTypedPath(None)(expr.expr)
            ).toNode
        })
    
        def firstSuperClassOfferingMethod(pos: Position, mthdName: Name.Method): Type = {
            // Find the next supertype in MRO that implements the method
            // `mthdName` (if any):
            val mro = env.thisCsym.mro
            val optTy = mro.firstSome { 
                case csym if !csym.methodsNamed(mthdName).isEmpty => 
                    Some(csym.toType)
                
                case csym => 
                    None
            }
            optTy.getOrElse {
                Error.NoSuperClassImplements(mthdName).report(global, pos)
                Type.Top
            }
        }
        
        def lowerExpr(optExpTy: Option[Type])(expr: in.Expr): out.Expr = expr match {
            case in.TypedPath(path) => out.TypedPath(path) // TODO Refine types further to elim this case
            case e: in.Tuple => lowerTuple(optExpTy)(e)
            case e: in.Block => lowerBlock(optExpTy)(e)
            case e: in.Cast => lowerCast(e)
            case e: in.PathNode => lowerPath(e)
            case e: in.Literal => lowerLiteralExpr(e)
            case e: in.Field => lowerField(optExpTy)(e)
            case e: in.MethodCall => lowerMethodCall(optExpTy)(e)
            case e: in.NewCtor => lowerNewCtor(e)
            case e: in.Null => lowerNull(optExpTy)(e)
            case e: in.ImpVoid => out.Null(Type.Void)
            case e: in.ImpThis => lowerImpThis(e)
        }
        
        def lowerToTypedPath(optExpTy: Option[Type])(expr: in.Expr): Path.Typed = {
            typedPathForExpr(lowerExpr(optExpTy)(expr))
        }
        
        def lowerToTypedPathNode(optExpTy: Option[Type])(expr: in.Expr): out.TypedPath = {
            typedPathNodeForExpr(lowerExpr(optExpTy)(expr))
        }

        def typedPathForExprFilter(filter: (Path.Typed => Boolean))(outExpr: out.Expr): Path.Typed = {
            outExpr match {
                // Extract paths meeting the filter:
                case out.TypedPath(outPath) if filter(outPath) => outPath
                
                // Store everything else into a variable and return that:
                case _ => {
                    val name = Name.LocalVar(tmpVarName(outExpr))
                    val sym = new VarSymbol.Local(outExpr.pos, Modifier.Set.empty, name, outExpr.ty)
                    val assign = withPosOf(outExpr, 
                        out.Assign(
                            List(
                                out.DeclareVarLvalue(
                                    List(), 
                                    out.TypeRef(outExpr.ty), 
                                    Ast.LocalName(name), 
                                    sym
                                )
                            ),
                            List(outExpr)
                        )
                    )
                    optStmts.foreach(_ += assign)
                    Path.TypedLocal(sym)
                }
            }
        }
        
        // TODO: Refactor type system to prove that atomic paths are composed only of atomic comp.
        def isAtomicPath(path: Path.Typed) = path match {
            case Path.TypedLocal(_) => true
            case Path.TypedTuple(_) => true
            case Path.TypedConstant(_) => true
            case _ => false
        }
        
        def typedPathForExpr(outExpr: out.Expr) = {
            typedPathForExprFilter(isAtomicPath)(outExpr)
        }
        
        def typedPathNodeForExpr(outExpr: out.Expr): out.TypedPath = {
            typedPathForExpr(outExpr).toNodeWithPosOf(outExpr)
        }
        
        
    }

}