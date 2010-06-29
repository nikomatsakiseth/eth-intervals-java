package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.util.parsing.input.Position

import Ast.{Lower => in}
import Ast.Lower.Extensions._
import Util._

case class Check(global: Global) {
    private[this] val emptyEnv = Env.empty(global)
    
    object In {
        def apply(env: Env, current: Path.Typed) = new In(env, current)
    }
    
    class In(env: Env, current: Path.Typed) {
        
        object At {
            def apply(node: Ast.Node) = new At(node)
        }

        class At(node: Ast.Node) {
            
            def checkTypeWf(ty: Type.Ref): Unit = {
                if(!env.typeIsFinalBy(ty, current)) {
                    Error.TypeNotFinal(ty).report(global, node.pos)
                }
            }
            
            def checkPathHasType(path: Path.Typed, ty: Type.Ref): Unit = {
                if(!env.pathHasType(path, ty)) {
                    Error.MustHaveType(path, ty).report(global, node.pos)
                }
            }
            
            def checkOwner(owner: Path.TypedOwner): Unit = {
                owner match {
                    case Path.Static =>
                    case owner: Path.Typed => checkPath(owner)
                }
            }
            
            def checkParamType(pair: (Pattern.Anon, Path.Typed)): Unit = {
                checkPathHasType(pair._2, pair._1.ty)
            }
            
            def checkPath(path: Path.Typed): Unit = {
                path match {
                    case Path.TypedLocal(sym) => {
                        // TODO Check that guard is readable.
                    }
                    case Path.TypedCast(_, castedPath) => {
                        checkPath(castedPath)
                    }
                    case Path.TypedConstant(_) => {
                    }
                    case Path.TypedField(base, sym) => {
                        checkOwner(base)
                        // TODO Check that guard is readable.
                    }
                    case path @ Path.TypedCall(rcvr, msym, args) => {
                        checkOwner(rcvr)
                        args.foreach(checkPath)
                        path.msig.parameterPatterns.zip(args).foreach(checkParamType)
                    }
                    case Path.TypedIndex(array, index) => {
                        checkPath(array)
                        checkPath(index)
                    }
                    case Path.TypedTuple(paths) => {
                        paths.foreach(checkPath)
                    }
                }
            }
            
            def checkPathAndType(path: Path.Typed, ty: Type.Ref): Unit = {
                checkPath(path)
                checkPathHasType(path, ty)
            }
            
        }
        
        def checkPath(path: in.TypedPath): Unit = {
            At(path).checkPath(path.path)
        }

        def checkPathAndType(path: in.TypedPath, ty: Type.Ref): Unit = {
            At(path).checkPathAndType(path.path, ty)
        }

        def checkReq(req: in.Requirement) = {
            req match {
                case in.PathRequirement(left, PcEq, right) => {
                    checkPath(left)
                    checkPath(right)                    
                }
                
                case in.PathRequirement(left, _, right) => {
                    checkPathAndType(left, Type.Guard)
                    checkPathAndType(right, Type.Interval)
                }
            }
        }
        
        def checkParam(pair: (Pattern.Anon, in.TypedPath)): Unit = {
            val (pattern, path) = pair
            At(path).checkPath(path.path)
            At(path).checkPathHasType(path.path, pattern.ty)
        }
        
        def checkExpr(expr: in.LowerTlExpr): Type.Ref = {
            expr match {
                case in.TypedPath(path) => {
                    At(expr).checkPath(path)
                    path.ty
                }
                case in.MethodCall(in.Super(_), name, args, (msym, msig)) => {
                    msig.parameterPatterns.zip(args).foreach(checkParam)
                    msig.returnTy
                }
                case in.NewCtor(_, args, (msym, msig), ty) => {
                    msig.parameterPatterns.zip(args).foreach(checkParam)
                    msig.returnTy
                }
                case in.Null(ty) => {
                    ty
                }
                case in.Block(async, in.TypeRef(returnTy), param, stmts, ty) => {
                    // TODO Create a fresh interval
                    //      Populate environment with appropriate relations
                    //      Check stmts in that interval
                    ty
                }
            }
        }
        
    }
    
    def checkAndAddReq(current: Path.Typed)(env: Env, req: in.Requirement) = {
        In(env, current).checkReq(req)
        
        req match {
            case in.PathRequirement(left, _, right) => {
                // TODO Require an annotation to add between non-final paths?
                if(
                    env.pathIsFinalBy(left.path, current) &&
                    env.pathIsFinalBy(right.path, current)
                ) {
                    env.plusRel(req.toReq)
                } else {
                    env
                }
            }
            
            case in.TypeRequirement(_, _, _) => {
                env.plusRel(req.toReq)
            }
        }
    }
    
    def checkAndAddAssign(current: Path.Typed)(env: Env, pair: (in.Lvalue, in.Expr)): Env = {
        val (lv, rv) = pair
        val ty = In(env, current).checkExpr(rv)
        val chk = In(env, current).At(rv)
        lv match {
            case in.DeclareVarLvalue(_, _, _, sym) => {
                chk.checkPathHasType(Path.TypedLocal(sym), ty)
                env.plusLocalVar(sym)
            }
                
            case in.ReassignVarLvalue(_, sym) => {
                chk.checkPathHasType(Path.TypedLocal(sym), ty)
                env
            }
            
            case in.FieldLvalue(_, sym) => {
                // TODO Check that dependent fields are assigned together.
                val path = env.lookupThis.toTypedPath / sym
                chk.checkPathHasType(path, ty)
                env
            }
        }
    }
        
    def checkAndAddStmt(current: Path.Typed)(env: Env, stmt: in.LowerStmt): Env = {
        stmt match {
            case stmt: in.LowerTlExpr => {
                In(env, current).checkExpr(stmt)
                env
            }
            
            case stmt @ in.Assign(lvs, rvs) => {
                lvs.zip(rvs).foldLeft(env)(checkAndAddAssign(current))
            }
            
            case stmt @ in.InlineInterval(name, in.Body(stmts), vsym) => {
                // Note: vsym is already in the environment.
                stmts.foldLeft(env)(checkAndAddStmt(vsym.toTypedPath))
            }
            
            case stmt @ in.MethodReturn(in.TypedPath(path)) => {
                env.optReturnTy match {
                    case None => Error.NoReturnHere().report(global, stmt.pos)
                    
                    case Some(returnTy) => {
                        In(env, current).At(stmt).checkPathAndType(path, returnTy)
                    }
                }
                env
            }
        }
    }
    
    def checkStmts(current: Path.Typed, env: Env, stmts: List[in.LowerStmt]): Unit = {
        stmts.foldLeft(env)(checkAndAddStmt(current))
    }
    
    def checkIntervalDecl(
        csym: ClassFromSource, 
        fsym: VarSymbol.Field, 
        decl: in.IntervalDecl
    ) = {
        val env = csym.checkEnv
        val thisPath = csym.loweredSource.thisSym.toTypedPath
        val initPath = env.typedPath(Path.ThisInit)
        In(env, initPath).checkPathAndType(decl.parent, Type.Interval)
        checkStmts(thisPath / fsym, csym.checkEnv, decl.body.stmts)
    }
    
    def checkMethodDecl(
        csym: ClassFromSource, 
        msym: MethodSymbol,
        decl: in.MethodDecl
    ) = {
        var env = csym.checkEnv
        
        // Create the "method" variable:
        val methodSym = new VarSymbol.Local(
            decl.pos,
            Modifier.Set.empty,
            Name.MethodLocal,
            Type.Interval
        )
        env = env.plusLocalVar(methodSym)
        val methodPath = methodSym.toTypedPath
        
        // Add in the requirements:
        //     Note that these may reference parameters.
        //     I put them in here so that they can be used to 
        //     validate the types of the parameters.  Is this OK?
        env = decl.requirements.foldLeft(env)(checkAndAddReq(methodPath))
        
        // Add in the parameters, check that types are WF: 
        decl.params.flatMap(_.varParams).foreach { vp =>
            In(env, methodPath).At(vp).checkTypeWf(vp.tref.ty)
            env = env.plusLocalVar(vp.sym)
        }
        
        // Check the statements in the body in the resulting environment:
        decl.body match {
            case in.Body(stmts) => {
                checkStmts(methodPath, env, stmts)
                
                // Check that ensures clauses are met:
                decl.ensures.foreach { reqNode =>
                    val req = reqNode.toReq
                    if(!env.relHolds(req)) {
                        Error.DoesNotEnsure(req).report(global, reqNode.pos)
                    }
                }
            }
            case in.AbstractBody() =>
        }
    }
    
    def checkFieldDecl(
        csym: ClassFromSource, 
        fsym: VarSymbol.Field,
        decl: in.FieldDecl
    ) = {
        
    }
    
    def checkRelDecl(
        csym: ClassFromSource, 
        decl: in.RelDecl
    ) = {
        
    }
    
    def classSymbol(csym: ClassFromSource) {
        inlineInterval("%s.check.inline".format(csym.name)) { inter =>

            csym.lowerMembers.foreach {
                case mem: LowerIntervalMember => {
                    checkIntervalDecl(csym, mem.sym, mem.memberDecl)
                }
                
                case mem: LowerMethodMember => {
                    checkMethodDecl(csym, mem.sym, mem.memberDecl)
                }
                
                case mem: LowerFieldMember => {
                    checkFieldDecl(csym, mem.sym, mem.memberDecl)
                }
                
                case mem: LowerRelDecl => {
                    checkRelDecl(csym, mem.memberDecl)
                }
                
                case mem => {
                    throw new RuntimeException("Unhandled member kind: %s".format(mem.memberDecl))
                }
            }
            
        }
    }

    
}