package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.util.parsing.input.Position

import com.smallcultfollowing.lathos.Lathos

import Ast.{Lower => in}
import Ast.Lower.Extensions._
import Util._

case class Check(global: Global) {
    val log = Lathos.context
    private[this] val emptyEnv = Env.empty(global)
    
    object In {
        def apply(env: Env, current: SPath[Phantasmal]) = new In(env, current)
    }
    
    class In(env: Env, current: SPath[Phantasmal]) {
        
        object At {
            def apply(node: Ast.Node) = new At(node)
        }

        class At(node: Ast.Node) {
            
            def checkTypeWf(ty: Type): Unit = {
                if(!env.typeIsFinalBy(ty, current)) {
                    Error.TypeNotFinal(ty).report(global, node.pos)
                }
            }
            
            def checkIsSubtype(subTy: Type, supTy: Type): Unit = {
                if(!env.isSubtype(subTy, supTy)) {
                    Error.MustBeSubtype(subTy, supTy).report(global, node.pos)
                }
            }
            
            def checkHasClass(path: SPath[Phantasmal], className: Name.Class): Unit = {
                if(!env.hasClass(path, className)) {
                    Error.MustHaveClass(path, className).report(global, node.pos)
                }
            }
            
            def checkPermitsWr(guardPath: Path): Unit = {
                if(!env.permitsWr(guardPath, current.toPath)) {
                    Error.DoesNotPermitWritesFrom(guardPath, current.toPath).report(global, node.pos)
                }
            }
            
            def checkPermitsRd(guardPath: Path): Unit = {
                if(!env.permitsRd(guardPath, current.toPath)) {
                    Error.DoesNotPermitReadsFrom(guardPath, current.toPath).report(global, node.pos)
                }
            }
            
            def checkIsAssignable(path: SPath[Reified], ty: Type): Unit = {
                if(!env.isAssignable(path, ty)) {
                    Error.MustHaveType(path, ty).report(global, node.pos)
                }
            }
            
            def checkOwner(owner: SPath.Owner[Reified]): Unit = {
                owner match {
                    case SPath.Static =>
                    case owner: SPath[Reified] => checkEvalPath(owner)
                }
            }
            
            def checkParamType(pair: (Type, SPath[Reified])): Unit = {
                checkIsAssignable(pair._2, pair._1)
            }
            
            def checkEvalPath(path: SPath[Reified]): Unit = {
                path match {
                    case SPath.Local(sym) => {
                        checkPermitsRd(sym.guardPath)
                    }
                    case SPath.Cast(_, castedPath) => {
                        checkEvalPath(castedPath)
                    }
                    case SPath.Constant(_) => {
                    }
                    case path @ SPath.Field(base, sym) => {
                        checkOwner(base)
                        checkPermitsRd(path.guardPath)
                    }
                    case path @ SPath.StaticCall(msym, args) => {
                        args.foreach(checkEvalPath)
                        path.msig.flatParamTypes.zip(args).foreach(checkParamType)                        
                    }
                    case path @ SPath.Call(rcvr, msym, args) => {
                        checkEvalPath(rcvr)
                        args.foreach(checkEvalPath)
                        path.msig.flatParamTypes.zip(args).foreach(checkParamType)
                    }
                    case SPath.Index(array, index) => {
                        checkEvalPath(array)
                        checkEvalPath(index)
                    }
                    case SPath.Tuple(paths) => {
                        paths.foreach(checkEvalPath)
                    }
                }
            }

            def checkPathAndClass(path: SPath[Phantasmal], cls: Name.Class): Unit = {
                //checkPath(path)
                checkHasClass(path, cls)
            }
            
            def checkEvalPathAndAssignable(path: SPath[Reified], ty: Type): Unit = {
                checkEvalPath(path)
                checkIsAssignable(path, ty)
            }
            
        }
        
        def checkPathAndClass(path: in.AnyPath, className: Name.Class): Unit = {
            At(path).checkPathAndClass(path.path, className)
        }

        def checkEvalPathAndAssignable(path: in.TypedPath, ty: Type): Unit = {
            log.indent("checkEvalPathAndAssignable(", path, ", ", ty, ")") {
                At(path).checkEvalPathAndAssignable(path.path, ty)
            }
        }

        def checkReq(req: in.Requirement) = {
            req match {
                case in.PathRequirement(left, rel, right) => {
                    checkPathAndClass(left, rel.classes._1)
                    checkPathAndClass(right, rel.classes._2)                    
                }
            }
        }
        
        def checkParam(pair: (Type, in.TypedPath)): Unit = {
            val (ty, path) = pair
            At(path).checkEvalPath(path.path)
            At(path).checkIsAssignable(path.path, ty)
        }
        
        def checkExpr(expr: in.LowerTlExpr): Type = log.indent("checkExpr(", expr, ")") {
            expr match {
                case in.TypedPath(path) => {
                    At(expr).checkEvalPath(path)
                    path.ty
                }
                case in.MethodCall(in.Super(_), name, args, (msym, msig)) => {
                    msig.flatParamTypes.zip(args).foreach(checkParam)
                    msig.returnTy
                }
                case in.NewCtor(_, args, (msym, msig), ty) => {
                    msig.flatParamTypes.zip(args).foreach(checkParam)
                    ty
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
    
    def checkAndAddReq(current: SPath[Phantasmal])(env: Env, req: in.Requirement) = {
        In(env, current).checkReq(req)
        
        req match {
            case in.PathRequirement(left, _, right) => {
                // TODO Require an annotation to add between non-final paths?
                if(
                    env.pathIsFinalBy(left.path, current) &&
                    env.pathIsFinalBy(right.path, current)
                ) {
                    env.plusFact(req.toFact)
                } else {
                    env
                }
            }
            
            case in.TypeRequirement(_, _, _) => {
                env.plusFact(req.toFact)
            }
        }
    }

    def checkAndAddAssign(current: SPath[Phantasmal])(env: Env, pair: (in.Lvalue, in.TypedPath)): Env = {
        val (lv, rv) = pair
        log.indent(lv, " <- ", rv) {
            val chk = In(env, current).At(rv)
            lv match {
                case in.DeclareVarLvalue(_, _, _, sym) => {
                    chk.checkEvalPathAndAssignable(rv.path, sym.ty)
                    env.plusLocalVar(sym)
                }

                case in.ReassignVarLvalue(_, sym) => {
                    chk.checkEvalPathAndAssignable(rv.path, sym.ty)
                    chk.checkPermitsWr(sym.guardPath)
                    env
                }

                case in.FieldLvalue(_, sym) => {
                    // TODO Check that dependent fields are assigned together.
                    chk.checkEvalPathAndAssignable(rv.path, sym.ty)
                    chk.checkPermitsWr(sym.guardPath)
                    env
                }
            }            
        }
    }
        
    def checkAndAddStmt(current: SPath[Phantasmal])(env: Env, stmt: in.LowerStmt): Env = 
        log.indent("checkAndAddStmt(", stmt, ")") {
            stmt match {
                case stmt: in.LowerTlExpr => {
                    In(env, current).checkExpr(stmt)
                    env
                }
                
                case in.DefineLv(lv, rv) => {
                    val ty = In(env, current).checkExpr(rv)
                    // Guaranteed by construction:
                    //In(env, current).At(rv).checkIsSubtype(ty, lv.ty)
                    env.plusLocalVar(lv)
                }
            
                case in.Assign(lvs, rvs) => {
                    lvs.zip(rvs).foldLeft(env)(checkAndAddAssign(current))
                }
            
                case stmt @ in.InlineInterval(name, in.Body(stmts), vsym) => {
                    // Note: vsym is already in the environment.
                    stmts.foldLeft(env)(checkAndAddStmt(vsym.toSPath))
                }
            
                case stmt @ in.MethodReturn(expr) => {
                    env.optReturnTy match {
                        case None => Error.NoReturnHere().report(global, stmt.pos)
                    
                        case Some(returnTy) => {
                            In(env, current).At(stmt).checkEvalPathAndAssignable(expr.path, returnTy)
                        }
                    }
                    env
                }
            }
        }
    
    def checkStmts(current: SPath[Phantasmal], env: Env, stmts: List[in.LowerStmt]): Unit = {
        stmts.foldLeft(env)(checkAndAddStmt(current))
    }
    
    def checkIntervalDecl(
        csym: ClassFromSource, 
        fsym: FieldSymbol, 
        decl: in.IntervalDecl
    ) = log.indent("checkIntervalDecl(", fsym, ")") {
        val env = csym.checkEnv
        val thisPath = csym.loweredSource.thisSym.toSPath
        val initPath = env.symPath(Path.ThisInit)
        In(env, initPath).checkEvalPathAndAssignable(decl.parent, Type.Interval)
        checkStmts(thisPath / fsym, csym.checkEnv, decl.body.stmts)
    }
    
    def checkMethodDecl(
        csym: ClassFromSource, 
        msym: MethodSymbol,
        decl: in.MethodDecl
    ) = log.indent("checkMethodDecl(", msym, ")") {
        var env = csym.checkEnv
        
        // Create the "method" variable:
        val methodSym = new LocalSymbol(
            decl.pos,
            Modifier.Set.empty,
            Name.MethodLocal,
            Type.Interval
        )
        env = env.plusLocalVar(methodSym)
        val methodPath = methodSym.toSPath
        
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
        
        // Add in the return type:
        env = env.withOptReturnTy(Some(decl.returnTref.ty))
        
        // Check the statements in the body in the resulting environment:
        decl.body match {
            case in.Body(stmts) => {
                checkStmts(methodPath, env, stmts)
                
                // Check that ensures clauses are met:
                decl.ensures.foreach { reqNode =>
                    if(!env.factHolds(reqNode.toFact)) {
                        Error.DoesNotEnsure(reqNode.toFact).report(global, reqNode.pos)
                    }
                }
            }
            case in.AbstractBody() =>
        }
    }
    
    def checkFieldDecl(
        csym: ClassFromSource, 
        fsym: FieldSymbol,
        decl: in.FieldDecl
    ) = {
        // TODO: Check that statements are valid etc
    }
    
    def checkRelDecl(
        csym: ClassFromSource, 
        decl: in.RelDecl
    ) = {
        // TODO: Check that paths are valid at least
    }
    
    def classSymbol(csym: ClassFromSource): Unit = {
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