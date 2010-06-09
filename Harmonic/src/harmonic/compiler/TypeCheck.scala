package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.util.parsing.input.Position

import Ast.{Lower => in}
import Ast.Lower.Extensions._
import Util._

case class TypeCheck(global: Global) {
    private[this] val emptyEnv = Env.empty(global)
    
    object InEnv {
        def apply(env: Env) = new InEnv(env)
    }
    
    class InEnv(env: Env) {
        
        object At {
            def apply(node: Ast.Node) = new At(node)
        }

        class At(node: Ast.Node) {
            
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
            
            def checkPathNode(path: in.TypedPath): Unit = {
                At(path).checkPath(path.path)
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
    
    def checkAssign(env: Env, pair: (in.Lvalue, in.Expr)): Env = {
        val (lv, rv) = pair
        val ty = InEnv(env).checkExpr(rv)
        lv match {
            case in.DeclareVarLvalue(_, _, _, sym) => {
                InEnv(env).At(rv).checkPathHasType(Path.TypedLocal(sym), ty)
                env.plusLocalVar(sym)
            }
                
            case in.ReassignVarLvalue(_, sym) => {
                InEnv(env).At(rv).checkPathHasType(Path.TypedLocal(sym), ty)
                env
            }
            
            case in.FieldLvalue(_, sym) => {
                // TODO Check that dependent fields are assigned together.
                val path = env.lookupThis.toTypedPath / sym
                InEnv(env).At(rv).checkPathHasType(path, ty)
                env
            }
        }
    }
        
    def checkStmt(env: Env, stmt: in.LowerStmt): Env = {
        stmt match {
            case stmt: in.LowerTlExpr => {
                InEnv(env).checkExpr(stmt)
                env
            }
            
            case stmt @ in.Assign(lvs, rvs) => {
                lvs.zip(rvs).foldLeft(env)(checkAssign)
            }
            
            case stmt @ in.InlineInterval(name, in.Body(stmts), vsym) => {
                // Note: vsym is already in the environment:
                stmts.foldLeft(env)(checkStmt)
            }
            
            case stmt @ in.MethodReturn(in.TypedPath(path)) => {
                env.optReturnTy match {
                    case None => Error.NoReturnHere().report(global, stmt.pos)
                    
                    case Some(returnTy) => {
                        InEnv(env).At(stmt).checkPath(path)
                        InEnv(env).At(stmt).checkPathHasType(path, returnTy)
                    }
                }
                env
            }
        }
    }
    
}