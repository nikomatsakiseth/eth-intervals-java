package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.util.parsing.input.Position

import Ast.{Lower => in}
import Ast.Lower.Extensions._
import Util._

case class TypeCheck(global: Global) {
    private[this] val emptyEnv = Env.empty(global)
    
    case class InEnv(env: Env) {
        
        case class At(node: Ast.Node) {
            
            def checkPathHasType(path: Path.Typed, ty: Type.Ref): Unit = {
                if(!env.pathHasType(path, ty)) {
                    Error.MustHaveType(path, ty).report(global, node.pos)
                }
            }
            
            def checkParamType(pair: (Pattern.Anon, Path.Typed)): Unit = {
                val (pattern, path) = pair
                checkPathHasType(path, pattern.ty)
            }
            
            def checkPath(path: Path.Typed): Unit = {
                path match {
                    case path @ Path.TypedLocal(sym) => {
                        // TODO Check that guard is readable?
                    }
                    case path @ Path.TypedCast(ty, path) => {
                        checkPath(path)
                    }
                    case path @ Path.TypedConstant(obj) => {
                    }
                    case path @ Path.TypedField(base, sym) => {
                        checkPath(base)
                        // TODO Check that guard is readable?
                    }
                    case path @ Path.TypedCall(rcvr, msym, args) => {
                        checkPath(rcvr)
                        args.foreach(checkPath)
                        path.msig.parameterPatterns.zip(args).foreach(checkParamType)
                    }
                    case path @ Path.TypedIndex(array, index) => {
                        checkPath(array)
                        checkPath(index)
                    }
                    case path @ Path.TypedTuple(paths) => {
                        paths.foreach(checkPath)
                    }
                }
            }

            def checkExpr(expr: in.LowerTlExpr): Type.Ref = {
                expr match {
                    case in.TypedPath(path) => {
                        checkPath(path)
                        path.ty
                    }
                    case in.MethodCall(in.Super, name, args, (msym, msig)) => {
                        args.foreach(checkPath)
                        msig.parameterPatterns.zip(args).foreach(checkParamType)
                    }
                    case in.NewCtor(_, args, (msym, msig), ty) => {
                        args.foreach(checkPath)
                        msig.parameterPatterns.zip(args).foreach(checkParamType)
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
        
    }
    
    def checkAssign(env: Env, pair: (in.Lvalue, in.Expr)): Env = {
        val (lv, rv) = pair
        val ty = checkExpr(rv)
        lv match {
            case in.DeclareVarLvalue(_, _, _, sym) => {
                At(rv).checkPathHasType(Path.TypedLocal(sym), ty)
                env.plusLocalVar(sym)
            }
                
            case in.ReassignVarLvalue(_, sym) => {
                At(rv).checkPathHasType(Path.TypedLocal(sym), ty)
                env
            }
            
            case in.FieldLvalue(_, sym) => {
                // TODO Check that dependent fields are assigned together.
                val path = env.lookupThis / sym
                At(rv).checkPathHasType(path, ty)
                env
            }
        }
    }
        
    def checkStmt(env: Env, stmt: in.LowerStmt): Env = {
        stmt match {
            case stmt: in.LowerTlExpr => {
                At(stmt).checkExpr(stmt)
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
                    case Some(returnTy) => checkPathHasType(path, returnTy)
                    case None => Error.NoReturnHere().report(global, stmt)
                }
                env
            }
        }
    }
    
}