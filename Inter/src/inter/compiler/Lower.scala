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
object Lower {
    
    // ___ Lookup tables ____________________________________________________
    
    class LookupTable(val state: CompilationState, map: Map[Name.Var, Symbol.Var]) {
        def +(sym: Symbol.Var) = new LookupTable(state, map + (sym.name -> sym))
        def ++(syms: Iterable[Symbol.Var]) = syms.foldLeft(this)(_ + _)
        def get(name: Name.Var) = map.get(name)
        def getOrError(name: Name.Var, optExpTy: Option[Symbol.Type]) = get(name) match {
            case Some(sym) => sym
            case None => Symbol.errorVar(name, optExpTy)
        }
    }
    
    object LookupTable {
        def empty(state: CompilationState) = new LookupTable(state, Map())
    }
    
    def lookupField(
        state: CompilationState,
        rcvrTy: Symbol.Type, 
        name: Name.Var
    ): Option[Symbol.Var] = {
        rcvrTy match {
            case Symbol.ClassType(className, _) => {
                val csym = state.symtab.classes(className)
                csym.fieldNamed(state)(name)
            }
            
            case Symbol.PathType(path, tvar) => {
                lookupField(state, upperBoundPathType(path, tvar), name)
            }
            
            case _ => None
        }
    }
    
    def lookupFieldOrError(
        state: CompilationState, 
        rcvrTy: Symbol.Type,  
        name: Name.Var,
        optExpTy: Option[Symbol.Type]
    ) = {
        lookupField(state, rcvrTy, name).getOrElse {
            Symbol.errorVar(name, optExpTy)
        }
    }    
    
    def lookupNonintrinsicMethods(
        state: CompilationState,
        rcvrTy: Symbol.Type, 
        name: Name.Method
    ): List[Symbol.Method] = {
        rcvrTy match {
            case Symbol.ClassType(className, _) => {
                val csym = state.symtab.classes(className)
                csym.methodsNamed(state)(name)
            }
            
            case Symbol.PathType(path, tvar) => {
                lookupNonintrinsicMethods(state, upperBoundPathType(path, tvar), name)                    
            }
            
            case _ => List()
        }
    }

    def lookupMethods(
        state: CompilationState,
        rcvrTy: Symbol.Type, 
        name: Name.Method
    ): List[Symbol.Method] = {
        state.lookupIntrinsic(rcvrTy, name) match {
            case Some(intrinsicSym) => List(intrinsicSym)
            case None => lookupNonintrinsicMethods(state, rcvrTy, name)
        }
    }
    
    // ___ Translate from Ast to Symbol and back again ______________________
    
    def patternType(pattern: in.Pattern): Symbol.Type = pattern match {
        case in.TuplePattern(patterns) => Symbol.TupleType(patterns.map(patternType))
        case in.VarPattern(_, tref, _, _) => symbolType(tref)
    }
    
    def symbolPattern(pattern: in.Pattern): Symbol.Pattern = pattern match {
        case in.TuplePattern(patterns) => Symbol.TuplePattern(patterns.map(symbolPattern))
        case in.VarPattern(_, tref, name, _) => new Symbol.Var(name.name, symbolType(tref))
    }
    
    def symbolType(tref: in.TypeRef): Symbol.Type = tref match {
        case in.PathType(path, tvar) => Symbol.PathType(namePath(path), tvar.name)
        case in.ClassType(name, targs) => Symbol.ClassType(name.qualName, targs.map(symbolTypeArg))
        case in.TupleType(types) => Symbol.TupleType(types.map(symbolType))
    }
    
    def symbolTypeArg(targ: in.TypeArg): Symbol.TypeArg = targ match {
        case in.TypeTypeArg(name, rel, tref) => Symbol.TypeTypeArg(name.name, rel, symbolType(tref))
        case in.PathTypeArg(name, rel, path) => Symbol.PathTypeArg(name.name, rel, namePath(path))
    }
    
    def namePath(path: in.Path): Name.Path = path match {
        case in.PathField(base, f, (), ()) => Name.PathField(namePath(base), f.name)
        case in.Var(name, (), ()) => Name.PathBase(name.name)
    }
    
    def astVarName(from: Ast.Node, name: Name.Var) = withPosOf(from,
        Ast.VarName(name.text)
    )
    
    def astPathVar(lookup: LookupTable)(from: Ast.Node, name: Name.Var) = withPosOf(from, {
        val sym = lookup.getOrError(name, None)
        out.Var(astVarName(from, name), sym, sym.ty)
    })
    
    def astPathField(lookup: LookupTable)(from: Ast.Node, base: Name.Path, name: Name.Var) = {
        withPosOf(from, {
            val astOwner = astPath(lookup)(from, base)
            val sym = lookupFieldOrError(lookup.state, astOwner.ty, name, None)
            out.PathField(astOwner, astVarName(from, name), sym, sym.ty)
        })
    }
    
    def astPath(lookup: LookupTable)(from: Ast.Node, path: Name.Path): out.Path = path match {
        case Name.PathBase(name) => astPathVar(lookup)(from, name)
        case Name.PathField(base, name) => astPathField(lookup)(from, base, name)
    }
    
    def astType(lookup: LookupTable)(from: Ast.Node, ty: Symbol.Type): out.TypeRef = {
        withPosOf(from, ty match {
            case Symbol.TupleType(tys) => 
                out.TupleType(tys.map(astType(lookup)(from, _)))
            case Symbol.PathType(path, tvar) => 
                out.PathType(
                    astPath(lookup)(from, path),
                    astVarName(from, tvar)
                )
            case Symbol.ClassType(name, targs) => 
                out.ClassType(
                    withPosOf(from, Ast.AbsName(name)),
                    targs.map(astTypeArg(lookup)(from, _))
                )
        })        
    }
    
    def astTypeArg(lookup: LookupTable)(from: Ast.Node, targ: Symbol.TypeArg) = {
        withPosOf(from, targ match {
            case Symbol.PathTypeArg(name, rel, path) =>
                out.PathTypeArg(
                    astVarName(from, name),
                    rel,
                    astPath(lookup)(from, path)
                )

            case Symbol.TypeTypeArg(name, rel, ty) =>
                out.TypeTypeArg(
                    astVarName(from, name),
                    rel,
                    astType(lookup)(from, ty)
                )
        })        
    }
    
    // ___ Method Symbol Creation ___________________________________________
    
    def symbolsForMethodsNamed(
        state: CompilationState, 
        sym: Symbol.ClassFromInterFile, 
        mthdName: Name.Method
    ) = {
        sym.methods.get(mthdName) match {
            case None => createSymbolsForMethodsNamed(state, sym, mthdName)
            case Some(res) => res
        }
    }
    
    def createSymbolsForMethodsNamed(
        state: CompilationState, 
        csym: Symbol.ClassFromInterFile, 
        mthdName: Name.Method
    ) = {
        csym.resolvedSource.members.flatMap(_.asMethodNamed(mthdName)) match {
            // No methods with that name:
            case List() => List()
            
            // One method found, but with unspecified return type:
            case List(mdecl @ in.MethodDecl(_, _, in.InferredTypeRef, _, _, _)) => {
                val memberId = Name.MethodId(csym.name, mthdName)
                
                if(state.inferStack(memberId)) {
                    // Cyclic inference: illegal.
                    if(!state.inferReported.contains(memberId)) {
                        state.inferReported += memberId
                        state.reporter.report(
                            mdecl.pos,
                            "explicit.type.required.due.to.cycle",
                            mthdName.toString
                        )
                    }
                    
                    List(Symbol.ErrorMethod) 
                } else {
                    val msym = lowerMethod(state, csym.resolvedSource, mdecl)
                    csym.methods(mthdName) = List(msym)
                    List(msym)
                }
            }
            
            // Multiple methods found, or one method w/ explicit return type:
            //    In the former case, no inference is permitted.
            case mdecls => {
                val msyms = mdecls.flatMap {
                    case mdecl @ in.MethodDecl(_, _, in.InferredTypeRef, _, _, _) => {
                        state.reporter.report(
                            mdecl.pos,
                            "explicit.type.required.due.to.overloading",
                            mthdName.toString
                        )
                        None
                    }
                    
                    case in.MethodDecl(_, parts, returnTy: in.TypeRef, _, _, _) => {
                        Some(new Symbol.Method(
                            name = mthdName,
                            returnTy = symbolType(returnTy),
                            receiver = new Symbol.Var(Name.ThisVar, Symbol.ClassType(csym.name, List())),
                            parameterPatterns = parts.map(p => symbolPattern(p.pattern))
                        ))
                    }
                }
                csym.methods(mthdName) = msyms
                msyms
            }
        }
        
    }
    
    def lowerMethod(state: CompilationState, cdecl: in.ClassDecl, mdecl: in.MethodDecl): Symbol.Method = {
        val clsName = cdecl.name.qualName
        val memberId = Name.MethodId(clsName, mdecl.name)
        assert(!state.inferStack(memberId))
        state.inferStack += memberId
        
        val receiver = new Symbol.Var(Name.ThisVar, Symbol.ClassType(clsName, List()))
        val parameters = mdecl.parts.map(p => symbolPattern(p.pattern))
        val lookup = LookupTable.empty(state) + receiver ++ parameters.flatMap(Symbol.vars)
        InScope(lookup)
        
        state.inferStack -= memberId
    }
    
    // ___ Type Checking ____________________________________________________
    
    def tmpVarName(fromExpr: in.Expr) = {
        "(%s@%s)".format(fromExpr.toString, fromExpr.pos.toString)
    }
    
    case class InScope(lookup: LookupTable, stmts: ListBuffer[out.Stmt]) {
        def introduceVar(fromExpr: in.Expr, toExpr: out.Expr): out.Var = {
            val text = tmpVarName(fromExpr)
            val sym = new Symbol.Var(Name.Var(text), toExpr.ty)
            val nameAst = withPosOf(fromExpr, Ast.VarName(text))
            val tyAst = astType(lookup)(fromExpr, sym.ty)
            val lv = withPosOf(fromExpr, out.VarLvalue(List(), tyAst, nameAst, sym))
            val assign = withPosOf(fromExpr, out.Assign(lv, withPosOf(fromExpr, toExpr)))
            stmts += assign
            withPosOf(fromExpr, out.Var(nameAst, sym, sym.ty))
        }
        
        def dummySubst(subst: Subst)(pat: Symbol.Pattern, text: String): Subst = pat match {
            case sym: Symbol.Var =>
                subst + (sym.name.toPath -> Name.PathBase(Name.Var(text)))
                
            case Symbol.TuplePattern(patterns) =>
                patterns.zipWithIndex.foldLeft(subst) { case (s, (p, i)) =>
                    dummySubst(s)(p, "%s_%d".format(text, i))
                }
        }
        
        def patSubst(subst: Subst)(pat: Symbol.Pattern, expr: in.Expr): Subst = {
            (pat, expr) match {
                case (Symbol.TuplePattern(patterns), in.Tuple(exprs, ())) if patterns.length == exprs.length =>
                    patterns.zip(exprs).foldLeft(subst) { case (s, (p, e)) => patSubst(s)(p, e) }
                
                case (sym: Symbol.Var, in.Var(name, (), ())) =>
                    subst + (sym.name.toPath -> name.name.toPath)
                
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
    
        def lowerAnnotation(ann: in.Annotation) = withPosOf(ann,
            out.Annotation(name = ann.name)
        )
    
        def lowerVar(optExpTy: Option[Symbol.Type])(v: in.Var) = withPosOf(v, {
            val sym = lookup.get(v.name.name).getOrElse {
                state.reporter.report(v.pos, "no.such.var", v.name.toString)
                Symbol.errorVar(v.name.name, optExpTy)
            }
            out.Var(v.name, sym, sym.ty)
        })
        
        def lowerPathField(path: in.PathField) = withPosOf(path, {
            // Note: very similar code to lowerField() below
            val owner = lowerPath(path.owner)
            val sym = lookupField(state, owner.ty, path.name)
            val subst = Subst(Name.ThisPath -> Name.Path(owner))
            out.PathField(owner, path.name, sym, subst.ty(sym.ty))
        })
    
        def lowerField(expr: in.Field) = introduceVar(expr, { 
            // Note: very similar code to lowerPathField() above
            val owner = lowerExprToVar(None)(expr.owner)
            val sym = lookupField(state, owner.ty, expr.name)
            val subst = Subst(Name.ThisPath -> Name.Path(owner))
            out.Field(owner, expr.name, sym, subst.ty(sym.ty))
        })
        
        def lowerPath(path: in.Path): out.Path = withPosOf(path, path match {
            case p: in.Var => lowerVar(None)(v)
            case p: in.PathField => lowerPathField(p)
        })
    
        def lowerOptionalTypeRef(otref: in.OptionalTypeRef): out.OptionalTypeRef = otref match {
            case in.InferredTypeRef => out.InferredTypeRef
            case tref: in.TypeRef => lowerTypeRef(tref)
        }
    
        def lowerTypeRef(tref: in.TypeRef): out.TypeRef = tref match {
            case in.PathType(path, tvar) => out.PathType(lowerPath(path), tvar)
            case in.ClassType(cn, targs) => out.ClassType(lowerName(cn), targs.map(lowerTypeArg))
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
        
        def lowerLiteralExpr(expr: in.Literal) = introduceVar(expr, {
            val ty = Symbol.ClassType(Name.Qual(expr.obj.getClass), List())
            out.Literal(expr.obj, ty)
        })
        
        def lowerPart(optExpTy: Option[Symbol.Ty])(part: in.CallPart) = withPosOf(part, {
            out.CallPart(part.ident, lowerExpr(optExpTy)(part.arg))
        })
        
        def lowerMethodCall(mcall: in.MethodCall) = introduceVar(mcall, {
            val rcvr = lowerExpr(None)(mcall.rcvr)
            
            // Find all potential methods:
            val msyms = lookupMethods(rcvr.ty, mcall.name)
                
            // Identify the best method (if any):
            val (parts, msym) = msyms match {
                case List() => {
                    reporter.report(v.pos, "no.such.method", rcvr.ty.toString, mcall.name.toString)
                    Symbol.ErrorMethod
                }
                
                // Exactly one match: We can do more with inferencing
                // in this case, as we know the expected type.
                case List(msym) => {
                    val subst = patSubsts(
                        msym.receiver   ::  msym.parameterPatterns, 
                        mcall.rcvr      ::  mcall.parts.map(_.arg)
                    )
                    val optExpTys = msym.parameters.map(p => Some(subst.ty(p.ty)))
                    val parts = optExpTys.zip(mcall.parts).map { case (e,p) => lowerPart(e)(p) }
                    (parts, msym)
                }
                
                case _ => {
                    val parts = mcall.parts.map(lowerPart(None))
                }
            }
            
            val subst = patSubsts(
                msym.receiver   ::  msym.parameterPatterns, 
                mcall.rcvr      ::  mcall.parts.map(_.arg)
            )

            if(sym != Symbol.ErrorMethod) {
                checkAssignable(
                    (msym.receiver  :: msym.parameters).map(subst.pattern), 
                    (rcvr           :: parts)
                )
            }

            out.MethodCall(rcvr, parts, msym, subst.ty(msym.returnTy))
        })
        
        def lowerNew(expr: in.New) = {
            out.New(lowerTypeRef(tref), lowerTuple(arg))
        }
        
        def lowerNull(optExpTy: Option[Symbol.Ty])(expr: in.Null) = introduceVar(expr, {
            val ty = optExpTy.getOrElse(Symbol.NullType)
            out.Null(ty)
        })

        def lowerTuple(tuple: in.Tuple) = withPosOf(tuple, {
            val exprs = tuple.exprs.map(lowerExpr)
            out.Tuple(exprs, symbol.TupleType(exprs.map(_.ty)))
        })
    
        def lowerExpr(optExpTy: Option[Symbol.Ty])(expr: in.Expr): out.LoweredExpr = expr match {
            case tuple: in.Tuple => lowerTuple(tuple)
            case tmpl: in.InlineTmpl => lowerInlineTmpl(tmpl)
            case in.AsyncTmpl(stmts) => out.AsyncTmpl(stmts.map(lowerStmt))
            case lit: in.Literal => lowerLiteralExpr(lit)
            case e: in.Var => lowerVar(e)
            case e: in.Field => lowerField(e)
            case e: in.MethodCall => lowerMethodCall(e)
            case e: in.New => lowerNew(e)
            case e: in.Null => lowerNull(optExpTy)(e)
            case in.ImpVoid => out.ImpVoid
            case in.ImpThis => out.ImpThis
        }
        
        def lowerExprToVar(optExpTy: Option[Symbol.Ty])(expr: in.Expr): out.Var = {
            lowerExpr(optExpTy)(expr) match {
                case v: out.Var => v
                case e => introduceVar(expr, e)
            }
        }

        def lowerInlineTmpl(tmpl: in.InlineTmpl) = out.InlineTmpl(
            stmts = tmpl.stmts.map(lowerStmt)
        )
        
    }

}