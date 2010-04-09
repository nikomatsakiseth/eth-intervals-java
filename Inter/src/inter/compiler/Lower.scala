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
    
    def sameLength(lst1: List[_], lst2: List[_]) = (lst1.length == lst2.length)
    
    def upperBoundPathType(path: Name.Path, tvar: Name.Var): Symbol.Type = {
        throw new RuntimeException("TODO")
    }
    
    // ___ Translate from Ast to Symbol and back again ______________________
    
    def methodId(csym: Symbol.ClassFromInterFile, mdecl: in.MethodDecl) = {
        val parameterPatterns = mdecl.parts.map(p => symbolPattern(p.pattern))
        Symbol.MethodId(csym.name, mdecl.name, parameterPatterns)
    }

    def patternType(pattern: in.Pattern): Symbol.Type = pattern match {
        case in.TuplePattern(patterns) => Symbol.TupleType(patterns.map(patternType))
        case in.VarPattern(_, tref, _, _) => symbolType(tref)
    }
    
    def symbolPattern(pattern: in.Pattern): Symbol.Pattern = pattern match {
        case in.TuplePattern(patterns) => Symbol.TuplePattern(patterns.map(symbolPattern))
        case in.VarPattern(_, tref, name, _) => Symbol.VarPattern(name.name, symbolType(tref))
    }
    
    def symbolType(tref: in.TypeRef): Symbol.Type = tref match {
        case in.PathType(path, tvar) => Symbol.PathType(namePath(path), tvar.name)
        case in.ClassType(name, targs) => Symbol.ClassType(name.qualName, targs.map(symbolTypeArg))
        case in.TupleType(types) => Symbol.TupleType(types.map(symbolType))
        case in.NullType() => Symbol.NullType
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
            case Symbol.NullType =>
                withPosOf(from, out.NullType())
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
        sym.methodSymbols.get(mthdName) match {
            case None => createSymbolsForMethodsNamed(state, sym, mthdName)
            case Some(res) => res
        }
    }
    
    def createSymbolsForMethodsNamed(
        state: CompilationState, 
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
                    None
                } else {
                    val outMdecl = lowerMethod(state, csym, mdecl)
                    Some(new Symbol.Method(
                        name = mthdName,
                        returnTy = outMdecl.returnTy,
                        receiver = Symbol.VarPattern(Name.ThisVar, Symbol.ClassType(csym.name, List())),
                        parameterPatterns = memberId.parameterPatterns
                    ))
                }
            }
            
            case in.MethodDecl(_, parts, returnTy: in.TypeRef, _, _, _) => {
                Some(new Symbol.Method(
                    name = mthdName,
                    returnTy = symbolType(returnTy),
                    receiver = Symbol.VarPattern(Name.ThisVar, Symbol.ClassType(csym.name, List())),
                    parameterPatterns = parts.map(p => symbolPattern(p.pattern))
                ))
            }
        }
        
        val mdecls = csym.resolvedSource.members.flatMap(_.asMethodNamed(mthdName))
        val msyms = mdecls.flatMap(forMethodDecl)
        csym.methodSymbols(mthdName) = msyms
        msyms
    }
    
    def lowerMethod(
        state: CompilationState, 
        csym: Symbol.ClassFromInterFile, 
        mdecl: in.MethodDecl
    ): out.MethodDecl = {
        val memberId = methodId(csym, mdecl)
        csym.loweredMethods.get(memberId).getOrElse {
            val cdecl = csym.resolvedSource
            val clsName = cdecl.name.qualName
            assert(!state.inferStack(memberId))
            state.inferStack += memberId

            val receiver = new Symbol.Var(Name.ThisVar, Symbol.ClassType(clsName, List()))
            val parameterPatterns = mdecl.parts.map(p => symbolPattern(p.pattern))
            val parameterSymbols = parameterPatterns.flatMap(Symbol.createVarSymbols)
            val lookup = LookupTable.empty(state) ++ (receiver :: parameterSymbols)
            val optBody = mdecl.optBody.map(lowerBody(lookup, _))

            val (returnTref, returnTy) = (mdecl.returnTref, optBody) match {
                case (in.InferredTypeRef(), None) => {
                    state.reporter.report(
                        mdecl.returnTref.pos, "explicit.return.type.reqd.if.abstract", mdecl.name.toString
                    )
                    (astType(lookup)(mdecl.returnTref, Symbol.NullType), Symbol.NullType)
                }

                case (in.InferredTypeRef(), Some(out.Body(stmts))) => {
                    val ty = stmts.last.ty
                    (astType(lookup)(stmts.last, ty), ty)
                }

                case (tref: in.TypeRef, _) => {
                    (InScope(lookup).lowerTypeRef(tref), symbolType(tref))
                }

            }

            val outMdecl = out.MethodDecl(
                annotations = mdecl.annotations.map(InScope(lookup).lowerAnnotation),
                parts = mdecl.parts.map(InScope(lookup).lowerDeclPart),
                returnTref = returnTref,
                returnTy = returnTy,
                requirements = mdecl.requirements.map(InScope(lookup).lowerRequirement),
                optBody = optBody
            )
            
            csym.loweredMethods(memberId) = outMdecl
            state.inferStack -= memberId
            outMdecl
        }
    }
    
    // ___ Lowering Types ___________________________________________________
    
    object InScope {
        def apply(lookup: LookupTable) = new InScope(lookup)
    }
    
    class InScope(lookup: LookupTable) {
        import lookup.state
        
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
            sym = lookup.get(pattern.name.name).get // should be a variable already created
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

        def lowerVar(optExpTy: Option[Symbol.Type])(v: in.Var) = withPosOf(v, {
            val sym = lookup.get(v.name.name).getOrElse {
                state.reporter.report(v.pos, "no.such.var", v.name.toString)
                Symbol.errorVar(v.name.name, optExpTy)
            }
            out.Var(v.name, sym, sym.ty)
        })
        
        def lowerPathField(optExpTy: Option[Symbol.Type])(path: in.PathField) = withPosOf(path, {
            // Note: very similar code to lowerField() below
            val owner = lowerPath(path.owner)
            val optSym = lookupField(state, owner.ty, path.name.name)
            val subst = Subst(Name.ThisPath -> Name.Path(owner))
            val sym = optSym.getOrElse {
                state.reporter.report(path.pos, "no.such.field", owner.ty.toString, path.name.toString)
                Symbol.errorVar(path.name.name, optExpTy)
            }
            out.PathField(owner, path.name, sym, subst.ty(sym.ty))
        })
    
        def lowerPath(path: in.Path): out.Path = withPosOf(path, path match {
            case p: in.Var => lowerVar(None)(p)
            case p: in.PathField => lowerPathField(None)(p)
        })
    
        def lowerOptionalTypeRef(otref: in.OptionalTypeRef): out.OptionalTypeRef = otref match {
            case in.InferredTypeRef() => withPosOf(otref, out.InferredTypeRef())
            case tref: in.TypeRef => lowerTypeRef(tref)
        }
    
        def lowerTypeRef(tref: in.TypeRef): out.TypeRef = tref match {
            case in.PathType(path, tvar) => out.PathType(lowerPath(path), tvar)
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
    
    def lowerBody(lookup: LookupTable, body: in.Body): out.Body = {
        withPosOf(body, out.Body(lowerStmts(lookup, body.stmts)))
    }
    
    def lowerStmts(lookup0: LookupTable, stmts: List[in.Stmt]): List[out.Stmt] = {
        var lookup = lookup0
        val result = new ListBuffer[out.Stmt]()
        
        stmts.foreach { stmt =>
            lookup = InScopeStmt(lookup, result).appendLoweredStmt(stmt)
        }
        
        result.toList
    }
    
    object InScopeStmt {
        def apply(lookup: LookupTable, stmts: ListBuffer[out.Stmt]) = new InScopeStmt(lookup, stmts)
    }
    
    class InScopeStmt(lookup: LookupTable, stmts: ListBuffer[out.Stmt]) extends InScope(lookup) {
        import lookup.state
        
        def extractSymbols(pattern: out.Pattern): List[Symbol.Var] = pattern match {
            case out.TuplePattern(patterns) => patterns.flatMap(extractSymbols)
            case out.VarPattern(_, _, _, sym) => List(sym)
        }
        
        def appendLoweredStmt(stmt: in.Stmt): LookupTable = {
            stmt match {
                case in.Assign(lvalue, rvalue) => {
                    val optExpTy = optTypeFromLvalue(lvalue)
                    val outRvalue = lowerExpr(optExpTy)(rvalue)
                    val outPattern = lowerLvalue(outRvalue.ty, lvalue)
                    stmts += withPosOf(stmt, out.Assign(outPattern, outRvalue))
                    lookup ++ extractSymbols(outPattern)
                }
                
                case in.Labeled(name, body) => {
                    stmts += withPosOf(stmt, out.Labeled(name, lowerBody(lookup, body)))
                    lookup
                }
                
                case expr: in.Expr => {
                    stmts += lowerExpr(None)(expr)
                    lookup
                }
            }
        }
        
        /** Given an lvalue, attempts to extract the type which 
          * the lvalue expects to be assigned to it.  This may
          * not be possible if the user has not fully specified
          * the type. In that case, None is returned. */
        def optTypeFromLvalue(lvalue0: in.Lvalue): Option[Symbol.Type] = {
            case class FailedException() extends Exception
            
            def theOldCollegeTry(lvalue: in.Lvalue): Symbol.Type = lvalue match {
                case in.TupleLvalue(lvalues) => 
                    Symbol.TupleType(lvalues.map(theOldCollegeTry))
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
        def lowerLvalue(rvalueTy: Symbol.Type, lvalue: in.Lvalue): out.Pattern = {
            withPosOf(lvalue, (rvalueTy, lvalue) match {
                // Unpack singleton tuples:
                case (ty, in.TupleLvalue(List(lv))) => lowerLvalue(ty, lv)
                case (Symbol.TupleType(List(ty)), lv) => lowerLvalue(ty, lv)
                
                // Unpack matching tuples:
                case (Symbol.TupleType(tys), in.TupleLvalue(lvalues)) if sameLength(tys, lvalues) => {
                    out.TuplePattern(
                        tys.zip(lvalues).map { case (t, l) => lowerLvalue(t, l) }
                    )
                }
                
                // If tuple sizes don't match, just infer NullType:
                case (_, in.TupleLvalue(lvalues)) => {
                    out.TuplePattern(
                        lvalues.map(lowerLvalue(Symbol.NullType, _))
                    )
                }
                
                // Pattern w/o type specified, use type from RHS:
                case (ty, in.VarLvalue(anns, inTref @ in.InferredTypeRef(), name, ())) => {
                    val tref = astType(lookup)(inTref, ty)
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
            val tyAst = astType(lookup)(fromExpr, sym.ty)
            val lv = withPosOf(fromExpr, out.VarPattern(List(), tyAst, nameAst, sym))
            val assign = withPosOf(fromExpr, out.Assign(lv, withPosOf(fromExpr, toExpr)))
            stmts += assign
            withPosOf(fromExpr, out.Var(nameAst, sym, sym.ty))
        }
        
        def dummySubst(subst: Subst)(pat: Symbol.Pattern, text: String): Subst = pat match {
            case Symbol.VarPattern(name, _) =>
                subst + (name.toPath -> Name.PathBase(Name.Var(text)))
                
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
    
        def lowerField(optExpTy: Option[Symbol.Type])(expr: in.Field) = introduceVar(expr, { 
            // Note: very similar code to lowerPathField() above.
            // Big difference is that this version generates statements.
            val owner = lowerExprToVar(None)(expr.owner)
            val optSym = lookupField(state, owner.ty, expr.name.name)
            val subst = Subst(Name.ThisPath -> Name.Path(owner))
            val sym = optSym.getOrElse {
                state.reporter.report(expr.pos, "no.such.field", owner.ty.toString, expr.name.toString)
                Symbol.errorVar(expr.name.name, optExpTy)
            }
            out.Field(owner, expr.name, sym, subst.ty(sym.ty))
        })
        
        def lowerLiteralExpr(expr: in.Literal) = introduceVar(expr, {
            val ty = Symbol.ClassType(Name.Qual(expr.obj.getClass), List())
            out.Literal(expr.obj, ty)
        })
        
        def lowerPart(optExpTy: Option[Symbol.Type])(part: in.CallPart) = withPosOf(part, {
            out.CallPart(part.ident, lowerExpr(optExpTy)(part.arg))
        })
        
        def lowerMethodCall(optExpTy: Option[Symbol.Type])(mcall: in.MethodCall) = introduceVar(mcall, {
            val rcvr = lowerExpr(None)(mcall.rcvr)
            
            // Find all potential methods:
            val msyms = lookupMethods(state, rcvr.ty, mcall.name)
                
            // Identify the best method (if any):
            msyms match {
                case List() => {
                    state.reporter.report(mcall.pos, "no.such.method", rcvr.ty.toString, mcall.name.toString)
                    out.Null(optExpTy.getOrElse(Symbol.NullType))
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
                    throw new RuntimeException("TODO--Method overloading")
                }
            }
        })
        
        def lowerNew(expr: in.New) = introduceVar(expr, {
            // XXX We could give an expected type for the arguments based on the constructor(s)
            // XXX But does it matter?  Prob. not
            out.New(lowerTypeRef(expr.tref), lowerTuple(None)(expr.arg), symbolType(expr.tref))
        })
        
        def lowerNull(optExpTy: Option[Symbol.Type])(expr: in.Null) = introduceVar(expr, {
            val ty = optExpTy.getOrElse(Symbol.NullType)
            out.Null(ty)
        })

        def lowerTuple(optExpTy: Option[Symbol.Type])(tuple: in.Tuple) = withPosOf(tuple, {
            val exprs = tuple.exprs.map(lowerExpr(None)) // XXX deconstruct optExpTy
            out.Tuple(exprs, Symbol.TupleType(exprs.map(_.ty)))
        })
    
        def lowerInlineTmpl(tmpl: in.InlineTmpl) = introduceVar(tmpl, out.InlineTmpl(
            stmts = lowerStmts(lookup, tmpl.stmts),
            ty = Symbol.ClassType(Name.IntervalTmplQual, List(
                Symbol.PathTypeArg(Name.IntervalTmplParent, PcEq, Name.MethodPath)
            ))
        ))
        
        def lowerAsyncTmpl(tmpl: in.AsyncTmpl) = introduceVar(tmpl, out.AsyncTmpl(
            stmts = lowerStmts(lookup, tmpl.stmts),
            ty = Symbol.ClassType(Name.AsyncIntervalTmplQual, List(
                Symbol.PathTypeArg(Name.IntervalTmplParent, PcEq, Name.MethodPath)
            ))
        ))
        
        def lowerImpThis(expr: in.Expr) = withPosOf(expr, {
            val sym = lookup.get(Name.ThisVar).get
            out.Var(astVarName(expr, Name.ThisVar), sym, sym.ty)
        })
        
        def lowerExpr(optExpTy: Option[Symbol.Type])(expr: in.Expr): out.LoweredExpr = expr match {
            case tuple: in.Tuple => lowerTuple(optExpTy)(tuple)
            case tmpl: in.InlineTmpl => lowerInlineTmpl(tmpl)
            case tmpl: in.AsyncTmpl => lowerAsyncTmpl(tmpl)
            case lit: in.Literal => lowerLiteralExpr(lit)
            case e: in.Var => lowerVar(optExpTy)(e)
            case e: in.Field => lowerField(optExpTy)(e)
            case e: in.MethodCall => lowerMethodCall(optExpTy)(e)
            case e: in.New => lowerNew(e)
            case e: in.Null => lowerNull(optExpTy)(e)
            case e: in.ImpVoid => introduceVar(expr, out.ImpVoid(Symbol.VoidType))
            case e: in.ImpThis => lowerImpThis(e)
        }
        
        def lowerExprToVar(optExpTy: Option[Symbol.Type])(expr: in.Expr): out.Var = {
            lowerExpr(optExpTy)(expr) match {
                case v: out.Var => v
                case e => introduceVar(expr, e)
            }
        }
        
    }

}