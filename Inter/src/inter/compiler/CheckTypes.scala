package inter.compiler

import scala.collection.immutable.Map

import Hl.{RN => in}
import Hl.{CT => out}
import Util._

/** Performs the basic type typeCheck. This is more-or-less equivalent
  * to your standard, run-of-the-mill Java type typeCheck, except that 
  * it does more inference. */
object CheckTypes {
    
    // ___ Translate from AST descriptions of a type to symbol ______________
    
    def patternType(pattern: in.Pattern): Symbol.Type = pattern match {
        case in.TuplePattern(patterns) => Symbol.TupleType(
            patterns.map(patternType)
        )
        
        case in.VarPattern(_, tref, _, _) => symbolType(tref)
    }
    
    def partSyms(part: in.DeclPart) = patternSyms(part.pattern)
    
    def patternSyms(pattern: in.Pattern): List[Symbol.Var] = pattern match {
        case in.TuplePattern(patterns) => 
            patterns.foldMap(patternSyms)
        case in.VarPattern(_, tref, name, _) => 
            List(new Symbol.Var(name.name, symbolType(tref)))
    }
    
    def symbolType(tref: in.TypeRef): Symbol.Type = tref match {
        case in.PathType(path, tvar) => Symbol.PathType(namePath(path), tvar.name)
        case in.ClassType(name, targs) => Symbol.ClassType(name.qualName, targs.map(symbolTypeArg))
        case in.TupleType(types) => Symbol.TupleType(types.map(symbolType))
    }
    
    def symbolTypeArg(targ: in.TypeArg): Symbol.TypeArg = targ match {
        case in.TypeTypeArg(name, rel, tref) => Symbol.TypeTypeArg(name.name, rel, symbolTypeArg(tref))
        case in.PathTypeArg(name, rel, path) => Symbol.PathTypeArg(name,name, rel, namePath(path))
    }
    
    def namePath(path: in.Path): Name.Path = path match {
        case in.PathField(base, f) => Name.PathField(namePath(base), f.name)
        case in.Var(name) => Name.PathBase(name.name)
    }
    
    // ___ Method Symbol Creation ___________________________________________
    
    def symbolsForMethodsNamed(
        state: CompilationState, 
        sym: Symbol.ClassFromInterFile, 
        mthdName: Name.Method
    ) = {
        sym.methods.get(mthdName) match {
            case None => createSymbolsForMethodsName(state, sym, mthdName)
            case Some(res) => res
        }
    }
    
    def createSymbolsForMethodsNamed(
        state: CompilationState, 
        sym: Symbol.ClassFromInterFile, 
        mthdName: Name.Method
    ) = {
        val clsTree = sym.typeCheckdSource
        clsTree.members.flatMap(_.asMethodNamed(mthdName)) match {
            // No methods with that name:
            case List() => List()
            
            // One method found, but with unspecified return type:
            case List(mdecl @ in.MethodDecl(_, _, in.InferredTypeRef, _, _, _)) => {
                val memberId = Name.MethodId(name, memName)
                
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
                    
                    List(symbol.ErrorMethod) 
                } else {
                    val sym = typeCheckMethod(state, sym.cdecl, mdecl)
                    sym.methods(mthdName) = List(sym)
                    List(sym)
                }
            }
            
            // Multiple methods found, or one method w/ explicit return type:
            //    In the former case, no inference is permitted.
            case List(mdecls) => {
                val syms = mdecls.flatMap {
                    case mdecl @ in.MethodDecl(_, _, in.InferredTypeRef, _, _, _) => {
                        state.reporter.report(
                            mdecl.pos,
                            "explicit.type.required.due.to.overloading",
                            mthdName.toString
                        )
                        None
                    }
                    
                    case in.MethodDecl(_, parts, returnTy: in.TypeRef, _, _, _) => {
                        new Symbol.Method(
                            name = mthdName,
                            retTypeRef = symbolType(returnTy),
                            receiver = new Symbol.Var(Name.ThisVar, Symbol.ClassType(clsName, List())),
                            parameters = parts.flatMap(partSyms)
                        )
                    }
                }
                sym.methods(mthdName) = syms
                syms
            }
        }
        
    }
    
    class LookupTable(map: Map[Name.Var, Symbol.Var]) {
        def +(sym: Symbol.Var) = new LookupTable(map + (sym.name -> sym))
        def ++(syms: Iterable[Symbol.Var]) = syms.foldLeft(this)(_ + _)
        def get(name: Name.Var) = map.get(name)
    }
    
    object LookupTable {
        val Empty = new LookupTable(Map())
    }
    
    def typeCheckMethod(state: CompilationState, cdecl: in.ClassDecl, mdecl: in.MethodDecl) = {
        val clsName = cdecl.name.qualName
        val memberId = Name.MethodId(clsName, memName)
        assert(!state.inferStack(memberId))
        state.inferStack += memberId
        
        val receiver = new Symbol.Var(Name.ThisVar, Symbol.ClassType(clsName, List()))
        val parameters = mdecl.parts.flatMap(partSyms)
        val lookup = LookupTable.Empty + receiver ++ parameters
        
        state.inferStack -= memberId
    }
    
    // ___ Type Checking ____________________________________________________
    
    class TypeChecker(lookup: LookupTable) {
        
        /* Yuck--- Should we first lower the AST as part of the post-processing? */
        
        def patSubst(allPatterns: List[Pattern], allExprs: List[in.Expr]): Subst = {
            allPatterns.zip(allExprs).foldLeft(Subst.empty) {
                case (subst, (Symbol.TuplePattern(patterns), in.Tuple(exprs)))
                if patterns.length == exprs.length =>
                    subst + patSubst(patterns, exprs)
                
                case (subst, (Symbol.TuplePattern(patterns), _)) =>
                    patterns.foldLeft(subst)(dummySubst)
                
                case (subst, sym: Symbol.Var, in.Var(name, (), ())) =>
                    subst + (sym.name.toPath -> name.name.toPath)
                    
                case (pat @ Symbol.Var(_), _) =>
                    dummySubst(List(pat))
            }
        }
    
        def checkAssignable(allPatterns: List[Pattern], allExprs: List[out.Expr]) = {
            allPatterns.zip(allExprs).foreach {
                case (Symbol.TuplePattern(patterns), out.Tuple)
            }
        }
    
        def typeCheckMethodDecl(decl: in.MethodDecl) = out.MethodDecl(
            annotations = decl.annotations.map(typeCheckAnnotation),
            parts = decl.parts.map(typeCheckDeclPart),
            returnTref = typeCheckOptionalTypeRef(decl.returnTref),
            requirements = decl.requirements.map(typeCheckRequirement),
            optBody = decl.optBody.map(typeCheckInlineTmpl),
            sym = decl.sym            
        )
    
        def typeCheckAnnotation(ann: in.Annotation) = out.Annotation(
            name = typeCheckName(ann.name)
        )
    
        def typeCheckPattern(pattern: in.Pattern): out.Pattern = pattern match {
            case tuplePat: in.TuplePattern => typeCheckTuplePattern(tuplePat)
            case varPat: in.VarPattern => typeCheckVarPattern(varPat)
        }
    
        def typeCheckTuplePattern(tuplePat: in.TuplePattern) = out.TuplePattern(
            patterns = tuplePat.patterns.map(typeCheckPattern)
        )
    
        def typeCheckVarPattern(varPat: in.VarPattern) = out.VarPattern(
            annotations = varPat.annotations.map(typeCheckAnnotation),
            tref = typeCheckTypeRef(varPat.tref),
            name = varPat.name,
            sym = varPat.sym
        )
    
        def typeCheckMember(mem: in.MemberDecl): out.MemberDecl = mem match {
            case decl: in.ClassDecl => typeCheckClassDecl(decl)
            case decl: in.IntervalDecl => typeCheckIntervalDecl(decl)
            case decl: in.MethodDecl => typeCheckMethodDecl(decl)
            case decl: in.FieldDecl => typeCheckFieldDecl(decl)
            case decl: in.RelDecl => typeCheckRelDecl(decl)
        }
    
        def typeCheckIntervalDecl(decl: in.IntervalDecl) = out.IntervalDecl(
            annotations = decl.annotations.map(typeCheckAnnotation),
            name = decl.name,
            optParent = decl.optParent.map(typeCheckPath),
            optBody = decl.optBody.map(typeCheckInlineTmpl),
            sym = decl.sym            
        )
    
        def typeCheckDeclPart(decl: in.DeclPart) = out.DeclPart(
            ident = decl.ident,
            pattern = typeCheckTuplePattern(decl.pattern)
        )
    
        def typeCheckRequirement(requirement: in.PathRequirement) = out.PathRequirement(
            left = typeCheckPath(requirement.left),
            rel = requirement.rel,
            right = typeCheckPath(requirement.right)
        )
    
        def typeCheckFieldDecl(decl: in.FieldDecl) = out.FieldDecl(
            annotations = decl.annotations.map(typeCheckAnnotation),
            name = decl.name,
            tref = typeCheckOptionalTypeRef(decl.tref),
            value = decl.value.map(typeCheckExpr),
            sym = decl.sym            
        )
    
        def typeCheckRelDecl(decl: in.RelDecl) = out.RelDecl(
            annotations = decl.annotations.map(typeCheckAnnotation),
            left = typeCheckPath(decl.left),
            kind = decl.kind,
            right = typeCheckPath(decl.right)
        )
    
        def typeCheckPath(path: in.Path): out.Path = path match {
            case in.Var(name, ()) => out.Var(name, ())
            case in.PathField(p, f) => out.PathField(typeCheckPath(p), f)
        }
    
        def typeCheckOptionalTypeRef(otref: in.OptionalTypeRef): out.OptionalTypeRef = otref match {
            case in.InferredTypeRef => out.InferredTypeRef
            case tref: in.TypeRef => typeCheckTypeRef(tref)
        }
    
        def typeCheckTypeRef(tref: in.TypeRef): out.TypeRef = tref match {
            case in.PathType(path, tvar) => out.PathType(typeCheckPath(path), tvar)
            case in.ClassType(cn, targs) => out.ClassType(typeCheckName(cn), targs.map(typeCheckTypeArg))
        }

        def typeCheckTypeArg(targ: in.TypeArg): out.TypeArg = targ match {
            case ttarg: in.TypeTypeArg => typeCheckTypeTypeArg(ttarg)
            case ptarg: in.PathTypeArg => typeCheckPathTypeArg(ptarg)
        }
    
        def typeCheckTypeTypeArg(targ: in.TypeTypeArg): out.TypeTypeArg = out.TypeTypeArg(
            name = targ.name, rel = targ.rel, typeRef = typeCheckTypeRef(targ.typeRef)
        )
    
        def typeCheckPathTypeArg(targ: in.PathTypeArg): out.PathTypeArg = out.PathTypeArg(
            name = targ.name, rel = targ.rel, path = typeCheckPath(targ.path)
        )
        
        def typeCheckStmts(stmts: List[in.Stmt]): List[out.Stmt] = stmts match {
            case List() => List()
            
            case in.Assign(lv, rv) :: stmts_r => {
                
            }
            
            case in.Labeled(name, blk) :: stmts_r => {
                out.Labeled(
                    name, typeCheckInlineTmpl(blk)
                ) :: typeCheckStmts(stmts_r)
            }
            
            case (expr: in.Expr) :: stmts_r => {
                typeCheckExpr(expr) :: typeCheckStmts(stmts_r)
            }
            
        }
    
        def typeCheckLvalue(lvalue: in.Lvalue): out.Lvalue = lvalue match {
            case in.TupleLvalue(lvalues) => out.TupleLvalue(lvalues.map(typeCheckLvalue))
            case in.VarLvalue(annotations, tref, name, ()) => out.VarLvalue(
                annotations = annotations.map(typeCheckAnnotation),
                tref = typeCheckOptionalTypeRef(tref),
                name = name,
                sym = ()
            )
        }
    
        def typeCheckExpr(optExpTy: Option[Symbol.Ty])(expr: in.Expr): out.Expr = expr match {
            case tuple: in.Tuple => typeCheckTuple(tuple)
            case tmpl: in.InlineTmpl => typeCheckInlineTmpl(tmpl)
            case in.AsyncTmpl(stmts) => out.AsyncTmpl(stmts.map(typeCheckStmt))
            case in.Literal(obj) => out.Literal(obj)
            case in.Assign(lv, rv) => out.Assign(typeCheckLvalue(lv), typeCheckExpr(rv))
            case e: in.Var => typeCheckVar(e)
            case in.Field(owner, name, ()) => out.Field(typeCheckExpr(owner), name, ())
            case e: in.MethodCall => typeCheckMethodCall(e)
            case in.New(tref, arg) => out.New(typeCheckTypeRef(tref), typeCheckTuple(arg))
            case in.Null() => out.Null(Symbol.NullType)
            case in.ImpVoid => out.ImpVoid
            case in.ImpThis => out.ImpThis
        }
        
        def lookupNonintrinsicMethod(rcvrTy: Symbol.Type, name: Name.Method): List[Symbol.Method] = {
            rcvrTy match {
                case Symbol.ClassType(name, _) => {
                    val csym = state.symtab.classes(name)
                    csym.methodsNamed(state)(name)
                }
                
                case Symbol.PathType(path, tvar) => {
                    
                }
                
                case _ => List()
            }
        }

        def typeCheckPart(optExpTy: Option[Symbol.Ty])(part: in.CallPart) = {
            out.CallPart(part.ident, typeCheckExpr(optExpTy)(part.arg))            
        }
        
        def typeCheckMethodCall(mcall: in.MethodCall) = {
            val rcvr = typeCheckExpr(mcall.rcvr)
            
            // Find all potential methods:
            val msyms = 
                state.typeCheckIntrinsics(rcvr.ty, mcall.name) match {
                    case Some(intrinsicSym) => List(intrinsicSym)
                    case None => lookupNonintrinsicMethod(rcvr.ty, mcall.name)
                }
                
            // Identify the best method (if any):
            val (parts, msym) = msyms match {
                case List() => {
                    reporter.report(v.pos, "no.such.method", rcvr.ty.toString, mcall.name.toString)
                    Symbol.ErrorMethod
                }
                
                // Exactly one match: We can do more with inferencing
                // in this case, as we know the expected type.
                case List(msym) => {
                    val optExpTys = msym.parameters.map(p => Some(p.ty)) /* XXX Subst */
                    val parts = optExpTys.zip(mcall.parts).map { case (e,p) => typeCheckPart(e)(p) }
                    msym
                }
                
                case _ => {
                    val parts = mcall.parts.map(typeCheckPart(None))
                }
            }
            
            val subst = Subst.pattern(state,
                msym.receiver   :: msym.parameters,
                rcvr            :: parts
            )

            // Check the types of the arguments:
            if(sym != Symbol.ErrorMethod)
                checkAssignable(
                    (msym.receiver  :: msym.parameters).map(subst.pattern), 
                    (rcvr           :: parts))

            out.MethodCall(rcvr, parts, msym, subst.ty(msym.returnTy))
            
        }

        def typeCheckVar(v: in.Var) = {
            lookup.get(v.name.name) match {
                case Some(sym) => out.Var(v.name, sym, sym.typeRef)
                case None => {
                    reporter.report(v.pos, "no.such.var", v.name.toString)
                    val sym = Symbol.errorVar(v.name.name)
                    out.Var(v.name, sym, sym.typeRef)
                }
            }
        }
    
        def typeCheckTuple(tuple: in.Tuple) = {
            val exprs = tuple.exprs.map(typeCheckExpr)
            out.Tuple(exprs, symbol.TupleType(exprs.map(_.ty)))
        }
    
        def typeCheckInlineTmpl(tmpl: in.InlineTmpl) = out.InlineTmpl(
            stmts = tmpl.stmts.map(typeCheckStmt)
        )
        
    }

}