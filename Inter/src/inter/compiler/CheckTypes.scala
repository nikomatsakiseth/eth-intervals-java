package inter.compiler

import scala.collection.immutable.Map

import Hl.{RN => in}
import Hl.{CT => out}
import Util._

/** Performs the basic type check. This is more-or-less equivalent
  * to your standard, run-of-the-mill Java type check, except that 
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
        val clsTree = sym.checkdSource
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
                    val sym = checkMethod(state, sym.cdecl, mdecl)
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
    
    def checkMethod(state: CompilationState, cdecl: in.ClassDecl, mdecl: in.MethodDecl) = {
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
        
        def checkMethodDecl(decl: in.MethodDecl) = out.MethodDecl(
            annotations = decl.annotations.map(checkAnnotation),
            parts = decl.parts.map(checkDeclPart),
            returnTref = checkOptionalTypeRef(decl.returnTref),
            requirements = decl.requirements.map(checkRequirement),
            optBody = decl.optBody.map(checkInlineTmpl),
            sym = decl.sym            
        )
    
        def checkAnnotation(ann: in.Annotation) = out.Annotation(
            name = checkName(ann.name)
        )
    
        def checkPattern(pattern: in.Pattern): out.Pattern = pattern match {
            case tuplePat: in.TuplePattern => checkTuplePattern(tuplePat)
            case varPat: in.VarPattern => checkVarPattern(varPat)
        }
    
        def checkTuplePattern(tuplePat: in.TuplePattern) = out.TuplePattern(
            patterns = tuplePat.patterns.map(checkPattern)
        )
    
        def checkVarPattern(varPat: in.VarPattern) = out.VarPattern(
            annotations = varPat.annotations.map(checkAnnotation),
            tref = checkTypeRef(varPat.tref),
            name = varPat.name,
            sym = varPat.sym
        )
    
        def checkMember(mem: in.MemberDecl): out.MemberDecl = mem match {
            case decl: in.ClassDecl => checkClassDecl(decl)
            case decl: in.IntervalDecl => checkIntervalDecl(decl)
            case decl: in.MethodDecl => checkMethodDecl(decl)
            case decl: in.FieldDecl => checkFieldDecl(decl)
            case decl: in.RelDecl => checkRelDecl(decl)
        }
    
        def checkIntervalDecl(decl: in.IntervalDecl) = out.IntervalDecl(
            annotations = decl.annotations.map(checkAnnotation),
            name = decl.name,
            optParent = decl.optParent.map(checkPath),
            optBody = decl.optBody.map(checkInlineTmpl),
            sym = decl.sym            
        )
    
        def checkDeclPart(decl: in.DeclPart) = out.DeclPart(
            ident = decl.ident,
            pattern = checkTuplePattern(decl.pattern)
        )
    
        def checkRequirement(requirement: in.PathRequirement) = out.PathRequirement(
            left = checkPath(requirement.left),
            rel = requirement.rel,
            right = checkPath(requirement.right)
        )
    
        def checkFieldDecl(decl: in.FieldDecl) = out.FieldDecl(
            annotations = decl.annotations.map(checkAnnotation),
            name = decl.name,
            tref = checkOptionalTypeRef(decl.tref),
            value = decl.value.map(checkExpr),
            sym = decl.sym            
        )
    
        def checkRelDecl(decl: in.RelDecl) = out.RelDecl(
            annotations = decl.annotations.map(checkAnnotation),
            left = checkPath(decl.left),
            kind = decl.kind,
            right = checkPath(decl.right)
        )
    
        def checkPath(path: in.Path): out.Path = path match {
            case in.Var(name, ()) => out.Var(name, ())
            case in.PathField(p, f) => out.PathField(checkPath(p), f)
        }
    
        def checkOptionalTypeRef(otref: in.OptionalTypeRef): out.OptionalTypeRef = otref match {
            case in.InferredTypeRef => out.InferredTypeRef
            case tref: in.TypeRef => checkTypeRef(tref)
        }
    
        def checkTypeRef(tref: in.TypeRef): out.TypeRef = tref match {
            case in.PathType(path, tvar) => out.PathType(checkPath(path), tvar)
            case in.ClassType(cn, targs) => out.ClassType(checkName(cn), targs.map(checkTypeArg))
        }

        def checkTypeArg(targ: in.TypeArg): out.TypeArg = targ match {
            case ttarg: in.TypeTypeArg => checkTypeTypeArg(ttarg)
            case ptarg: in.PathTypeArg => checkPathTypeArg(ptarg)
        }
    
        def checkTypeTypeArg(targ: in.TypeTypeArg): out.TypeTypeArg = out.TypeTypeArg(
            name = targ.name, rel = targ.rel, typeRef = checkTypeRef(targ.typeRef)
        )
    
        def checkPathTypeArg(targ: in.PathTypeArg): out.PathTypeArg = out.PathTypeArg(
            name = targ.name, rel = targ.rel, path = checkPath(targ.path)
        )
        
        def checkStmts(stmts: List[in.Stmt]): List[out.Stmt] = stmts match {
            case List() => List()
            
            case in.Assign(lv, rv) :: stmts_r => {
                
            }
            
            case in.Labeled(name, blk) :: stmts_r => {
                out.Labeled(
                    name, checkInlineTmpl(blk)
                ) :: checkStmts(stmts_r)
            }
            
            case (expr: in.Expr) :: stmts_r => {
                checkExpr(expr) :: checkStmts(stmts_r)
            }
            
        }
    
        def checkLvalue(lvalue: in.Lvalue): out.Lvalue = lvalue match {
            case in.TupleLvalue(lvalues) => out.TupleLvalue(lvalues.map(checkLvalue))
            case in.VarLvalue(annotations, tref, name, ()) => out.VarLvalue(
                annotations = annotations.map(checkAnnotation),
                tref = checkOptionalTypeRef(tref),
                name = name,
                sym = ()
            )
        }
    
        def checkExpr(expr: in.Expr): out.Expr = expr match {
            case tuple: in.Tuple => checkTuple(tuple)
            case tmpl: in.InlineTmpl => checkInlineTmpl(tmpl)
            case in.AsyncTmpl(stmts) => out.AsyncTmpl(stmts.map(checkStmt))
            case in.Literal(obj) => out.Literal(obj)
            case in.Assign(lv, rv) => out.Assign(checkLvalue(lv), checkExpr(rv))
            case e: in.Var => checkVar(e)
            case in.Field(owner, name, ()) => out.Field(checkExpr(owner), name, ())
            case e: in.MethodCall => checkMethodCall(e)
            case in.New(tref, arg) => out.New(checkTypeRef(tref), checkTuple(arg))
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

        def checkPart(part: in.CallPart) = {
            out.CallPart(part.ident, checkExpr(part.arg))            
        }
    
        def checkMethodCall(mcall: in.MethodCall) = {
            val rcvr = checkExpr(mcall.rcvr)
            val parts = mcall.parts.map(checkPart)
            
            // Find all potential methods:
            val msyms = 
                state.checkIntrinsics(rcvr.ty, mcall.name) match {
                    case Some(intrinsicSym) => List(intrinsicSym)
                    case None => lookupNonintrinsicMethod(rcvr.ty, mcall.name)
                }
                
            // Identify the best method (if any):
            val msym = msyms match {
                case List() => {
                    reporter.report(v.pos, "no.such.method", rcvr.ty.toString, mcall.name.toString)
                    Symbol.ErrorMethod
                }
                
                case List(msym) => msym
                
                case _ => throw new RuntimeException("To Do: Overloading")
            }
            
            val subst = Subst.expr(state,
                msym.receiver   :: msym.parameters,
                rcvr            :: parts
            )

            // Check the types of the arguments:
            if(sym != Symbol.ErrorMethod) {
                // XXX
            }
            
            out.MethodCall(rcvr, parts, msym, subst.ty(msym.returnTy))
        }

        def checkVar(v: in.Var) = {
            lookup.get(v.name.name) match {
                case Some(sym) => out.Var(v.name, sym, sym.typeRef)
                case None => {
                    reporter.report(v.pos, "no.such.var", v.name.toString)
                    val sym = Symbol.errorVar(v.name.name)
                    out.Var(v.name, sym, sym.typeRef)
                }
            }
        }
    
        def checkTuple(tuple: in.Tuple) = {
            val exprs = tuple.exprs.map(checkExpr)
            out.Tuple(exprs, symbol.TupleType(exprs.map(_.ty)))
        }
    
        def checkInlineTmpl(tmpl: in.InlineTmpl) = out.InlineTmpl(
            stmts = tmpl.stmts.map(checkStmt)
        )
        
    }

}