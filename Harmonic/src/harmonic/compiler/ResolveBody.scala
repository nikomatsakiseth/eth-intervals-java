package harmonic.compiler

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Map

import Ast.{Parse => in}
import Ast.{Resolve => out}
import Util._

/** After the headers have been resolved, we can resolve the names that
  * appear in the rest of the body.  
  *                                                                         
  * We use a symbol table to track the user-defined names that are in 
  * scope.  Entries in the symbol table reflect fields, ghosts, types,
  * or local variables.  If a particular name cannot be found in the symbol
  * table, we will then try to interpret it as a type or package.
  * 
  * The symbol tables are initially constructed using the member names
  * extracted during header resolution (see `ResolveHeader`). */
case class ResolveBody(state: CompilationState, compUnit: in.CompUnit) 
extends Resolve(state, compUnit) 
{
    def resolveClassBody(csym: Symbol.Class, cdecl: in.ClassDecl) = {
        val symTab = constructSymbolTable(csym)
        val outCdecl = InScope(symTab, true).resolveClassDecl(cdecl)

        if(state.config.dumpResolvedTrees) {
            outCdecl.println(PrettyPrinter.stdout)
        }
    }
    
    /** Creates a symbol table containing entries defined by
      * supertypes of `csym`.  A key is only included if all
      * supertypes agree on its value. */
    def mergeSuperSymbolTables(csym: Symbol.Class): SymTab.Map = {
        val superCsyms = csym.superClassNames(state).map(state.classes)
        val superSymtabs = superCsyms.map(constructSymbolTable)
        superSymtabs match {
            // Micro-optimize the case of zero or one supertypes:
            case List() => emptySymTab 
            case List(symTab0) => symTab 

            // General case of multiple supertypes:
            case symTab0 :: remSymTabs => {
                // Retain a (key, value) pair if all supertypes agree:
                def retainPair(pair: (String, SymTabEntry)) = {
                    val (Key, Value) = pair
                    superSymtabs.forall { symTab =>
                        symTab.get(Key) match {
                            case Some(Value) => true // mapped to same value: good
                            case Some(_) => false    // mapped to something else: bad!
                            case None => true        // not mapped at all: good
                        }
                    }
                }                    
                remSymTabs.foldLeft(symTab0.filter(retainPair)) {
                    case (m, symTabN) => m ++ symTabN.filter(retainPair)
                }
            }
        }
    }

    /** Creates a symbol table containing the members of `csym` */
    def constructSymbolTable(csym: Symbol.Class): SymTab.Map = {
        var superSymTab = mergeSuperSymbolTables(csym)
        csym.varMembers(state).foldLeft(superSymTab)(_ + _)
    }
    
    class ResolveParams(var scope: InScope, inParams: List[in.Param]) {
        def addEntry(text: String): InScope
        
        def resolveParam(param: in.Param): out.Param = withPosOf(param, param match {
            case tupleParam: in.TupleParam => resolveTupleParam(tupleParam)
            case varParam: in.VarParam => resolveVarParam(varParam)
        })

        def resolveTupleParam(tupleParam: in.TupleParam) = {
            val outParams = tupleParam.params.map(resolveParam)
            withPosOf(tupleParam, out.TupleParam(outParams))
        }

        def resolveVarParam(varParam: in.VarParam) = {
            val outAnnotations = varParam.annotations.map(scope.resolveAnnotation)
            val outTypeRef = scope.resolveTypeRef(varParam.tref)
            scope = addEntry(varParam.name.text)
            withPosOf(varParam, out.VarParam(
                annotations = outAnnotations,
                tref = outTypeRef,
                name = varParam.name,
                sym = ()
            ))            
        }
        
        val outParams = inParams.map(resolveParam)
    }
    
    class ResolveClassParams(className: Name.Class, scope0: InScope, inParam: in.Param)
    extends ResolveParams(scope0, List(inParams)) {
        def outParam = outParams.head
        
        def addEntry(text: String) = {
            scope.symTab.get(entry.text) match {
                case Some(_) => {
                    // TODO Decide when params on inner classes can shadow
                    state.reporter.report(
                        localName.pos,
                        "shadowed.class.param",
                        text
                    )
                }
                case _ =>
            }
            scope.addEntry(SymTab.InstanceField(Name.MemberVar(className, text)))
        }
    }
    
    class ResolveMethodParams(scope0: InScope, inParams: List[in.Param])
    extends ResolveParam(scope0, inParams) {
        def addEntry(text: String) = {
            scope.symTab.get(entry.text) match {
                case Some(SymTab.LocalVar(_)) => {
                    state.reporter.report(
                        localName.pos,
                        "shadowed.method.param",
                        text
                    )
                }
                case _ =>
            }
            scope.addEntry(SymTab.LocalVar(Name.LocalVar(text)))
        }
    }
    
    class ResolveLvalue(var scope: InScope, inParams: List[in.Param]) {
        def resolveLvalue(lvalue: in.Lvalue): out.Lvalue = withPosOf(lvalue, lvalue match {
            case lvalue: in.TupleLvalue => resolveTupleLvalue(lvalue)
            case in.DeclareVarLvalue(ann, tref, name, ()) => resolveDeclareVar(lvalue, ann, tref, name)
            case in.ReassignVarLvalue(name, ()) => resolveReassignLvalue(lvalue, name)
            case in.FieldLvalue(name, ()) => resolveFieldLvalue(lvalue, name)
        })

        def resolveTupleLvalue(lvalue: in.TupleLvalue) = withPosOf(lvalue, {
            val outLvalues = lvalue.lvalues.map(resolveLvalue)
            out.TupleLvalue(outLvalues)
        })

        def resolveDeclareVarLvalue(
            lvalue: in.Lvalue,
            annotations: List[in.Annotation],
            tref: in.OptionalTypeRef,
            name: in.SimpleName
        ) = withPosOf(lvalue, {
            val outAnnotations = lvalue.annotations.map(scope.resolveAnnotation)
            val outTypeRef = scope.resolveOptionalTypeRef(lvalue.tref)
            
            scope.symTab.get(name.text) match {
                case Some(SymTab.LocalVar(_)) => {
                    state.reporter.report(
                        localName.pos,
                        "shadowed.local.var",
                        name.text
                    )
                }
                case _ =>
            }
            
            val localName = Ast.LocalName(Name.LocalVar(name.text))
            scope = scope.addEntry(SymTab.Local(localName.name))
            out.DeclareVarLvalue(outAnnotations, outTypeRef, localName, ())
        })
        
        def resolveReassignVarLvalue(
            lvalue: in.Lvalue,
            name: in.SimpleName
        ) = withPosOf(lvalue, {
            scope.symTab.get(name.text) match {
                case None => {
                    // Actually a variable declaration:
                    val localName = Ast.LocalName(Name.LocalVar(name.text))
                    resolveDeclareVarLvalue(lvalue, List(), in.OptionalTypeRef(), localName)
                }
                
                case Some(SymTab.Local(name)) => {
                    out.ReassignVarLvalue(Ast.VarName(name), ())
                }
                
                case Some(SymTab.StaticField(name)) => {
                    resolveFieldLvalue(lvalue, Ast.MemberName(name))
                }
                
                case Some(SymTab.InstanceField(name)) => {
                    if(scope.isStatic) {
                        state.reporter.report(
                            expr.pos, 
                            "cannot.reference.in.static.context", 
                            name.toString
                        )
                    }
                    resolveFieldLvalue(lvalue, Ast.MemberName(name))
                }
                
                case Some(SymTab.Type(name)) => {
                    state.reporter.report(
                        localName.pos,
                        "not.assignable",
                        name.toString
                    )
                }
            }
        })
        
        def resolveFieldLvalue(
            lvalue: in.Lvalue,
            name: Ast.MemberName
        ) = withPosOf(lvalue, {
            out.FieldLvalue(name, ())
        })
        
        val outParams = inParams.map(resolveParam)
    }
    
    case class InScope(
        symTab: SymTab.Map,
        isStatic: Boolean
    ) {
        def addEntry(entry: SymTab.Entry) = {
            copy(symTab + entry)            
        }
        
        // ___ Declarations _____________________________________________________

        def resolveClassDecl(cdecl: in.ClassDecl) = withPosOf(cdecl, {
            val className = cdecl.name.toClass(compUnit.pkg.name)
            val resolve = new ResolveClassParams(className, this, cdecl.pattern)
            
            out.ClassDecl(
                name = Ast.ClassName(className),
                annotations = cdecl.annotations.map(resolveAnnotation),
                superClasses = cdecl.superClasses.map(resolveName),
                pattern = resolve.outParam,
                members = cdecl.members.map(resolve.scope.resolveMember),
                sym = ()
            )            
        })

        def resolveAnnotation(ann: in.Annotation) = withPosOf(ann, out.Annotation(
            name = resolveName(ann.name)
        ))

        def resolveMember(mem: in.MemberDecl): out.MemberDecl = withPosOf(mem, mem match {
            case decl: in.ClassDecl => resolveClassDecl(decl)
            case decl: in.IntervalDecl => resolveIntervalDecl(decl)
            case decl: in.MethodDecl => resolveMethodDecl(decl)
            case decl: in.FieldDecl => resolveFieldDecl(decl)
            case decl: in.RelDecl => resolveRelDecl(decl)
        })

        def resolveIntervalDecl(decl: in.IntervalDecl) = withPosOf(decl, out.IntervalDecl(
            annotations = decl.annotations.map(resolveAnnotation),
            name = decl.name,
            optParent = decl.optParent.map(resolvePath),
            optBody = decl.optBody.map(resolveBody)
        ))
        
        def resolveMethodDecl(decl: in.MethodDecl) = {
            val resolveParam = new ResolveMethodParams(this, decl.params)
            val mthdScope = resolveParam.scope
            withPosOf(decl, out.MethodDecl(
                annotations = decl.annotations.map(mthdScope.resolveAnnotation),
                receiverSym = (),
                parts = decl.parts.zip(resolveParam.outParams).map(mthdScope.resolveDeclPart),
                returnTref = mthdScope.resolveOptionalTypeRef(decl.returnTref),
                returnTy = (),
                requirements = decl.requirements.map(mthdScope.resolveRequirement),
                optBody = decl.optBody.map(mthdScope.resolveBody)
            ))
        }

        def resolveDeclPart(pair: (in.DeclPart, out.Param)) = {
            val (decl, outParam) = pair
            withPosOf(decl, out.DeclPart(decl.ident, outParam))
        }

        def resolveRequirement(requirement: in.PathRequirement) = withPosOf(requirement, out.PathRequirement(
            left = resolvePath(requirement.left),
            rel = requirement.rel,
            right = resolvePath(requirement.right)
        ))

        def resolveFieldDecl(decl: in.FieldDecl) = withPosOf(decl, out.FieldDecl(
            annotations = decl.annotations.map(resolveAnnotation),
            name = decl.name,
            tref = resolveOptionalTypeRef(decl.tref),
            ty = (),
            optBody = decl.optBody.map(resolveBody)
        ))

        def resolveRelDecl(decl: in.RelDecl) = withPosOf(decl, out.RelDecl(
            annotations = decl.annotations.map(resolveAnnotation),
            left = resolvePath(decl.left),
            kind = decl.kind,
            right = resolvePath(decl.right)
        ))

        // ___ Paths ____________________________________________________________
        //
        // 
        
        type EitherClassNameOr[T] = Either[List[String], T]
        
        def resolvePathToEither(path: in.AstPath): EitherClassNameOr[out.AstPath] = withPosOfR(path, {
            path match {
                case in.PathBase(in.SimpleName(text), ()) => {
                    symTab.get(text) match {
                        case None => {
                            Left(List(text))
                        }

                        case Some(SymTab.Local(name)) => {
                            Right(out.PathBase(Ast.LocalName(name), ()))
                        }

                        case Some(SymTab.Type(_)) => {
                            state.reporter.report(expr.pos, "cannot.use.type.var.here", expr)
                            Right(out.PathErr(()))
                        }

                        case Some(SymTab.InstanceField(name)) if isStatic => {
                            state.reporter.report(expr.pos, "cannot.reference.in.static.context", name.toString)
                            Right(out.PathErr(()))
                        }
                            
                        case Some(SymTab.InstanceField(name)) => {
                            Right(out.PathField(
                                out.Var(Ast.LocalName(Name.ThisVar), ()),
                                Ast.MemberName(name),
                                (), ()
                            ))
                        }

                        case Some(SymTab.StaticField(name)) => {
                            Right(out.PathBase(out.MemberName(name), ()))
                        }
                    }
                }
                
                case in.PathBase(memberName: in.MemberName, ()) => {
                    // TODO Find out whether member is static or not.
                    Right(out.PathBase(out.MemberName(name), ()))
                }
                
                case in.PathDot(base, simpleName @ in.SimpleName(text), (), ()) => {
                    resolvePathToEither(base) match {
                        case Left(names) => text :: names
                        case Right(outBase) => out.PathDot(outBase, simpleName, (), ())
                    }
                }
                
                case in.PathDot(base, memberName: in.MemberName, (), ()) => {
                    resolvePathToEither(base) match {
                        case Left(names) => {
                            state.reporter.report(path.pos, "no.such.var", names.last)
                            out.PathErr()                            
                        }
                        case Right(outBase) => out.PathDot(outBase, memberName, (), ())
                    }
                }
            }
        })
        
        def resolvePathToPath(path: in.AstPath) = withPosOf(path, {
            resolvePathToEither(path) match {
                case Left(names) => {
                    state.reporter.report(path.pos, "no.such.var", names.last)
                    out.PathErr()
                }
                
                case Right(path) => path
            }
        })
        
        def resolvePathToClassName(path: in.AstPath) = withPosOf(path, {
            resolvePathToEither(path) match {
                case Left(names) =>
                    resolveToClass(tref.pos, names).getOrElse(Name.ObjectQual)
                        
                case Right(path) => {
                    state.reporter.report(
                        tref.pos,
                        "expected.type.here",
                        path.toString
                    )              
                    Name.ObjectQual      
                }
            }
        })
        
        // ___ Types ____________________________________________________________
        
        def resolveOptionalTypeRef(otref: in.OptionalTypeRef): out.OptionalTypeRef = withPosOf(otref, otref match {
            case in.InferredTypeRef() => out.InferredTypeRef()
            case tref: in.ParseTypeRef => resolveTypeRef(tref)
        })
        
        def resolveTypeRef(tref: in.ParseTypeRef): out.ResolveTypeRef = withPosOf(tref, tref match {
            case in.NullType() => out.NullType()
            
            case in.TupleType(trefs) => out.TupleType(trefs.map(resolveTypeRef))
            
            // `a.b.c` could be path or class type:
            case in.PathType(in.PathDot(base, in.SimpleName(name), (), ())) => {
                resolvePathToEither(base) match {
                    case Left(names) => {
                        val className = resolveToClass(tref.pos, name :: names).getOrElse(Name.ObjectQual)
                        out.ClassType(Ast.ClassName(className), List())
                    }
                    
                    case Right(outBase) =>
                        out.TypeVar(outBase, name)                        
                }
            }
            
            // `a.b.(foo.Bar.T)` must be path type:
            case in.PathType(in.PathDot(base, memberName: in.MemberName, (), ())) => {
                val outBase = resolvePathToPath(base)
                out.TypeVar(outBase, memberName)                        
            }                
            
            // `a` must be class type:
            case in.PathType(path: in.PathBase) => {
                val className = resolvePathToClassName(path)
                out.ClassType(Ast.ClassName(className), List())            
            }
            
            // `a.b.c[...]` must be class type:
            case in.ConstrainedType(path, typeArgs) => {
                val className = resolvePathToClassName(path)
                val outTypeArgs = typeArgs.map(resolveTypeArg)
                out.ClassType(className, outTypeArgs)
            }
        })

        def resolveTypeArg(targ: in.TypeArg): out.TypeArg = withPosOf(targ, targ match {
            case ttarg: in.TypeTypeArg => resolveTypeTypeArg(ttarg)
            case ptarg: in.PathTypeArg => resolvePathTypeArg(ptarg)
        })

        def resolveTypeTypeArg(targ: in.TypeTypeArg): out.TypeTypeArg = withPosOf(targ, out.TypeTypeArg(
            name = targ.name, rel = targ.rel, typeRef = resolveTypeRef(targ.typeRef)
        ))

        def resolvePathTypeArg(targ: in.PathTypeArg): out.PathTypeArg = withPosOf(targ, out.PathTypeArg(
            name = targ.name, rel = targ.rel, path = resolvePath(targ.path)
        ))
        
        // ___ Statements and Expressions _______________________________________
        
        def resolveStmts(stmts: List[in.Stmt]) = stmts match {
            case (expr: in.Expr) :: stmts => 
                resolveExpr(expr) :: resolveStmts(stmts)
                
            case stmt @ in.Assign(lv, rv) :: stmts => {
                val outRv = resolveExpr(rv)
                val resolveLocal = new ResolveLocal(this, lv)
                val outAssign = withPosOf(stmt, out.Assign(resolveLocal.outLocal, rv))
                val outStmts = resolveLocal.scope.resolveStmts(stmts)
                outAssign :: outStmts
            }
            case stmt @ in.Labeled(name, body) :: stmts => {
                val outStmt = withPos(stmt, out.Labeled(name, resolveBody(body)))
                outStmt :: resolveStmts(stmts)
            }
        }

        def resolveTupleLocal(tupLocal: in.TupleLocal): out.TupleLocal = withPosOf(tupLocal, {
            out.TupleLocal(tupLocal.locals.map(resolveLocal))
        })

        def resolveLocal(local: in.Local): out.Local = withPosOf(local, local match {
            case tupLocal: in.TupleLocal => resolveTupleLocal(tupLocal)
            case in.VarLocal(annotations, tref, name, ()) => out.VarLocal(
                annotations = annotations.map(resolveAnnotation),
                tref = resolveOptionalTypeRef(tref),
                name = name,
                sym = ()
            )
        })

        def resolveNewCtor(expr: in.NewCtor): out.NewCtor = withPosOf(expr, {
            out.NewCtor(
                tref = resolveTypeRef(expr.tref),
                arg = resolveExpr(expr.arg),
                msym = (),
                ty = ()
            )
        })

        def resolveNewAnon(expr: in.NewAnon): out.NewAnon = withPosOf(expr, {
            out.NewAnon(
                tref = resolveTypeRef(expr.tref),
                arg = resolveExpr(expr.arg),
                members = expr.members.map(resolveMember),
                csym = (),
                msym = (),
                ty = ()
            )
        })

        def pathToExpr(path: out.Path): out.Expr = withPosOf(path, {
            case out.PathErr(()) => out.Null(())
            case out.PathBase(name, ()) => out.Var(name, ())
            case out.PathDot(base, name, (), ()) => out.Field(pathToExpr(base), name, (), ())
        })
        
        def resolveVar(expr: Ast.Node, text: String): out.Expr = withPosOf(expr, {
            val inPath = withPosOf(expr, in.PathBase(expr.name, ()))
            pathToExpr(resolvePathToPath(inPath))
        })
        
        def resolveOwner(owner: in.Owner): out.Owner = withPosOf(owner, {
            owner match {
                case in.Static(name) => {
                    state.requireLoadedOrLoadable(name)
                    out.Static(name)
                }
                
                case in.PathExpr(path) => {
                    resolvePathToEither(path) match {
                        case Left(names) => {
                            // Could not resolve the owner `a.b` as an expression.
                            // `a.b` must be a type name and `c` a static field
                            // for this to be valid.
                            resolveToClass(names) match {
                                case None => out.Null(()) // Error in resolving `a.b`.
                                case Some(className) => out.Static(className)
                            }
                        }
                        
                        case Right(outPath) => pathToExpr(outPath)
                    }
                }
                
                case expr: in.Expr => {
                    resolveExpr(expr)
                }
            }
        })
        
        // Only invoked for the final field in a path.
        // i.e., Given `a.b.c`, would only be invoked on `.c`.
        def resolveField(expr: in.Field) = withPosOf(expr, {
            out.Field(resolveOwner(expr.owner), expr.name, (), ())
        })
        
        def resolveRcvr(rcvr: in.Rcvr): out.Rcvr = withPosOf(rcvr, {
            rcvr match {
                case in.Super(()) => out.Super(())
                case owner: in.Owner => resolveOwner(owner) 
            }
        })
        
        def resolveMethodCall(expr: in.MethodCall) = {
            out.MethodCall(resolveRcvr(rcvr), parts.map(resolvePart), ())
        }
        
        def resolvePathExpr(expr: in.PathExpr) = {
            pathToExpr(resolvePathToPath(expr.path))
        }

        def resolveExpr(expr: in.Expr): out.Expr = withPosOf(expr, expr match {
            case tuple: in.Tuple => resolveTuple(tuple)
            case tmpl: in.Block => resolveBlock(tmpl)
            case in.Cast(v, t, ()) => out.Cast(resolveExpr(v), resolveTypeRef(t), ())
            case e: in.Literal => resolveLiteral(e)
            case e: in.PathExpr => resolvePathExpr(e)
            case e: in.Var => resolveVarExpr(e)
            case e: in.Field => resolveField(e)
            case e: in.MethodCall => resolveMethodCall(e)
            case e: in.NewCtor => resolveNewCtor(e)
            case e: in.NewAnon => resolveNewAnon(e)
            case in.Null(()) => out.Null(())
            case in.ImpVoid(()) => out.ImpVoid(())
            case in.ImpThis(()) => out.Var(Ast.LocalName(Name.ThisVar), ())
        })

        def resolveLiteral(expr: in.Literal) = {
            state.requireLoadedOrLoadable(expr.pos, Name.Qual(expr.obj.getClass))
            out.Literal(expr.obj, ())            
        }

        def resolvePart(part: in.CallPart) = withPosOf(part, out.CallPart(
            ident = part.ident,
            arg = resolveExpr(part.arg)
        ))

        def resolveTuple(tuple: in.Tuple) = withPosOf(tuple, out.Tuple(
            exprs = tuple.exprs.map(resolveExpr)
        ))

        def resolveBlock(tmpl: in.Block) = withPosOf(tmpl, out.Block(
            async = tmpl.async,
            returnTref = resolveOptionalTypeRef(tmpl.returnTref),
            returnTy = (),
            param = resolveTupleLocal(tmpl.param),
            stmts = resolveStmts(tmpl.stmts),
            ty = ()
        ))

        def resolveBody(body: in.Body) = withPosOf(body, out.Body(
            stmts = resolveStmts(body.stmts)
        ))
    }
}