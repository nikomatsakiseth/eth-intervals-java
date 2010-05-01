package harmonic.compiler

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Map

import scala.util.parsing.input.Position

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
            case List() => SymTab.empty 
            case List(symTab) => symTab

            // General case of multiple supertypes:
            case symTab0 :: remSymTabs => {
                // Retain a (key, value) pair if all supertypes agree:
                def retainPair(pair: (String, SymTab.Entry)) = {
                    val (key, value) = pair
                    superSymtabs.forall { symTab =>
                        symTab.get(key) match {
                            case Some(v) => (v == value) // mapped to same value: good
                            case None => true            // not mapped at all: good
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
        def addEntry(pos: Position, text: String): InScope
        
        def resolveParam(param: in.Param): out.Param = withPosOf(param, param match {
            case tupleParam: in.TupleParam => resolveTupleParam(tupleParam)
            case varParam: in.VarParam => resolveVarParam(varParam)
        })

        def resolveTupleParam(tupleParam: in.TupleParam) = {
            val outParams = tupleParam.params.map(resolveParam)
            withPosOf(tupleParam, out.TupleParam(outParams))
        }

        def resolveVarParam(varParam: in.VarParam) = withPosOf(varParam, {
            val outAnnotations = varParam.annotations.map(scope.resolveAnnotation)
            val outTypeRef = scope.resolveOptionalTypeRef(varParam.tref)
            scope = addEntry(varParam.pos, varParam.name.name.text)
            out.VarParam(
                annotations = outAnnotations,
                tref = outTypeRef,
                name = varParam.name,
                sym = ()
            )
        })
        
        val outParams = inParams.map(resolveParam)
    }
    
    class ResolveClassParams(className: Name.Class, scope0: InScope, inParam: in.Param)
    extends ResolveParams(scope0, List(inParam)) {
        def outParam = outParams.head
        
        def addEntry(pos: Position, text: String) = {
            scope.symTab.get(text) match {
                case Some(_) => {
                    // TODO Decide when params on inner classes can shadow
                    Error.ShadowedClassParam(text).report(state, pos)
                }
                case _ =>
            }
            scope.addEntry(SymTab.InstanceField(Name.Member(className, text)))
        }
    }
    
    class ResolveMethodParams(scope0: InScope, inParams: List[in.Param])
    extends ResolveParams(scope0, inParams) {
        def addEntry(pos: Position, text: String) = {
            scope.symTab.get(text) match {
                case Some(SymTab.LocalVar(_)) => {
                    Error.ShadowedMethodParam(text).report(state, pos)
                }
                case _ =>
            }
            scope.addEntry(SymTab.LocalVar(Name.LocalVar(text)))
        }
    }
    
    class ResolveLvalue(var scope: InScope, inLvalue: in.Lvalue) {
        def resolveLvalue(lvalue: in.Lvalue): out.Lvalue = withPosOf(lvalue, lvalue match {
            case in.TupleLvalue(lvalues) => {
                val outLvalues = lvalues.map(resolveLvalue)
                out.TupleLvalue(outLvalues)                
            }
            case in.DeclareVarLvalue(ann, tref, name, ()) => 
                resolveDeclareVarLvalue(lvalue, ann, tref, name)
            case in.ReassignVarLvalue(name, ()) => 
                resolveReassignVarLvalue(lvalue, name)
            case in.FieldLvalue(name, ()) => 
                resolveFieldLvalue(lvalue, name)
        })

        def resolveDeclareVarLvalue(
            lvalue: in.Lvalue,
            annotations: List[in.Annotation],
            tref: in.OptionalParseTypeRef,
            name: Ast.LocalName
        ) = withPosOf(lvalue, {
            val outAnnotations = annotations.map(scope.resolveAnnotation)
            val outTypeRef = scope.resolveOptionalTypeRef(tref)
            
            scope.symTab.get(name.name.text) match {
                case Some(SymTab.LocalVar(_)) =>
                    Error.ShadowedLocalVar(name.name.text).report(state, name.pos)
                case _ =>
            }
            
            scope = scope.addEntry(SymTab.LocalVar(name.name))
            out.DeclareVarLvalue(outAnnotations, outTypeRef, name, ())
        })
        
        def resolveReassignVarLvalue(
            lvalue: in.Lvalue,
            name: Ast.LocalName
        ) = withPosOf(lvalue, {
            val text = name.name.text
            scope.symTab.get(text) match {
                case None => {
                    // Actually a variable declaration w/ inferred type:
                    resolveDeclareVarLvalue(lvalue, List(), in.InferredTypeRef(), name)
                }
                
                case Some(SymTab.LocalVar(lname)) => {
                    out.ReassignVarLvalue(Ast.LocalName(lname), ())
                }
                
                case Some(SymTab.StaticField(mname)) => {
                    out.FieldLvalue(Ast.MemberName(mname), ())
                }
                
                case Some(SymTab.InstanceField(mname)) => {
                    if(scope.isStatic) {
                        Error.NotInStaticScope(mname).report(state, name.pos)
                    }
                    out.FieldLvalue(Ast.MemberName(mname), ())
                }
                
                case Some(SymTab.Type(mname)) => {
                    // An error will be reported in Lower:
                    // Error.NotField(name).report(state, name.pos)
                    out.FieldLvalue(Ast.MemberName(mname), ())
                }
            }
        })
        
        def resolveFieldLvalue(
            lvalue: in.Lvalue,
            name: in.MN
        ) = withPosOf(lvalue, {
            out.FieldLvalue(resolveMemberName(name), ())
        })
        
        val outLvalue = resolveLvalue(inLvalue)
    }
    
    def resolveMemberName(relName: in.RelName) = {
        relName match {
            case in.RelBase(name) => 
                Ast.ClasslessMemberName(Name.ClasslessMember(name))
            case in.RelDot(base, name) => {
                val className = resolveName(base).name
                Ast.MemberName(Name.Member(className, name))
            }
        }
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
                members = cdecl.members.map(resolve.scope.resolveMember(className, _)),
                sym = ()
            )            
        })

        def resolveAnnotation(ann: in.Annotation) = withPosOf(ann, 
            out.Annotation(resolveName(ann.name))
        )

        def resolveMember(className: Name.Class, mem: in.MemberDecl): out.MemberDecl = withPosOf(mem, {
            mem match {
                case decl: in.IntervalDecl => resolveIntervalDecl(className, decl)
                case decl: in.MethodDecl => resolveMethodDecl(decl)
                case decl: in.FieldDecl => resolveFieldDecl(className, decl)
                case decl: in.RelDecl => resolveRelDecl(decl)
            }
        })

        def resolveIntervalDecl(className: Name.Class, decl: in.IntervalDecl) = withPosOf(decl, {
            out.IntervalDecl(
                annotations = decl.annotations.map(resolveAnnotation),
                name = Ast.MemberName(Name.Member(className, decl.name.nm)),
                optParent = decl.optParent.map(resolvePathToPath),
                optBody = decl.optBody.map(resolveBody)
            )
        })
        
        def resolveMethodDecl(decl: in.MethodDecl) = withPosOf(decl, {
            val resolveParam = new ResolveMethodParams(this, decl.params)
            val mthdScope = resolveParam.scope
            out.MethodDecl(
                annotations = decl.annotations.map(mthdScope.resolveAnnotation),
                receiverSym = (),
                name = decl.name,
                params = resolveParam.outParams,
                returnTref = mthdScope.resolveOptionalTypeRef(decl.returnTref),
                requirements = decl.requirements.map(mthdScope.resolveRequirement),
                optBody = decl.optBody.map(mthdScope.resolveBody)
            )
        })

        def resolveRequirement(requirement: in.PathRequirement) = withPosOf(requirement, {
            out.PathRequirement(
                left = resolvePathToPath(requirement.left),
                rel = requirement.rel,
                right = resolvePathToPath(requirement.right)
            )
        })

        def resolveFieldDecl(className: Name.Class, decl: in.FieldDecl) = withPosOf(decl, {
            out.FieldDecl(
                annotations = decl.annotations.map(resolveAnnotation),
                name = Ast.MemberName(Name.Member(className, decl.name.nm)),
                tref = resolveOptionalTypeRef(decl.tref),
                optBody = decl.optBody.map(resolveBody)
            )
        })

        def resolveRelDecl(decl: in.RelDecl) = withPosOf(decl, {
            out.RelDecl(
                annotations = decl.annotations.map(resolveAnnotation),
                left = resolvePathToPath(decl.left),
                kind = decl.kind,
                right = resolvePathToPath(decl.right)
            )
        })

        // ___ Paths ____________________________________________________________
        
        type EitherQualOr[T] = Either[Name.Qual, T]
        
        def resolvePathToEither(path: in.AstPath): EitherQualOr[out.AstPath] = withPosOfR(path, {
            path match {
                case in.PathBase(in.RelBase(text), ()) => {
                    symTab.get(text) match {
                        case None => {
                            Left(resolveAgainstPackage(Name.Root, text))
                        }

                        case Some(SymTab.LocalVar(name)) => {
                            Right(out.PathBase(Ast.LocalName(name), ()))
                        }

                        case Some(SymTab.Type(mname)) => {
                            Error.NotField(mname).report(state, path.pos)
                            Right(out.PathErr(path.toString))
                        }

                        case Some(SymTab.InstanceField(mname)) if isStatic => {
                            Error.NotInStaticScope(mname).report(state, path.pos)
                            Right(out.PathErr(path.toString))
                        }
                            
                        case Some(SymTab.InstanceField(mname)) => {
                            Right(out.PathDot(
                                out.Var(Ast.LocalName(mname.ThisVar), ()),
                                Ast.MemberName(name),
                                (), ()
                            ))
                        }

                        case Some(SymTab.StaticField(name)) => {
                            Right(out.PathBase(Ast.MemberName(name), ()))
                        }
                    }
                }
                
                case in.PathBase(relName, ()) => {
                    // TODO we assume `relName` is static, but it could refer to a field of `this`
                    Right(out.PathBase(resolveMemberName(relName), ()))
                }
                
                case in.PathDot(base, relBase @ in.RelBase(text), (), ()) => {
                    resolvePathToEither(base) match {
                        case Left(pkgName: Name.Package) =>
                            Left(resolveAgainstPackage(pkgQual, text))
                        case Left(className: Name.Class) =>
                            resolveAgainstClass(clsQual, text) match {
                                case Some(innerClassName) => Left(innerClassName)
                                case None => {
                                    val memberName = Name.Member(className, text)
                                    Right(out.PathBase(Ast.MemberName(memberName)))
                                }
                            }
                        case Right(outBase) => {
                            val classlessName = Name.ClasslessMember(text)
                            Right(out.PathDot(outBase, Ast.ClasslessMemberName(classlessName), (), ()))
                        }
                    }
                }
                
                case in.PathDot(base, memberName: in.RelDot, (), ()) => {
                    resolvePathToEither(base) match {
                        case Left(name) => {
                            Error.ExpPath(name).report(state, path.pos)
                            Right(out.PathErr(()))
                        }
                        case Right(outBase) => {
                            Right(out.PathDot(outBase, resolveMemberName(memberName), (), ()))
                        }
                    }
                }
            }
        })
        
        def resolvePathToPath(path: in.AstPath) = withPosOf(path, {
            resolvePathToEither(path) match {
                case Left(name) => {
                    Error.ExpPath(name).report(state, path.pos)
                    out.PathErr(path.toString)
                }
                
                case Right(path) => path
            }
        })
        
        def resolvePathToClassName(path: in.AstPath) = withPosOf(path, {
            resolvePathToEither(path) match {
                case Left(names) =>
                    resolveToClass(tref.pos, names).getOrElse(Name.ObjectClass)

                case Right(path) => {
                    Error.ExpType(path).report(state, path.pos)
                    Name.ObjectClass
                }
            }
        })
        
        // ___ Types ____________________________________________________________
        
        def resolveOptionalTypeRef(otref: in.OptionalParseTypeRef): out.OptionalResolveTypeRef = {
            withPosOf(otref, 
                otref match {
                    case in.InferredTypeRef() => out.InferredTypeRef()
                    case tref: in.ParseTypeRef => resolveTypeRef(tref)
                }
            )
        }
        
        def resolveTypeRef(tref: in.ParseTypeRef): out.ResolveTypeRef = withPosOf(tref, tref match {
            case in.NullType() => out.NullType()
            
            case in.TupleType(trefs) => out.TupleType(trefs.map(resolveTypeRef))
            
            // `a.b.c` could be path or class type:
            case in.PathType(in.PathDot(base, in.SimpleName(name), (), ())) => {
                resolvePathToEither(base) match {
                    case Left(names) => {
                        val className = resolveToClass(tref.pos, name :: names).getOrElse(Name.ObjectClass)
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

        def resolveTypeTypeArg(targ: in.TypeTypeArg): out.TypeTypeArg = withPosOf(targ, {
            out.TypeTypeArg(
                resolveMemberName(targ.name), targ.rel, resolveTypeRef(targ.typeRef)
            )           
        })

        def resolvePathTypeArg(targ: in.PathTypeArg): out.PathTypeArg = withPosOf(targ, {
            out.PathTypeArg(
                resolveMemberName(targ.name), targ.rel, resolvePath(targ.path)
            )
        })
        
        // ___ Statements and Expressions _______________________________________
        
        def resolveStmts(stmts: List[in.Stmt]): List[out.Stmt] = stmts match {
            case (expr: in.Expr) :: stmts => 
                resolveExpr(expr) :: resolveStmts(stmts)
                
            case stmt @ in.Assign(lv, rv) :: stmts => {
                val outRv = resolveExpr(rv)
                val resolveLocal = new ResolveLvalue(this, lv)
                val outAssign = withPosOf(stmt, out.Assign(resolveLocal.outLvalue, outRv))
                val outStmts = resolveLocal.scope.resolveStmts(stmts)
                outAssign :: outStmts
            }
            case stmt @ in.Labeled(name, body) :: stmts => {
                val outStmt = withPos(stmt, out.Labeled(name, resolveBody(body)))
                outStmt :: resolveStmts(stmts)
            }
        }

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
            out.MethodCall(
                rcvr = resolveRcvr(rcvr), 
                name = expr.name, 
                args = expr.args.map(resolveExpr),
                data = ()
            )
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

        def resolveTuple(tuple: in.Tuple) = withPosOf(tuple, 
            out.Tuple(tuple.exprs.map(resolveExpr))
        )

        def resolveBlock(tmpl: in.Block) = withPosOf(tmpl, 
            out.Block(
                async = tmpl.async,
                returnTref = resolveOptionalTypeRef(tmpl.returnTref),
                returnTy = (),
                param = resolveTupleLocal(tmpl.param),
                stmts = resolveStmts(tmpl.stmts),
                ty = ()
            )
        )

        def resolveBody(body: in.Body) = withPosOf(body, 
            out.Body(resolveStmts(body.stmts))
        )
    }
}