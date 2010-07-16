package harmonic.compiler

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Map

import scala.util.parsing.input.Position

import com.smallcultfollowing.lathos.Lathos

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
case class ResolveBody(global: Global, compUnit: in.CompUnit) 
extends Resolve(global, compUnit) 
{
    def resolveClassBody(csym: ClassFromSource, cdecl: in.ClassDecl) = {
        
        // Class parameters must be resolved starting from an empty scope.
        // This guarantees that they don't reference other fields in their
        // types.  They are permitted to reference other class params, but
        // only those that appear earlier in the list.
        val emptyScope = InScope(Map())
        val resolve = new ResolveClassParams(csym.name, emptyScope, cdecl.pattern)

        // Process the rest of the class with all members in scope:
        val classScope = InScope(constructSymbolTable(csym))
        
        csym.ResolvedSource.v = withPosOf(cdecl,
            out.ClassDecl(
                name = Ast.ClassName(csym.name),
                annotations = cdecl.annotations.map(classScope.resolveAnnotation),
                extendsDecls = cdecl.extendsDecls.map(classScope.resolveExtendsDecl),
                pattern = resolve.outParam,
                members = cdecl.members.map(classScope.resolveMember(csym.name, _)),
                sym = (),
                thisSym = ()
            )
        )

        if(global.config.dumpResolvedTrees) {
            PrettyPrinter.dumpTo(System.out, csym.resolvedSource)
        }
    }
    
    /** Creates a symbol table containing entries defined by
      * supertypes of `csym`.  A key is only included if all
      * supertypes agree on its value. */
    def mergeSuperSymbolTables(csym: ClassSymbol): SymTab.Map = {
        val superCsyms = csym.superClassNames.map(global.csym)
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
    def constructSymbolTable(csym: ClassSymbol): SymTab.Map = {
        var superSymTab = mergeSuperSymbolTables(csym)
        csym.varMembers.foldLeft(superSymTab)(_ + _) + SymTab.LocalVar(Name.ThisLocal)
    }
    
    abstract class ResolveParams(var scope: InScope, inParams: List[in.Param[Unit]]) {
        def addEntry(pos: Position, text: String): InScope
        
        def resolveParam(param: in.Param[Unit]): out.Param[Unit] = withPosOf(param, param match {
            case tupleParam: in.TupleParam[Unit] => resolveTupleParam(tupleParam)
            case varParam: in.VarParam[Unit] => resolveVarParam(varParam)
        })

        def resolveTupleParam(tupleParam: in.TupleParam[Unit]) = withPosOf(tupleParam, {
            val outParams = tupleParam.params.map(resolveParam)
            out.TupleParam(outParams)
        })

        def resolveVarParam(varParam: in.VarParam[Unit]) = withPosOf(varParam, {
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
    
    class ResolveClassParams(className: Name.Class, scope0: InScope, inParam: in.Param[Unit])
    extends ResolveParams(scope0, List(inParam)) {
        def outParam = outParams.head
        
        def addEntry(pos: Position, text: String) = {
            scope.symTab.get(text) match {
                case Some(_) => {
                    // TODO Decide when params on inner classes can shadow
                    Error.ShadowedClassParam(text).report(global, pos)
                }
                case _ =>
            }
            scope.addEntry(SymTab.InstanceField(Name.Member(className, text)))
        }
    }
    
    class ResolveMethodParams(scope0: InScope, inParams: List[in.Param[Unit]])
    extends ResolveParams(scope0, inParams) {
        def addEntry(pos: Position, text: String) = {
            scope.symTab.get(text) match {
                case Some(SymTab.LocalVar(_)) => {
                    Error.ShadowedMethodParam(text).report(global, pos)
                }
                case _ =>
            }
            scope.addEntry(SymTab.LocalVar(Name.LocalVar(text)))
        }
    }
    
    class ResolveBlockParam(scope0: InScope, inParam: in.Param[Unit])
    extends ResolveMethodParams(scope0, List(inParam)) {
        def outParam = outParams.head
    }
    
    class ResolveLvalues(var scope: InScope, inLvalues: List[in.Lvalue]) {
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
                    Error.ShadowedLocalVar(name.name.text).report(global, name.pos)
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
                    out.FieldLvalue(Ast.MemberName(mname), ())
                }
                
                case Some(SymTab.Ghost(mname)) => {
                    // An error will be reported in Lower:
                    // Error.NotField(name).report(global, name.pos)
                    out.FieldLvalue(Ast.MemberName(mname), ())
                }
                
                case Some(SymTab.Type(mname)) => {
                    // An error will be reported in Lower:
                    // Error.NotField(name).report(global, name.pos)
                    out.FieldLvalue(Ast.MemberName(mname), ())
                }
            }
        })
        
        def resolveFieldLvalue(
            lvalue: in.Lvalue,
            name: in.MNC
        ) = withPosOf(lvalue, {
            out.FieldLvalue(resolveDottedMemberName(name), ())
        })
        
        val outLvalues = inLvalues.map(resolveLvalue)
    }

    def resolveDottedMemberName(relDot: in.RelDot) = withPosOf(relDot, {
        val in.RelDot(base, mname) = relDot
        val className = resolveName(base).name
        Ast.MemberName(Name.Member(className, mname))
    })
    
    def resolveMemberName(relName: in.RelName) = withPosOf(relName, {
        relName match {
            case in.RelBase(name) => 
                Ast.ClasslessMemberName(Name.ClasslessMember(name))
            case relDot: in.RelDot =>
                resolveDottedMemberName(relDot)
        }
    })

    case class InScope(
        symTab: SymTab.Map
    ) {
        def addEntry(entry: SymTab.Entry) = {
            copy(symTab + entry)            
        }
        
        // ___ Declarations _____________________________________________________

        def resolveExtendsDecl(extendsDecl: in.ExtendsDecl) = withPosOf(extendsDecl, {
            out.ExtendsDecl(
                className = resolveName(extendsDecl.className),
                args      = extendsDecl.args.map(resolveExtendsArg),
                data      = ()
            )
        })
        
        def resolveExtendsArg(arg: in.ExtendsArg): out.ExtendsArg = withPosOf(arg, {
            arg match {
                case arg: in.TupleExtendsArg => 
                    out.TupleExtendsArg(arg.args.map(resolveExtendsArg))
                case arg: in.PathExtendsArg =>
                    out.PathExtendsArg(resolvePathToPath(arg.path))
            }
        })

        def resolveAnnotation(ann: in.Annotation) = withPosOf(ann, 
            out.Annotation(
                resolveName(ann.name),
                ann.args.map(resolvePathToPath)
            )
        )

        def resolveMember(className: Name.Class, mem: in.MemberDecl): out.MemberDecl = withPosOf(mem, {
            mem match {
                case decl: in.IntervalDecl => resolveIntervalDecl(className, decl)
                case decl: in.MethodDecl => resolveMethodDecl(decl)
                case decl: in.FieldDecl => resolveFieldDecl(className, decl)
                case decl: in.RelDecl => resolveRelDecl(decl)
                case decl: in.GhostDecl => resolveGhostDecl(className, decl)
            }
        })

        def resolveIntervalDecl(className: Name.Class, decl: in.IntervalDecl) = withPosOf(decl, {
            out.IntervalDecl(
                annotations = decl.annotations.map(resolveAnnotation),
                name        = Ast.MemberName(Name.Member(className, decl.name.nm)),
                parent      = resolvePathToPath(decl.parent),
                body        = resolveBody(decl.body)
            )
        })
        
        def resolveMethodDecl(decl: in.MethodDecl) = withPosOf(decl, {
            val resolveParam = new ResolveMethodParams(this, decl.params)
            val mthdScope = resolveParam.scope
            out.MethodDecl(
                annotations  = decl.annotations.map(mthdScope.resolveAnnotation),
                name         = decl.name,
                params       = resolveParam.outParams,
                returnTref   = mthdScope.resolveOptionalTypeRef(decl.returnTref),
                requirements = decl.requirements.map(mthdScope.resolveRequirement),
                ensures      = decl.ensures.map(mthdScope.resolveRequirement),
                body         = mthdScope.resolveAbstractableBody(decl.body)
            )
        })

        def resolveRequirement(requirement: in.Requirement) = withPosOf(requirement, {
            requirement match {
                case in.PathRequirement(left, rel, right) =>
                    out.PathRequirement(resolvePathToPath(left), rel, resolvePathToPath(right))
                    
                case in.TypeRequirement(left, rel, right) =>
                    out.TypeRequirement(resolveTypeRef(left), rel, resolveTypeRef(right))
            }
        })
        
        def resolveGhostDecl(className: Name.Class, decl: in.GhostDecl) = withPosOf(decl, {
            out.GhostDecl(
                name = Ast.MemberName(Name.Member(className, decl.name.nm)),
                bound = resolveName(decl.bound)
            )
        })

        def resolveFieldDecl(className: Name.Class, decl: in.FieldDecl) = withPosOf(decl, {
            out.FieldDecl(
                annotations = decl.annotations.map(resolveAnnotation),
                name = Ast.MemberName(Name.Member(className, decl.name.nm)),
                tref = resolveOptionalTypeRef(decl.tref),
                body = resolveBody(decl.body)
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
        
        sealed abstract class ResolvePathResult
        case class ResolvedToList(names: List[String]) extends ResolvePathResult
        sealed abstract class ResolvePathFinalResult extends ResolvePathResult
        case class ResolvedToClass(className: Name.Class) extends ResolvePathFinalResult
        case class ResolvedToPath(path: out.AnyPathNode) extends ResolvePathFinalResult
        
        object ResolvePathResult {
            def apply(names: List[String]) = {
                val quals = resolveRelList(names)
                quals.firstSome(_.asClassName) match {
                    case Some(className) => ResolvedToClass(className)
                    case None => ResolvedToList(names)
                }
            }
        }
        
        def withPosOfRes(path: in.AnyPathNode, res: ResolvePathResult) = res match {
            case ResolvedToList(_) => res
            case ResolvedToClass(_) => res
            case ResolvedToPath(outPath) => ResolvedToPath(withPosOf(path, outPath))
        }
        
        def resolvePathToAny(path: in.AnyPathNode): ResolvePathResult = withPosOfRes(path, {
            def thisField(memberName: Name.Member) = {
                out.PathDot(
                    out.PathBase(Ast.LocalName(Name.ThisLocal), ()),
                    Ast.MemberName(memberName),
                    ()
                )
            }
            path match {
                case in.PathErr(text) => 
                    ResolvedToPath(out.PathErr(text))
                
                case in.PathBase(in.RelBase(text), ()) => {
                    symTab.get(text) match {
                        case None => {
                            ResolvePathResult(List(text))
                        }

                        case Some(SymTab.LocalVar(name)) => {
                            ResolvedToPath(out.PathBase(Ast.LocalName(name), ()))
                        }

                        case Some(SymTab.Type(mname)) => {
                            Error.NotField(mname).report(global, path.pos)
                            ResolvedToPath(out.PathErr(path.toString))
                        }

                        case Some(SymTab.InstanceField(mname)) => {
                            ResolvedToPath(thisField(mname))
                        }

                        case Some(SymTab.Ghost(mname)) => {
                            ResolvedToPath(thisField(mname))
                        }

                        case Some(SymTab.StaticField(name)) => {
                            ResolvedToPath(out.PathBase(Ast.MemberName(name), ()))
                        }
                    }
                }
                
                case in.PathBase(relDot: in.RelDot, ()) => {
                    val memberName = resolveDottedMemberName(relDot)
                    if(symTab.valuesIterator.exists(_.isInstanceFieldNamed(memberName.name)))
                        ResolvedToPath(thisField(memberName.name))
                    else 
                        ResolvedToPath(out.PathBase(memberName, ()))
                }
                
                case in.PathDot(base, relBase @ in.RelBase(text), ()) => {
                    resolvePathToAny(base) match {
                        case ResolvedToList(names) => 
                            ResolvePathResult(text :: names)
                            
                        case ResolvedToClass(className) =>
                            resolveAgainstClass(className, text) match {
                                case Some(innerClassName) =>  {
                                    ResolvedToClass(innerClassName)                                    
                                }
                                
                                case None => {
                                    val memberName = Name.Member(className, text)
                                    ResolvedToPath(out.PathBase(Ast.MemberName(memberName), ()))
                                }
                            }
                            
                        case ResolvedToPath(outBase) => {
                            val classlessName = Name.ClasslessMember(text)
                            ResolvedToPath(out.PathDot(outBase, Ast.ClasslessMemberName(classlessName), ()))
                        }
                    }
                }
                
                case in.PathDot(base, memberName: in.RelDot, ()) => {
                    val outBase = resolvePathToPath(base)
                    ResolvedToPath(out.PathDot(outBase, resolveMemberName(memberName), ()))
                }
                
                case in.PathBaseCall(className, methodName, args, ()) => {
                    ResolvedToPath(out.PathBaseCall(className, methodName, args.map(resolvePathToPath), ()))
                }
                
                case in.PathCall(receiver, methodName, args, ()) => {
                    val pathArgs = args.map(resolvePathToPath)
                    
                    ResolvedToPath(
                        resolvePathToFinal(receiver) match {
                            case ResolvedToClass(className) => {
                                out.PathBaseCall(className, methodName, pathArgs, ())                            
                            }

                            case ResolvedToPath(path) => {
                                out.PathCall(path, methodName, pathArgs, ())
                            }
                        }
                    )
                }
                
            }
        })
        
        def resolvePathToFinal(path: in.AnyPathNode): ResolvePathFinalResult = {
            resolvePathToAny(path) match {
                case ResolvedToList(names) => ResolvedToClass(resolveToClassOrObject(path.pos, names))
                case ResolvedToClass(className) => ResolvedToClass(className)
                case ResolvedToPath(path) => ResolvedToPath(path)
            }
        }
        
        def resolvePathToPath(path: in.AnyPathNode): out.AnyPathNode = withPosOf(path, {
            resolvePathToAny(path) match {
                case ResolvedToList(names) => {
                    Error.NoSuchVar(names.last).report(global, path.pos)
                    out.PathErr(path.toString)
                }
                
                case ResolvedToClass(className) => {
                    Error.ExpPathNotClass(className).report(global, path.pos)
                    out.PathErr(path.toString)
                }
                
                case ResolvedToPath(path) => {
                    path
                }
            }
        })
        
        def resolvePathToClassName(path: in.AnyPathNode): Name.Class = {
            resolvePathToFinal(path) match {
                case ResolvedToClass(className) => {
                    className                    
                }
                    
                case ResolvedToPath(_) => {
                    Error.ExpClassName(path.toString).report(global, path.pos)
                    Name.ObjectClass          
                }
            }
        }
        
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
            case in.PathType(in.PathDot(base, in.RelBase(name), ())) => {
                resolvePathToAny(base) match {
                    case ResolvedToList(names) => {
                        val className = resolveToClassOrObject(tref.pos, name :: names)
                        out.ClassType(Ast.ClassName(className), List())
                    }
                    
                    case ResolvedToClass(outerClassName) => {
                        val className = Name.Class(outerClassName, name)
                        global.requireLoadedOrLoadable(tref.pos, className)
                        out.ClassType(Ast.ClassName(className), List())
                    }
                    
                    case ResolvedToPath(outBase) => {
                        out.TypeVar(
                            outBase, 
                            Ast.ClasslessMemberName(Name.ClasslessMember(name))
                        )                        
                    }
                }
            }
            
            // `a.b.(foo.Bar.T)` must be path type:
            case in.PathType(in.PathDot(base, relDot: in.RelDot, ())) => {
                val memberName = resolveDottedMemberName(relDot)
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
                out.ClassType(Ast.ClassName(className), outTypeArgs)
            }
            
            case in.PathType(in.PathErr(_)) => {
                out.NullType() // TODO Refine types further to elim this case
            }
            
            // `a.b.c()` cannot be a type. 
            // TODO Can the parse produce this here?
            case in.PathType(in.PathBaseCall(_, _, _, _)) | in.PathType(in.PathCall(_, _, _, _)) => {
                Error.ExpClassName(tref.toString).report(global, tref.pos)
                out.NullType()
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
                resolveMemberName(targ.name), targ.rel, resolvePathToPath(targ.path)
            )
        })
        
        // ___ Statements and Expressions _______________________________________
        
        def resolveStmts(stmts: List[in.Stmt]): List[out.Stmt] = stmts match {
            case Nil => Nil
            
            case (expr: in.Expr) :: stmts => 
                resolveExpr(expr) :: resolveStmts(stmts)
                
            case (stmt @ in.Assign(lvs, rvs)) :: stmts => {
                val outRvs = rvs.map(resolveExpr)
                val resolveLvs = new ResolveLvalues(this, lvs)
                val outAssign = withPosOf(stmt, out.Assign(resolveLvs.outLvalues, outRvs))
                val outStmts = resolveLvs.scope.resolveStmts(stmts)
                outAssign :: outStmts
            }
            
            case (stmt @ in.MethodReturn(expr)) :: stmts => {
                val outStmt = withPosOf(stmt, out.MethodReturn(resolveExpr(expr)))
                outStmt :: resolveStmts(stmts)
            }
            
            case (stmt @ in.InlineInterval(name, body, ())) :: stmts => {
                val scopeWithInline = addEntry(SymTab.LocalVar(name.name))
                val outBody = scopeWithInline.resolveBody(body)
                val outStmt = withPosOf(stmt, out.InlineInterval(name, outBody, ()))
                outStmt :: scopeWithInline.resolveStmts(stmts)
            }
        }

        def resolveNewCtor(expr: in.NewCtor): out.NewCtor = withPosOf(expr, {
            out.NewCtor(
                tref = resolveTypeRef(expr.tref),
                args = expr.args.map(resolveExpr),
                data = (),
                ty   = ()
            )
        })

        def resolveOwner(owner: in.Owner): out.Owner = withPosOf(owner, {
            owner match {
                case in.Static(name) => {
                    global.requireLoadedOrLoadable(owner.pos, name)
                    out.Static(name)
                }
                
                case expr: in.Expr => {
                    resolveExpr(expr)
                }
            }
        })
        
        // Only invoked when the owner of the field is
        // not a path, such as in `a.b().c`
        def resolveField(expr: in.Field) = withPosOf(expr, {
            val memberName = resolveMemberName(expr.name)
            out.Field(resolveOwner(expr.owner), memberName, ())
        })
        
        def resolveRcvr(rcvr: in.Rcvr): out.Rcvr = withPosOf(rcvr, {
            rcvr match {
                case in.Super(()) => out.Super(())
                case owner: in.Owner => resolveOwner(owner) 
            }
        })
        
        def resolveMethodCall(expr: in.MethodCall) = {
            out.MethodCall(
                rcvr = resolveRcvr(expr.rcvr), 
                name = expr.name, 
                args = expr.args.map(resolveExpr),
                data = ()
            )
        }
        
        def resolveExpr(expr: in.Expr): out.Expr = withPosOf(expr, expr match {
            case in.TypedPath(path) => out.TypedPath(path) // TODO Refine types further to elim this case
            case tuple: in.Tuple => resolveTuple(tuple)
            case tmpl: in.Block => resolveBlock(tmpl)
            case in.Cast(v, t) => out.Cast(resolveExpr(v), resolveTypeRef(t))
            case expr: in.Literal => resolveLiteral(expr)
            case expr: in.AnyPathNode => resolvePathToPath(expr)
            case expr: in.Field => resolveField(expr)
            case expr: in.MethodCall => resolveMethodCall(expr)
            case expr: in.NewCtor => resolveNewCtor(expr)
            case in.Null(()) => out.Null(())
            case in.ImpVoid(()) => out.ImpVoid(())
            case in.ImpThis(()) => out.varExpr(Name.ThisLocal)
        })

        def resolveLiteral(expr: in.Literal) = {
            global.requireLoadedOrLoadable(expr.pos, Name.Class(expr.obj.getClass))
            out.Literal(expr.obj, ())            
        }

        def resolveTuple(tuple: in.Tuple) = withPosOf(tuple, 
            out.Tuple(tuple.exprs.map(resolveExpr))
        )

        def resolveBlock(tmpl: in.Block) = withPosOf(tmpl, {
            val param = new ResolveBlockParam(this, tmpl.param)
            out.Block(
                async = tmpl.async,
                returnTref = param.scope.resolveOptionalTypeRef(tmpl.returnTref),
                param = param.outParam,
                stmts = param.scope.resolveStmts(tmpl.stmts),
                ty = ()
            )
        })
        
        def resolveAbstractableBody(body: in.AbstractableBody) = withPosOf(body, {
            body match {
                case in.AbstractBody() => out.AbstractBody()
                case body: in.Body => resolveBody(body)
            }
        })

        def resolveBody(body: in.Body) = withPosOf(body, 
            out.Body(resolveStmts(body.stmts))
        )
    }
}