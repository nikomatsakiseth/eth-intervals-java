package harmonic.compiler

import ch.ethz.intervals._
import Ast.{Resolve => in}
import Ast.{Lower => out}

class LowerMember(
    global: Global,
    csym: ClassFromSource,
    members: Interval,
    val inMemberDecl: in.MemberDecl
) {
    // ___ AST ______________________________________________________________
    
    private[this] var outMemberDecl: out.MemberDecl = null
    private[this] val inter = inState.master.subinterval(during = members) { inter =>
        outMemberDecl = Lower(global).lowerMemberDecl(inMemberDecl)
    }
    
    /** Read lowered member decl without blocking */
    def memberDecl = {
        assert(Intervals.checkReadable(inter))
        result
    }
    
    // ___ Symbol Creation __________________________________________________
    
    def toOptSymbol = inMemberDecl match {
        case decl: in.MemberDecl => toOptMethodSymbol(decl.name)
        case decl: in.FieldDecl => toOptFieldSymbol(decl.name)
        case _ => None
    }
    
    // ______ Method Symbols ________________________________________________
    //
    // toOptMethodSymbol() tries to create a method symbol with the given 
    // name.  Returns None if not able.  This could be for two reasons:
    // - This member is not a method named MthdName.
    // - The return type was not specified and could not be inferred.
    
    // Only accessible under lock!
    private[this] var optMthdSymbol: Option[MethodSymbol] = None

    def toOptMethodSymbol(MthdName: Name.Method) = {
        
        def createSymbolOnce(func: => (in.MethodDecl, List[out.Param], out.TypeRef)) = {
            synchronized {
                optMthdSymbol match {
                    case Some(msym) => msym
                    
                    case None => {
                        val (decl, outParams, outReturnTref) = func
                        val msym = new MethodSymbol(
                            pos       = decl.pos,
                            modifiers = Modifier.forResolvedAnnotations(decl.annotations),
                            kind      = Symbol.Inter,
                            clsName   = csym.name,
                            name      = MthdName,
                            MethodSignature(
                                returnTy          = outReturnTref.ty,
                                receiverTy        = csym.toType,
                                parameterPatterns = outParams.map(out.toPatternRef)
                            )                        
                        )
                        optMthdSymbol = Some(msym)
                        msym
                    }
                }
            }
        }
        
        inMemberDecl match {
            case inDecl @ in.MethodDecl(_, MthdName, _, param, inReturnTref: in.ResolveTypeRef, _, _) => {
                // Fully explicit, can construct the symbol.
                createSymbolOnce {
                    val (outParams, env) = Lower(global).lowerMethodParams(csym.classEnv, params)
                    val outReturnTref = Lower(global).InScope(env).lowerTypeRef(inReturnTref)
                    (inDecl, outParams, outReturnTref)                    
                }
            }

            case in.MethodDecl(_, MthdName, _, _, inReturnTref: in.InferredTypeRef, _, None) => {
                // Cannot infer the type of an abstract method.
                global.reporter.report(inReturnTref.pos, 
                    "explicit.type.required.if.abstract", 
                    MthdName.toString
                )
                None
            }

            case inDecl @ in.MethodDecl(_, MthdName, _, _, in.InferredTypeRef(), _, Some(_)) => {
                // Wait for lowering to finish, then construct symbol.  This might fail.
                try {
                    inter.join()
                    createSymbolOnce {
                        val outDecl = memberDecl.asInstanceOf[out.MethodDecl]
                        (inDecl, outDecl.params, outDecl.returnTref)
                    }
                } catch {
                    case _: CycleException => {
                        global.reporter.report(inDecl.returnTref.pos, 
                            "explicit.type.required.due.to.cycle",
                            MthdName.toString
                        )
                        None                     
                    }
                }
            }

            case _ => None                
        }
    
    }
    
    // ______ Field Symbols ________________________________________________
    //
    // toOptFieldSymbol() tries to create a field symbol with the given 
    // name.  Returns None if not able.  This could be for two reasons:
    // - This member is not a field named MemberName.
    // - The field type was not specified and could not be inferred.
    
    private[this] var optFieldSymbol: Option[VarSymbol.Field] = None

    def toOptFieldSymbol(MemName: Name.Member) = {
        
        def createSymbolOnce(func: => (in.FieldDecl, out.TypeRef)) = {
            synchronized {
                optFieldSymbol match {
                    case Some(fsym) => fsym
                    
                    case None => {
                        val (decl, outTref) = func
                        val fsym = new VarSymbol.Field(
                            modifiers = Modifier.forResolvedAnnotations(decl.annotations),
                            name      = decl.name,
                            ty        = outTref.ty
                        )
                        optFieldSymbol = Some(fsym)
                        fsym
                    }
                }
            }
        }
        
        inMemberDecl match {
            case inDecl @ in.FieldDecl(_, MemName, inTref: in.ResolveTypeRef, _) => {
                // Fully explicit.  Construct the symbol.
                createSymbolOnce {
                    val outTref = Lower(global).InScope(csym.classEnv).lowerTypeRef(inTref)
                    (inDecl, outTref)
                }
            }
            
            case in.FieldDecl(_, MemName, inTref: in.InferredTypeRef, None) => {
                // Cannot infer the type of an abstract field.
                global.reporter.report(inTref.pos, 
                    "explicit.type.required.if.abstract", 
                    MemName.toString
                )
                None
            }
            
            case inDecl @ in.FieldDecl(_, MemName, in.InferredTypeRef(), Some(_)) => {
                // Wait for lowering to finish, then construct symbol.  This might fail.
                try {
                    inter.join()
                    createSymbolOnce {
                        val outDecl = memberDecl.asInstanceOf[out.FieldDecl]
                        (inDecl, outDecl.tref)
                    }
                } catch {
                    case _: CycleException => {
                        global.reporter.report(inDecl.tref.pos, 
                            "explicit.type.required.due.to.cycle",
                            MemName.toString
                        )
                        None                     
                    }
                }
            }
        }
        
    }    
    
}