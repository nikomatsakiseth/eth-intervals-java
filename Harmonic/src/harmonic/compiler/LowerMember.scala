package harmonic.compiler

import ch.ethz.intervals._
import Ast.{Resolve => in}
import Ast.{Lower => out}
import Ast.Lower.Extensions._
import Util._

class LowerMember(
    global: Global,
    csym: ClassFromSource,
    val inMemberDecl: in.MemberDecl
) {
    // ___ AST ______________________________________________________________
    
    private[this] val inter: Interval = {
        global.master.subinterval(during = List(csym.members)) { inter =>
            debugIndent("lowering %s", inMemberDecl) {
                outMemberDecl.v = Lower(global).lowerMemberDecl(csym, inMemberDecl)                
            }
        }
    }
    private[this] val outMemberDecl = new GuardedBy[out.MemberDecl](inter)
    
    /** Read lowered member decl without blocking */
    def memberDecl = {
        outMemberDecl.v
    }
    
    // ___ Symbol Creation __________________________________________________
    
    // ______ Method Symbols ________________________________________________
    //
    // toOptMethodSymbol() tries to create a method symbol with the given 
    // name.  Returns None if not able.  This could be for two reasons:
    // - This member is not a method named MthdName.
    // - The return type was not specified and could not be inferred.
    
    // Only accessible under lock!
    private[this] var optMthdSymbol: Option[MethodSymbol] = None

    def toOptMethodSymbol(MthdName: Name.Method): Option[MethodSymbol] = {
        
        def createSymbolOnce(func: => (in.MethodDecl, List[out.Param[VarSymbol.Local]], out.TypeRef)) = {
            synchronized {
                optMthdSymbol match {
                    case Some(msym) => msym
                    
                    case None => {
                        val (decl, outParams, outReturnTref) = func
                        val msym = new MethodSymbol(
                            pos       = decl.pos,
                            modifiers = Modifier.forResolvedAnnotations(decl.annotations),
                            kind      = MethodKind.HarmonicVirtual,
                            clsName   = csym.name,
                            name      = MthdName,
                            MethodSignature(
                                returnTy          = outReturnTref.ty,
                                receiverTy        = csym.toType,
                                parameterPatterns = outParams.map(_.toPatternRef)
                            )                        
                        )
                        optMthdSymbol = Some(msym)
                        msym
                    }
                }
            }
        }
        
        def fallback(inDecl: in.MethodDecl) = {
            createSymbolOnce { // fall back to a return type of Object:
                val (outParams, env) = Lower(global).lowerMethodParams(csym.classEnv, inDecl.params)
                (inDecl, outParams, out.TypeRef(Type.Top))  
            }            
        }
        
        inMemberDecl match {
            case inDecl @ in.MethodDecl(_, MthdName, params, inReturnTref: in.ResolveTypeRef, _, _) => {
                // Fully explicit, can construct the symbol.
                Some(createSymbolOnce {
                    val (outParams, env) = Lower(global).lowerMethodParams(csym.classEnv, params)
                    val outReturnTref = Lower(global).InEnv(env).lowerTypeRef(inReturnTref)
                    (inDecl, outParams, outReturnTref)                    
                })
            }

            case inDecl @ in.MethodDecl(_, MthdName, _, inReturnTref: in.InferredTypeRef, _, None) => {
                // Cannot infer the type of an abstract method.
                global.reporter.report(inReturnTref.pos, 
                    "explicit.type.required.if.abstract", 
                    MthdName.toString
                )
                Some(fallback(inDecl))
            }

            case inDecl @ in.MethodDecl(_, MthdName, _, in.InferredTypeRef(), _, Some(_)) => {
                // Wait for lowering to finish, then construct symbol.  This might fail.
                try {
                    inter.join()
                    Some(createSymbolOnce {
                        val outDecl = memberDecl.asInstanceOf[out.MethodDecl]
                        (inDecl, outDecl.params, outDecl.returnTref)
                    })
                } catch {
                    case _: IntervalException.Cycle => {
                        global.reporter.report(inDecl.returnTref.pos, 
                            "explicit.type.required.due.to.cycle",
                            MthdName.toString
                        )
                        Some(fallback(inDecl))
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

    def toOptFieldSymbol(MemName: Name.Member): Option[VarSymbol.Field] = {
        
        def createSymbolOnce(func: => (List[in.Annotation], out.TypeRef)) = {
            synchronized {
                optFieldSymbol match {
                    case Some(fsym) => fsym
                    
                    case None => {
                        val (annotations, outTref) = func
                        val fsym = new VarSymbol.Field(
                            pos       = inMemberDecl.pos,
                            modifiers = Modifier.forResolvedAnnotations(annotations),
                            name      = MemName,
                            ty        = outTref.ty,
                            kind      = FieldKind.Harmonic
                        )
                        optFieldSymbol = Some(fsym)
                        fsym
                    }
                }
            }
        }
        
        def fallback(inDecl: in.FieldDecl) = {
            createSymbolOnce { // fall back to a return type of Object:
                (inDecl.annotations, out.TypeRef(Type.Top))  
            }            
        }
        
        inMemberDecl match {
            case inDecl @ in.FieldDecl(_, Ast.MemberName(MemName), inTref: in.ResolveTypeRef, _) => {
                // Fully explicit.  Construct the symbol.
                Some(createSymbolOnce {
                    val outTref = Lower(global).InEnv(csym.classEnv).lowerTypeRef(inTref)
                    (inDecl.annotations, outTref)
                })
            }
            
            case inDecl @ in.FieldDecl(_, Ast.MemberName(MemName), in.InferredTypeRef(), _) => {
                // Wait for lowering to finish, then construct symbol.  This might fail.
                try {
                    inter.join()
                    Some(createSymbolOnce {
                        val outDecl = memberDecl.asInstanceOf[out.FieldDecl]
                        (inDecl.annotations, outDecl.tref)
                    })
                } catch {
                    case _: IntervalException.Cycle => {
                        global.reporter.report(inDecl.tref.pos, 
                            "explicit.type.required.due.to.cycle",
                            MemName.toString
                        )
                        Some(fallback(inDecl))
                    }
                }
            }
            
            case inDecl @ in.IntervalDecl(_, Ast.MemberName(MemName), _, _) => {
                Some(createSymbolOnce {
                    (inDecl.annotations, out.TypeRef(Type.AsyncInterval))
                })
            }
            
            case _ => None
        }
        
    }    
    
}