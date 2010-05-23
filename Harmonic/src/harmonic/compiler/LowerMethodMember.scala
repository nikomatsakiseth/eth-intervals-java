package harmonic.compiler

import ch.ethz.intervals._
import Ast.{Resolve => in}
import Ast.{Lower => out}
import Ast.Lower.Extensions._
import Util._

class LowerMethodMember(
    global: Global,
    csym: ClassFromSource,
    inMethodDecl: in.MethodDecl
) extends LowerMember(global, csym, inMethodDecl) {
    
    // ___ paramInter _______________________________________________________
    //
    // Lower the parameters for the method.  Even when the return type is 
    // specified, we must block waiting for this interval to complete before
    // we can construct the method signature.

    private[this] val paramInter: Interval = {
        global.master.subinterval(
            during = List(csym.members)
        ) { inter =>
            outSig.v = Lower(global).lowerMethodParams(csym.classEnv, inMethodDecl.params)
        }
    }
    private[this] val outSig = new GuardedBy[(List[Pattern.Ref], Env)](paramInter)
    
    // ___ symInter _______________________________________________________
    //
    // Creates the method symbol.

    private[this] val symInter: Interval = {
        global.master.subinterval(
            after = List(paramInter.getEnd),
            during = List(csym.members)
        ) { inter =>
            
            val (patterns, env) = outSig.v
            
            def createMethodSymbol(returnTy: Type.Ref) = {
                new MethodSymbol(
                    pos       = inMethodDecl.pos,
                    modifiers = Modifier.forResolvedAnnotations(inMethodDecl.annotations),
                    kind      = MethodKind.HarmonicVirtual,
                    clsName   = csym.name,
                    name      = inMethodDecl.name,
                    MethodSignature(
                        returnTy          = returnTy,
                        parameterPatterns = patterns
                    )                        
                )
            }
            
            def fallback = createMethodSymbol(Type.Null)

            sym.v = (inMethodDecl.returnTref, inMethodDecl.optBody) match {
                // Explicit return type:
                case (inReturnTref: in.ResolveTypeRef, _) => {
                    createMethodSymbol(Lower(global).InEnv(env).lowerTypeRef(inReturnTref))
                }

                // Abstract method with inferred return type:
                case (in.InferredTypeRef(), None) => {
                    global.reporter.report(inReturnTref.pos, 
                        "explicit.type.required.if.abstract", 
                        MthdName.toString
                    )
                    fallback
                }

                // Inferred return type:
                //   Wait for lowering of method body to complete.
                //   This could cause a cycle (except caught in toOptMethodSymbol)
                case (in.InferredTypeRef(), Some(_)) => {
                    try {
                        createMethodSymbol(outMemberDecl.join.returnTref.ty)
                    } catch {
                        case _: IntervalException.Cycle => {
                            Error.CircularMethodType(csym.name, inMethodDecl.name).report(
                                global, inMethodDecl.pos
                            )
                            fallback
                        }
                    }
                }
            }
        }        
    }
    private[this] val sym = new GuardedBy[MethodSymbol](symInter)
    
    def toOptMethodSymbol(mthdName: Name.Method): Option[MethodSymbol] = {
        if(inMethodDecl.isNamed(mthdName)) {
            try {
                Some(sym.join)
            } catch {
                case _: IntervalException.Cycle => {                
                    Error.CircularMethodType(csym.name, inMethodDecl.name).report(
                        global, inMethodDecl.pos
                    )
                    Some(MethodSymbol.error(inMethodDecl.name, csym.name))
                }
            }
        } else None
    }     
    
    // ___ bodyInter ________________________________________________________
    //
    // Lower the method body.  Follows paramInter.
    
    private[this] val bodyInter: Interval = {
        global.master.subinterval(
            after = List(paramInter.getEnd)
            during = List(csym.members)
        ) { inter =>
            debugIndent("lowering %s", inMemberDecl) {
                outMethodDecl.v = Lower(global).lowerMethodDecl(csym, inMethodDecl)                
            }
        }
    }
    private[this] val outMethodDecl = new GuardedBy[out.MethodDecl](inter)
    
    override def memberDecl = outMethodDecl.v
    
}
