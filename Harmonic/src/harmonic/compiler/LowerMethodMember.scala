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
    
    // ___ Lowering the Parameters __________________________________________

    private[this] val paramLower: AsyncInterval = {
        global.master.subinterval(
            schedule = false,
            name = "%s.%s.paramLower".format(csym.name, inMethodDecl.name),
            during = List(csym.members)
        ) { inter =>
            outSig.v = Lower(global).lowerMethodParams(csym.classEnv, inMethodDecl.params)
        }
    }
    
    private[this] val outSig = new GuardedBy[(List[out.Param[VarSymbol.Local]], Env)](paramLower)
    
    // ___ Lowering the Method Body _________________________________________
    
    private[this] val memberLower: AsyncInterval = {
        global.master.subinterval(
            schedule = false,
            name = "%s.%s.memberLower".format(csym.name, inMethodDecl.name),
            during = List(csym.members),
            after = List(paramLower.getEnd)
        ) { inter =>
            debugIndent("lowering %s", inMemberDecl) {
                val (outParams, env) = outSig.v
                outMethodDecl.v = Lower(global).lowerMethodDecl(csym, inMethodDecl, outParams, env)                
            }
        }
    }
    
    private[this] val outMethodDecl = new GuardedBy[out.MethodDecl](memberLower)
    
    override def memberDecl = outMethodDecl.v

    // ___ Creating and Elaborating the Symbol ______________________________
    //
    // If the method return type is explicitly specified, then the method 
    // symbol can be created without lowering the method body.  Otherwise,
    // we must wait until the method body has been lowered so that we can
    // derive the desired result.
    
    private[this] val symCreate: AsyncInterval = {
        def createSymbol(
            outParams: List[out.Param[VarSymbol.Local]],
            outReturnTref: out.TypeRef
        ) = {
            new MethodSymbol(
                pos       = inMethodDecl.pos,
                modifiers = Modifier.forResolvedAnnotations(inMethodDecl.annotations),
                kind      = MethodKind.HarmonicVirtual,
                clsName   = csym.name,
                name      = inMethodDecl.name,
                elaborate = symElaborate,
                gather    = csym.gather,
                msig      = MethodSignature(
                    returnTy          = outReturnTref.ty,
                    parameterPatterns = outParams.map(_.toPatternRef)
                )
            )                    
        }
        
        inMethodDecl.returnTref match {
            // Return type is inferred:
            case in.InferredTypeRef() => {
                global.master.subinterval(
                    schedule = false,
                    name = "%s.%s.symCreate".format(csym.name, inMethodDecl.name),
                    during = List(csym.members),
                    after = List(paramLower.getEnd, memberLower.getEnd)
                ) { inter =>
                    sym.v = createSymbol(outMethodDecl.v.params, outMethodDecl.v.returnTref)
                }                
            }
            
            // Return type is explicit:
            case inReturnTref: in.ResolveTypeRef => {
                global.master.subinterval(
                    schedule = false,
                    name = "%s.%s.symCreate".format(csym.name, inMethodDecl.name),
                    during = List(csym.members),
                    after = List(paramLower.getEnd)
                ) { inter =>
                    val (outParams, env) = outSig.v
                    val outReturnTref = Lower(global).InEnv(env).lowerTypeRef(inReturnTref)
                    sym.v = createSymbol(outParams, outReturnTref)
                }                
            }
        }
    }
        
    private[this] val sym = new GuardedBy[MethodSymbol](symCreate)
    
    private[this] val symElaborate: AsyncInterval = {
        global.master.subinterval(
            schedule = false,
            name = "%s.%s.symElaborate".format(csym.name, inMethodDecl.name),
            during = List(csym.members),
            after = List(symCreate.getEnd, memberLower.getEnd)
        ) { inter =>
            sym.v.Requirements.v = outMethodDecl.v.requirements.map(_.toReq)
            sym.v.Ensures.v = outMethodDecl.v.ensures.map(_.toReq)
        }
    }
    
    override def toOptMethodSymbol(mthdName: Name.Method): Option[MethodSymbol] = {
        if(inMethodDecl.name.is(mthdName)) {
            try {
                Some(sym.join)
            } catch {
                case _: IntervalException.Cycle => {
                    global.reporter.report(
                        inMethodDecl.returnTref.pos, 
                        "explicit.type.required.due.to.cycle",
                        mthdName.toString
                    )
                    Some(MethodSymbol.error(mthdName, csym.name))
                }                
            }
        } else None
    }
    
    paramLower.schedule
    memberLower.schedule
    symCreate.schedule
    symElaborate.schedule
    
}
