package harmonic.compiler

import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import ch.ethz.intervals._
import Ast.{Resolve => in}
import Ast.{Lower => out}
import Ast.Lower.Extensions._
import com.smallcultfollowing.lathos.Lathos
import Util._

class LowerMethodMember(
    global: Global,
    csym: ClassFromSource,
    inMethodDecl: in.MethodDecl
) extends LowerMember(global, csym, inMethodDecl) {
    implicit val implicitGlobal = global
    import global.debugServer
    
    // ___ MethodSymbol class _______________________________________________
    
    class MethodFromSource(
        val pos: Position,
        val modifiers: Modifier.Set,
        val className: Name.Class,         /** Class in which the method is defined. */
        val name: Name.Method,           /** Name of the method. */
        val msig: MethodSignature[Pattern.Ref]
    ) extends VirtualMethodSymbol {
        override def toString = "MethodSymbol(%s.%s, %x)".format(className, name, System.identityHashCode(this))
        
        val kind = MethodKind.HarmonicVirtual
        
        val GuardPath = new GuardedBy[Path](symElaborate)
        def guardPath = GuardPath.v

        val Requirements = new GuardedBy[List[inference.Fact]](symElaborate)
        def requirements = Requirements.v

        val Ensures = new GuardedBy[List[inference.Fact]](symElaborate)
        def ensures = Ensures.v
    }

    // ___ Lowering the Parameters __________________________________________

    private[this] val paramLower: AsyncInterval = {
        global.master.subinterval(
            schedule = false,
            name = "%s.%s:paramLower".format(csym.name, inMethodDecl.name),
            parentPage = csym.classPage,
            during = List(csym.members)
        ) { inter =>
            outSig.v = Lower(global).lowerMethodParams(csym.classEnv, inMethodDecl.params)
        }
    }
    
    private[this] val outSig = new GuardedBy[(List[out.Param[LocalSymbol]], Env)](paramLower)
    
    // ___ Lowering the Method Body _________________________________________
    
    private[this] val memberLower: AsyncInterval = {
        global.master.subinterval(
            schedule = false,
            name = "%s.%s:memberLower".format(csym.name, inMethodDecl.name),
            parentPage = csym.classPage,
            during = List(csym.members),
            after = List(paramLower.getEnd)
        ) { inter =>
            val (outParams, env) = outSig.v
            outMethodDecl.v = Lower(global).lowerMethodDecl(csym, inMethodDecl, outParams, env)                
        }
    }
    
    private[this] val outMethodDecl = new GuardedBy[out.MethodDecl](memberLower)
    
    override def memberDecl: out.MethodDecl = outMethodDecl.v

    // ___ Creating and Elaborating the Symbol ______________________________
    //
    // If the method return type is explicitly specified, then the method 
    // symbol can be created without lowering the method body.  Otherwise,
    // we must wait until the method body has been lowered so that we can
    // derive the desired result.
    
    private[this] val symCreate: AsyncInterval = {
        def createSymbol(
            outParams: List[out.Param[LocalSymbol]],
            outReturnTref: out.TypeRef
        ) = {
            new MethodFromSource(
                pos       = inMethodDecl.pos,
                modifiers = Modifier.forResolvedAnnotations(inMethodDecl.annotations),
                className = csym.name,
                name      = inMethodDecl.name,
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
                    name = "%s.%s:symCreate".format(csym.name, inMethodDecl.name),
                    parentPage = csym.classPage,
                    during = List(csym.members),
                    after = List(paramLower.getEnd, memberLower.getEnd)
                ) { inter =>
                    Sym.v = createSymbol(outMethodDecl.v.params, outMethodDecl.v.returnTref)
                }                
            }
            
            // Return type is explicit:
            case inReturnTref: in.ResolveTypeRef => {
                global.master.subinterval(
                    schedule = false,
                    name = "%s.%s:symCreate".format(csym.name, inMethodDecl.name),
                    parentPage = csym.classPage,
                    during = List(csym.members),
                    after = List(paramLower.getEnd)
                ) { inter =>
                    val (outParams, env) = outSig.v
                    val outReturnTref = Lower(global).InEnv(env).lowerTypeRef(inReturnTref)
                    Sym.v = createSymbol(outParams, outReturnTref)
                }                
            }
        }
    }
        
    private[this] val Sym = new GuardedBy[MethodFromSource](symCreate)
    def sym = Sym.v
    
    private[this] val symElaborate: AsyncInterval = {
        global.master.subinterval(
            schedule = false,
            name = "%s.%s.symElaborate".format(csym.name, inMethodDecl.name),
            parentPage = csym.classPage,
            during = List(csym.members),
            after = List(symCreate.getEnd, memberLower.getEnd)
        ) { inter =>
            sym.Requirements.v = outMethodDecl.v.requirements.map(_.toFact)
            sym.Ensures.v = outMethodDecl.v.ensures.map(_.toFact)
            sym.GuardPath.v = Path.RacyGuard // TODO Issue #11
        }
    }
    
    override def toOptMethodSymbol(mthdName: Name.Method): Option[MethodSymbol] = {
        if(inMethodDecl.name.is(mthdName)) {
            try {
                Some(Sym.join)
            } catch {
                case _: IntervalException.Cycle => {
                    Error.ExplicitTypeRequiredDueToCycle(mthdName.toString).report(global, inMethodDecl.returnTref.pos)
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
