package harmonic.compiler

import ch.ethz.intervals._
import com.smallcultfollowing.lathos.model.Context
import Ast.{Resolve => in}
import Ast.{Lower => out}
import Ast.Lower.Extensions._
import Util._

class LowerFieldMember(
    global: Global,
    csym: ClassFromSource,
    inFieldDecl: in.FieldDecl
) extends LowerMember(global, csym, inFieldDecl) {
    import global.debugServer
    
    // ___ Lowering the Field _______________________________________________

    private[this] val memberLower: AsyncInterval = {
        global.master.subinterval(
            schedule = false,
            name = "%s.%s:memberLower".format(csym.name, inFieldDecl.name),
            during = List(csym.members)
        ) { inter =>
            val log = global.logForInter(csym.classPage, inter)
            outFieldDecl.v = Lower(global).lowerFieldDecl(csym, inFieldDecl)                
        }
    }
    
    private[this] val outFieldDecl = new GuardedBy[out.FieldDecl](memberLower)

    override def memberDecl: out.FieldDecl = outFieldDecl.v
    
    // ___ Creating and Elaborating the Symbol ______________________________
    //
    // If the field type is to be inferred, this must happen after lowering
    // the body, but it is otherwise independent.
    
    private[this] val symCreate: AsyncInterval = {
        def createFieldSymbol(ty: Type) = {
            new VarSymbol.Field(
                pos       = inFieldDecl.pos,
                modifiers = Modifier.forResolvedAnnotations(inFieldDecl.annotations),
                name      = inFieldDecl.name.name,
                ty        = ty,
                kind      = FieldKind.Harmonic
            )            
        }
        
        inFieldDecl.tref match {
            case in.InferredTypeRef() => {
                global.master.subinterval(
                    schedule = false,
                    name = "%s.%s:symCreate".format(csym.name, inFieldDecl.name),
                    during = List(csym.members),
                    after = List(memberLower.getEnd)
                ) { inter =>
                    Sym.v = createFieldSymbol(outFieldDecl.v.tref.ty)
                }
            }
            
            case inTref: in.ResolveTypeRef => {
                global.master.subinterval(
                    schedule = false,
                    name = "%s.%s:symCreate".format(csym.name, inFieldDecl.name),
                    during = List(csym.members)
                ) { inter =>
                    val log = global.logForInter(csym.classPage, inter)
                    val outTref = Lower(global).InEnv(csym.classEnv).lowerTypeRef(inTref)
                    Sym.v = createFieldSymbol(outTref.ty)
                }
            }
        }
    }
    private[this] val Sym = new GuardedBy[VarSymbol.Field](symCreate)
    def sym = Sym.v
    
    private[this] val symElaborate: AsyncInterval = {
        global.master.subinterval(
            schedule = false,
            name = "%s.%s:symElaborate".format(csym.name, inFieldDecl.name),
            during = List(csym.members),
            after = List(symCreate.getEnd)
        ) { inter =>
            
        }
    }
    
    override def toOptFieldSymbol(memName: Name.Member): Option[VarSymbol.Field] = {
        if(inFieldDecl.name.is(memName)) {
            try {
                Some(Sym.join)                
            } catch {
                case _: IntervalException.Cycle => {
                    Error.ExplicitTypeRequiredDueToCycle(memName.toString).report(global, inFieldDecl.tref.pos)
                    Some(VarSymbol.errorField(memName, None))
                }
            }
        } else None
    }
    
    memberLower.schedule
    symCreate.schedule
    symElaborate.schedule
}