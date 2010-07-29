package harmonic.compiler

import ch.ethz.intervals._
import com.smallcultfollowing.lathos.Context
import Ast.{Resolve => in}
import Ast.{Lower => out}
import Ast.Lower.Extensions._
import Util._

class LowerFieldMember(
    global: Global,
    csym: ClassFromSource,
    inFieldDecl: in.FieldDecl
) extends LowerMember(global, csym, inFieldDecl) {
    implicit val implicitGlobal = global
    import global.debugServer
    
    // ___ Lowering the Field _______________________________________________

    private[this] val memberLower: AsyncInterval = {
        global.master.subinterval(
            schedule = false,
            parentPage = csym.classPage,
            name = "%s.%s:memberLower".format(csym.name, inFieldDecl.name),
            during = List(csym.members)
        ) { inter =>
            outFieldDecl.v = Lower(global).lowerFieldDecl(csym, inFieldDecl)                
        }
    }
    
    private[this] val outFieldDecl = new GuardedBy[out.FieldDecl](memberLower)

    override def memberDecl: out.FieldDecl = outFieldDecl.v
    
    // ___ Creating and Elaborating the Symbol ______________________________
    //
    // If the field type is to be inferred, this must happen after lowering
    // the body, but it is otherwise independent.
    
    private[this] val symType: AsyncInterval = {
        inFieldDecl.tref match {
            case in.InferredTypeRef() => {
                global.master.subinterval(
                    schedule = false,
                    name = "%s.%s:symCreate".format(csym.name, inFieldDecl.name),
                    parentPage = csym.classPage,
                    during = List(csym.members),
                    after = List(memberLower.getEnd)
                ) { inter =>
                    sym.Ty.v = outFieldDecl.v.tref.ty
                }
            }
            
            case inTref: in.ResolveTypeRef => {
                global.master.subinterval(
                    schedule = false,
                    name = "%s.%s:symCreate".format(csym.name, inFieldDecl.name),
                    parentPage = csym.classPage,
                    during = List(csym.members)
                ) { inter =>
                    val outTref = Lower(global).InEnv(csym.classEnv).lowerTypeRef(inTref)
                    sym.Ty.v = outTref.ty
                }
            }
        }
    }
    
    private[this] val symElaborate: AsyncInterval = {
        global.master.subinterval(
            schedule = false,
            parentPage = csym.classPage,
            name = "%s.%s:symElaborate".format(csym.name, inFieldDecl.name),
            during = List(csym.members),
            after = List(symType.getEnd, memberLower.getEnd)
        ) { inter =>
            // Obtain guard path from @Mutable annotation:
            sym.InitializedTo.v = memberDecl.body match {
                case out.Body(List(out.TypedPath(spath))) => {
                    Some(spath.toPath)
                }
                
                case _ => None
            }
            
            sym.GuardPath.v = memberDecl.annotations.find(_.name.is(Name.MutableClass)) match {
                case Some(out.Annotation(_, List(arg))) => arg.path.toPath
                case _ => Path.Final
            }
        }
    }
    
    override def toOptFieldSymbol(memName: Name.Member): Option[FieldSymbol] = {
        inFieldDecl.name.is(memName).toOption(sym)
    }
    
    // ___ Symbol Class _____________________________________________________
    
    class FieldFromSource extends FieldSymbol {
        override val pos = inFieldDecl.pos
        override val modifiers = Modifier.forResolvedAnnotations(inFieldDecl.annotations)
        override val name = inFieldDecl.name.name
        override val kind = FieldKind.Harmonic
        
        val Ty = new GuardedBy[Type](symType)
        override def ty = {
            try {
                Ty.join                
            } catch {
                case _: IntervalException.Cycle => {
                    Error.ExplicitTypeRequiredDueToCycle(name.toString).report(global, inFieldDecl.tref.pos)
                    Type.Top
                }
            }
        }
        
        val InitializedTo = new GuardedBy[Option[Path]](symElaborate)
        override def initializedTo = InitializedTo.v
        
        val GuardPath = new GuardedBy[Path](symElaborate)
        override def guardPath = GuardPath.v
    }
    
    val sym = new FieldFromSource()
    
    // ___ Schedule intervals _______________________________________________
    
    memberLower.schedule
    symType.schedule
    symElaborate.schedule
}