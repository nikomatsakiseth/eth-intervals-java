package harmonic.compiler

import ch.ethz.intervals._
import Ast.{Resolve => in}
import Ast.{Lower => out}
import Ast.Lower.Extensions._
import Util._

class LowerIntervalMember(
    global: Global,
    csym: ClassFromSource,
    inIntervalDecl: in.IntervalDecl
) extends LowerMember(global, csym, inIntervalDecl) {
    implicit val implicitGlobal = global
    
    // ___ Symbol ___________________________________________________________
    
    class IntervalField extends FieldSymbol {
        override val pos = inIntervalDecl.pos
        override val modifiers = Modifier.forResolvedAnnotations(inIntervalDecl.annotations)
        override val name = inIntervalDecl.name.name
        override val ty = Type.AsyncInterval
        override val initializedTo = None
        override val guardPath = Path.Final
        override val kind = FieldKind.Harmonic
    }
    
    val sym = new IntervalField()

    // ___ Lowered Decl _____________________________________________________

    private[this] val memberLower: AsyncInterval = {
        global.master.subinterval(
            schedule = false,
            name = "%s.%s.memberLower".format(csym.name, inIntervalDecl.name),
            parentPage = csym.classPage,
            during = List(csym.members)
        ) { inter =>
            outIntervalDecl.v = Lower(global).lowerIntervalDecl(csym, inIntervalDecl)
        }
    }
    
    private[this] val outIntervalDecl = new GuardedBy[out.IntervalDecl](memberLower)
    
    override def memberDecl: out.IntervalDecl = outIntervalDecl.v
    
    override def toOptFieldSymbol(memName: Name.Member): Option[FieldSymbol] = {
        if(inIntervalDecl.name.name.is(memName)) Some(sym)
        else None
    }
    
    memberLower.schedule
    
}