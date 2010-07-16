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
    
    val sym = {
        new VarSymbol.Field(
            pos       = inIntervalDecl.pos,
            modifiers = Modifier.forResolvedAnnotations(inIntervalDecl.annotations),
            name      = inIntervalDecl.name.name,
            ty        = Type.AsyncInterval,
            kind      = FieldKind.Harmonic,
            elaborate = csym.create // i.e., the current interval
        )        
    }
    
    sym.GuardPath.v = Path.Final
    
    override def toOptFieldSymbol(memName: Name.Member): Option[VarSymbol.Field] = {
        if(inIntervalDecl.name.name.is(memName)) Some(sym)
        else None
    }
    
    memberLower.schedule
    
}