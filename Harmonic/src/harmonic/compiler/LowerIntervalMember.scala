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

    private[this] val sym = {
        new VarSymbol.Field(
            pos       = inFieldDecl.pos,
            modifiers = Modifier.forResolvedAnnotations(inFieldDecl.annotations),
            name      = inFieldDecl.name,
            ty        = Type.AsyncInterval,
            kind      = FieldKind.Harmonic
        )        
    }
    
    override def toOptFieldSymbol(memName: Name.Member): Option[VarSymbol.Field] = {
        if(inIntervalDecl.name.name.is(memName)) Some(sym)
        else None
    }
    
    // ___ Body Interval ____________________________________________________

    private[this] val bodyInter: Interval = {
        global.master.subinterval(during = List(csym.members)) { inter =>
            debugIndent("lowering %s", inFieldDecl) {
                outIntervalDecl.v = Lower(global).lowerIntervalDecl(csym, inIntervalDecl)
            }
        }
    }
    
    private[this] val outIntervalDecl = new GuardedBy[out.IntervalDecl](bodyInter)
    
    override def memberDecl: out.MemberDecl = outIntervalDecl.v
    
}