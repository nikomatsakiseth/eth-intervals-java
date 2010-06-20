package harmonic.compiler

import ch.ethz.intervals._
import Ast.{Resolve => in}
import Ast.{Lower => out}
import Ast.Lower.Extensions._
import Util._

class LowerRelDecl(
    global: Global,
    csym: ClassFromSource,
    inRelDecl: in.RelDecl
) extends LowerMember(global, csym, inRelDecl) {
    
    private[this] val memberLower: AsyncInterval = {
        global.master.subinterval(
            schedule = false,
            name = "%s.(%s).memberLower".format(csym.name, inRelDecl),
            during = List(csym.members)
        ) { inter =>
            debugIndent("lowering %s", inRelDecl) {
                outRelDecl.v = Lower(global).lowerRelDecl(csym, inRelDecl)
            }
        }
    }
    
    private[this] val outRelDecl = new GuardedBy[out.RelDecl](memberLower)
    
    override def memberDecl = outRelDecl.v
    
    memberLower.schedule
    
}