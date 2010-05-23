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
    
    private[this] val bodyInter: Interval = {
        global.master.subinterval(during = List(csym.members)) { inter =>
            debugIndent("lowering %s", inFieldDecl) {
                outRelDecl.v = Lower(global).lowerRelDecl(csym, inRelDecl)
            }
        }
    }
    
    private[this] val outRelDecl = new GuardedBy[out.RelDecl](bodyInter)
    
    override def memberDecl = outRelDecl.v
    
}