package harmonic.compiler

import com.smallcultfollowing.lathos._
import com.smallcultfollowing.lathos.Lathos

import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition

import Util._

class GhostSymbol(
    val pos: Position, 
    
    /** The name of the ghost. */
    val name: Name.Member,
    
    /** All objects bound to this ghost must be instance of this class */
    val bound: Name.Class
) extends Symbol with DebugPage {
    override def toString = "Ghost(%s, %x)".format(name, System.identityHashCode(this))
    
    val modifiers = Modifier.Set.empty
    
    def isNamed(aName: Name.Member) = (name == aName)
}