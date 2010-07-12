package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable

import com.smallcultfollowing.lathos

import Ast.{Lower => in}
import Util._

case class Gather(global: Global) {
    implicit val implicitGlobal = global
    
    def forSym(csym: ClassFromSource): Unit = {
        val log = lathos.Lathos.context
        
        GatherOverrides(global).forSym(csym)
        GatherExtends(global).forSym(csym)
    }
}