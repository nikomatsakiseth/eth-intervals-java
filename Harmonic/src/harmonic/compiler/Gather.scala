package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable

import Ast.{Lower => in}
import Util._

case class Gather(global: Global) {
    def forSym(csym: ClassFromSource): Unit = {
        GatherOverrides(global).forSym(csym)
        GatherExtends(global).forSym(csym)
    }
}