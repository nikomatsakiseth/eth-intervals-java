package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable

import com.smallcultfollowing.lathos.model.Context

import Ast.{Lower => in}
import Util._

case class Gather(global: Global, log: Context) {
    def forSym(csym: ClassFromSource): Unit = {
        GatherOverrides(global, log).forSym(csym)
        GatherExtends(global, log).forSym(csym)
    }
}