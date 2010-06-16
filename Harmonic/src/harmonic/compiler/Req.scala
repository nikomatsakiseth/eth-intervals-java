package harmonic.compiler

import scala.collection.immutable.Set

import Util._

object Req {
    abstract class Any
    case class T(left: Type.Ref, rel: TcRel, right: Type.Ref) extends Any
    case class P(left: Path.Ref, rel: PcRel, right: Path.Ref) extends Any
}