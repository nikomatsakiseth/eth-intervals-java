package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable

import Ast.{Lower => in}
import Util._

/** Given a class C, creates a flattened list
  * ExtendsDecl listing the supertypes and the
  * arguments provided to them.  All uses of a
  * given class must provide equivalent arguments
  * up to equality.  The order in which those
  * constructors are executed will be the MRO
  * order. */
case class GatherExtends(global: Global) {
    
}