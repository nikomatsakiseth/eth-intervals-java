package harmonic.compiler

import scala.collection.mutable
import scala.util.parsing.input.Position
import Error.CanFail
import ch.ethz.intervals.Interval

trait Symbol {
    def pos: Position
    def modifiers: Modifier.Set
    def isError: Boolean = false
}