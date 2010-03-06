package ch.ethz.intervals

import scala.util.parsing.input.Position

/** Throw when we access a variable that would be 
  * defined but for an error */
class DependentFailure(lv: ir.VarName) extends RuntimeException {
    override def toString = "DependentFailure(%s)"
}

