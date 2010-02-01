package ch.ethz.intervals

import scala.util.parsing.input.Position

class CheckFailure(msg: String, args: Any*) extends RuntimeException {
    def toError(pos: Position) = ir.Error(pos, msg, args.map(_.toString).toArray)
    override def toString = "%s(%s)".format(msg, args.mkString(", "))        
}

