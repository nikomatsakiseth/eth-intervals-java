package ch.ethz.intervals

import scala.util.parsing.input.Position

class CheckFailure(msg: String, args: Any*) extends RuntimeException {
    override def toString = "%s(%s)".format(msg, args.mkString(", "))        
    
    def toError(pos: Position) = pos match {
        case dpos: Util.DummyPosition =>
            ir.Error(pos, msg, args.map(a => dpos.rewrite(a.toString)).toArray)
        case _ =>
            ir.Error(pos, msg, args.map(_.toString).toArray)
    }
}

