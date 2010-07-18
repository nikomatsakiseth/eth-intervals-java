package harmonic.compiler

import ch.ethz.intervals._
import Util._

class GuardedBy[T](pass: Interval)(implicit global: Global) {
    private[this] var value: Option[T] = None
    
    override def toString = "GuardedBy(%s by %s)".format(value, pass)
    
    def join = {
        pass.join()
        v
    }
    
    def v: T = {
        assert(Intervals.checkReadable(pass))
        value.get
    }
    
    def v_=(v: T) = {
        assert(Intervals.checkWritable(pass))
        value = Some(v)
    }
}