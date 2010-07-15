package harmonic.compiler.mock

import ch.ethz.intervals._
import harmonic.compiler._

abstract class MockInterval(
    val path: Path.Ref,
    menv: MockEnv
) extends MockObject with RoInterval {
    import MethodId.{GetParent, GetStart, GetEnd}
    
    override def getParent = menv.mock(path.call(GetParent)).asInstanceOf[RoInterval]
    
    override def getStart = menv.mock(path.call(GetStart)).asInstanceOf[RoPoint]
    
    override def getEnd = menv.mock(path.call(GetEnd)).asInstanceOf[RoPoint]
    
    private[this] def contains(rel: PcRel)(obj: Object) = {
        obj match {
            case obj: MockObject => {
                menv.contains(rel.toFact(path, obj.path))
            }
            case _ => false
        }
    }
    
    override def locks(lock: RoLock) = {
        contains(PcLocks)(lock)
    }
    
    override def isInline: Boolean = {
        !menv.queryRGivenL(classOf[K.InlineSubOf], path).isEmpty
    }

    override def isSubintervalOf(inter: Interval): Boolean = {
        (inter == this) || contains(PcSubOf)(inter)
    }
    
    override def isInlineSubintervalOf(inter: Interval): Boolean = {
        (inter == this) || contains(PcInlineSubOf)(inter)
    }
}