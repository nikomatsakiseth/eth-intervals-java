package harmonic.compiler.mock

import ch.ethz.intervals._
import harmonic.compiler._

class MockRoInterval(
    val path: Path,
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
        !menv.queryRGivenL(path, classOf[K.InlineSubOf]).isEmpty
    }

    override def isSubintervalOfOrEqualTo(inter: RoInterval): Boolean = {
        (inter == this) || contains(PcSubOf)(inter)
    }
    
    override def isInlineSubintervalOfOrEqualTo(inter: RoInterval): Boolean = {
        (inter == this) || contains(PcInlineSubOf)(inter)
    }
    
    override def checkWritable(mr: RoPoint, inter: RoInterval): RuntimeException = {
        impl.IntervalImpl.checkWritableImpl(this, mr, inter)
    }
    
    override def checkReadable(mr: RoPoint, inter: RoInterval): RuntimeException = {
        impl.IntervalImpl.checkReadableImpl(this, mr, inter)
    }

    override def ensuresFinal(mr: RoPoint, inter: RoInterval): RuntimeException = {
        impl.IntervalImpl.ensuresFinalImpl(this, mr, inter)
    }

    override def checkLockable(inter: RoInterval, lock: RoLock): RuntimeException = {
        impl.IntervalImpl.checkLockableImpl(this, inter, lock)
    }

}