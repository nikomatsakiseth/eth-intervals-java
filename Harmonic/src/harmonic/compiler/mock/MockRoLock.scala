package harmonic.compiler.mock

import ch.ethz.intervals._
import harmonic.compiler._

class MockRoLock(
    val path: Path.Ref,
    menv: MockEnv
) extends MockObject with RoLock {

    override def checkWritable(mr: RoPoint, inter: RoInterval): RuntimeException = {
        impl.LockImpl.checkWritableImpl(this, mr, inter)
    }
    
    override def checkReadable(mr: RoPoint, inter: RoInterval): RuntimeException = {
        impl.LockImpl.checkReadableImpl(this, mr, inter)
    }

    override def ensuresFinal(mr: RoPoint, inter: RoInterval): RuntimeException = {
        impl.LockImpl.ensuresFinalImpl(this, mr, inter)
    }

    override def checkLockable(inter: RoInterval, lock: RoLock): RuntimeException = {
        impl.LockImpl.checkLockableImpl(this, inter, lock)
    }
    
}