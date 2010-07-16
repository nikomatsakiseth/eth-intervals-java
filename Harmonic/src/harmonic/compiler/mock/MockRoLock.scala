package harmonic.compiler.mock

import ch.ethz.intervals._
import harmonic.compiler._

class MockRoLock(
    val path: Path.Ref,
    menv: MockEnv
) extends MockObject with RoLock {
    
}