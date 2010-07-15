package harmonic.compiler.mock

import harmonic.compiler.Path
import harmonic.compiler.inference

trait MockEnv {
    def contains(fact: inference.Fact): Boolean
    def queryRGivenL[L, R](kind: Class[_ <: inference.Fact.Binary[L, R]], left: L): Set[R]
    def mock(path: Path.Ref): Object
}