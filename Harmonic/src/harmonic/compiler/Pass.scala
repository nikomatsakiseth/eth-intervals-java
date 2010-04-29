package harmonic.compiler

/** A unit of work in the compiler. 
  * Essentially an interval. */
class Pass(
    val func: (() => Unit),
    var dependencies: List[Pass]
) {
    def isEligible(activePasses: scala.collection.Set[Pass]) = 
        !dependencies.exists(activePasses)
    
    def addDependency(after: Pass) = {
        dependencies = (after :: dependencies)
    }
}

