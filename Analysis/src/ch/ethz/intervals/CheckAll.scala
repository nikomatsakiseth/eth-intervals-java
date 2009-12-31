package ch.ethz.intervals

class CheckAll(prog: Prog) {
    
    // Checks the prog, adding any errors to prog.errors.  Also
    // returns a string describing the phase in which we failed.
    // This return value is used in the TestAnalysis module.
    def check = {
        val allPhases = List[(String, CheckPhase)](
            ("wf", new WfCheck(prog)),
            ("cr", new ComputeRelations(prog)),
            ("tc", new TypeCheck(prog))
        )
        def checkPhases(phases: List[(String, CheckPhase)]): String = phases match {
            case List() => "success"
            case (nm, chk) :: tl =>
                chk.checkProg
                if(prog.errors.isEmpty) checkPhases(tl)
                else nm
        }
        checkPhases(allPhases)        
    }
 
}