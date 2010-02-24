package ch.ethz.intervals

import org.scalatest.SuperSuite
import java.io.File

class TestAll extends SuperSuite(List(
    new TestAnalysis(),
    new TestTcEnv(),
    new TestPlugin()
))

object TestAll {
    val DEBUG_DIR = new File("debug-logs")
    
    // These substitutions are performed.  They are not needed in program
    // text, but are useful in the expected error messages:
    val substs = List(
        ("#Constructor", ir.f_objCtor),
        ("#Creator", ir.f_creator),
        ("#Object", ir.c_object),
        ("#Interval", ir.c_interval),
        ("#Guard", ir.c_guard),
        ("#Point", ir.c_point),
        ("#Lock", ir.c_lock),
        ("#String", ir.c_string),
        ("#void", ir.c_void)
    )
    
    def subst(text0: String) = {
        substs.foldLeft(text0) { case (t, (a, b)) => t.replace(a, b.toString) }        
    }
    
}