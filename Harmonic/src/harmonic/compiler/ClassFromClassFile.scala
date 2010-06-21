package harmonic.compiler

import ch.ethz.intervals._
import Util._

class ClassFromClassFile(
    val name: Name.Class,
    val global: Global,
    file: java.io.File
) extends ClassFromCompiledSource {
    
    protected[this] def loadData(inter: Interval) = {
        throw new RuntimeException("TODO-- Implement .class file loading!")        
    }
    
}

