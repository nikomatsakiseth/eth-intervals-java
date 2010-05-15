package harmonic.compiler

import Util._

class ClassFromClassFile(
    val name: Name.Class,
    val global: Global,
    file: java.io.File
) extends ClassFromCompiledSource {
    
    protected[this] def loadData = {
        throw new RuntimeException("TODO-- Implement .class file loading!")        
    }
    
}

