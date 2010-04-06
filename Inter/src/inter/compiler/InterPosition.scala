package inter.compiler

import scala.util.parsing.input.Position

trait InterPosition extends Position {
    def file: java.io.File
}

object InterPosition {
    
    def forFile(file: java.io.File) = new InterPosition() {
        def file = file
        def line = 1
        def column = 1
        override def lineContents = ""
    }
    
}