package harmonic.compiler

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
    
    val unknown = new Position() {
        def line = 1
        def column = 1
        override def lineContents = "<unknown>"
    }

    def forClassNamed(name: Name.Qual) = new InterPosition() {
        def file = new java.io.File(name.toString)
        def line = 1
        def column = 1
        override def lineContents = ""
    }
    
    def forClass(cls: java.lang.Class[_]) = new InterPosition() {
        def file = new java.io.File(cls.toString)
        def line = 1
        def column = 1
        override def lineContents = ""
    }
    
}