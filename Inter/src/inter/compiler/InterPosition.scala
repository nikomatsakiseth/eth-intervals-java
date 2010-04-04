package inter.compiler

import scala.util.parsing.input.Position

trait InterPosition extends Position {
    def file: java.io.File
}