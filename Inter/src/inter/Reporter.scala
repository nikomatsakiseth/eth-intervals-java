package inter

import java.io.PrintStream
import scala.util.parsing.input.Position
import collection.mutable.ListBuffer

class Reporter {
    case class Error(pos: Position, msgKey: String, msgArgs: List[String])
    
    private[this] val errors = new ListBuffer[Error]()
    
    def hasErrors = !errors.isEmpty
    
    def report(pos: Position, msgKey: String, msgArgs: String*) = this.synchronized {
        errors += Error(pos, msgKey, msgArgs.toList)
    }
    
    def print(out: PrintStream) = this.synchronized {
        val sorted = errors.toList.sort((e1, e2) => e1.pos < e2.pos)
        for(err <- sorted) {
            out.printf("Error at %s: %s(%s)\n", err.pos, err.msgKey, err.msgArgs.mkString(", "))
            out.println(err.pos.longString)
        }
    }
}