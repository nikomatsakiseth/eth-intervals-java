package harmonic.compiler

import java.io.PrintStream
import scala.util.parsing.input.Position
import collection.mutable.ListBuffer

class Reporter(config: Config) {
    case class Error(pos: Position, msgKey: String, msgArgs: List[String]) {
        def msg = 
            if(false && config.localize) "" // TODO: Localization
            else "%s(%s)".format(msgKey, msgArgs.mkString(", "))
    }
    
    private[this] val errors = new ListBuffer[Error]()
    
    def hasErrors = !errors.isEmpty
    
    def report(pos: Position, msgKey: String, msgArgs: String*) = this.synchronized {
        errors += Error(pos, msgKey, msgArgs.toList)
    }
    
    def posString(pos: Position) = {
        val file = 
            if(pos.isInstanceOf[InterPosition]) pos.asInstanceOf[InterPosition].file.toString
            else "?"
        "%s:%s:%s".format(file, pos.line, pos.column)
    }
    
    def print(out: PrintStream) = this.synchronized {
        val sorted = errors.toList.sortWith((e1, e2) => e1.pos < e2.pos)
        for(err <- sorted) {
            out.printf("%s: %s\n", posString(err.pos), err.msg)
            out.println(err.pos.longString)
        }
    }
}