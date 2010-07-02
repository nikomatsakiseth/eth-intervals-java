package harmonic.compiler

import com.smallcultfollowing.lathos.model.Context

abstract class PrettyPrinter {
    protected[this] var ind = 0
    
    def indent() {
        ind += 2
    }
    
    def undent() {
        ind -= 2
    }
    
    // Start a new line, printing fmt.format(args)
    def newl(fmt: String, args: Any*): Unit
    
    // Continue previous line, printing fmt.format(args)
    def addl(fmt: String, args: Any*): Unit
    
    def indented(start: String, end: String)(func: => Unit) {
        addl(start)
        indent()
        func
        undent()
        newl(end)
    }
}

object PrettyPrinter {
    
    def stdout = new PrettyPrinter() {
        override def newl(fmt: String, args: Any*) {
            System.out.println("")
            System.out.print(" " * ind)
            addl(fmt, args: _*)
        }
        override def addl(fmt: String, args: Any*) {
            System.out.print(fmt.format(args.map(_.toString): _*))
        }
    }
    
    def debug(log: Context) = new PrettyPrinter() {
        override def newl(fmt: String, args: Any*) {
            log.log(" " * ind)
            log.append(fmt.format(args: _*))
        }
        override def addl(fmt: String, args: Any*) {
            log.append(fmt.format(args: _*))
        }        
    }
    
}