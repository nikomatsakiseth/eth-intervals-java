package harmonic.compiler

abstract class PrettyPrinter {
    def indent(): Unit
    def undent(): Unit
    def write(fmt: String, args: Any*): Unit
    def writeln(fmt: String, args: Any*): Unit
    
    def indented(start: String, end: String)(func: => Unit) {
        write(start)
        indent()
        writeln("")
        func
        undent()
        write(end)
    }
}

object PrettyPrinter {
    
    object stdout extends PrettyPrinter {
        var ind = 0
        var nl = false
        override def indent() {
            ind += 2
        }
        override def undent() {
            ind -= 2
        }
        override def write(fmt: String, args: Any*) {
            if(nl) {
                System.out.print(" " * ind)
                nl = false
            }
            System.out.print(fmt.format(args.map(_.toString): _*))
        }
        override def writeln(fmt: String, args: Any*) {
            if(nl) System.out.print(" " * ind)
            System.out.println(fmt.format(args.map(_.toString): _*))
            nl = true
        }
    }
    
    def debug: PrettyPrinter = new PrettyPrinter {
        val buffer = new StringBuilder()
        override def indent() {
            Util.debugAddIndent
        }
        override def undent() {
            Util.debugRemoveIndent
        }
        override def write(fmt: String, args: Any*) {
            buffer.append(fmt.format(args.map(_.toString): _*))
            if(buffer.length > 0) {
                if(buffer.charAt(buffer.length - 1) == '\n') {
                    buffer.setLength(buffer.length - 1)
                    Util.debug("%s", buffer.toString)
                    buffer.setLength(0)
                }
            }
        }
        override def writeln(fmt: String, args: Any*) {
            write(fmt, args: _*)
            write("\n")
        }
    }    
    
}