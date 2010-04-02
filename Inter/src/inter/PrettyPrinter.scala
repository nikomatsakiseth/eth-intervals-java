package inter

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
    
}