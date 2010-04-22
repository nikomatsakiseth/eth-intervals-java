package harmonic

import scala.collection.immutable.Map
import scala.collection.mutable
import harmonic.lang.Application
import harmonic.compiler.ByteCode.implSuffix
import java.io

object Launch {
    
    val version = "1.0"
    
    class Instance(
        in: io.InputStream,
        out: io.PrintStream, 
        err: io.PrintStream
    ) {
        case class UsageError(val msg: String) extends RuntimeException
        
        var optAppClassName: Option[String] = None
        val appArgs = new mutable.ArrayBuffer[String]()

        private[this] def parseArgs(args: Array[String]) {
            for(arg <- args) {
                if(optAppClassName.isEmpty) {
                    optAppClassName = Some(arg)
                } else {
                    appArgs += arg
                }
            }
        }

        def main(args: Array[String]) {
            try {
                parseArgs(args)
                
                val appClassName = optAppClassName.getOrElse {
                    throw new UsageError("No application class name provided.")
                }
                
                val cls = {
                    try {
                        Class.forName(appClassName + implSuffix) 
                    } catch {
                        case _: ClassNotFoundException => 
                            throw new UsageError("Harmonic class '%s' not found.".format(appClassName))
                    }
                }
                
                if(!classOf[Application].isAssignableFrom(cls))
                    throw new UsageError("'%s' does not extend harmonic.lang.Application.".format(appClassName))
                
                val ctor = {
                    try {
                        cls.getConstructor()
                    } catch {
                        case _: NoSuchMethodException =>
                            throw new UsageError("Application class expects parameters.")
                    }
                }
                
                val app = ctor.newInstance().asInstanceOf[Application]
                app.main(appArgs.toArray)
            } catch {
                case UsageError(msg) => {
                    err.printf("harm: Launch tool for the Harmonic Language, version %s\n", version)
                    err.printf("\n")
                    err.printf("Usage: harm fully.qualified.application.class.name [args]\n")
                    err.printf("\n")
                    err.printf("Error: %s\n", msg)
                }
            }
        }
    }
    
    def main(args: Array[String]) {
        new Instance(System.in, System.out, System.err).main(args)
    }
    
}