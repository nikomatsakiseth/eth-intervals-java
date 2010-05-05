package harmonic

import scala.collection.immutable.Map
import scala.collection.mutable
import harmonic.lang.Application
import harmonic.compiler.ByteCode.implSuffix
import java.io

object Harmonize {
    
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
                val res = app.main() //appArgs.toArray)
                if(res != null) out.printf("%s\n", res)
            } catch {
                case UsageError(msg) => {
                    err.printf("The Harmonic Language, version %s\n", version)
                    err.printf("\n")
                    err.printf("Usage: harmonize run fully.qualified.application.class.name [args]\n")
                    err.printf("\n")
                    err.printf("Error: %s\n", msg)
                }
            }
        }
    }
    
    def main(args: Array[String]) {
        if(args.length > 0 && args(0) == "run") {
            new Instance(System.in, System.out, System.err).main(args.slice(1, args.length))
        } else if(args.length > 0 && args(0) == "compile") {
            harmonic.compiler.Main.main(args.slice(1, args.length))
        } else {
            System.err.printf("The Harmonic Language, version %s\n", version)
            System.err.printf("\n")
            System.err.printf("Usage:\n")
            System.err.printf("  harmonize compile ...\n")
            System.err.printf("  harmonize run ...\n")
            System.err.printf("\n")
        }
    }
    
}