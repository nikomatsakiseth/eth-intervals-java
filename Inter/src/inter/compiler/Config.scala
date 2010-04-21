package inter.compiler

import java.io.File
import java.io.PrintStream
import scala.collection.mutable.ListBuffer

class Config
{
    val version = "1.0"
    val sourceExt = ".inter"
    val classExt = ".class"
    val sourcePaths = new ListBuffer[File]()
    val classPaths = new ListBuffer[File]()
    val inputFiles = new ListBuffer[File]()
    var outputDir = new File(".")
    var dumpParsedTrees = false
    var dumpResolvedTrees = false
    var dumpLoweredTrees = false
    var dumpBytecode = false
    var ignoreErrors = false
    var useReflection = true
    var localize = false
    
    private[this] def usage(err: PrintStream) {
        err.printf("interc: Compiler for the Inter language, version %s\n", version)
        err.printf("\n")
        err.printf("Usage: inter [options] sourcefiles\n")
        err.printf("\n")
        err.printf("Options:\n")
        err.printf("  -d <dir>\n")
        err.printf("  -classpath <paths>\n")
        err.printf("  -sourcepath <paths>\n")
        err.printf("  --no-reflection\n")
        err.printf("  --no-localize\n")
        err.printf("\n")
        err.printf("Debugging options:\n")
        err.printf("  --dump-parsed-trees\n")
        err.printf("  --dump-resolved-trees\n")
        err.printf("  --dump-lowered-trees\n")
        err.printf("  --dump-bytecode\n")
        err.printf("  --ignore-errors\n")
    }
    
    private[this] def addDirs(files: ListBuffer[File], paths: String) {
        files ++= paths.split(":").map(s => new File(s))
    }
    
    private[this] def relativeFiles(paths: ListBuffer[File], ext: String)(name: Name.Qual) = {
        val baseName = name.asRelPath
        paths.toList.flatMap { path =>
            val file = new File(path, baseName + ext)
            if(file.exists) Some(file)
            else None
        }
    }
    
    def sourceFiles(name: Name.Qual) = relativeFiles(sourcePaths, sourceExt)(name)
    def classFiles(name: Name.Qual) = relativeFiles(classPaths, classExt)(name)
    
    def reflectiveClasses(name: Name.Qual) = {
        if(!useReflection) None
        else {
            try {
                Some(Class.forName(name.toString))
            } catch {
                case _: java.lang.ClassNotFoundException => None
            }
        }
    }
    
    def loadFrom(args: Array[String]): Boolean = {
        var i = 0
        try {
            if(args.isEmpty)
                usage(System.err)
            while(i < args.length) {
                if(args(i) == "-classpath" || args(i) == "-cp") {
                    addDirs(classPaths, args(i+1))
                    i += 2
                } else if(args(i) == "-sourcepath") {
                    addDirs(sourcePaths, args(i+1))
                    i += 2
                } else if(args(i) == "-d") {
                    outputDir = new File(args(i+1))
                    i += 2
                } else if(args(i) == "--dump-parsed-trees") {
                    dumpParsedTrees = true
                    i += 1
                } else if(args(i) == "--dump-resolved-trees") {
                    dumpResolvedTrees = true
                    i += 1
                } else if(args(i) == "--dump-lowered-trees") {
                    dumpLoweredTrees = true
                    i += 1
                } else if(args(i) == "--dump-bytecode") {
                    dumpBytecode = true
                    i += 1
                } else if(args(i) == "--ignore-errors") {
                    ignoreErrors = true
                    i += 1
                } else if(args(i) == "--no-reflection") {
                    useReflection = false
                    i += 1
                } else if(args(i) == "--no-localize") {
                    localize = false
                    i += 1
                } else if(args(i) startsWith "-") {
                    usage(System.err)
                    System.err.printf("\nUnrecognized option '%s'\n", args(i))
                    return false
                } else {
                    inputFiles += new File(args(i))
                    i += 1
                }
            }
            return true         
        } catch {
            case _: java.lang.ArrayIndexOutOfBoundsException =>
                usage(System.err)
        }
        return false
    }
}