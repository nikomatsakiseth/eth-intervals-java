package ch.ethz.intervals

import org.scalatest.junit.JUnitSuite
import org.scalatest.FunSuite
import scala.collection.mutable.ListBuffer
import scala.collection.jcl.Conversions._
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import Util._

import java.io.File

import javax.tools.Diagnostic
import javax.tools.DiagnosticCollector
import javax.tools.JavaFileObject
import javax.tools.Diagnostic.Kind
import javax.tools.JavaCompiler
import javax.tools.StandardJavaFileManager
import javax.tools.ToolProvider

class TestPlugin extends JUnitSuite {
    
    def fileName(jfo: JavaFileObject) =
        if(jfo == null) "null"
        else jfo.getName
        
    case class DiagError(jfo: JavaFileObject, line: Long, msg: String) 
    extends Ordered[DiagError] {
        def compare(d: DiagError) = {
            if(fileName(jfo) != fileName(d.jfo))
                fileName(jfo).compare(fileName(d.jfo))
            else if(line != d.line)
                line.compare(d.line)
            else
                msg.compare(d.msg)
        }
        
        override def toString = 
            "%s (%s:%d)".format(msg, fileName(jfo), line)
    }
    def diagToDiagError(diag: Diagnostic[_ <: JavaFileObject]) = 
        DiagError(diag.getSource, diag.getLineNumber, diag.getMessage(null))
    
    case class JdkConfig(
        dir: String,
        addSourcepath: List[String],
        classpath: List[String],
        bclasspath: List[String],
        debug: Boolean,
        isolated: Boolean) 
    {
        val srcDir = "%s/src".format(dir)
        val binDir = "%s/bin-test".format(dir)
        val jdkDir = "%s/jdk".format(dir)
        
        val sourcepath = "%s:%s".format(srcDir, addSourcepath.mkString(":"))
        
        val isolatedOpts = 
            if(isolated) List("-bootclasspath","","-extdirs","")
            else List()
            
        val debugOpts =
            if(debug) List("-g")
            else List()
            
        val opts = 
            List(
                "-nowarn",
                "-d", binDir,
    			"-Xlint:-unchecked",
    			"-Xlint:-deprecation",
    			"-processor", classOf[IntervalsChecker].getName,
    			"-classpath", classpath.mkString(":"),
    			"-Xbootclasspath/p:%s".format(bclasspath.mkString(":")),
    			"-sourcepath", sourcepath
            ) ++ isolatedOpts ++ debugOpts
    }
    val unitTest = JdkConfig(
        /* dir: */              "test-plugin", 
        /* addSourcepath: */    List(), 
        /* classpath: */        List(
                                    "target/classes", 
                                    "lib/jsr308-all-1.04.jar",
                                    "lib/Intervals.jar"
                                ) ++ System.getProperty("java.class.path").split(':'), 
        /* bclasspath: */       List(
                                    "lib/jsr308-all-1.04.jar"
                                ),
        /* debug: */            true, 
        /* isolated: */         false)
    
    def expectedErrors(jfos: List[JavaFileObject]): List[DiagError] = {
        var expErrors = List[DiagError]()
        val errorRegex = "// (\\^*)ERROR (.*)".r
        jfos.foreach { jfo =>
            val fileText = jfo.getCharContent(true).toString
            val fileLines = fileText.split('\n')
            fileLines.zipWithIndex.foreach { case (fileLine, idx) =>
                errorRegex.findFirstMatchIn(fileLine) match {
                    case None => ()
                    case Some(m) => 
                        val offset = m.end(1) - m.start(1)
                        val text = fileLine.slice(m.start(2), m.end(2))
                        expErrors = DiagError(jfo, idx - offset, text) :: expErrors
                }
            }
        }
        expErrors
    }
    
    def compareAndReport(
        config: JdkConfig,
        diagnostics: DiagnosticCollector[JavaFileObject],
        jfos: List[JavaFileObject],
        success: Boolean
    ) = {
        val expErrors = expectedErrors(jfos).sort(_ < _)
        val actErrors = diagnostics.getDiagnostics.toList.map(diagToDiagError).sort(_ < _)
        
        var expErrorsRemaining = expErrors
        var matchedErrors = 0
        
        val log = new Log.TmpHtmlLog()
        
        log("jfos: %s", jfos)
        log.indented("javac opts") { config.opts.foreach(log.apply) }
        
        try {
            log.indented("Expected Errors") { expErrors.foreach(log.apply) }

            log.indented("Encountered Errors") {
                actErrors.foreach { actError =>
                    expErrorsRemaining.indexOf(actError) match {
                        case -1 => // Unexpected error:
                            log("Unexpected Error: %s", actError)
                        case i => // Skipped some errors
                            expErrorsRemaining.take(i).foreach { expError =>
                                log("Skipped Error: %s", expError)
                            }
                            expErrorsRemaining = expErrorsRemaining.drop(i)
                            matchedErrors += 1
                            log("Matched Error: %s", actError)
                    }
                }            
            }

            assertEquals(matchedErrors, expErrors.length)
            assertEquals(matchedErrors, actErrors.length)
            assertEquals(expErrors.isEmpty, success)            
        } catch {
            case t: Throwable => // only print log if test fails:
                System.out.println("Error matching output for failed test:")
                System.out.println(log.outURI)
                throw t            
        }
    }
    
    def javac(config: JdkConfig, fileNames: String*) = {
        val files = fileNames.map(fileName => new File("%s/%s".format(config.srcDir, fileName)))
        val compiler = ToolProvider.getSystemJavaCompiler
        val diagnostics = new DiagnosticCollector[JavaFileObject]
        val fileManager = compiler.getStandardFileManager(diagnostics, null, null)
        val compUnits = fileManager.getJavaFileObjects(files: _*)
        val success = compiler.getTask(null, fileManager, diagnostics, config.opts, null, compUnits).call
        compareAndReport(config, diagnostics, javaToScala(compUnits).toList, success.booleanValue)
    }
    
    @Test 
    def testPlugin() {
        javac(unitTest, "basic/ParseReqs.java")        
    }
}