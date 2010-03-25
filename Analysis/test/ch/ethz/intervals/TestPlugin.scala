package ch.ethz.intervals

import org.scalatest.Suite
import org.scalatest.BeforeAndAfter
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import Util._

import java.io.File

import javax.tools.Diagnostic
import javax.tools.Diagnostic.Kind
import javax.tools.DiagnosticCollector
import javax.tools.JavaFileObject
import javax.tools.JavaCompiler
import javax.tools.StandardJavaFileManager
import javax.tools.ToolProvider

import java.net.URLEncoder

import ch.ethz.intervals.log.LogDirectory

class TestPlugin extends Suite with BeforeAndAfter {
    import TestAll.DEBUG_DIR
    
    // Edit these to control logging:
    val logTests: Set[String] = Set() // Set("testTspTsp")
    val logPertinent: List[String] = List("*(*loadConfig*)", "*(*main*)")
    
    def fileName(jfo: JavaFileObject) =
        if(jfo == null) "null"
        else jfo.getName
        
    case class DiagError(jfo: JavaFileObject, line: Long, msg: String) 
    extends Ordered[DiagError] {
        // Note: column is not used for equality or other comparison purposes
        var column: Long = 1
        
        def compare(d: DiagError) = {
            if(fileName(jfo) != fileName(d.jfo))
                fileName(jfo).compare(fileName(d.jfo))
            else if(line != d.line)
                line.compare(d.line)
            else
                msg.compare(d.msg)
        }
        
        def matches(diag: DiagError) = {
            (jfo == diag.jfo) && (line == diag.line) && (msg glob diag.msg)
        }
        
        def toTxmtUrl = {
            if(jfo != null)
                "txmt://open?url=file://%s&line=%d&column=%d".format(
                    URLEncoder.encode(jfo.toUri.getPath.toString, "UTF-8"), line, column
                )
            else
                ""
        }
        
        override def toString = 
            "%s (%s:%s:%s)".format(msg, fileName(jfo), line, column)
    }
    def diagToDiagError(diag: Diagnostic[_ <: JavaFileObject]) = {
        val d = DiagError(diag.getSource, diag.getLineNumber, diag.getMessage(null))
        d.column = diag.getColumnNumber
        d
    }
    
    case class JdkConfig(
        dir: String,
        jdkDir: String,
        addSourcepath: List[String],
        classpath: List[String],
        bootClasspath: List[String],
        xBootClasspath: List[String],
        debug: Boolean) 
    {
        val srcDir = "%s/src".format(dir)
        val binDir = "%s/bin-test".format(dir)
        
        val sourcepath = "%s:%s:%s".format(srcDir, jdkDir, addSourcepath.mkString(":"))
        
        val bcpOpts = List(
            "-bootclasspath", ":".join(bootClasspath),
            "-extdirs", ""
        )
            
        val debugOpts =
            if(debug) List("-g")
            else List()
            
        val procOpts = 
            List("-processor", classOf[IntervalsChecker].getName)
            
        val opts =
            List(
                "-nowarn",
                "-d", binDir,
    			"-Xlint:-unchecked",
    			"-Xlint:-deprecation",
    			"-classpath", classpath.mkString(":"),
    			"-Xbootclasspath/p:%s".format(":".join(xBootClasspath)),
    			"-sourcepath", sourcepath
            ) ++ bcpOpts ++ procOpts ++ debugOpts
    }
    val unitTest = JdkConfig(
        dir = "test-plugin", 
        jdkDir = "test-plugin/jdk",
        addSourcepath = List(),
        classpath = List(
            "bin", 
            "lib/jsr308-all-1.04.jar",
            "lib/pcollections-1.0.0.jar",
            "lib/Intervals.jar"
        ) ++ System.getProperty("java.class.path").split(':'), 
        bootClasspath = List(
            "annotated_java_standard_library.jar"
        ),
        xBootClasspath = List(
            "lib/jsr308-all-1.04.jar"
        ),
        debug = true
    )
    
    def deleteContentsOfDirectory(baseDir: File, delDir: File): Unit = {
        if(delDir.getCanonicalPath.startsWith(baseDir.getCanonicalPath)) { // safeguard
            for(file <- delDir.listFiles) {
                if(!file.getName.startsWith(".")) {
                    if(file.isDirectory) {
                        deleteContentsOfDirectory(baseDir, file)
                    } 
                    file.delete                                    
                }
            }
        }
    }
    
    override def beforeEach() {
        val binDir = new File(unitTest.binDir)
        deleteContentsOfDirectory(binDir, binDir)
    }
    
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
                        expErrors = DiagError(jfo, idx - offset + 1, text) :: expErrors
                }
            }
        }
        expErrors
    }
    
    def compareAndReport(
        logDirectory: LogDirectory,
        config: JdkConfig,
        diagnostics: DiagnosticCollector[JavaFileObject],
        jfos: List[JavaFileObject],
        success: Boolean
    ) = logDirectory.mainSplitLog.indexLog.indented("compareAndReport()") {
        val log = logDirectory.mainSplitLog.detailLog
        
        val expErrors = expectedErrors(jfos).sortWith(_ < _)
        val actErrors = diagnostics.getDiagnostics.toList.map(diagToDiagError).sortWith(_ < _)
        
        var expErrorsRemaining = expErrors
        var matchedErrors = 0
        
        try {
            val unexpectedErrors = actErrors.filterNot(actError => 
                expErrors.exists(_ matches actError))
            val missingErrors = expErrors.filterNot(expError =>
                actErrors.exists(expError matches _))
                
            def logErrors(adj: String, errors: Iterable[DiagError]) = {
                log.indented("%s Errors", adj) {
                    errors.foreach { error =>
                        log.linkTo(error.toTxmtUrl, "%s Error: %s", adj, error)                
                    }                    
                }
            }
            
            logErrors("Unexpected", unexpectedErrors)
            logErrors("Missing", missingErrors)
            logErrors("Expected", expErrors)
            logErrors("Actual", actErrors)
                
            assertEquals(0, unexpectedErrors.length)
            assertEquals(0, missingErrors.length)
        } catch {
            case t: Throwable => // only print log if test fails:
                System.out.println("Error matching output for failed test:")
                System.out.println(logDirectory.mainSplitLog.uri)
                throw t            
        }
    }
    
    def javac(config: JdkConfig, fileName0: String, otherFileNames: String*) = {
        val testName = {
            val stelems = new Throwable().fillInStackTrace.getStackTrace
            stelems(1).getMethodName
        }
        
        // Create debug directory for this test and subdirectory for the checker:
        val logDirectory = LogDirectory.newLogDirectory(DEBUG_DIR, testName)
        try {
            val log = logDirectory.detailLog
            val checkerDebugDir = LogDirectory.newFile(logDirectory.dir, "IntervalsChecker", "")        
            var opts = config.opts
            if(logTests(testName)) {
                opts = "-AINTERVALS_DEBUG_DIR=%s".format(checkerDebugDir) :: opts
                opts = "-AINTERVALS_PERTINENT=%s".format(",".join(logPertinent)) :: opts
            }
            logDirectory.indexLog.linkTo(
                new File(checkerDebugDir, "index.html").toURI.toString, 
                "IntervalsChecker logs")

            // Determine list of filenames and get JavaFileObjects:
            val fileNames = (fileName0 :: otherFileNames.toList).toArray
            val files = fileNames.map(fileName => new File("%s/%s".format(config.srcDir, fileName)))
            val compiler = ToolProvider.getSystemJavaCompiler
            val diagnostics = new DiagnosticCollector[JavaFileObject]
            val fileManager = compiler.getStandardFileManager(diagnostics, null, null)
            val compUnits = fileManager.getJavaFileObjects(files: _*)

            // Invoke javac and compare results:
            log("compUnits: %s", compUnits)
            log.indented("javac opts") { opts.foreach(log.apply) }
            val optsColl = asCollection(opts)
            val success = compiler.getTask(null, fileManager, diagnostics, optsColl, null, compUnits).call
            compareAndReport(logDirectory, config, diagnostics, compUnits.toList, success.booleanValue)            
        } finally {
            logDirectory.mainSplitLog.flush            
        }
    }
    
    // ___ Basic Tests ______________________________________________________
    
    def testParseReqs() {
        javac(unitTest, "basic/ParseReqs.java")        
    }
    
    def testWriteFromWrongInterval() {
        javac(unitTest, "basic/WriteFromWrongInterval.java")        
    }
    
    def testVariousLoops() {
        javac(unitTest, "basic/VariousLoops.java")        
    }
    
    def testCircularGhostsA() {
        javac(unitTest, "basic/CircularGhostsA.java")
    }
    
    def testCircularHb() {
        javac(unitTest, "basic/CircularHb.java")
    }
    
    def testLists() {
        javac(unitTest, "basic/Lists.java")
    }
    
    def testIf() {
        javac(unitTest, "basic/If.java")
    }
    
    def testOnlyFinal() {
        javac(unitTest, "basic/OnlyFinal.java")
    }
    
    def testInlineIntervals() {
        javac(unitTest, "basic/InlineIntervals.java")
    }
    
    def testStaticFields() {
        javac(unitTest, "basic/StaticFields.java")
    }
    
    def testStaticMethods() {
        javac(unitTest, "basic/StaticMethods.java")
    }
    
    // ___ BBPC Application Tests ___________________________________________
    
    def testBbpc() {
        javac(unitTest, "bbpc/Producer.java")
    }
    
    // ___ TSP Application Tests ____________________________________________
    
    def testTspTourElement() {
        javac(unitTest, "erco/intervals/tsp/TourElement.java")
    }

    def testTspTspSolver() {
        javac(unitTest, "erco/intervals/tsp/TspSolver.java")
    }
    
    def testTspSplitTour() {
        javac(unitTest, "erco/intervals/tsp/SplitTour.java")
    }

    def testTspTsp() {
        javac(unitTest, "erco/intervals/tsp/Tsp.java", "erco/intervals/tsp/RunSolver.java")
    }

}