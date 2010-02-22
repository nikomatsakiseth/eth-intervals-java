package ch.ethz.intervals

import org.scalatest.junit.JUnitSuite
import org.scalatest.FunSuite
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import Util._
import ch.ethz.intervals.log.LogDirectory
import ch.ethz.intervals.log.LogStack
import scala.util.parsing.input.Position
import java.net.URLEncoder
import java.io.File

class TestGenerics extends JUnitSuite { 
    import TestAll.DEBUG_DIR
    
    // A program fragment setting up the types we will test:
    def setup = {
        val invokingMthdName = {
            val stelems = new Throwable().fillInStackTrace.getStackTrace
            stelems(2).getMethodName
        }        
        
        val logDirectory = LogDirectory.newLogDirectory(DEBUG_DIR, "TestGenerics-" + invokingMthdName)
        val logStack = new LogStack(logDirectory.mainSplitLog)
        val indexLog = logDirectory.indexLog
        
        val text = TestAll.subst(
            """
            
            class List 
                <E <: #Object>
            extends #Object 
            {
                this:E get(scalar idx)
                requires this.Constructor hb method
                requires this.#Creator readableBy method
                {
                    return;
                }
                
                void add(this:E e)
                requires this.Constructor hb method
                requires this.#Creator writableBy method
                {
                    return;
                }
            }
            
            """
        )
        
        val text1 = text.replaceAll("//[^\n]*", "")
        val parser = new IrParser()
        val cds = 
            parser.parse(parser.classDecls)(text1) match {
                case n: parser.NoSuccess =>
                    throw new RuntimeException("Parse failure: " + n.toString)
                case parser.Success(cds, _) =>
                    cds
            }            
        new Prog(logStack, cds, ir.cds_special ++ ir.cds_unit_test)        
    }
    
    @Test
    def subtype() {
        val prog = setup
        val env = prog.env_empty
        
        assertTrue(env.isSubtype(
            ir.wt_constructedPoint, ir.wt_constructedPoint
        ))
        assertFalse(!env.isSubtype(
            ir.wt_constructedPoint, ir.wt_constructedInterval
        ))
    }
    
}