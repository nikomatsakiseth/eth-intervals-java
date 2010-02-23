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
import ch.ethz.intervals.log.SplitLog
import scala.util.parsing.input.Position
import java.net.URLEncoder
import java.io.File

class TestGenerics extends JUnitSuite { 
    import TestAll.DEBUG_DIR
    
    val logTests: Set[String] = Set()
    
    // Implicits for concisely creating WcClassTypes:
    case class EnhancedWcClassType(wct: ir.WcClassType) {
        def g(f: ir.FieldName, wp: ir.WcPath) = {
            ir.WcClassType(
                wct.c,
                ir.WcGhost(f, wp) :: wct.wghosts,
                wct.wtargs
            )
        }
        def ta(tv: ir.TypeVarName, wt: ir.WcTypeRef) = {
            ir.WcClassType(
                wct.c,
                wct.wghosts,
                ir.TypeArg(tv, wt) :: wct.wtargs
            )
        }
        def taExtends(tv: ir.TypeVarName, b: ir.TypeBounds) = {
            ir.WcClassType(
                wct.c,
                wct.wghosts,
                ir.BoundedTypeArg(tv, b) :: wct.wtargs
            )
        }
    }
    implicit def wcClassType2EnhancedWcClassType(wct: ir.WcClassType) =
        EnhancedWcClassType(wct)
    implicit def className2EnhancedWcClassType(c: ir.ClassName) =
        EnhancedWcClassType(ir.WcClassType(c, List(), List()))
    def ub(wts: ir.WcTypeRef*) = 
        ir.TypeBounds(wts.toList, None)
    def lb(wts: ir.WcTypeRef*) = 
        ir.TypeBounds(List(), Some(wts.toList))
    
    // A program fragment setting up the types we will test:
    def setup(text0: String) = {
        val invokingMthdName = {
            val stelems = new Throwable().fillInStackTrace.getStackTrace
            stelems(1).getMethodName
        }        
        
        val mainSplitLog = {
            if(logTests(invokingMthdName)) {
                LogDirectory.newLogDirectory(DEBUG_DIR, "TestAnalysis").mainSplitLog
            } else {
                SplitLog.devNullSplitLog
            }
        }
        val logStack = new LogStack(mainSplitLog)
        
        val text = TestAll.subst(text0).replaceAll("//[^\n]*", "")        
        val parser = new IrParser()
        val cds = 
            parser.parse(parser.classDecls)(text) match {
                case n: parser.NoSuccess =>
                    throw new RuntimeException("Parse failure: " + n.toString)
                case parser.Success(cds, _) =>
                    cds
            }            
        new Prog(logStack, cds, ir.cds_special ++ ir.cds_unit_test)        
    }
    
    val listText = """
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
    val c_List = ir.ClassName("List")
    val tv_E = ir.TypeVarName("E")
    
    @Test
    def listTypeArgNonVariant() {
        val prog = setup(listText)
        val env = prog.env_empty
        
        assertTrue(env.isSubtype(
            c_List.ta(tv_E, ir.c_interval.wct),
            c_List.ta(tv_E, ir.c_interval.wct)
        ))

        assertFalse(env.isSubtype(
            c_List.ta(tv_E, ir.c_guard.wct),
            c_List.ta(tv_E, ir.c_interval.wct)
        ))
        
        assertFalse(env.isSubtype(
            c_List.ta(tv_E, ir.c_interval.wct),
            c_List.ta(tv_E, ir.c_guard.wct)
        ))
    }
    
}