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

class TestTcEnv extends JUnitSuite { 
    import TestAll.DEBUG_DIR
    
    val logTests: Set[String] = Set()
    
    // Implicits for concisely creating WcClassTypes:
    case class EnhancedWcClassType(wct: ir.WcClassType) {
        def apply(f: ir.FieldName, wp: ir.WcPath) = {
            ir.WcClassType(
                wct.c,
                ir.WcGhost(f, wp) :: wct.wghosts,
                wct.wtargs
            )
        }
        def apply(tv: ir.TypeVarName, wt: ir.WcTypeRef): ir.WcClassType = {
            ir.WcClassType(
                wct.c,
                wct.wghosts,
                ir.TypeArg(tv, wt) :: wct.wtargs
            )
        }
        def apply(tv: ir.TypeVarName, b: ir.TypeBounds): ir.WcClassType = {
            ir.WcClassType(
                wct.c,
                wct.wghosts,
                ir.BoundedTypeArg(tv, b) :: wct.wtargs
            )
        }
    }
    implicit def wcClassType2EnhancedWcClassType(wct: ir.WcClassType) = EnhancedWcClassType(wct)
        
    case class EnhancedPath(p: ir.Path) {
        def apply(tv: ir.TypeVarName) = ir.PathType(p, tv)
    }
    implicit def path2EnhancedPath(p: ir.Path) = EnhancedPath(p)
    implicit def lv2EnhancedPath(lv: ir.VarName) = EnhancedPath(lv.path)
    
    def hbNow(ps: ir.Path*) = ir.WcHbNow(ps.toList)
    def readableBy(ps: ir.Path*) = ir.WcReadableBy(ps.toList)
    def writableBy(ps: ir.Path*) = ir.WcWritableBy(ps.toList)
        
    def ext(wt: ir.WcTypeRef, wts: ir.WcTypeRef*) = ir.TypeBounds(wt :: wts.toList, List())
    def sup(wt: ir.WcTypeRef, wts: ir.WcTypeRef*) = ir.TypeBounds(List(ir.c_object.ct), wt :: wts.toList)
    val extSup = ir.TypeBounds
    
    // A program fragment setting up the types we will test:
    def setup(text0: String)(func: => (Prog => Unit)) = {
        val invokingMthdName = {
            val stelems = new Throwable().fillInStackTrace.getStackTrace
            stelems(1).getMethodName
        }        
        
        val mainSplitLog = {
            if(logTests(invokingMthdName)) {
                LogDirectory.newLogDirectory(DEBUG_DIR, "TestTcEnv-%s".format(invokingMthdName)).mainSplitLog
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
        val prog = new Prog(logStack, cds, ir.cds_special ++ ir.cds_unit_test)        
        try {
            func(prog)
        } catch {
            case t: Throwable =>
                System.out.println("Debugging output for \"%s\":".format(invokingMthdName))
                System.out.println(mainSplitLog.uri)
                throw t                
        }
    }
    
    def assertSubtype(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef)(implicit env: TcEnv) {
        env.prog.logStack.indexLog.indented("assertSubtype(%s, %s)", wt_sub, wt_sup) {
            assertTrue(
                "Assert %s <: %s".format(wt_sub, wt_sup),
                env.isSubtype(wt_sub, wt_sup))            
        }
    }
    
    def assertPathHasType(p: ir.Path, wt_sup: ir.WcTypeRef)(implicit env: TcEnv) {
        env.prog.logStack.indexLog.indented("assertPathHasType(%s, %s)", p, wt_sup) {
            assertTrue(
                "Assert %s <: %s".format(p, wt_sup),
                env.pathHasType(env.reifiedPath(p), wt_sup))
        }
    }
    
    def assertNotSubtype(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef)(implicit env: TcEnv) {
        env.prog.logStack.indexLog.indented("assertNotSubtype(%s, %s)", wt_sub, wt_sup) {
            assertFalse(
                "Assert %s not <: %s".format(wt_sub, wt_sup),
                env.isSubtype(wt_sub, wt_sup))
        }
    }
    
    def assertNotPathHasType(p: ir.Path, wt_sup: ir.WcTypeRef)(implicit env: TcEnv) {
        env.prog.logStack.indexLog.indented("assertNotPathHasType(%s, %s)", p, wt_sup) {
            assertFalse(
                "Assert %s not <: %s".format(p, wt_sup),
                env.pathHasType(env.reifiedPath(p), wt_sup))
        }
    }
    
    def assertOrdered(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef)(implicit env: TcEnv) {
        env.prog.logStack.indexLog.indented("assertOrdered(%s, %s)", wt_sub, wt_sup) {
            assertTrue(
                "Assert %s <: %s".format(wt_sub, wt_sup),
                env.isSubtype(wt_sub, wt_sup))
            assertFalse(
                "Assert %s not <: %s".format(wt_sup, wt_sub),
                env.isSubtype(wt_sup, wt_sub))
        }
    }
    
    def assertNotOrdered(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef)(implicit env: TcEnv) {
        env.prog.logStack.indexLog.indented("assertNotOrdered(%s, %s)", wt_sub, wt_sup) {
            assertFalse(
                "Assert %s <: %s".format(wt_sub, wt_sup),
                env.isSubtype(wt_sub, wt_sup))
            assertFalse(
                "Assert %s not <: %s".format(wt_sup, wt_sub),
                env.isSubtype(wt_sup, wt_sub))
        }
    }
    
    def assertEqual(wt_sub: ir.WcTypeRef, wt_sup: ir.WcTypeRef)(implicit env: TcEnv) {
        env.prog.logStack.indexLog.indented("assertEqual(%s, %s)", wt_sub, wt_sup) {
            assertTrue(
                "Assert %s <: %s".format(wt_sub, wt_sup),
                env.isSubtype(wt_sub, wt_sup))
            assertTrue(
                "Assert %s not <: %s".format(wt_sub, wt_sup),
                env.isSubtype(wt_sub, wt_sup))
        }
    }
    
    val ct_object = ir.c_object.ct
    val ct_guard = ir.c_guard.ct
    val ct_interval = ir.c_interval.ct
    
    // ___ Generic Type Arguments ___________________________________________
    
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
     
    class MyList 
        <F <: #Object>
    extends List 
        <E: this:F>
    {
    }  
    
    class YourList 
    extends List 
    {
    }  
    
    class IntervalList 
    extends List 
        <E: #Interval>
    {
    }  
    """
    val ct_List = ir.ClassName("List").ct
    val ct_MyList = ir.ClassName("MyList").ct
    val ct_YourList = ir.ClassName("YourList").ct
    val ct_IntervalList = ir.ClassName("IntervalList").ct
    
    val tv_E = ir.TypeVarName("E")
    val tv_F = ir.TypeVarName("F")    
    
    @Test
    def listTypeArgNonVariant() = setup(listText) { prog =>
        implicit val env = prog.env_empty

        assertSubtype(ct_List(tv_E, ct_interval), ct_List(tv_E, ct_interval))
        assertNotSubtype(ct_List(tv_E, ct_guard), ct_List(tv_E, ct_interval))

        assertNotSubtype(ct_List(tv_E, ct_interval), ct_List(tv_E, ct_guard))
    }
    
    @Test
    def listWithLowerBoundedTypeArgs() = setup(listText) { prog =>
        implicit val env = prog.env_empty
        
        val List_extends_guard = ct_List(tv_E, ext(ct_guard))
        val List_extends_interval = ct_List(tv_E, ext(ct_interval))
        
        assertSubtype(ct_List(tv_E, ct_guard), List_extends_guard)
        assertSubtype(ct_List(tv_E, ct_interval), List_extends_guard)

        assertNotSubtype(ct_List(tv_E, ct_guard), List_extends_interval)
        assertSubtype(ct_List(tv_E, ct_interval), List_extends_interval)

        assertSubtype(List_extends_interval, List_extends_guard)
        assertNotSubtype(List_extends_guard, List_extends_interval)
        
        assertNotSubtype(List_extends_guard, ct_List(tv_E, ct_guard))
    }
    
    @Test
    def listWithUpperBoundedTypeArgs() = setup(listText) { prog =>
        implicit val env = prog.env_empty
        
        val List_super_guard = ct_List(tv_E, sup(ct_guard))
        val List_super_interval = ct_List(tv_E, sup(ct_interval))
        
        assertSubtype(ct_List(tv_E, ct_guard), List_super_guard)
        assertNotSubtype(ct_List(tv_E, ct_interval), List_super_guard)
        
        assertSubtype(ct_List(tv_E, ct_guard), List_super_interval)
        assertSubtype(ct_List(tv_E, ct_interval), List_super_interval)
        
        assertNotSubtype(List_super_interval, List_super_guard)
        assertSubtype(List_super_guard, List_super_interval)
        
        assertNotSubtype(List_super_guard, ct_List(tv_E, ct_guard))
    }
    
    @Test
    def boundTypeArgs() = setup(listText) { prog =>
        implicit val env = prog.env_empty
        
        assertSubtype(ct_IntervalList, ct_List(tv_E, ct_interval))
        assertNotSubtype(ct_IntervalList, ct_List(tv_E, ct_guard))
        assertSubtype(ct_IntervalList, ct_List(tv_E, ext(ct_interval)))
        assertSubtype(ct_IntervalList, ct_List(tv_E, ext(ct_guard)))
        assertSubtype(ct_IntervalList, ct_List(tv_E, sup(ct_interval)))

        assertNotSubtype(ct_List(tv_E, ct_interval), ct_IntervalList)
    }    
    
    @Test
    def inheritedUnboundTypeArgs() = setup(listText) { prog =>
        implicit val env = prog.env_empty
        
        assertSubtype(ct_YourList(tv_E, ct_guard), ct_List(tv_E, ct_guard))
        assertNotSubtype(ct_List(tv_E, ct_guard), ct_YourList(tv_E, ct_guard))

        assertNotSubtype(ct_YourList(tv_E, ct_interval), ct_List(tv_E, ct_guard))
        assertSubtype(ct_YourList(tv_E, ct_interval), ct_List(tv_E, ext(ct_guard)))
        assertNotSubtype(ct_YourList(tv_E, ct_interval), ct_List(tv_E, sup(ct_guard)))

        assertSubtype(ct_YourList(tv_E, ct_guard), ct_List(tv_E, ct_guard))
        assertSubtype(ct_YourList(tv_E, ct_guard), ct_List(tv_E, ext(ct_guard)))
        assertSubtype(ct_YourList(tv_E, ct_guard), ct_List(tv_E, sup(ct_guard)))
    }    
    
    @Test
    def redirectedTypeArgs() = setup(listText) { prog =>
        implicit val env = prog.env_empty
        
        assertSubtype(ct_MyList(tv_F, ct_guard), ct_List(tv_E, ct_guard))
        assertNotSubtype(ct_List(tv_E, ct_guard), ct_MyList(tv_F, ct_guard))
        assertNotSubtype(ct_MyList(tv_F, ct_interval), ct_List(tv_E, ct_guard))
        assertNotSubtype(ct_MyList(tv_F, ct_guard), ct_List(tv_E, ct_interval))

        assertSubtype(ct_MyList(tv_F, ct_guard), ct_List(tv_E, ext(ct_guard)))
        assertSubtype(ct_MyList(tv_F, ct_interval), ct_List(tv_E, ext(ct_guard)))

        assertNotSubtype(ct_MyList(tv_F, ct_guard), ct_List(tv_E, ext(ct_interval)))
        assertSubtype(ct_MyList(tv_F, ct_interval), ct_List(tv_E, ext(ct_interval)))

        assertSubtype(ct_MyList(tv_F, ct_guard), ct_List(tv_E, sup(ct_guard)))
        assertNotSubtype(ct_MyList(tv_F, ct_interval), ct_List(tv_E, sup(ct_guard)))

        assertSubtype(ct_MyList(tv_F, ct_guard), ct_List(tv_E, sup(ct_interval)))
        assertSubtype(ct_MyList(tv_F, ct_interval), ct_List(tv_E, sup(ct_interval)))
    }
    
    // ___ Recursive Types __________________________________________________
    
    val recursiveText = """
    // Java doesn't allow such a construction, but we do:
    class InBaseClass
        <E <: this:F>
        <F <: this:E>
    extends #Object {}
    
    class TwoParam
        <E <: #Object>
        <F <: #Object>
    extends #Object {}        
    """
    
    val ct_InBaseClass = ir.ClassName("InBaseClass").ct
    val ct_TwoParam = ir.ClassName("TwoParam").ct
    
    @Test
    def mutuallyRecursiveInBaseClass() = setup(recursiveText) { prog =>
        implicit var env = prog.env_empty
        
        val lv_x = ir.VarName("x")
        val lv_y = ir.VarName("y")        
        env = env.addReifiedLocal(lv_x, ct_InBaseClass)
        env = env.addReifiedLocal(lv_y, ct_InBaseClass)
        
        assertEqual(lv_x(tv_E), lv_x(tv_F))
        assertNotOrdered(lv_x(tv_E), lv_y(tv_F))
        
        // These two types ARE equal, but we are unable to conclude that
        // because we don't understand that an upper bound on E is also an 
        // upper bound on F.  
        //assertEqual(ct_InBaseClass(tv_E, ct_guard), ct_InBaseClass(tv_F, ct_guard))
        
        // This is a slightly weaker form of the above line.  We reach the right conclusion
        // here because it only involves lower bounds.
        assertEqual(ct_InBaseClass(tv_E, ext(ct_guard)), ct_InBaseClass(tv_F, ext(ct_guard)))
    }
    
    @Test
    def mutuallyRecursiveParameters() = setup(recursiveText) { prog =>
        implicit var env = prog.env_empty
        
        val lv_x = ir.VarName("x")
        val wct_x = ct_InBaseClass(tv_E, lv_x(tv_F))(tv_F, lv_x(tv_E))
        env = env.addReifiedLocal(lv_x, wct_x)
        assertPathHasType(lv_x.path, wct_x)
        assertNotPathHasType(lv_x.path, ct_InBaseClass(tv_E, ct_guard))
    }

    // ___ HbNow ____________________________________________________________
    
    @Test
    def happened() = setup("") { prog =>
        val lv_w = ir.VarName("w")
        val lv_x = ir.VarName("x")
        val lv_y = ir.VarName("y")
        val lv_z = ir.VarName("z")
        
        implicit var env = prog.env_empty
        env = env.addGhostLocal(lv_w, ir.c_interval)
        env = env.addGhostLocal(lv_x, ir.c_interval)
        env = env.addGhostLocal(lv_y, ir.c_interval)
        env = env.addGhostLocal(lv_z, ir.c_interval)
        env = env.addHbInter(env.immutableCanonLv(lv_x), env.immutableCanonLv(lv_z))
        env = env.withCurrent(lv_z)
        
        assertEqual(
            ct_object(ir.f_creator, hbNow()),
            ct_object(ir.f_creator, hbNow()))
            
        assertEqual(
            ct_object(ir.f_creator, hbNow(lv_x.path, lv_y.path)),
            ct_object(ir.f_creator, hbNow(lv_x.path, lv_y.path)))
        
        assertEqual( // Because x happened
            ct_object(ir.f_creator, hbNow(lv_y.path)),
            ct_object(ir.f_creator, hbNow(lv_x.path, lv_y.path)))
            
        assertOrdered(
            ct_object(ir.f_creator, hbNow(lv_w.path)),
            ct_object(ir.f_creator, hbNow(lv_w.path, lv_y.path)))
            
        assertOrdered(
            ct_object(ir.f_creator, hbNow()),
            ct_object(ir.f_creator, hbNow(lv_x.path, lv_y.path)))
            
        assertNotSubtype(
            ct_object(ir.f_creator, hbNow(lv_z.path)),
            ct_object(ir.f_creator, hbNow()))
    }
    
        
}