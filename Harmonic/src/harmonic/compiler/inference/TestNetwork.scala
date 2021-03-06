package harmonic.compiler.inference

import com.smallcultfollowing.lathos.JettyLathosServer
import com.smallcultfollowing.lathos.Context
import harmonic.compiler.Util._

// Unit tests for the inference code.

object TestNetwork {
    
    case class A(i: Int) extends Fact.Backward
    case class B(i: Int) extends Fact.Backward
    case class C(i: Int) extends Fact.Backward
    case class D(i: Int) extends Fact.Backward
    
    case class W(i: Int) extends Fact.Forward
    case class X(i: Int) extends Fact.Forward
    case class Y(i: Int) extends Fact.Forward
    case class Z(i: Int) extends Fact.Forward
    
    // ___ simpleTest _______________________________________________________
    //
    // X(i) => Y(i)
    // W(i), Y(i) => Z(i)
    
    def assertSimple(factSet: FactSet[_], xs: Set[X], ys: Set[Y], ws: Set[W], zs: Set[Z]) = {
        assert(factSet.query(classOf[X]) == xs)
        assert(factSet.query(classOf[Y]) == ys)
        assert(factSet.query(classOf[W]) == ws)
        assert(factSet.query(classOf[Z]) == zs)
    }
    
    def makeSimpleNetwork(context: Context) = {
        val network = new Network[Unit](context.server)
        
        network.addRule(new Rule.ReflectiveForward[Unit]() {
            override def toString = "X(i) => Y(i)"
            def trigger(unit: Unit, x: X) = {
                val y = Y(x.i)
                List(y)
            }
        })
        
        network.addRule(new Rule.ReflectiveForward[Unit]() {
            override def toString = "W(i), Y(i) => Z(i)"
            def trigger(unit: Unit, w: W, y: Y) = {
                if(w.i == y.i) {
                    val z = Z(w.i)
                    List(z)
                } else {
                    Nil
                }
            }
        })
        
        network
    }
    
    def testSimpleAssertDuring(context: Context): Unit = {
        val network = makeSimpleNetwork(context)
        
        val factSet0 = network.emptyFactSet(())
        context.log("factSet0 = ", factSet0)
        assertSimple(factSet0, Set(), Set(), Set(), Set())
        
        val factSet1 = factSet0.plusFacts(List(X(0)), ())
        context.log("factSet1 = ", factSet1)
        assertSimple(factSet1, Set(X(0)), Set(Y(0)), Set(), Set())
        
        val factSet2 = factSet1.plusFacts(List(X(1)), ())
        context.log("factSet2 = ", factSet2)
        assertSimple(factSet2, Set(X(0), X(1)), Set(Y(0), Y(1)), Set(), Set())

        val factSet3 = factSet2.plusFacts(List(W(1)), ())
        context.log("factSet3 = ", factSet3)
        assertSimple(factSet3, Set(X(0), X(1)), Set(Y(0), Y(1)), Set(W(1)), Set(Z(1)))
    }
    
    def testSimpleAssertAfter(context: Context): Unit = {
        val network = makeSimpleNetwork(context)
        
        val factSet0 = network.emptyFactSet(())
        val factSet1 = factSet0.plusFacts(List(X(0)), ())
        val factSet2 = factSet1.plusFacts(List(X(1)), ())
        val factSet3 = factSet2.plusFacts(List(W(1)), ())
        
        context.log("factSet0 = ", factSet0)
        context.log("factSet1 = ", factSet1)
        context.log("factSet2 = ", factSet2)
        context.log("factSet3 = ", factSet3)
        
        assertSimple(factSet3, Set(X(0), X(1)), Set(Y(0), Y(1)), Set(W(1)), Set(Z(1)))
        assertSimple(factSet2, Set(X(0), X(1)), Set(Y(0), Y(1)), Set(), Set())
        assertSimple(factSet1, Set(X(0)), Set(Y(0)), Set(), Set())
        assertSimple(factSet0, Set(), Set(), Set(), Set())
    }
    
    def testSimpleValueBasedQueries(context: Context): Unit = {
        val network = makeSimpleNetwork(context)
        
        val factSet0 = network.emptyFactSet(())
        val factSet1 = factSet0.plusFacts(List(X(0)), ())
        val factSet2 = factSet1.plusFacts(List(X(1)), ())
        
        assert(factSet2.query(classOf[X], Some(0)).size == 1)
        assert(factSet2.query(classOf[X], Some(1)).size == 1)
        assert(factSet2.query(classOf[X], None).size == 1)
        
        assert(factSet2.query(classOf[Y], Some(0)).size == 1)
        assert(factSet2.query(classOf[Y], Some(1)).size == 1)
        assert(factSet2.query(classOf[Y], None).size == 1)
    }
    
    // ___ backwardsTest ____________________________________________________
    //
    // X(i) => Y(i)
    // X(i), Y(i) => A(i)
    
    def makeBackwardNetwork(context: Context) = {
        new Network[Unit](context.server) {
            addRule(new Rule.ReflectiveForward[Unit]() {
                override def toString = "X(i) => Y(i)"
                def trigger(unit: Unit, x: X) = {
                    val y = Y(x.i)
                    List(y)
                }
            })
            
            addRule(new Rule.ReflectiveBackward[Unit]() {
                override def toString = "X(i), Y(i) => A(i)"
                def trigger(recurse: Recurse[Unit], a: A) = {
                    recurse.contains(X(a.i)) && recurse.contains(Y(a.i))
                }
            })
        }
    }
    
    def testBackwardsContains(context: Context): Unit = {
        val network = makeBackwardNetwork(context)
        
        val factSet0 = network.emptyFactSet(())
        context.log("factSet0 = ", factSet0)
        assert(!factSet0.contains(A(0)), ())
        assert(!factSet0.contains(A(1)), ())
        
        val factSet1 = factSet0.plusFacts(List(X(0)), ())
        context.log("factSet1 = ", factSet1)
        assert(factSet1.contains(A(0)), ())
        assert(!factSet1.contains(A(1)), ())        
    }    
    
    // ___ testing 'framework' ______________________________________________
    //
    // Because JUnit and ScalaTest are just too hard
    
    def main(args: Array[String]) {
        val server = JettyLathosServer.start(8080)
        server.addDefaultRenderers
        val ctx = server.context
        
        var failed = 0
        var total = 0
        for(method <- getClass.getMethods) {
            if(method.getName.startsWith("test")) {
                method.setAccessible(true)
                val page = ctx.pushEmbeddedChild(method.getName, "Running test ", method.getName)
                try {
                    method.invoke(this, ctx)                    
                } catch { case t =>
                    ctx.log("Failed with throwable: ", t)
                    failed = failed + 1
                }
                ctx.pop(page)
                total = total + 1
            }
        }
        
        println("%d failures out of %d tests".format(failed, total))
        if(failed != 0)
            server.join
    }
}