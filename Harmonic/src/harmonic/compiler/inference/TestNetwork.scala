package harmonic.compiler.inference

import com.smallcultfollowing.lathos.http.JettyLathosServer
import com.smallcultfollowing.lathos.model.Context
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
    
    def assertSimple(factSet: FactSet, xs: Set[X], ys: Set[Y], ws: Set[W], zs: Set[Z]) = {
        assert(factSet.allFactsOfKind(classOf[X]) == xs)
        assert(factSet.allFactsOfKind(classOf[Y]) == ys)
        assert(factSet.allFactsOfKind(classOf[W]) == ws)
        assert(factSet.allFactsOfKind(classOf[Z]) == zs)
    }
    
    def makeSimpleNetwork(context: Context) = {
        val network = new Network(context.server)
        
        network.addRule(new Rule.ReflectiveForward() {
            def trigger(state: Network#State, x: X) = {
                val y = Y(x.i)
                state.log.log(x, " => ", y)
                List(y)
            }
        })
        
        network.addRule(new Rule.ReflectiveForward() {
            def trigger(state: Network#State, w: W, y: Y) = {
                if(w.i == y.i) {
                    val z = Z(w.i)
                    state.log.log(w, ", ", y, " => ", z)
                    List(z)
                } else {
                    state.log.log(w, ", ", y, " =/> (nothing)")
                    Nil
                }
            }
        })
        
        network
    }
    
    def testSimpleAssertDuring(context: Context): Unit = {
        val network = makeSimpleNetwork(context)
        
        val factSet0 = EmptyFactSet(network)
        context.log("factSet0 = ", factSet0)
        assertSimple(factSet0, Set(), Set(), Set(), Set())
        
        val factSet1 = factSet0.plusFacts(List(X(0)))
        context.log("factSet1 = ", factSet1)
        assertSimple(factSet1, Set(X(0)), Set(Y(0)), Set(), Set())
        
        val factSet2 = factSet1.plusFacts(List(X(1)))
        context.log("factSet2 = ", factSet2)
        assertSimple(factSet2, Set(X(0), X(1)), Set(Y(0), Y(1)), Set(), Set())

        val factSet3 = factSet2.plusFacts(List(W(1)))
        context.log("factSet3 = ", factSet3)
        assertSimple(factSet3, Set(X(0), X(1)), Set(Y(0), Y(1)), Set(W(1)), Set(Z(1)))
    }
    
    def testSimpleAssertAfter(context: Context): Unit = {
        val network = makeSimpleNetwork(context)
        
        val factSet0 = EmptyFactSet(network)
        val factSet1 = factSet0.plusFacts(List(X(0)))
        val factSet2 = factSet1.plusFacts(List(X(1)))
        val factSet3 = factSet2.plusFacts(List(W(1)))
        
        context.log("factSet0 = ", factSet0)
        context.log("factSet1 = ", factSet1)
        context.log("factSet2 = ", factSet2)
        context.log("factSet3 = ", factSet3)
        
        assertSimple(factSet3, Set(X(0), X(1)), Set(Y(0), Y(1)), Set(W(1)), Set(Z(1)))
        assertSimple(factSet2, Set(X(0), X(1)), Set(Y(0), Y(1)), Set(), Set())
        assertSimple(factSet1, Set(X(0)), Set(Y(0)), Set(), Set())
        assertSimple(factSet0, Set(), Set(), Set(), Set())
    }
    
    // ___ backwardsTest ____________________________________________________
    //
    // X(i) => Y(i) if (i is even)
    // X(i), Y(i) => A(i)
    
    def makeBackwardNetwork(context: Context) = {
        val network = new Network(context.server)
        
        network.addRule(new Rule.ReflectiveForward() {
            def trigger(state: Network#State, x: X) = {
                val y = Y(x.i)
                state.log.log(x, " => ", y)
                List(y)
            }
        })
        
        network.addRule(new Rule.ReflectiveForward() {
            def trigger(state: Network#State, w: W, y: Y) = {
                if(w.i == y.i) {
                    val z = Z(w.i)
                    state.log.log(w, ", ", y, " => ", z)
                    List(z)
                } else {
                    state.log.log(w, ", ", y, " =/> (nothing)")
                    Nil
                }
            }
        })
        
        network
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
                val page = ctx.pushChild(method.getName, "Running test ", method.getName)
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
        server.join
    }
}