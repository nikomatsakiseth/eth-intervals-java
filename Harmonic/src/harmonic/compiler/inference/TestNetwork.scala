package harmonic.compiler.inference

import com.smallcultfollowing.lathos.http.JettyLathosServer
import com.smallcultfollowing.lathos.model.LathosServer
import com.smallcultfollowing.lathos.model.Context

// Unit tests for the inference code.

object TestNetwork {
    
    // ___ simpleTest _______________________________________________________
    //
    // X(i) => Y(i)
    // W(i), Y(i) => Z(i)
    
    case class W(i: Int) extends Fact.Forward
    case class X(i: Int) extends Fact.Forward
    case class Y(i: Int) extends Fact.Forward
    case class Z(i: Int) extends Fact.Forward
    
    def simpleTest(context: Context): Unit = {
        val network = new Network()
        
        network.addRule(new Rule.Forward() {
            val inputKinds = List(classOf[X])
            
            def derive(state: Network#State, facts: List[Fact.Forward]) = {
                val x = facts(0).asInstanceOf[X]
                List(Y(x.i))
            }
        })
        
        network.addRule(new Rule.Forward() {
            val inputKinds = List(classOf[W], classOf[Y])
            
            def derive(state: Network#State, facts: List[Fact.Forward]) = {
                val w = facts(0).asInstanceOf[W]
                val y = facts(1).asInstanceOf[Y]
                if(w.i == y.i) List(Z(w.i))
                else Nil
            }
        })
        
        val factSet0 = EmptyFactSet(network)
        assert(factSet0.allFactsOfKind(classOf[X]) == Set())
        assert(factSet0.allFactsOfKind(classOf[Y]) == Set())
        assert(factSet0.allFactsOfKind(classOf[W]) == Set())
        assert(factSet0.allFactsOfKind(classOf[Z]) == Set())
        
        val factSet1 = factSet0.plusFacts(List(X(0)))
        assert(factSet1.allFactsOfKind(classOf[X]) == Set(X(0)))
        assert(factSet1.allFactsOfKind(classOf[Y]) == Set(Y(0)))
        assert(factSet1.allFactsOfKind(classOf[W]) == Set())
        assert(factSet1.allFactsOfKind(classOf[Z]) == Set())
        
        val factSet2 = factSet1.plusFacts(List(X(1)))
        assert(factSet2.allFactsOfKind(classOf[X]) == Set(X(0), X(1)))
        assert(factSet2.allFactsOfKind(classOf[Y]) == Set(Y(0), Y(1)))
        assert(factSet2.allFactsOfKind(classOf[W]) == Set())
        assert(factSet2.allFactsOfKind(classOf[Z]) == Set())

        val factSet3 = factSet1.plusFacts(List(W(1)))
        assert(factSet3.allFactsOfKind(classOf[X]) == Set(X(0), X(1)))
        assert(factSet3.allFactsOfKind(classOf[Y]) == Set(Y(0), Y(1)))
        assert(factSet3.allFactsOfKind(classOf[W]) == Set(W(1)))
        assert(factSet3.allFactsOfKind(classOf[Z]) == Set(Z(1)))
    }
    
    // ___ testing 'framework' ______________________________________________
    //
    // Because JUnit and ScalaTest are just too hard
    
    def main(args: Array[String]) {
        val server = JettyLathosServer.start(8080)
        server.addDataRenderer(new com.smallcultfollowing.lathos.model.ThrowableDataRenderer())
        val ctx = server.context
        
        var failed = 0
        var total = 0
        for(method <- getClass.getMethods) {
            if(method.getName.endsWith("Test")) {
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
        
        simpleTest(ctx)
        println("%d failures out of %d tests".format(failed, total))
        server.join
    }
}