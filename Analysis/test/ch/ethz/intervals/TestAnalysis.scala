package ch.ethz.intervals

import org.scalatest.junit.JUnitSuite
import org.scalatest.FunSuite
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import Util._

case class ExpError(msg: String, args: List[String])

class TestAnalysis extends JUnitSuite { 
    
    def tc(text: String, expErrors: List[ExpError]) = {
        val log = new Log.TmpHtmlLog()
        try {
            val text1 = text.replaceAll("//[^\n]*", "")
            val parser = new IrParser()
            val cds = 
                parser.parse(parser.classDecls)(text1) match {
                    case n: parser.NoSuccess =>
                        throw new RuntimeException("Parse failure: " + n.toString)
                    case parser.Success(cds, _) =>
                        cds
                }            
            val prog = new Prog(cds)
            val tc = new TypeCheck(log, prog)
            tc.check
        
            log.indented("Encountered Errors: ") {                
                for(error <- prog.errors)
                    log("ExpError(\"%s\", List(%s))", 
                        error.msg, ", ".join("\"", error.args, "\""))                    
            }

            assertEquals(expErrors.length, prog.errors.length)
            for((expError, error) <- expErrors.zip(prog.errors.toList)) {
                assertEquals(expError.msg, error.msg)
                assertEquals(expError.args, error.args)
            }            
        } catch {
            case t: Throwable => // only print log if test fails:
                System.out.println("Debugging output for failed test:")
                System.out.println(log.outURI)
                throw t
        }
    }
    
    @Test 
    def basic() {
        tc(
            """
            class Producer<Guard<?> g> extends Object {
                Void run(Point<?> end) 
                    (->end):Wr(g)
                {
                    Data<this.g> d = new Data<this.g>();
                    Consumer<this.g> cTask = new Consumer<this.g>(d);
                    Interval<?> cInter = interval end cTask ();
                    return this->run(end); // endless loop
                }
            }
            class Consumer<Guard<?> g> extends Task[run(end)=Wr(this.g)] {
                final Data<this.g> data guardedBy readOnly;
                
                Void run(Point<?> end)
                    Wr(this.g)
                {
                    return null;
                }
            }
            class Data<Guard<?> g> extends Object {                
            }
            """,
            List(
            )
        )
    }
    
    @Test 
    def classDisjointGhosts() {
        tc(
            """
            class Foo<Guard<?> g, Guard<?> h> 
                this.g # this.h 
            extends Object 
            {
            }
            class Bar<> extends Object {
                Void run(Guard<?> a, Guard<?> b)
                {
                    Foo<?,?> f = new Foo<a, b>();
                    return null;
                }
            }
            """,
            List(
                ExpError("intervals.not.disjoint", List("a", "b"))
            )
        )
    }

    @Test 
    def classDisjointFinal() {
        tc(
            """
            class Foo<>
                this.g # this.h 
            extends Object 
            {
                final Guard<?> g guardedBy readOnly;
                final Guard<?> h guardedBy readOnly;
            }
            class Bar<> extends Object {
                Void run(Guard<?> a, Guard<?> b)
                {
                    Foo f = new Foo(a, b);
                    return null;
                }
            }
            """,
            List(
                ExpError("intervals.not.disjoint", List("a", "b"))
            )
        )
    }
    
    @Test 
    def classDisjointWf() {
        tc(
            """
            class Foo<>
                this.g # this.h 
            extends Object 
            {
                final Guard<?> g guardedBy readOnly;
                Guard<?> h guardedBy readOnly;
            }
            """,
            List(
                ExpError("intervals.not.final", List("this", "Foo<>", "h"))
            )
        )
    }        
}