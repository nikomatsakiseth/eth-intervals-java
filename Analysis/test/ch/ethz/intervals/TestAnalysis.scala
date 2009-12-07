package ch.ethz.intervals

import org.scalatest.junit.JUnitSuite
import org.scalatest.FunSuite
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class TestAnalysis extends JUnitSuite { 
    
    def tc(text: String, expErrors: List[(Integer, String)]) = {
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
                    log("%s", error)
            }

            assertEquals(expErrors.length, prog.errors.length)
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
                    (-end):Wr(g)
                {
                    Data<this.g> d = new Data<this.g>();
                    Consumer<this.g> cTask = new Consumer<this.g>();
                    c->data = d;
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
    
}