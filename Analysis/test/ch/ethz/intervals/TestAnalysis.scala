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
                    (->end):Wr(this.g)
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
    def bbpc() {
        tc(
            """
            class Data<Interval p> extends Object<this.p> {
                Object<this.p> o guardedBy this.p;
            }
            
            class Producer extends Interval {
                Consumer c guardedBy constructor;
                             
                Producer nextProd guardedBy this;
                Data<this> data guardedBy this;
                
                Producer(Consumer c)
                {
                    this->c = c;
                    
                    this.c hb start;
                }

                Void run()
                requires current == this
                {
                    this->data = new Data<this>(); // "produce"
                    this->o = new Object<this>();  // "produce"
                    
                    Interval nextCons = c->nextCons;                    
                    Interval nextProd = new Producer(nextCons);
                    this->nextProd = nextProd;
                    
                    return null;
                }
            }
            
            class Consumer extends Interval {
                Consumer nextCons guardedBy this;
            }
            
            class DummyConsumer<Interval init> extends Consumer 
            {
                Consumer c guardedBy this.init;
                
                DummyConsumer() 
                requires this.init hb this 
                {                    
                }
                
                Void run()
                {
                    nextCons = c;
                }
            }

            class RealConsumer extends Consumer {
                Producer p guardedBy constructor;
                
                Consumer(Producer p)
                {
                    this->p = p;
                    
                    this.p hb start;
                }

                Void run()
                requires current == this
                {
                    Data<hb current> data = p->data; // "consume" 
                    Object<hb current> o = data->o;  // "consume"
                    
                    Interval nextProd = p->nextProd;
                    Interval nextCons = new Consumer(nextProd);
                    this->nextCons = nextCons;
                    
                    return null;
                }
            }
            
            class BBPC extends Interval {
                Void run()
                requires current == this
                {
                    DummyConsumer<this> d0 = new DummyConsumer<this>();
                    Producer p = new Producer(d0);

                    DummyConsumer<this> d1 = new DummyConsumer<this>();                    
                    d0->c = d1;

                    Consumer c = new Consumer(p);
                    d1->c = c;                        
                    
                    return null;
                }
            }
            
            """,
            List(
            )
        )
    }  
    
    def bbpcData() {
        tc(
            """
            class Data<Interval p> extends Object<this.p> {
                Object<this.p> o guardedBy this.p;
                constructor() {}
            }
            
            class ProdData<Interval p> extends Object<this.p> {
                Data<this.p> data guardedBy this.p;                              
                Interval nextProd guardedBy this.p;
                ProdData<this.nextProd> nextPdata guardedBy this.p;                
                constructor() {}
            }
            
            class ConsData<Interval c> extends Object<this.c> {
                Interval nextCons guardedBy this.c;
                ConsData<this.nextCons> nextCdata guardedBy this.c;
                constructor() {}
            }
            
            class Producer extends Interval {
                ConsData<hb this> cdata guardedBy constructor;
                ProdData<this> pdata guardedBy constructor;
                
                constructor(Interval c, ConsData<c> cdata)
                {
                    c.end hb this.start;
                    this->cdata = cdata;
                    ProdData<this> pdata = new ProdData<this>();
                    this->pdata = pdata;
                }

                Void run()
                requires current == this
                {
                    ProdData<this> pdata = this->pdata;
                    
                    Data<this> data = new Data<this>(); // "produce"
                    pdata->data = data;
                    
                    // Note: Non-trivial deduction here that equates
                    // nextCons with cdata.nextCons!
                    Interval nextCons = cdata->nextCons;
                    ConsData<nextCons> nextCdata = cdata->nextCdata;                    
                    
                    Interval nextProd = new Producer(nextCons, nextCdata);
                    this->nextProd = nextProd;
                    
                    return null;
                }
            }
            
            class Consumer extends Consumer {
                ProdData<hb this> pdata guardedBy constructor;
                ConsData<this> cdata guardedBy constructor;
                
                constructor(Interval p, ProdData<p> pdata)
                {
                    p.end hb this.start;
                    this->pdata = pdata;
                    ConsData<this> cdata = new ConsData<this>();
                    this->cdata = cdata;
                }

                Void run()
                requires current == this
                {
                    Data<hb this> data = pdata->data; // "consume" 
                    
                    Interval nextProd = pdata->nextProd;
                    ProdData<nextProd> nextPdata = pdata->nextPdata;
                    
                    Interval nextCons = new Consumer(nextProd, nextPdata);
                    cdata->nextCons = nextCons;
                    return null;
                }
            }
            
            class BBPC extends Interval {
                Void run()
                requires current == this
                {
                    ConsumerData<this> d0 = new ConsumerData<this>();
                    ConsumerData<this> d1 = new ConsumerData<this>();                    
                    d0->nextCons = this;
                    d0->nextCdata = d1;
                    
                    Producer p = new Producer(this, d0);                    
                    ProdData<p> pdata = p->pdata;
                    
                    Consumer c = new Consumer(p, pdata);
                    d1->nextCons = c; 
                    ConsData<c> cdata = c->cdata;
                    d1->nextCdata = cdata;                    
                    return null;
                }
            }
            
            """,
            List(
            )
        )
    }  
    
    @Test 
    def hoh() {
        tc(
            """
            class Link<
                Guard<locked> lst
            > extends Object {
                final Guard<locked> lock guardedBy readOnly;
                Link<this.lst> next guardedBy this.lock;
                Void fld guardedBy this.lock;
            }
            
            class ProcLink<Guard<locked> lst> extends Task
            {
                final Link<lst> link guardedBy readOnly;
                
                Void run(Point<?> end)
                    Wr(this.link.lock)
                {
                    this.link->fld = null;
                                        
                    Link<lst> next = this.link->next;
                    ProcLink<lst> nlTask = new ProcLink<lst>(next);                
                    Interval<?> nlInter = interval end nlTask (nlTask.link.lock);
                }
            }
            
            class ProcList
            """,
            List(
            )
        )
    }
                
}