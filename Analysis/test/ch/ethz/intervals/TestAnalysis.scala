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

    /*
    @Test 
    def bbpc() {
        tc(
            """
            class Data<Interval p> extends Object<this.p> {
                Object<this.p> o guardedBy this.p;
                constructor() {}
            }
            
            class Producer extends Interval {
                Consumer c guardedBy this.constructor;
                             
                Producer nextProd guardedBy this;
                Data<this> data guardedBy this;
                
                constructor(Consumer c)
                {
                    this->c = c;
                }

                Void addDependencies()
                {
                    this.c hb start; 
                }
                
                Void run()
                requires current == this
                {
                    Data<this> data = new Data<this>();
                    this->data = data; // "produce"
                    
                    Interval nextCons = c->nextCons;                    
                    Interval nextProd = new Producer(nextCons);
                    this->nextProd = nextProd;
                }
            }
            
            class Consumer extends Interval {
                Consumer nextCons guardedBy this;
                constructor() {}
            }
            
            class DummyConsumer<Interval init> extends Consumer 
            {
                Consumer c guardedBy this.init;
                
                constructor() 
                requires this.init hb this 
                {                    
                }
                
                Void run()
                {
                    Consumer c = this->c;
                    this->nextCons = c;
                }
            }

            class RealConsumer extends Consumer {
                Producer p guardedBy this.constructor;
                
                constructor(Producer p)
                {
                    this->p = p;                    
                }
                
                Void addDependencies()
                {
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
                }
            }
            
            class BBPC extends Interval {
                constructor() {}
                
                Void run()
                requires current == this
                {
                    DummyConsumer<this> d0 = new DummyConsumer<this>();
                    Producer p = new Producer(d0);

                    DummyConsumer<this> d1 = new DummyConsumer<this>();                    
                    d0->c = d1;

                    Consumer c = new Consumer(p);
                    d1->c = c;                        
                }
            }
            
            """,
            List(
            )
        )
    }
    */
    
    @Test
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
                ConsData<hb this> cdata guardedBy this.constructor;
                ProdData<this> pdata guardedBy this.constructor;
                
                constructor(Interval c, ConsData<c> cdata)
                {
                    c hb this;
                    this->cdata = cdata;
                    ProdData<this> pdata = new ProdData<this>();
                    this->pdata = pdata;
                }

                Void run()
                requires current == this
                {
                    ProdData<this> pdata = this->pdata;
                    ConsData<hb this> cdata = this->cdata;
                    
                    Data<this> data = new Data<this>(); // "produce"
                    pdata->data = data;
                    
                    // Note: Non-trivial deduction here that equates
                    // nextCons with cdata.nextCons!
                    Interval nextCons = cdata->nextCons;
                    ConsData<nextCons> nextCdata = cdata->nextCdata;                    
                    
                    Interval nextProd = new Producer(nextCons, nextCdata);
                    pdata->nextProd = nextProd;
                }
            }
            
            class Consumer extends Interval {
                ProdData<hb this> pdata guardedBy this.constructor;
                ConsData<this> cdata guardedBy this.constructor;
                
                constructor(Interval p, ProdData<p> pdata)
                {
                    p hb this;
                    this->pdata = pdata;
                    ConsData<this> cdata = new ConsData<this>();
                    this->cdata = cdata;
                }

                Void run()
                requires current == this
                {
                    ProdData<hb this> pdata = this->pdata;
                    ConsData<this> cdata = this->cdata;
                    
                    Data<hb this> data = pdata->data; // "consume" 
                    
                    Interval nextProd = pdata->nextProd;
                    ProdData<nextProd> nextPdata = pdata->nextPdata;
                    
                    Interval nextCons = new Consumer(nextProd, nextPdata);
                    cdata->nextCons = nextCons;
                }
            }
            
            class BBPC extends Interval {
                constructor() {}
                
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
                }
            }
            
            """,
            List(
            )
        )
    }  
                
}