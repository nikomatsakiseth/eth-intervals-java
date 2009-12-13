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
                for(error <- prog.errors) {
                    log.indented(
                        "ExpError(\"%s\", List(%s))", 
                        error.msg, ", ".join("\"", error.args, "\"")
                    ) {
                        val pos = error.loc.pos
                        log("Line %s Column %s", pos.line, pos.column)
                        log(pos.longString)
                    }
                }
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
    def linkedFields() {
        tc(
            """
            class Linked<Interval creator> extends Object<this.creator> {
                Interval inter requires this.creator;
                Object<this.inter> obj requires this.creator;
                
                constructor() 
                requires subinterval this.creator
                {                   
                }
                
                void setBothOk(Interval inter, Object<p> obj) 
                requires subinterval this.creator
                {
                    this->inter = p;
                    this->obj = obj;
                }
                
                void setBothWrongOrder(Interval inter, Object<p> obj) 
                requires subinterval this.creator
                {
                    this->obj = obj;
                    this->inter = p;
                }
            }
            """,
            List(
            )
        )
    }    
    
    @Test
    def extendedInit() {
        tc(
            """
            class ExtendedInit<Interval init> extends Object<this.init> {
                String f1 requires this.init;
                String f2 requires this.init;
                
                constructor(String f1) 
                requires subinterval this.init
                {
                    this->f1 = f1;
                }
                
                void additionalInit(String f2)
                requires subinterval this.init
                {
                    this->f2 = f2;
                }
                
                void afterInit()
                requires this.init hb method
                {
                    String f1 = this->f1; // safe to read both of these...
                    String f2 = this->f2; // ...because init is complete.
                    
                    this->f1 = f2;        // But edits are not permitted.
                }                
            }
            """,
            List(
            )
        )
    } 
    
    @Test
    def bbpcData() {
        tc(
            """
            class Data<Interval p> extends Object<this.p> {
                Object<this.p> o requires this.p;
                constructor() {}
            }
            
            class ProdData<Interval p> extends Object<this.p> {
                Data<this.p> data requires this.p;                              
                Interval nextProd requires this.p;
                ProdData<this.nextProd> nextPdata requires this.p;                
                constructor() {}
            }
            
            class ConsData<Interval c> extends Object<this.c> {
                Interval nextCons requires this.c;
                ConsData<this.nextCons> nextCdata requires this.c;
                constructor() {}
            }
            
            class Producer extends Interval {
                ConsData<hb this> cdata requires this.constructor;
                ProdData<this> pdata requires this.constructor;
                
                constructor(Interval c, ConsData<c> cdata)
                {
                    c hb this;
                    this->cdata = cdata;
                    ProdData<this> pdata = new ProdData<this>();
                    this->pdata = pdata;
                }

                Void run()
                requires subinterval this
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
                ProdData<hb this> pdata requires this.constructor;
                ConsData<this> cdata requires this.constructor;
                
                constructor(Interval p, ProdData<p> pdata)
                {
                    p hb this;
                    this->pdata = pdata;
                    ConsData<this> cdata = new ConsData<this>();
                    this->cdata = cdata;
                }

                Void run()
                requires subinterval this
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
                requires subinterval this
                {
                    ConsData<this> d0 = new ConsData<this>();
                    ConsData<this> d1 = new ConsData<this>();                    
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