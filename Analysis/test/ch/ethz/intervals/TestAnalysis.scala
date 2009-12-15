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
    
    def tc(text: String) {
        val log = new Log.TmpHtmlLog()
        try {
            // Extract errors:
            val tag = "// ERROR "
            val expErrors = text.lines.zipWithIndex.filter(_._1.contains(tag)).map { 
                case ((line, idx)) => (line.substring(line.indexOf(tag) + tag.length), idx+1)
            }.toList
            
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
        
            var matched = 0
            log.indented("Expected Errors:") {
                expErrors.foreach { case ((error, idx)) => log("Line %s: %s", idx, error) }
            }
            log.indented("Encountered Errors: ") {                
                for(error <- prog.errors) {
                    log.indented(
                        error
                    ) {
                        val pos = error.loc.pos
                        
                        log("Line %s Column %s", pos.line, pos.column)
                        log(pos.longString)
                        
                        expErrors.find(_._2.toInt == pos.line.toInt) match {
                            case None => 
                                log("Unexpected Error!")
                            case Some((msg, _)) if error.toString != msg.trim =>
                                log("Unexpected Message (not '%s')!", msg.trim)
                            case Some(_) =>
                                matched = matched + 1
                        }
                    }
                }
            }

            assertEquals(matched, prog.errors.length)          // All errors were matched.
            assertEquals(expErrors.length, prog.errors.length) // Correct number of exp. errors.
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
                    super();                    
                }
                
                void setBothOk(Interval inter, Object<inter> obj) 
                requires subinterval this.creator
                {
                    this->inter = inter;
                    this->obj = obj;
                }
                
                void setBothWrongOrder(Interval inter, Object<inter> obj) 
                requires subinterval this.creator
                {
                    this->obj = obj; // ERROR intervals.expected.subtype(obj, Object<inter>{}, Object<this.inter>{})
                    this->inter = inter;
                }
            }
            """
        )
    }    
    
    @Test
    def constructorTypes() {
        tc(
            """
            class Ctor extends Object<this.constructor> {
                
                String c requires this.constructor;
                
                Ctor constructor unctor requires this.constructor;
                Ctor ctor requires this.constructor;
                
                constructor() 
                {         
                    super();                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor<>{c}, Ctor<>{})
                }
                
                // This method is invokable from both within and without the
                // constructor.  It cannot read fields like 'c' because that
                // might permit a data race if the 'this' pointer were shared
                // during the constructor.  (We could perhaps loosen this rule for this.constructor)
                constructor void ctorMethod1() 
                {
                    String c = this->c; // ERROR intervals.not.readable(this.constructor)
                    
                    this->unctor = this; // ERROR intervals.not.writable(this.constructor)
                    this->ctor = this; // ERROR intervals.not.writable(this.constructor)
                }
                
                constructor void ctorMethod2() 
                requires subinterval this.constructor
                {
                    String c = this->c; 
                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor<>{c}, Ctor<>{})
                }
                
                void method(Ctor constructor unconstructed, Ctor constructed)
                {
                    String a1 = constructed->toString();                    
                    String a2 = constructed->c;
                    
                    String b1 = unconstructed->toString(); // ERROR intervals.rcvr.must.be.constructed(unconstructed)
                    String b2 = unconstructed->c; // ERROR intervals.not.readable(unconstructed.constructor)
                }
            }
            """
        )
    }    

    @Test
    def overriddenMethodsCannotAddRequirements() {
        tc(
            """
            """
        )
    }

    @Test
    def superCtors() {
        tc(
            """
            class Z extends Object<this.constructor> {
                constructor(String s) {
                    super(s); // ERROR intervals.wrong.number.method.arguments(0, 1) 
                }
            }
            
            class A extends Object<this.constructor> {
                String s requires this.constructor;
                
                constructor(String s) {
                    super();
                }
            }
            
            class B1 extends A {
                constructor(String t, String w) {
                    super(t, w); // ERROR intervals.wrong.number.method.arguments(1, 2)
                }
            }
            
            class B2 extends A {
                constructor(Void v) {
                    super(v); // ERROR intervals.expected.subtype(v, Void<>{}, String<>{})
                }                
            }
            
            class B3 extends A {
                constructor(String t) {
                    super(t);
                    
                    this->s = t; // ERROR intervals.not.writable(this.super)
                }
            }     
                   
            class B4 extends A {
                String t requires this.constructor;
                constructor(String s, String t) {
                    super(s);
                    this->t = t;
                }
                
                String toString() {
                    return this.t;
                }
            }       
                 
            class B5 extends A {
                String t requires this.constructor;
                constructor(String t) {
                    super(t);
                    
                    String s = this->s; // this.super is readable
                    this->t = s; 
                }
            }     
            """
        )
    }
    
    @Test
    def constructorTypesAndSubtypes() {
        tc(
            """
            class A extends Object<this.constructor> {
                
                constructor() {
                    super();
                }
                
            }
            
            class B extends A {
                
                constructor() {
                    super();
                }
            }
            
            class C extends B {
                
                constructor() {
                    super();
                }
            }
            class Ctor extends Object<this.constructor> {
                
                String c requires this.constructor;
                
                Ctor constructor unctor requires this.constructor;
                Ctor ctor requires this.constructor;
                
                constructor() 
                {         
                    super();                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor<>{c}, Ctor<>{})
                }
                
                // This method is invokable from both within and without the
                // constructor.  It cannot read fields like 'c' because that
                // might permit a data race if the 'this' pointer were shared
                // during the constructor.  (We could perhaps loosen this rule for this.constructor)
                constructor void ctorMethod1() 
                {
                    String c = this->c; // ERROR intervals.not.readable(this.constructor)
                    
                    this->unctor = this; // ERROR intervals.not.writable(this.constructor)
                    this->ctor = this; // ERROR intervals.not.writable(this.constructor)
                }
                
                constructor void ctorMethod2() 
                requires subinterval this.constructor
                {
                    String c = this->c; 
                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor<>{c}, Ctor<>{})
                }
                
                void method(Ctor constructor unconstructed, Ctor constructed)
                {
                    String a1 = constructed->toString();                    
                    String a2 = constructed->c;
                    
                    String b1 = unconstructed->toString(); // ERROR intervals.rcvr.must.be.constructed(unconstructed)
                    String b2 = unconstructed->c; // ERROR intervals.not.readable(unconstructed.constructor)
                }
            }
            """
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
                    super();
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
                    
                    // But edits are not permitted.
                    this->f1 = f2;        // ERROR intervals.not.writable(this.init)
                }                
            }
            """
        )
    } 
    
    @Test
    def bbpcData() {
        tc(
            """
            class Data<Interval p> extends Object<this.p> {
                Object<this.p> o requires this.p;
                constructor() {
                    super();                    
                }
            }
            
            class ProdData<Interval p> extends Object<this.p> {
                Data<this.p> data requires this.p;                              
                Interval nextProd requires this.p;
                ProdData<this.nextProd> nextPdata requires this.p;                
                constructor() {
                    super();                    
                }
            }
            
            class ConsData<Interval c> extends Object<this.c> {
                Interval nextCons requires this.c;
                ConsData<this.nextCons> nextCdata requires this.c;
                constructor() {
                    super();                    
                }
            }
            
            class Producer extends Interval {
                ConsData<hb this> cdata requires this.constructor;
                ProdData<this> pdata requires this.constructor;
                
                constructor(Interval c, ConsData<c> cdata)
                {
                    super();
                    c hb this;
                    this->cdata = cdata;
                    ProdData<this> pdata = new();
                    this->pdata = pdata;
                }

                Void run()
                requires subinterval this
                {
                    ProdData<this> pdata = this->pdata;
                    ConsData<hb this> cdata = this->cdata;
                    
                    Data<this> data = new(); // "produce"
                    pdata->data = data;
                    
                    // Note: Non-trivial deduction here that equates
                    // nextCons with cdata.nextCons!
                    Interval nextCons = cdata->nextCons;
                    ConsData<nextCons> nextCdata = cdata->nextCdata;                    
                    
                    Producer nextProd = new(nextCons, nextCdata);
                    pdata->nextProd = nextProd;
                }
            }
            
            class Consumer extends Interval {
                ProdData<hb this> pdata requires this.constructor;
                ConsData<this> cdata requires this.constructor;
                
                constructor(Interval p, ProdData<p> pdata)
                {
                    super();
                    p hb this;
                    this->pdata = pdata;
                    ConsData<this> cdata = new();
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
                    
                    Consumer nextCons = new(nextProd, nextPdata);
                    cdata->nextCons = nextCons;
                }
            }
            
            class BBPC extends Interval {
                constructor() {
                    super();                    
                }
                
                Void run()
                requires subinterval this
                {
                    ConsData<this> d0 = new();
                    ConsData<this> d1 = new();                    
                    d0->nextCons = this;
                    d0->nextCdata = d1;
                    
                    Producer p = new(this, d0);                    
                    ProdData<p> pdata = p->pdata;
                    
                    Consumer c = new(p, pdata);
                    d1->nextCons = c; 
                    ConsData<c> cdata = c->cdata;
                    d1->nextCdata = cdata;                    
                }
            }
            
            """
        )
    }  

}