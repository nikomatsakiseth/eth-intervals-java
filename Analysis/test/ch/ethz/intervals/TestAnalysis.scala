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
                
                Void setBothOkWhenGivenAsParameters(Interval inter, Object<inter> obj) 
                requires subinterval this.creator
                {
                    this->inter = inter;
                    this->obj = obj;
                }
                
                Void setBothOkWhenOneIsCreated(Interval inter)
                requires subinterval this.creator
                {
                    Object<inter> obj = new();
                    
                    this->inter = inter;
                    this->obj = obj;
                }
                
                Void setBothWrongOrder(Interval inter, Object<inter> obj) // ERROR intervals.must.assign.first(this.obj)
                requires subinterval this.creator
                {
                    this->obj = obj; // ERROR intervals.expected.subtype(obj, Object<inter>{}, Object<this.inter>{})
                    this->inter = inter;
                }
                
                Void setOneNotOk(Interval inter) // ERROR intervals.must.assign.first(this.obj)
                requires subinterval this.creator
                {
                    this->inter = inter;
                }
                
                Void anotherMethod()
                {
                }
                
                Void invokingAnotherMethodInBetweenNotOk(Interval inter, Object<inter> obj) 
                requires subinterval this.creator
                {
                    this->inter = inter;
                    this->anotherMethod(); // ERROR intervals.must.assign.first(this.obj)
                    this->obj = obj;
                }
                
                Void invokingAnotherMethodAfterIsOk(Interval inter, Object<inter> obj) 
                requires subinterval this.creator
                {
                    this->inter = inter;
                    this->obj = obj;                    
                    this->anotherMethod();
                }
                
                Void creatingObjectsInBetweenNotOk(Interval inter, Object<inter> obj) 
                requires subinterval this.creator
                {
                    this->inter = inter;
                    Object<inter> obj2 = new(); // ERROR intervals.must.assign.first(this.obj)
                    this->obj = obj;
                }
                
                Void creatingObjectsAfterIsOk(Interval inter, Object<inter> obj) 
                requires subinterval this.creator
                {
                    this->inter = inter;
                    this->obj = obj;
                    Object<inter> obj2 = new();
                }
            }
            """
        )
    }    
    
    @Test
    def neverLinkedToLockWhichGuardsYou() {
        tc(
            """
            class Data<Lock lock> extends Object<this.lock> {                
                constructor() {
                    super();
                }
            }
            
            class Link extends Object<this.constructor> {
                Lock lock requires this.constructor;
                
                // Although this.data's type mentions this.lock,
                // the two fields are not linked, because
                // updates to data can only occur when 
                // this.lock is constant.
                Data<this.lock> data requires this.lock;
                
                // XXX This error is currently thrown.  I am re-thinking
                // XXX whether we should be linked to such fields: I now
                // XXX think that if the guard of a field is writable by
                // XXX you, then the field itself is writable, and hence linked!
                constructor(Lock lock) { // ERROR intervals.must.assign.first(this.data)
                    super();
                    this->lock = lock; // n.b.: if lock/data WERE linked, would make data invalid
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
                constructor Void ctorMethod1() 
                {
                    String c = this->c; // ERROR intervals.not.readable(this.constructor)
                    
                    this->unctor = this; // ERROR intervals.not.writable(this.constructor)
                    this->ctor = this; // ERROR intervals.not.writable(this.constructor)
                }
                
                constructor Void ctorMethod2() 
                requires subinterval this.constructor
                {
                    String c = this->c; 
                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor<>{c}, Ctor<>{})
                }
                
                Void method(Ctor constructor unconstructed, Ctor constructed)
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
    def inheritedAssumptionsFromCtor() {
        tc(
            """
            class Foo<Interval inter, Lock lock> extends Interval {
                Interval final requires this.constructor;
                
                constructor(Interval f) 
                {
                    super();
                    this->final = f;
                    
                    // Establish that this.final hb this:
                    //    This relation will be inherited by other methods.
                    f hb this;
                }
                
                Void readFinal(Object<this.final> o) 
                requires method subinterval this
                {
                    // this.final hb this, and method is a subinterval of this,
                    // so this.final hb method:
                    String s1 = o->toString();
                }
                
                Void readFinal1(Object<this.final> o) 
                {
                    // Here we do not require this method be
                    // invoked with method a subinterval of this,
                    // so we don't know that this.final is readable.
                    String s1 = o->toString(); // ERROR intervals.requirement.not.met(requires this.final readable by method)
                }
                
                constructor Void readFinal2(Object<this.final> o) 
                requires method subinterval this
                {
                    // This method could be invoked in the constructor,
                    // so we can't even use this.final in an attribute.
                    String s1 = o->toString(); // ERROR intervals.illegal.path.attr(this.final, m)
                }
            }
            """
        )
    }
    
    @Test
    def inheritedAssumptionsOnlyIncludeTemporarilyAliasedFields() {
        tc(
            """
            class Foo<Interval inter, Lock lock> extends Interval {
                Interval final requires this.constructor;
                
                constructor(Interval f) 
                {
                    super();
                    this->final = f;
                    
                    this->emptyMethod();
                    
                    // Analysis is not smart enough to realize that this->final still equals f:
                    f hb this;
                }
                
                constructor Void emptyMethod()
                {                    
                }
                
                Void readFinal(Object<this.final> o) 
                requires method subinterval this
                {
                    String s1 = o->toString(); // ERROR intervals.requirement.not.met(requires this.final readable by method)
                }                
            }
            """
        )
    }    

    @Test
    def overriddenMethodsCannotAddRequirements() {
        tc(
            """
            class A<Interval a, Interval b> extends Object<this.constructor> 
            {
                constructor() {
                    super();
                }
                
                Void aHbB() 
                requires this.a hb this.b
                {                    
                }
            }
            
            class FewerReqs<Interval a, Interval b> extends A<this.a, this.b> 
            {
                constructor() {
                    super();
                }
                
                Void aHbB() // ok to have fewer reqs...
                {                    
                    // ...but then invoking super is not necessarily safe:
                    super->aHbB(); // ERROR intervals.requirement.not.met(requires this.a hb this.b)
                }
            }
            
            class SameReqs<Interval a, Interval b> extends A<this.a, this.b> 
            {
                constructor() {
                    super();
                }
                
                Void aHbB() // ok to have same reqs...
                requires this.a hb this.b 
                {
                    // ...and in that case, super can be safely invoked:
                    super->aHbB();
                }
            }
            
            class ImpliedReqs<Interval a, Interval b> extends A<this.a, this.b> 
            {
                constructor() {
                    super();
                }
                
                Void aHbB() // ok to have different reqs where super => sub...
                requires this.a readableBy this.b 
                {
                    // ...but then invoking super is not necessarily safe:
                    super->aHbB(); // ERROR intervals.requirement.not.met(requires this.a hb this.b)
                }
            }
            
            class UnsupportedReqs1<Interval a, Interval b> extends A<this.a, this.b> 
            {
                constructor() {
                    super();
                }
                
                Void aHbB() // ERROR intervals.override.adds.req(requires method subinterval of this.constructor)
                requires method subinterval this.constructor 
                {                    
                }
            }
            
            class UnsupportedReqs2<Interval a, Interval b> extends A<this.a, this.b>
            {
                constructor() {
                    super();
                }
                
                Void aHbB() // ERROR intervals.override.adds.req(requires this.b hb this.a)
                requires this.b hb this.a 
                {                    
                }
            }
            
            class UnsupportedReqs3<Interval a, Interval b> extends A<this.a, this.b>
            {
                constructor() {
                    super();
                }
                
                Void aHbB() // ERROR intervals.override.adds.req(requires this.b readable by this.a)
                requires this.b readableBy this.a
                {                    
                }
            }
            """
        )
    }
    
    @Test 
    def thisBeforeSuperCtor() {
        tc(
            """
            class SetField extends Object<this.constructor> {
                String s requires this.constructor;
                
                constructor(String s) {
                    this->s = s; // ERROR intervals.illegal.path.attr(this, g)
                    super();
                }                
            }
            
            class GetField extends Object<this.constructor> {
                String s requires this.constructor;
                
                constructor() {
                    String s = this->s; // ERROR intervals.illegal.path.attr(this, g)
                    super();
                }                
            }
            
            class Y extends Object<this.constructor> {
                constructor() { super(); }
                
                Void m1() {                
                }
                
                Void m2(Z constructor z) {
                }
            }
            
            class Z extends Object<this.constructor> {
                constructor(Y y) {
                    y->m1(); // Ok to do stuff here that doesn't touch this.
                    y->m2(this); // ERROR intervals.illegal.path.attr(this, g)
                    super();
                }        
            }
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
                    this->s = s;
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
            """
        )
    }
    
    @Test
    def superInterval() {
        tc(
            """
            class A extends Object<this.constructor> {
                String s requires this.constructor;
                
                constructor(String s) {
                    super();
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
            
            class C extends Object<this.constructor> {
                constructor() { 
                    super();
                }
                
                Void mthdReadA(B5 constructor b) {
                    String s = b->s; // No error, b.super readable.                    
                }
                
                Void mthdReadB(B5 constructor b) {
                    String s = b->t; // ERROR intervals.not.readable(b.constructor)
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
                constructor Void ctorMethod1() 
                {
                    String c = this->c; // ERROR intervals.not.readable(this.constructor)
                    
                    this->unctor = this; // ERROR intervals.not.writable(this.constructor)
                    this->ctor = this; // ERROR intervals.not.writable(this.constructor)
                }
                
                constructor Void ctorMethod2() 
                requires subinterval this.constructor
                {
                    String c = this->c; 
                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor<>{c}, Ctor<>{})
                }
                
                Void method(Ctor constructor unconstructed, Ctor constructed)
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
                requires method subinterval this.init
                {
                    super();
                    this->f1 = f1;
                }
                
                Void additionalInit(String f2)
                requires method subinterval this.init
                {
                    this->f2 = f2;
                }
                
                Void afterInit()
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
    def subinterval() {
        tc(
            """
            class Monitor extends Object<this.constructor> {
                Lock lock requires this.constructor;
                
                constructor() {
                    super();
                    Lock lock = new();
                    this->lock = lock;
                }
            }
            
            class StringRegister extends Monitor {
                String value requires this.lock;
                
                constructor() {
                    super();
                }

                String brokenGet() 
                {
                    String v = this->value; // ERROR intervals.not.readable(this.lock)
                    // return v; /* commented out due to error above */
                }

                Void brokenSet(String v) 
                {
                    this->value = v; // ERROR intervals.not.writable(this.lock)
                }
                
                String get() 
                {
                    subinterval x locks this.lock {
                        String v = this->value; // Note: variables are all METHOD-SCOPE
                    }
                    return v;
                }
                
                Void set(String v) 
                {
                    subinterval x locks this.lock {
                        this->value = v;
                    }
                }
                
                String toString() 
                {
                    String s = this->get();
                    return s;
                }
                
            }
            """
        )
    }

    // Checks the basic rules for interface inheritance.  We should
    // add more complete tests, but since these are enforced by javac
    // anyway they are hardly high priority.
    @Test
    def interfaceInheritance() {
        tc(
            """
            interface class IFoo<Interval i> extends Object<this.i> {
                constructor() {
                    super();
                }
            }
            
            interface class IBar extends Ok { // ERROR intervals.superType.not.interface(Ok)
                constructor() {
                    super();
                }
            }
            
            class Ok extends Object<this.constructor>, IFoo<this.constructor> {
                constructor() {
                    super();
                }                
            }
            
            class BadExtendsInter extends IFoo<this.constructor>, Object<this.constructor> { // ERROR intervals.superType.interface(IFoo)
                constructor() {
                    super();
                }                
            }
            
            class BadExtendsClass extends Ok<this.constructor>, Object<this.constructor> { // ERROR intervals.superType.not.interface(Object)
                constructor() {
                    super();
                }                
            }
            """
        )
    }

    @Test
    def hoh() {
        tc(
            """
            class Data<Lock lock> extends Object<this.lock> {
                String fld requires this.lock;
                
                constructor() {
                    super();
                }                
            }
            
            class Link extends Object<this.constructor> {
                Lock lock requires this.constructor;
                
                Data<this.lock> data requires this.lock;
                Link nextLink requires this.lock;
                
                constructor() {
                    super();
                    Lock lock = new();
                    this->lock = lock;
                    
                    subinterval x locks lock {
                        Data<this.lock> data = null;
                        this->data = data;

                        Link nextLink = null;
                        this->nextLink = nextLink;                        
                    }
                }
            }
            
            class HohLink extends Interval {
                Link link requires this.constructor;
                
                constructor(Link link) {
                    super();
                    this->link = link;                 
                    this locks link.lock; 
                }
                
                Data<this.link.lock> transform(Data<this.link.lock> inData) 
                requires this.link.lock writableBy method
                {
                    Data<this.link.lock> outData = new();
                    String fld = inData->fld;
                    outData->fld = fld;
                    return outData;
                }
                
                Void run() 
                requires method subinterval this
                {
                    // Update data:
                    Data<this.link.lock> oldData = this.link->data;
                    Data<this.link.lock> newData = this->transform(oldData);
                    this.link->data = newData;
                    
                    // Start next link:
                    Link nextLink = this.link->nextLink;
                    HohLink nextInter = new(nextLink);
                    //XXX nextInter.start hb this.end;
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
                    ProdData<nextProd> nextPdata = nextProd->pdata;
                    pdata->nextPdata = nextPdata;
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
                    ConsData<nextCons> nextCdata = nextCons->cdata;
                    cdata->nextCdata = nextCdata;
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