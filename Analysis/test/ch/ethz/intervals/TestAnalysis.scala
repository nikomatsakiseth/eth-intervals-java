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
            class Linked extends Object {
                Interval inter requires this.creator;
                Object<creator: this.inter> obj requires this.creator;
                
                constructor() 
                requires method subinterval this.creator
                {                   
                    super();                    
                }
                
                Void setBothOkWhenGivenAsParameters(Interval inter, Object<creator: inter> obj) 
                requires method subinterval this.creator
                {
                    this->inter = inter;
                    this->obj = obj;
                }
                
                Void setBothOkWhenOneIsCreated(Interval inter)
                requires method subinterval this.creator
                {
                    obj = new Object<creator: inter>();
                    
                    this->inter = inter;
                    this->obj = obj;
                }
                
                Void setBothWrongOrder(Interval inter, Object<creator: inter> obj) // ERROR intervals.must.assign.first(this.obj)
                requires method subinterval this.creator
                {
                    this->obj = obj; // ERROR intervals.expected.subtype(obj, Object<creator: inter>{}, Object<creator: this.inter>{})
                    this->inter = inter;
                }
                
                Void setOneNotOk(Interval inter) // ERROR intervals.must.assign.first(this.obj)
                requires method subinterval this.creator
                {
                    this->inter = inter;
                }
                
                Void anotherMethod()
                {
                }
                
                Void invokingAnotherMethodInBetweenNotOk(Interval inter, Object<creator: inter> obj) 
                requires method subinterval this.creator
                {
                    this->inter = inter;
                    this->anotherMethod(); // ERROR intervals.must.assign.first(this.obj)
                    this->obj = obj;
                }
                
                Void invokingAnotherMethodAfterIsOk(Interval inter, Object<creator: inter> obj) 
                requires method subinterval this.creator
                {
                    this->inter = inter;
                    this->obj = obj;                    
                    this->anotherMethod();
                }
                
                Void creatingObjectsInBetweenNotOk(Interval inter, Object<creator: inter> obj) 
                requires method subinterval this.creator
                {
                    this->inter = inter;
                    obj2 = new Object<creator: inter>(); // ERROR intervals.must.assign.first(this.obj)
                    this->obj = obj;
                }
                
                Void creatingObjectsAfterIsOk(Interval inter, Object<creator: inter> obj) 
                requires method subinterval this.creator
                {
                    this->inter = inter;
                    this->obj = obj;
                    obj2 = new Object<creator: inter>();
                }
            }
            """
        )
    }    
    
    @Test
    def neverLinkedToLockWhichGuardsYou() {
        tc(
            """
            class Data<Lock lock> extends Object<creator: this.lock> {                
                constructor() {
                    super();
                }
            }
            
            class Link extends Object<creator: this.constructor> {
                Lock lock requires this.constructor;
                
                // Although this.data's type mentions this.lock,
                // the two fields are not linked, because
                // updates to data can only occur when 
                // this.lock is constant.
                Data<lock: this.lock> data requires this.lock;
                
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
            class Ctor extends Object<creator: this.constructor> {
                
                String c requires this.constructor;
                
                Ctor constructor unctor requires this.constructor;
                Ctor ctor requires this.constructor;
                
                constructor() 
                {         
                    super();                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor{c}, Ctor{})
                }
                
                // This method is invokable from both within and without the
                // constructor.  It cannot read fields like 'c' because that
                // might permit a data race if the 'this' pointer were shared
                // during the constructor.  (We could perhaps loosen this rule for this.constructor)
                constructor Void ctorMethod1() 
                {
                    c = this->c; // ERROR intervals.not.readable(this.constructor)
                    
                    this->unctor = this; // ERROR intervals.not.writable(this.constructor)
                    this->ctor = this; // ERROR intervals.not.writable(this.constructor)
                }
                
                constructor Void ctorMethod2() 
                requires method subinterval this.constructor
                {
                    c = this->c; 
                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor{c}, Ctor{})
                }
                
                Void method(Ctor constructor unconstructed, Ctor constructed)
                {
                    a1 = constructed->toString();                    
                    a2 = constructed->c;
                    
                    b1 = unconstructed->toString(); // ERROR intervals.rcvr.must.be.constructed(unconstructed)
                    b2 = unconstructed->c; // ERROR intervals.not.readable(unconstructed.constructor)
                }
            }
            """
        )
    }    

    @Test
    def inheritedAssumptionsFromCtor() {
        tc(
            """
            class Foo extends Interval {
                Interval final requires this.constructor;
                
                constructor(Interval f) 
                {
                    super();
                    this->final = f;
                    
                    // Establish that this.final hb this:
                    //    This relation will be inherited by other methods.
                    f hb this;
                }
                
                Void readFinal(Object<creator: this.final> o) 
                requires method subinterval this
                {
                    // this.final hb this, and method is a subinterval of this,
                    // so this.final hb method:
                    o->toString();
                }
                
                Void readFinal1(Object<creator: this.final> o) 
                {
                    // Here we do not require this method be
                    // invoked with method a subinterval of this,
                    // so we don't know that this.final is readable.
                    o->toString(); // ERROR intervals.requirement.not.met(requires this.final readable by method)
                }
                
                constructor Void readFinal2(Object<creator: this.final> o) 
                requires method subinterval this
                {
                    // This method could be invoked in the constructor,
                    // so we can't even use this.final in an attribute.
                    o->toString(); // ERROR intervals.illegal.path.attr(this.final, m)
                }
            }
            """
        )
    }
    
    @Test
    def inheritedAssumptionsOnlyIncludeTemporarilyAliasedFields() {
        tc(
            """
            class Foo extends Interval {
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
                
                Void readFinal(Object<creator: this.final> o) 
                requires method subinterval this
                {
                    o->toString(); // ERROR intervals.requirement.not.met(requires this.final readable by method)
                }                
            }
            """
        )
    }    

    @Test
    def overriddenMethodsCannotAddRequirements() {
        tc(
            """
            class A<Interval a><Interval b> extends Object<creator: this.constructor> 
            {
                constructor() {
                    super();
                }
                
                Void aHbB() 
                requires this.a hb this.b
                {                    
                }
            }
            
            class FewerReqs extends A
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
            
            class SameReqs extends A
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
            
            class ImpliedReqs extends A
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
            
            class UnsupportedReqs1 extends A 
            {
                constructor() {
                    super();
                }
                
                Void aHbB() // ERROR intervals.override.adds.req(requires method subinterval of this.constructor)
                requires method subinterval this.constructor 
                {                    
                }
            }
            
            class UnsupportedReqs2 extends A
            {
                constructor() {
                    super();
                }
                
                Void aHbB() // ERROR intervals.override.adds.req(requires this.b hb this.a)
                requires this.b hb this.a 
                {                    
                }
            }
            
            class UnsupportedReqs3 extends A
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
            class SetField extends Object<creator: this.constructor> {
                String s requires this.constructor;
                
                constructor(String s) {
                    this->s = s; // ERROR intervals.illegal.path.attr(this, g)
                    super();
                }                
            }
            
            class GetField extends Object<creator: this.constructor> {
                String s requires this.constructor;
                
                constructor() {
                    s = this->s; // ERROR intervals.illegal.path.attr(this, g)
                    super();
                }                
            }
            
            class Y extends Object<creator: this.constructor> {
                constructor() { super(); }
                
                Void m1() {                
                }
                
                Void m2(Z constructor z) {
                }
            }
            
            class Z extends Object<creator: this.constructor> {
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
            class Z extends Object {
                constructor(String s) {
                    super(s); // ERROR intervals.wrong.number.method.arguments(0, 1) 
                }
            }
            
            class A extends Object {
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
                    super(v); // ERROR intervals.expected.subtype(v, Void{}, String{})
                }                
            }            
            """
        )
    }
    
    @Test
    def superInterval() {
        tc(
            """
            class A extends Object<creator: this.constructor> {
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
                    
                    s = this->s; // this.super is readable
                    this->t = s; 
                }
            }     
            
            class C extends Object<creator: this.constructor> {
                constructor() { 
                    super();
                }
                
                Void mthdReadA(B5 constructor b) {
                    s = b->s; // No error, b.super readable.                    
                }
                
                Void mthdReadB(B5 constructor b) {
                    s = b->t; // ERROR intervals.not.readable(b.constructor)
                }
            }
            """
        )
    }
    
    @Test
    def constructorTypesAndSubtypes() {
        tc(
            """
            class A extends Object<creator: this.constructor> {
                
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
            class Ctor extends Object<creator: this.constructor> {
                
                String c requires this.constructor;
                
                Ctor constructor unctor requires this.constructor;
                Ctor ctor requires this.constructor;
                
                constructor() 
                {         
                    super();                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor{c}, Ctor{})
                }
                
                // This method is invokable from both within and without the
                // constructor.  It cannot read fields like 'c' because that
                // might permit a data race if the 'this' pointer were shared
                // during the constructor.  (We could perhaps loosen this rule for this.constructor)
                constructor Void ctorMethod1() 
                {
                    c = this->c; // ERROR intervals.not.readable(this.constructor)
                    
                    this->unctor = this; // ERROR intervals.not.writable(this.constructor)
                    this->ctor = this; // ERROR intervals.not.writable(this.constructor)
                }
                
                constructor Void ctorMethod2() 
                requires method subinterval this.constructor
                {
                    c = this->c; 
                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor{c}, Ctor{})
                }
                
                Void method(Ctor constructor unconstructed, Ctor constructed)
                {
                    a1 = constructed->toString();                    
                    a2 = constructed->c;
                    
                    b1 = unconstructed->toString(); // ERROR intervals.rcvr.must.be.constructed(unconstructed)
                    b2 = unconstructed->c; // ERROR intervals.not.readable(unconstructed.constructor)
                }
            }
            """
        )
    }
    
    @Test
    def extendedInit() {
        tc(
            """
            class ExtendedInit<Interval init> extends Object<creator: this.init> {
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
                    f1 = this->f1; // safe to read both of these...
                    f2 = this->f2; // ...because init is complete.
                    
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
            class Monitor extends Object<creator: this.constructor> {
                Lock lock requires this.constructor;
                
                constructor() {
                    super();
                    lock = new Lock();
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
                    v = this->value; // ERROR intervals.not.readable(this.lock)
                    // return v; /* commented out due to error above */
                }

                Void brokenSet(String v) 
                {
                    this->value = v; // ERROR intervals.not.writable(this.lock)
                }
                
                String get() 
                {
                    subinterval x locks this.lock {
                        v = this->value; // Note: variables are all METHOD-SCOPE
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
                    s = this->get();
                    return s;
                }
                
            }
            """
        )
    }
    
    @Test
    def multipleInheritance() {
        tc(
            """
            interface class IFoo extends Object {
                constructor() {
                    super();
                }
                
                Object m1()
                requires this.creator readableBy method
                {                    
                }
            }
            
            class Foo1 extends Object, IFoo {
                Object f requires this.creator;
                
                constructor() {
                    super();
                }
                
                Object m1() // n.b.: same requirements as IFoo
                requires this.creator readableBy method
                {
                    f = this->f;
                    return f;
                }
            } 
            
            class Foo2 extends Object, IFoo {
                Object f requires this.creator;
                
                constructor() {
                    super();
                }
                
                Object m1() // n.b.: fewer requirements than IFoo
                {
                }
            }
            
            class Bar extends Object {
                Foo1<creator: this.creator> foo1 requires this.constructor;
                Foo2<creator: this.creator> foo2 requires this.constructor;
                
                constructor() {
                    super();
                }
                
                Void assign() {
                    ifoo1 = this->foo1;
                    ifoo2 = this->foo2;                    
                }
                
                Void invokeThroughInterface() {
                    foo2 = this->foo2;
                    ifoo2 = (IFoo<creator: this.creator>)foo2;
                    ifoo2->m1(); // ERROR intervals.requirement.not.met(requires this.creator readable by method)
                }
                
                Void invokeThroughFoo1() {
                    foo1 = this->foo1;
                    foo1->m1(); // ERROR intervals.requirement.not.met(requires this.creator readable by method)
                }
                
                Void invokeThroughFoo2() {
                    foo2 = this->foo2;
                    foo2->m1();
                }
            }
            """
        )
    }
    
    @Test
    def ghostTypes()
    {
        tc(
            """
            class Foo<String s><Lock l><Interval i> extends Object {
                constructor() {
                    super();
                }                
            }
            
            class Bar extends Object {
                constructor() {
                    super();
                }
                
                Void run() {
                    s = (String)null;
                    l = (Lock)null;
                    i = (Interval)null;
                                        
                    f1 = new Foo<s: s><l: l><i: i><creator: i>();
                    f2 = new Foo<s: l><l: l><i: i><creator: i>(); // ERROR intervals.expected.subtype(l, Lock{}, String{})
                    f3 = new Foo<s: s><l: s><i: i><creator: i>(); // ERROR intervals.expected.subtype(s, String{}, Lock{})
                    f4 = new Foo<s: s><l: l><i: s><creator: i>(); // ERROR intervals.expected.subtype(s, String{}, Interval{})
                    f5 = new Foo<s: s><l: l><i: i><creator: s>(); // ERROR intervals.expected.subtype(s, String{}, Interval{})
                }
            }
            """
        )
    }
    
    @Test
    def superTypePreservesGhosts()
    {
        tc(
            """
            class C1<Lock l1> extends Object {
                constructor() {
                    super();
                }
            }
            
            class C2<Lock l2> extends C1 {
                constructor() {
                    super();
                }                                
            }
            
            class D<Interval a><Lock b><Lock c><Interval d> extends Object { 
                Object<creator: this.a> creatorA;
                C1<l1: this.b> l1B;
                C1<creator: this.a><l1: this.b> creatorAl1B;
                
                constructor() {
                    super();
                }                
                
                Void ok() 
                requires this.creator writableBy method
                {
                    obj = new C2<creator: this.a><l1: this.b><l2: this.c>();
                    this->creatorA = obj;
                    this->l1B = obj;
                    this->creatorAl1B = obj; 
                }
                
                Void creatorWrong() 
                requires this.creator writableBy method
                {
                    obj = new C2<creator: this.d><l1: this.b><l2: this.c>();
                    this->creatorA = obj; // ERROR intervals.expected.subtype(obj, C2<creator: this.d><l1: this.b><l2: this.c>{}, Object<creator: this.a>{})
                    this->l1B = obj; 
                    this->creatorAl1B = obj; // ERROR intervals.expected.subtype(obj, C2<creator: this.d><l1: this.b><l2: this.c>{}, C1<creator: this.a><l1: this.b>{})
                } 
                
                Void l1Wrong() 
                requires this.creator writableBy method
                {
                    obj = new C2<creator: this.a><l1: this.c><l2: this.b>();
                    this->creatorA = obj;
                    this->l1B = obj; // ERROR intervals.expected.subtype(obj, C2<creator: this.a><l1: this.c><l2: this.b>{}, C1<l1: this.b>{})
                    this->creatorAl1B = obj; // ERROR intervals.expected.subtype(obj, C2<creator: this.a><l1: this.c><l2: this.b>{}, C1<creator: this.a><l1: this.b>{})
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
            interface class IFoo extends Object {
                constructor() {
                    super();
                }
            }
            
            interface class IBar extends Ok { // ERROR intervals.superType.not.interface(Ok)
                constructor() {
                    super();
                }
            }
            
            class Ok extends Object, IFoo {
                constructor() {
                    super();
                }                
            }
            
            class BadExtendsInter extends IFoo, Object { // ERROR intervals.superType.interface(IFoo)
                constructor() {
                    super();
                }                
            }
            
            class BadExtendsClass extends Ok, Object { // ERROR intervals.superType.not.interface(Object)
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
            class Data<Lock lock> extends Object<creator: this.lock> {
                String fld requires this.lock;
                
                constructor() {
                    super();
                }                
            }
            
            class Link extends Object<creator: this.constructor> {
                Lock lock requires this.constructor;
                
                Data<lock: this.lock> data requires this.lock;
                Link nextLink requires this.lock;
                
                constructor() {
                    super();
                    lock = new Lock();
                    this->lock = lock;
                    
                    subinterval x locks lock {
                        data = (Data<lock: this.lock>)null;
                        this->data = data;

                        nextLink = (Link)null;
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
                
                Data<lock: this.link.lock> transform(Data<lock: this.link.lock> inData) 
                requires this.link.lock writableBy method
                {
                    outData = new Data<lock: this.link.lock>();
                    fld = inData->fld;
                    outData->fld = fld;
                    return outData;
                }
                
                Void run() 
                requires method subinterval this
                {
                    // Update data:
                    oldData = this.link->data;
                    newData = this->transform(oldData);
                    this.link->data = newData;
                    
                    // Start next link:
                    nextLink = this.link->nextLink;
                    nextInter = new HohLink(nextLink);
                    //XXX nextInter.start hb this.end;
                }                
            }
            """
        )
    }

    @Test
    def blockSuccCheckTypes() {
        tc(
            """
            class Class extends Object {
                constructor() {
                    super();
                }
                
                // ______________________________________________________________________
                Object<creator: this.creator> ok(Object<creator: this.creator> a)                                 
                {
                    // Presumably some test would be used to choose
                    // between succ 1 and 2:
                    succ 1();
                    succ 2();
                }
                () // Block 1
                {
                    b1 = (Object<creator: this.creator>)null;
                    succ 3(b1);
                }
                () // Block 2
                {
                    succ 3(a); // method parameters are in-scope here
                }
                (Object<creator: this.creator> b3) // Block 3
                {
                    return b3;
                }

                // ______________________________________________________________________
                Object<creator: this.creator> bad()
                {
                    // Presumably some test would be used to choose
                    // between succ 1 and 2:
                    b2 = (Object<creator: this>)null;
                    succ 1();
                    succ 2(b2); // ERROR intervals.expected.subtype(b2, Object<creator: this>{}, Object<creator: this.creator>{})
                }
                () // Block 1
                {
                    b1 = (Object<creator: this>)null;
                    succ 2(b1); // ERROR intervals.expected.subtype(b1, Object<creator: this>{}, Object<creator: this.creator>{})
                }
                (Object<creator: this.creator> b3) // Block 2
                {
                    return b3;
                }
            }
            """
        )
    }
    
    @Test
    def bbpcData() {
        tc(
            """
            class Data extends Object {
                Object<creator: this.creator> o;
                constructor() {
                    super();                    
                }
            }
            
            class ProdData extends Object {
                Data<creator: this.creator> data;
                Interval nextProd;
                ProdData<creator: this.nextProd> nextPdata;
                constructor() {
                    super();                    
                }
            }
            
            class ConsData extends Object {
                Interval nextCons;
                ConsData<creator: this.nextCons> nextCdata;
                constructor() {
                    super();                    
                }
            }
            
            class Producer extends Interval {
                ConsData<creator: hb this> cdata requires this.constructor;
                ProdData<creator: this> pdata requires this.constructor;
                
                constructor(Interval c, ConsData<creator: c> cdata)
                {
                    super();
                    c hb this;
                    this->cdata = cdata;
                    pdata = new ProdData<creator: this>();
                    this->pdata = pdata;
                }

                Void run()
                requires method subinterval this
                {
                    pdata = this->pdata;
                    cdata = this->cdata;
                    
                    data = new Data<creator: this>(); // "produce"
                    pdata->data = data;
                    
                    // Note: Non-trivial deduction here that equates
                    // nextCons with cdata.nextCons!
                    nextCons = cdata->nextCons;
                    nextCdata = cdata->nextCdata;                    
                    
                    nextProd = new Producer(nextCons, nextCdata);
                    pdata->nextProd = nextProd;
                    nextPdata = nextProd->pdata;
                    pdata->nextPdata = nextPdata;
                }
            }
            
            class Consumer extends Interval {
                ProdData<creator: hb this> pdata requires this.constructor;
                ConsData<creator: this> cdata requires this.constructor;
                
                constructor(Interval p, ProdData<creator: p> pdata)
                {
                    super();
                    p hb this;
                    this->pdata = pdata;
                    cdata = new ConsData<creator: this>();
                    this->cdata = cdata;
                }

                Void run()
                requires method subinterval this
                {
                    pdata = this->pdata;
                    cdata = this->cdata;
                    
                    data = pdata->data; // "consume" 
                    
                    nextProd = pdata->nextProd;
                    nextPdata = pdata->nextPdata;
                    
                    nextCons = new Consumer(nextProd, nextPdata);
                    cdata->nextCons = nextCons;
                    nextCdata = nextCons->cdata;
                    cdata->nextCdata = nextCdata;
                }
            }
            
            class BBPC extends Interval {
                constructor() {
                    super();                    
                }
                
                Void run()
                requires method subinterval this
                {
                    d0 = new ConsData<creator: this>();
                    d1 = new ConsData<creator: this>();
                    d0->nextCons = this;
                    d0->nextCdata = d1;
                    
                    p = new Producer(this, d0);                    
                    pdata = p->pdata;
                    
                    c = new Consumer(p, pdata);
                    d1->nextCons = c; 
                    cdata = c->cdata;
                    d1->nextCdata = cdata;                    
                }
            }
            
            """
        )
    }  

}