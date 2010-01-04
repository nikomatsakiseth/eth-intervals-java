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
    
    // ___ Test running infrastructure ______________________________________
    
    def runTest(errorPhase: String, text: String) {
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
            val prog = new Prog(log, cds)
            
            val failPhase = new CheckAll(prog).check
        
            var matched = 0
            log.indented("Expected Errors:") {
                expErrors.foreach { case ((error, idx)) => log("Line %s: %s", idx, error) }
            }
            log.indented("Encountered Errors: ") {                
                for(error <- prog.errors) {
                    log.indented(
                        error
                    ) {
                        val pos = error.pos
                        
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

            assertEquals(matched, prog.errors.size)          // All errors were matched.
            assertEquals(expErrors.length, prog.errors.size) // Correct number of exp. errors.
            assertEquals(errorPhase, failPhase)
        } catch {
            case t: Throwable => // only print log if test fails:
                System.out.println("Debugging output for failed test:")
                System.out.println(log.outURI)
                throw t
        }        
    }
    
    // The name of the method indicates the phase in which errors are expected:
    
    def wf(text: String) {
        runTest("wf", text)
    }
    
    def cr(text: String) {
        runTest("cr", text)        
    }
    
    def tc(text: String) {
        runTest("tc", text)
    }
    
    def success(text: String) {
        runTest("success", text)
    }
    
    // ___ Tests ____________________________________________________________

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
                    this->obj = obj; // ERROR intervals.expected.subtype(obj, Object<creator: inter>{}, Object<creator: this.inter>{})
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
                    this->obj = obj; // ERROR intervals.expected.subtype(obj, Object<creator: inter>{}, Object<creator: this.inter>{})
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
                
                Scalar c requires this.constructor;
                
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
                    a1 = constructed->toScalar();                    
                    a2 = constructed->c;
                    
                    b1 = unconstructed->toScalar(); // ERROR intervals.rcvr.must.be.constructed(unconstructed)
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
                    o->toScalar();
                }
                
                Void readFinal1(Object<creator: this.final> o) 
                {
                    // Here we do not require this method be
                    // invoked with method a subinterval of this,
                    // so we don't know that this.final is readable.
                    o->toScalar(); // ERROR intervals.requirement.not.met(requires this.final readable by method)
                }
                
                constructor Void readFinal2(Object<creator: this.final> o) 
                requires method subinterval this
                {
                    // This method could be invoked in the constructor,
                    // so we can't even use this.final in an attribute.
                    o->toScalar(); // ERROR intervals.illegal.path.attr(this.final, m)
                }
            }
            """
        )
    }

    @Test
    def inheritedAssumptionsFromSupertypes() {
        tc(
            """
            class Super extends Interval 
            {
                Interval inter requires this.constructor;
                
                constructor before(Interval inter)
                {
                    this->inter = inter;
                    inter hb this;
                }
                
                constructor unrelated(Interval inter)
                {
                    this->inter = inter;
                }
            }
            
            class Sub1 extends Super
            {     
                Object<creator: this.inter> obj requires this.constructor;
                
                constructor(Interval inter)
                {
                    super before(inter);
                }
                
                Void run() 
                requires method subinterval this
                {
                    this.obj->toScalar();
                }
            }
            
            class Sub2 extends Super
            {     
                Object<creator: this.inter> obj requires this.constructor;
                
                constructor(Interval inter)
                {
                    super unrelated(inter);
                }
                
                Void run() 
                requires method subinterval this
                {
                    this.obj->toScalar(); // ERROR intervals.requirement.not.met(requires this.inter readable by method)
                }
            }           
            """
        )
    }
    
    @Test
    def inheritedAssumptionsFromMultipleCtors() {
        tc(
            """
            class Foo extends Interval {
                Interval unrelated requires this.constructor;
                Interval maybeRelated requires this.constructor;
                Interval before requires this.constructor;
                
                constructor c1(Interval a, Interval b, Interval c) 
                {
                    super();
                    this->unrelated = a;
                    this->maybeRelated = b;
                    this->before = c;
                    
                    b hb this;
                    c hb this;                    
                }
                
                constructor c2(Interval a, Interval b, Interval c) 
                {
                    super();
                    this->unrelated = a;
                    this->maybeRelated = b;
                    this->before = c;
                    
                    c hb this;                    
                }
                
                Void mu(Object<creator: this.unrelated> o) 
                requires method subinterval this
                {
                    o->toScalar(); // ERROR intervals.requirement.not.met(requires this.unrelated readable by method)
                }
                
                Void mr(Object<creator: this.maybeRelated> o) 
                requires method subinterval this
                {
                    o->toScalar(); // ERROR intervals.requirement.not.met(requires this.maybeRelated readable by method)
                }
                
                Void mb(Object<creator: this.before> o) 
                requires method subinterval this
                {
                    o->toScalar(); 
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
                    o->toScalar(); // ERROR intervals.requirement.not.met(requires this.final readable by method)
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
    def superCtors() {
        wf(
            """
            class Z extends Object {
                constructor(Scalar s) {
                    super(s); // ERROR intervals.wrong.number.method.arguments(0, 1) 
                }
            }
            
            class A extends Object {
                Scalar s requires this.constructor;
                
                constructor(Scalar s) {
                    super();
                    this->s = s;
                }
            }
            
            class B1 extends A {
                constructor(Scalar t, Scalar w) {
                    super(t, w); // ERROR intervals.wrong.number.method.arguments(1, 2)
                }
            }            
            """
        )
        
        tc(
            """
            class A extends Object {
                Scalar s requires this.constructor;
                
                constructor(Scalar s) {
                    super();
                    this->s = s;
                }
            }
            
            class B2 extends A {
                constructor(Void v) {
                    super(v); // ERROR intervals.expected.subtype(v, Void{}, Scalar{})
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
                Scalar s requires this.constructor;
                
                constructor(Scalar s) {
                    super();
                }
            }
            
            class B3 extends A {
                constructor(Scalar t) {
                    super(t);
                    
                    this->s = t; // ERROR intervals.not.writable(this.super)
                }
            }     
                   
            class B4 extends A {
                Scalar t requires this.constructor;
                
                constructor(Scalar s, Scalar t) {
                    super(s);
                    this->t = t;
                }
                
                Scalar toScalar() {
                    return this.t;
                }
            }       
                 
            class B5 extends A {
                Scalar t requires this.constructor;
                constructor(Scalar t) {
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
                
                Scalar c requires this.constructor;
                
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
                    a1 = constructed->toScalar();                    
                    a2 = constructed->c;
                    
                    b1 = unconstructed->toScalar(); // ERROR intervals.rcvr.must.be.constructed(unconstructed)
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
                Scalar f1 requires this.init;
                Scalar f2 requires this.init;
                
                constructor(Scalar f1) 
                requires method subinterval this.init
                {
                    super();
                    this->f1 = f1;
                }
                
                Void additionalInit(Scalar f2)
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
            
            class ScalarRegister extends Monitor {
                Scalar value requires this.lock;
                
                constructor() {
                    super();
                }

                Scalar brokenGet() 
                {
                    v = this->value; // ERROR intervals.not.readable(this.lock)
                    // return v; /* commented out due to error above */
                }

                Void brokenSet(Scalar v) 
                {
                    this->value = v; // ERROR intervals.not.writable(this.lock)
                }
                
                Scalar get() 
                {
                    subinterval push x locks this.lock;
                    v = this->value; 
                    subinterval pop x;
                    return v;
                }
                
                Void set(Scalar v) 
                {
                    subinterval push x locks this.lock;
                    this->value = v;
                    subinterval pop x;
                }
                
                Scalar toScalar() 
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
    def ghostTypesLocals()
    {
        tc(
            """
            class Foo<Scalar s><Lock l><Interval i> extends Object {
                constructor() {
                    super();
                }                
            }
            
            class Bar extends Object {
                constructor() {
                    super();
                }
                
                Void run() {
                    s = (Scalar)null;
                    l = (Lock)null;
                    i = (Interval)null;
                                        
                    f1 = new Foo<s: s><l: l><i: i><creator: i>();
                    f2 = new Foo<s: l><l: l><i: i><creator: i>(); // ERROR intervals.expected.subtype(l, Lock{}, Scalar{})
                    f3 = new Foo<s: s><l: s><i: i><creator: i>(); // ERROR intervals.expected.subtype(s, Scalar{}, Lock{})
                    f4 = new Foo<s: s><l: l><i: s><creator: i>(); // ERROR intervals.expected.subtype(s, Scalar{}, Interval{})
                    f5 = new Foo<s: s><l: l><i: i><creator: s>(); // ERROR intervals.expected.subtype(s, Scalar{}, Interval{})
                }
            }
            """
        )
    }
    
    // At the moment, we don't validate the types of 
    // arguments to field types, only in new statements.
    // See TODO file.
    // @Test
    def ghostTypesFields()
    {
        tc(
            """
            class Foo<Scalar s> extends Object {
                constructor() {
                    super();
                }                
            }
            
            class Bar<Scalar s><Interval i> extends Object {
                Foo<s: this.s><creator: i> f1; 
                Foo<s: this.i><creator: i> f2; // ERROR intervals.expected.subtype(this.i, Interval{}, Scalar{})
                Foo<s: this.s><creator: s> f3; // ERROR intervals.expected.subtype(this.s, Scalar{}, Interval{})
                
                constructor() {
                    super();
                }
            }
            """
        )
    }
    
    @Test
    def illegalLinkedFields()
    {
        tc(
            """
            class Data1 extends Object {
                Interval i;                
                constructor() { super(); }
            }
            
            class Data2 extends Object {
                Scalar s;
                constructor() { super(); }
            }
            
            class Direct extends Object {
                Interval i;
                Data2<creator: this.i> data2; // Safe, because i is in the same class.
                constructor() { super(); }
            }
            
            class Indirect1 extends Object {
                Data1 data1;
                
                // It's not permitted for data2 depend on this.data1.i because
                // it is not declared in the same class and it is not constant 
                // during this.creator (data2's guard).
                Data2<creator: this.data1.i> data2; // ERROR intervals.illegal.type.dep(this.data1.i, this.creator)
                constructor() { super(); }
            }
                        
            class Indirect2 extends Object {
                // It doesn't matter that data1 is only created by the
                // ctor, because we don't know that data2 is only modified after ctor completes!
                // See TODO for more thoughts.
                Data1<creator: this.constructor> data1 requires this.constructor;
                Data2<creator: this.data1.i> data2; // ERROR intervals.illegal.type.dep(this.data1.i, this.creator)
                constructor() { super(); }
            }
            
            class Indirect3 extends Interval {
                // This is safe, because for intervals, we know that this.constructor hb this:
                Data1<creator: this.constructor> data1 requires this.constructor;
                Data2<creator: this.data1.i> data2 requires this;
                constructor() { super(); }
            }

            class Indirect4 extends Object {
                // Not safe: Constructor could load up data1 where this.data.i is the current
                // method, store data2, then call a method on data1 which changes this.data1.i,
                // making data2 invalid.
                Data1<creator: this.constructor> data1 requires this.constructor;
                Data2<creator: this.data1.i> data2 requires this.data1.i; // ERROR intervals.illegal.type.dep(this.data1.i, this.data1.i)
                constructor() { super(); }
            }
            """
        )
    }
    
    @Test
    def illegalClassInReq
    {
        wf(
            """
            class Data extends Object {
                Interval i;
                
                constructor() { super(); }
            }
            
            class C extends Object {
                constructor() { super(); }
                
                Void mthd(
                    Data<creator: readableBy method> m1, 
                    Data<creator: readableBy method> m2, 
                    Data<creator: hb method> c1,
                    Data<creator: hb method> c2
                ) 
                requires c1.i hb c2.i
                requires c1 hb c2 // ERROR intervals.expected.subclass.of.any(Data, Array(Point, Interval))
                requires c1.i hb c2 // ERROR intervals.expected.subclass.of.any(Data, Array(Point, Interval))
                {
                    
                }
            }
            """
        )    
    }
    
    @Test
    def mutableHbRelations
    {
        tc(
            """
            class Data extends Object {
                Interval i;
                
                constructor() { super(); }
            }
            
            class C extends Object {
                constructor() { super(); }
                
                Void mthd(
                    Data<creator: readableBy method> m1, 
                    Data<creator: readableBy method> m2, 
                    Data<creator: hb method> c1,
                    Data<creator: hb method> c2
                ) 
                requires c1.i hb c2.i
                requires m1.i hb m2.i // ERROR intervals.illegal.path.attr(m1.i, m)
                requires c1.i hb m2.i // ERROR intervals.illegal.path.attr(m2.i, m)
                {
                    
                }
            }
            """
        )   
    }
    
    @Test
    def fieldsTypesWf()
    {
        wf(
            """
            class Foo<Scalar s> extends Object {
                constructor() {
                    super();
                }                
            }
            
            class Bar<Scalar s><Interval i> extends Object {
                Foo<s: this.s><creator: this.i> f1; 
                Foo<t: this.s><creator: this.i> f2; // ERROR intervals.no.such.ghost(Foo, t)
                Foo<t: this.s><creator: i> f3; // ERROR intervals.no.such.variable(i)
                Foo<t: this.s><creator: method> f4; // ERROR intervals.no.such.variable(method)
                
                constructor() {
                    super();
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
        wf(
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
        success(
            """
            class Data<Lock lock> extends Object<creator: this.lock> {
                Scalar fld requires this.lock;
                
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
                    
                    subinterval push x locks lock;
                    data = (Data<lock: this.lock>)null;
                    this->data = data;

                    nextLink = (Link)null;
                    this->nextLink = nextLink;                        
                    subinterval pop x;
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
    def blockGotoCheckTypes() {
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
                    goto 1();
                    goto 2();
                }
                () // Block 1
                {
                    b1 = (Object<creator: this.creator>)null;
                    goto 3(b1);
                }
                () // Block 2
                {
                    goto 3(a); // method parameters are in-scope here
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
                    goto 1();
                    goto 2(b2); // ERROR intervals.expected.subtype(b2, Object<creator: this>{}, Object<creator: this.creator>{})
                }
                () // Block 1
                {
                    b1 = (Object<creator: this>)null;
                    goto 2(b1); // ERROR intervals.expected.subtype(b1, Object<creator: this>{}, Object<creator: this.creator>{})
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
    def shadowGhostsInSuperType() {
        wf(
            """
            class Super
                <Interval i> 
            extends Object {
                constructor() {
                    super();
                }
            }
            
            class Sub
                <Interval i> // ERROR intervals.shadowed.ghost(Super, i)
            extends Super {
                constructor() {
                    super();
                }
            }
            """            
        )
    }
    
    @Test
    def duplicateGhostsInSameType() {
        wf(
            """
            class Sub
                <Interval i>
                <Interval i> // ERROR intervals.duplicate.field(i)
            extends Object {
                constructor() {
                    super();
                }
            }
            """            
        )
    }

    @Test
    def bbpcData() {
        success(
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