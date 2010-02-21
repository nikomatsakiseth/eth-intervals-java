package ch.ethz.intervals

import org.scalatest.junit.JUnitSuite
import org.scalatest.FunSuite
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import Util._
import ch.ethz.intervals.log.LogDirectory
import ch.ethz.intervals.log.LogStack
import scala.util.parsing.input.Position
import java.net.URLEncoder
import java.io.File

case class ExpError(msg: String, args: List[String])

class TestAnalysis extends JUnitSuite { 
    import TestAll.DEBUG_DIR
    
    // ___ Test running infrastructure ______________________________________
    
    // These substitutions are performed.  They are not needed in program
    // text, but are useful in the expected error messages:
    def substs = List(
        ("#Creator", ir.f_creator),
        ("#Object", ir.c_object),
        ("#Interval", ir.c_interval),
        ("#Guard", ir.c_guard),
        ("#Point", ir.c_point),
        ("#Lock", ir.c_lock),
        ("#String", ir.c_string),
        ("#void", ir.c_void)
    )
    
    def runTest(errorPhase: String, text0: String) {
        val (baseUrl, baseLineNumber) = {
            val exc = new Throwable().fillInStackTrace
            val stelems = exc.getStackTrace
            val stelem = stelems(2)
            val file = new File(stelem.getFileName)            
            (URLEncoder.encode(file.getAbsolutePath, "UTF-8"), stelem.getLineNumber)
        }        
        
        val logDirectory = LogDirectory.newLogDirectory(DEBUG_DIR, "TestAnalysis")
        val logStack = new LogStack(logDirectory.mainSplitLog)
        val indexLog = logDirectory.indexLog
        try {
            val text = substs.foldLeft(text0) { case (t, (a, b)) => t.replace(a, b.toString) }
            
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
            val prog = new Prog(logStack, cds, ir.cds_special ++ ir.cds_unit_test)
            
            val failPhase = new CheckAll(prog).check
            
            def logError(line: Int, msg: String) = {                
                // skip the url: to get it right we'd have to traverse the subdirectory
                // to find TestAnalysis.scala etc
                val txmtUrl = "txmt://open?line=%d".format(baseLineNumber + line)
                indexLog.linkTo(txmtUrl, "Line %s (%s): %s", line, line+baseLineNumber, msg) 
            }
        
            var matched = 0
            indexLog.indented("Expected Errors:") {
                expErrors.foreach { case ((error, idx)) => 
                    logError(idx, error)
                }
            }
            
            indexLog.indented("Encountered Errors: ") {                
                for(error <- logStack.errors.toList.reverse) {
                    val pos = error.pos
                    
                    expErrors.find(_._2.toInt == pos.line.toInt) match {
                        case None => 
                            indexLog.indented("Unexpected error") {
                                logError(
                                    pos.line.toInt, 
                                    error.toString
                                )                                
                                indexLog(pos.longString)
                            }
                        case Some((msg, _)) if error.toString != msg.trim =>
                            indexLog.indented("Wrong message (not '%s')", msg.trim) {
                                logError(
                                    pos.line.toInt, 
                                    error.toString
                                )                                
                                indexLog(pos.longString)
                            }
                        case Some(_) =>
                            indexLog.indented("Expected error") {
                                logError(
                                    pos.line.toInt, 
                                    error.toString
                                )
                                indexLog(pos.longString)
                            }
                            matched = matched + 1
                    }
                }
            }

            assertEquals(matched, prog.errors.size)          // All errors were matched.
            assertEquals(expErrors.length, prog.errors.size) // Correct number of exp. errors.
            assertEquals(errorPhase, failPhase)
        } catch {
            case t: Throwable => // only print log if test fails:
                System.out.println("Debugging output for failed test:")
                System.out.println(logDirectory.mainSplitLog.uri)
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
            class Linked extends #Object {
                #Interval inter requires this.#Creator;
                #Object@#Creator(this.inter) obj requires this.#Creator;
                
                Constructor() 
                requires method subinterval this.#Creator
                {                   
                    super();                    
                    return;
                }
                
                void setBothOkWhenGivenAsParameters(#Interval inter, #Object@#Creator(inter) obj) 
                requires method subinterval this.#Creator
                {
                    this->inter = inter;
                    this->obj = obj;
                    return;
                }
                
                void setBothOkWhenOneIsCreated(#Interval inter)
                requires method subinterval this.#Creator
                {
                    obj = new #Object@#Creator(inter)();
                    
                    this->inter = inter;
                    this->obj = obj;
                    return;
                }
                
                void setBothWrongOrder(#Interval inter, #Object@#Creator(inter) obj) 
                requires method subinterval this.#Creator
                {
                    this->obj = obj; // ERROR intervals.expected.subtype(obj, @#Creator(inter) #Object, @#Creator(this.inter) #Object)
                    this->inter = inter;
                    return; // ERROR intervals.must.assign.first(this.obj)
                }
                
                void setOneNotOk(#Interval inter) 
                requires method subinterval this.#Creator
                {
                    this->inter = inter;
                    return; // ERROR intervals.must.assign.first(this.obj)
                }
                
                void anotherMethod()
                {
                    return;
                }
                
                void invokingAnotherMethodInBetweenNotOk(#Interval inter, #Object@#Creator(inter) obj) 
                requires method subinterval this.#Creator
                {
                    this->inter = inter;
                    this->anotherMethod(); // ERROR intervals.must.assign.first(this.obj)
                    this->obj = obj; 
                    return;
                }
                
                void invokingAnotherMethodAfterIsOk(#Interval inter, #Object@#Creator(inter) obj) 
                requires method subinterval this.#Creator
                {
                    this->inter = inter;
                    this->obj = obj;                    
                    this->anotherMethod();
                    return;
                }
                
                void creatingObjectsInBetweenNotOk(#Interval inter, #Object@#Creator(inter) obj) 
                requires method subinterval this.#Creator
                {
                    this->inter = inter;
                    obj2 = new #Object@#Creator(inter)(); // ERROR intervals.must.assign.first(this.obj)
                    this->obj = obj; 
                    return;
                }
                
                void creatingObjectsAfterIsOk(#Interval inter, #Object@#Creator(inter) obj) 
                requires method subinterval this.#Creator
                {
                    this->inter = inter;
                    this->obj = obj;
                    obj2 = new #Object@#Creator(inter)();
                    return;
                }
            }
            """
        )
    }    
    
    @Test
    def neverLinkedToLockWhichGuardsYou() {
        tc(
            """
            class Data@lock(#Lock) extends #Object@#Creator(this.lock) {                
            }
            
            class Link extends #Object@#Creator(this.Constructor) {
                #Lock lock requires this.Constructor;
                
                // Although this.data's type mentions this.lock,
                // the two fields are not linked, because
                // updates to data can only occur when 
                // this.lock is constant.
                Data@lock(this.lock) data requires this.lock;
                
                // XXX This error is currently thrown.  I am re-thinking
                // XXX whether we should be linked to such fields: I now
                // XXX think that if the guard of a field is writable by
                // XXX you, then the field itself is writable, and hence linked!
                Constructor(#Lock lock) { 
                    super();
                    this->lock = lock; // n.b.: if lock/data WERE linked, would make data invalid
                    return; // ERROR intervals.must.assign.first(this.data)
                }
            }
            """
        )
    }
    
    @Test
    def constructorTypes() {
        tc(
            """
            class Ctor extends #Object@#Creator(this.Constructor) {
                
                String c requires this.Constructor;
                
                Ctor {Ctor} unctor requires this.Constructor;
                Ctor ctor requires this.Constructor;
                
                Constructor() 
                {         
                    super();                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor constructor, Ctor)
                    return;
                }
                
                // This method is invokable from both within and without the
                // constructor.  It cannot read fields like 'c' because that
                // might permit a data race if the 'this' pointer were shared
                // during the constructor.  (We could perhaps loosen this rule for this.Constructor)
                void ctorMethod1() 
                {
                    c = this->c; // ERROR intervals.not.readable(this.Constructor)
                    
                    this->unctor = this; // ERROR intervals.not.writable(this.Constructor)
                    this->ctor = this; // ERROR intervals.not.writable(this.Constructor)
                    return;
                }
                
                void ctorMethod2() 
                requires method subinterval this.Constructor
                {
                    c = this->c; 
                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor constructor, Ctor)
                    return;
                }
                
                void method(Ctor constructor unconstructed, Ctor constructed)
                requires this.Constructor hb method
                {
                    a1 = constructed->toString();                    
                    a2 = constructed->c;
                    
                    b1 = unconstructed->toString(); // ERROR intervals.rcvr.must.be.constructed(unconstructed)
                    b2 = unconstructed->c; // ERROR intervals.not.readable(unconstructed.constructor)
                    return;
                }
            }
            """
        )
    }    

    @Test
    def inheritedAssumptionsFromCtor() {
        tc(
            """
            class Foo extends #Interval {
                #Interval final requires this.Constructor;
                
                Constructor(#Interval f) 
                {
                    super();
                    this->final = f;
                    
                    // Establish that this.final hb this:
                    //    This relation will be inherited by other methods.
                    f hb this;
                    return;
                }
                
                void readFinal(#Object@#Creator(this.final) o) 
                requires method subinterval this
                requires this.Constructor hb method
                {
                    // this.final hb this, and method is a subinterval of this,
                    // so this.final hb method:
                    o->toString();
                    return;
                }
                
                void readFinal1(#Object@#Creator(this.final) o) 
                requires this.Constructor hb method
                {
                    // Here we do not require this method be
                    // invoked with method a subinterval of this,
                    // so we don't know that this.final is readable.
                    o->toString(); // ERROR intervals.requirement.not.met(requires this.final readable by method)
                    return;
                }
                
                void readFinal2(#Object@#Creator(this.final) o) 
                {
                    // This method could be invoked in the constructor,
                    // so we can't even use this.final in an attribute.
                    o->toString(); // ERROR intervals.must.be.immutable(this.final)
                    return;
                }
            }
            """
        )
    }

    @Test
    def inheritedAssumptionsFromSupertypes() {
        tc(
            """
            class Super extends #Interval 
            {
                #Interval inter requires this.Constructor;
                
                Constructor before(#Interval inter)
                {
                    this->inter = inter;
                    inter hb this;
                    return;
                }
                
                Constructor unrelated(#Interval inter)
                {
                    this->inter = inter;
                    return;
                }
            }
            
            class Sub1 extends Super
            {     
                #Object@#Creator(this.inter) obj requires this.Constructor;
                
                Constructor(#Interval inter)
                {
                    super before(inter);
                    return;
                }
                
                void run() 
                requires method subinterval this
                {
                    this.obj->toString();
                    return;
                }
            }
            
            class Sub2 extends Super
            {     
                #Object@#Creator(this.inter) obj requires this.Constructor;
                
                Constructor(#Interval inter)
                {
                    super unrelated(inter);
                    return;
                }
                
                void run() 
                requires method subinterval this
                {
                    this.obj->toString(); // ERROR intervals.requirement.not.met(requires this.inter readable by method)
                    return;
                }
            }           
            """
        )
    }
    
    @Test
    def inheritedAssumptionsFromMultipleCtors() {
        tc(
            """
            class Foo extends #Interval {
                #Interval unrelated requires this.Constructor;
                #Interval maybeRelated requires this.Constructor;
                #Interval before requires this.Constructor;
                
                Constructor c1(#Interval a, #Interval b, #Interval c) 
                {
                    super();
                    this->unrelated = a;
                    this->maybeRelated = b;
                    this->before = c;
                    
                    b hb this;
                    c hb this;                    
                    return;
                }
                
                Constructor c2(#Interval a, #Interval b, #Interval c) 
                {
                    super();
                    this->unrelated = a;
                    this->maybeRelated = b;
                    this->before = c;
                    
                    c hb this;                    
                    return;
                }
                
                void mu(#Object@#Creator(this.unrelated) o) 
                requires method subinterval this
                {
                    o->toString(); // ERROR intervals.requirement.not.met(requires this.unrelated readable by method)
                    return;
                }
                
                void mr(#Object@#Creator(this.maybeRelated) o) 
                requires method subinterval this
                {
                    o->toString(); // ERROR intervals.requirement.not.met(requires this.maybeRelated readable by method)
                    return;
                }
                
                void mb(#Object@#Creator(this.before) o) 
                requires method subinterval this
                {
                    o->toString(); 
                    return;
                }
            }
            """
        )
    }
    
    @Test
    def inheritedAssumptionsOnlyIncludeTemporarilyAliasedFields() {
        tc(
            """
            class Foo extends #Interval {
                #Interval final requires this.Constructor;
                
                Constructor(#Interval f) 
                {
                    super();
                    this->final = f;
                    
                    this->emptyMethod();
                    
                    // Analysis is not smart enough to realize that this->final still equals f:
                    f hb this;
                    return;
                }
                
                constructor void emptyMethod()
                {                    
                    return;
                }
                
                void readFinal(#Object@#Creator(this.final) o) 
                requires method subinterval this
                {
                    o->toString(); // ERROR intervals.requirement.not.met(requires this.final readable by method)
                    return;
                }                
            }
            """
        )
    }    

    @Test
    def overriddenMethodsCannotAddRequirements() {
        tc(
            """
            class A@a(#Interval)@b(#Interval) extends #Object@#Creator(this.Constructor) 
            {
                void aHbB() 
                requires this.a hb this.b
                {                    
                    return;
                }
            }
            
            class FewerReqs extends A
            {
                void aHbB() // ok to have fewer reqs...
                {                    
                    // ...but then invoking super is not necessarily safe:
                    super->aHbB(); // ERROR intervals.requirement.not.met(requires this.a hb this.b)
                    return;
                }
            }
            
            class SameReqs extends A
            {
                void aHbB() // ok to have same reqs...
                requires this.a hb this.b 
                {
                    // ...and in that case, super can be safely invoked:
                    super->aHbB();
                    return;
                }
            }
            
            class ImpliedReqs extends A
            {
                void aHbB() // ok to have different reqs where super => sub...
                requires this.a readableBy this.b 
                {
                    // ...but then invoking super is not necessarily safe:
                    super->aHbB(); // ERROR intervals.requirement.not.met(requires this.a hb this.b)
                    return;
                }
            }
            
            class UnsupportedReqs1 extends A 
            {
                void aHbB() // ERROR intervals.override.adds.req(requires method subinterval of this.Constructor)
                requires method subinterval this.Constructor 
                {                    
                    return;
                }
            }
            
            class UnsupportedReqs2 extends A
            {
                void aHbB() // ERROR intervals.override.adds.req(requires this.b hb this.a)
                requires this.b hb this.a 
                {                    
                    return;
                }
            }
            
            class UnsupportedReqs3 extends A
            {
                void aHbB() // ERROR intervals.override.adds.req(requires this.b readable by this.a)
                requires this.b readableBy this.a
                {                    
                    return;
                }
            }
            """
        )
    }
    
    @Test
    def superCtors() {
        wf(
            """
            class Z extends #Object {
                Constructor(String s) {
                    super(s); // ERROR intervals.wrong.number.method.arguments(0, 1) 
                    return;
                }
            }
            
            class A extends #Object {
                String s requires this.Constructor;
                
                Constructor(String s) {
                    super();
                    this->s = s;
                    return;
                }
            }
            
            class B1 extends A {
                Constructor(String t, String w) {
                    super(t, w); // ERROR intervals.wrong.number.method.arguments(1, 2)
                    return;
                }
            }            
            """
        )
        
        tc(
            """
            class A extends #Object {
                String s requires this.Constructor;
                
                Constructor(String s) {
                    super();
                    this->s = s;
                    return;
                }
            }
            
            class B2 extends A {
                Constructor(void v) {
                    super(v); // ERROR intervals.expected.subtype(v, void, #String)
                    return;
                }                
            }            
            """
        )
    }
    
    @Test
    def superInterval() {
        tc(
            """
            class A extends #Object@#Creator(this.Constructor) {
                String s requires this.Constructor;
                
                Constructor(String s) {
                    super();
                    return;
                }
            }
            
            class B3 extends A {
                Constructor(String t) {
                    super(t);
                    
                    this->s = t; // ERROR intervals.not.writable(this.super)
                    return;
                }
            }     
                   
            class B4 extends A {
                String t requires this.Constructor;
                
                Constructor(String s, String t) {
                    super(s);
                    this->t = t;
                    return;
                }
                
                String toString() {
                    return this.t;
                }
            }       
                 
            class B5 extends A {
                String t requires this.Constructor;
                Constructor(String t) {
                    super(t);
                    
                    s = this->s; // this.super is readable
                    this->t = s; 
                    return;
                }
            }     
            
            class C extends #Object@#Creator(this.Constructor) {
                Constructor() { 
                    super();
                    return;
                }
                
                void mthdReadA(B5 constructor b) {
                    s = b->s; // No error, b.super readable.                    
                    return;
                }
                
                void mthdReadB(B5 constructor b) {
                    s = b->t; // ERROR intervals.not.readable(b.constructor)
                    return;
                }
            }
            """
        )
    }
    
    @Test
    def constructorTypesAndSubtypes() {
        tc(
            """
            class A extends #Object@#Creator(this.Constructor) {
                
                Constructor() {
                    super();
                    return;
                }
                
            }
            
            class B extends A {
                
                Constructor() {
                    super();
                    return;
                }
            }
            
            class C extends B {
                
                Constructor() {
                    super();
                    return;
                }
            }
            class Ctor extends #Object@#Creator(this.Constructor) {
                
                String c requires this.Constructor;
                
                Ctor constructor unctor requires this.Constructor;
                Ctor ctor requires this.Constructor;
                
                Constructor() 
                {         
                    super();                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor constructor, Ctor)
                    return;
                }
                
                // This method is invokable from both within and without the
                // constructor.  It cannot read fields like 'c' because that
                // might permit a data race if the 'this' pointer were shared
                // during the constructor.  (We could perhaps loosen this rule for this.Constructor)
                constructor void ctorMethod1() 
                {
                    c = this->c; // ERROR intervals.not.readable(this.Constructor)
                    
                    this->unctor = this; // ERROR intervals.not.writable(this.Constructor)
                    this->ctor = this; // ERROR intervals.not.writable(this.Constructor)
                    return;
                }
                
                constructor void ctorMethod2() 
                requires method subinterval this.Constructor
                {
                    c = this->c; 
                    
                    this->unctor = this;
                    this->ctor = this; // ERROR intervals.expected.subtype(this, Ctor constructor, Ctor)
                    return;
                }
                
                void method(Ctor constructor unconstructed, Ctor constructed)
                {
                    a1 = constructed->toString();                    
                    a2 = constructed->c;
                    
                    b1 = unconstructed->toString(); // ERROR intervals.rcvr.must.be.constructed(unconstructed)
                    b2 = unconstructed->c; // ERROR intervals.not.readable(unconstructed.constructor)
                    return;
                }
            }
            """
        )
    }
    
    @Test
    def extendedInit() {
        tc(
            """
            class ExtendedInit@init(#Interval) extends #Object@#Creator(this.init) {
                String f1 requires this.init;
                String f2 requires this.init;
                
                Constructor(String f1) 
                requires method subinterval this.init
                {
                    super();
                    this->f1 = f1;
                    return;
                }
                
                void additionalInit(String f2)
                requires method subinterval this.init
                {
                    this->f2 = f2;
                    return;
                }
                
                void afterInit()
                requires this.init hb method
                {
                    f1 = this->f1; // safe to read both of these...
                    f2 = this->f2; // ...because init is complete.
                    
                    // But edits are not permitted.
                    this->f1 = f2;        // ERROR intervals.not.writable(this.init)
                    return;
                }                
            }
            """
        )
    } 
    
    @Test
    def subinterval() {
        tc(
            """
            class Monitor extends #Object@#Creator(this.Constructor) {
                #Lock lock requires this.Constructor;
                
                Constructor() {
                    super();
                    lock = new Lock();
                    this->lock = lock;
                    return;
                }
            }
            
            class scalarRegister extends Monitor {
                String value requires this.lock;
                
                Constructor() {
                    super();
                    return;
                }

                String brokenGet() 
                {
                    v = this->value; // ERROR intervals.not.readable(this.lock)
                    // return v; /* commented out due to error above */
                }

                void brokenSet(String v) 
                {
                    this->value = v; // ERROR intervals.not.writable(this.lock)
                    return;
                }
                
                String get() 
                {
                    subinterval x locks this.lock {
                        v = this->value;                         
                        break 0(v); // 0 == seq, 1 == subinter
                    } => (String v1);
                    return v1;
                }
                
                void set(String v) 
                {
                    subinterval x locks this.lock {
                        this->value = v;
                        break 0();
                    }
                    return;
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
            interface class IFoo extends #Object {
                Constructor() {
                    super();
                }
                
                #Object m1()
                requires this.#Creator readableBy method
                {
                    return;
                }
            }
            
            class Foo1 extends #Object, IFoo {
                #Object f requires this.#Creator;
                
                Constructor() {
                    super();
                    return;
                }
                
                #Object m1() // n.b.: same requirements as IFoo
                requires this.#Creator readableBy method
                {
                    f = this->f;
                    return f;
                }
            } 
            
            class Foo2 extends #Object, IFoo {
                #Object f requires this.#Creator;
                
                Constructor() {
                    super();
                    return;
                }
                
                #Object m1() // n.b.: fewer requirements than IFoo
                {
                    return;
                }
            }
            
            class Bar extends #Object {
                Foo1@#Creator(this.#Creator) foo1 requires this.Constructor;
                Foo2@#Creator(this.#Creator) foo2 requires this.Constructor;
                
                Constructor() {
                    super();
                    return;
                }
                
                void assign() {
                    ifoo1 = this->foo1;
                    ifoo2 = this->foo2;                    
                    return;
                }
                
                void invokeThroughInterface() {
                    foo2 = this->foo2;
                    ifoo2 = (IFoo@#Creator(this.#Creator))foo2;
                    ifoo2->m1(); // ERROR intervals.requirement.not.met(requires this.`#Creator` readable by method)
                    return;
                }
                
                void invokeThroughFoo1() {
                    foo1 = this->foo1;
                    foo1->m1(); // ERROR intervals.requirement.not.met(requires this.`#Creator` readable by method)
                    return;
                }
                
                void invokeThroughFoo2() {
                    foo2 = this->foo2;
                    foo2->m1();
                    return;
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
            class Foo@s(#String)@l(#Lock)@i(#Interval) extends #Object {
                Constructor() {
                    super();
                    return;
                }                
            }
            
            class Bar extends #Object {
                Constructor() {
                    super();
                    return;
                }
                
                void run() {
                    s = (#String)null;
                    l = (#Lock)null;
                    i = (#Interval)null;
                                        
                    f1 = new Foo@s(s)@l(l)@i(i)@#Creator(i)();
                    f2 = new Foo@s(l)@l(l)@i(i)@#Creator(i)(); // ERROR intervals.expected.subtype(l, #Lock, #String)
                    f3 = new Foo@s(s)@l(s)@i(i)@#Creator(i)(); // ERROR intervals.expected.subtype(s, #String, #Lock)
                    f4 = new Foo@s(s)@l(l)@i(s)@#Creator(i)(); // ERROR intervals.expected.subtype(s, #String, #Interval)
                    f5 = new Foo@s(s)@l(l)@i(i)@#Creator(s)(); // ERROR intervals.expected.subtype(s, #String, #Interval)
                    return;
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
            class Foo@s(#String) extends #Object {
                Constructor() {
                    super();
                    return;
                }                
            }
            
            class Bar@s(#String)@i(#Interval) extends #Object {
                Foo@s(this.s)@#Creator(i) f1; 
                Foo@s(this.i)@#Creator(i) f2; // ERROR intervals.expected.subtype(this.i, Interval, String)
                Foo@s(this.s)@#Creator(s) f3; // ERROR intervals.expected.subtype(this.s, String, Interval)
                
                Constructor() {
                    super();
                    return;
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
            class Data1 extends #Object {
                #Interval i;                
            }
            
            class Data2 extends #Object {
                #String s;
            }
            
            class Direct extends #Object {
                #Interval i;
                Data2@#Creator(this.i) data2; // Safe, because i is in the same class.
            }
            
            class Indirect1 extends #Object {
                Data1 data1;
                
                // It's not permitted for data2 depend on this.data1.i because
                // it is not declared in the same class and it is not constant 
                // during this.#Creator (data2's guard).
                Data2@#Creator(this.data1.i) data2; // ERROR intervals.illegal.type.dep(this.data1.i, this.`#Creator`)
            }
                        
            class Indirect2 extends #Object {
                // It doesn't matter that data1 is only created by the
                // ctor, because we don't know that data2 is only modified after ctor completes!
                // See TODO for more thoughts.
                Data1@#Creator(this.Constructor) data1 requires this.Constructor;
                Data2@#Creator(this.data1.i) data2; // ERROR intervals.illegal.type.dep(this.data1.i, this.`#Creator`)
            }
            
            class Indirect3 extends #Interval {
                // This is safe, because for intervals, we know that this.Constructor hb this:
                Data1@#Creator(this.Constructor) data1 requires this.Constructor;
                Data2@#Creator(this.data1.i) data2 requires this;
            }

            class Indirect4 extends #Object {
                // Not safe: Constructor could load up data1 where this.data.i is the current
                // method, store data2, then call a method on data1 which changes this.data1.i,
                // making data2 invalid.
                Data1@#Creator(this.Constructor) data1 requires this.Constructor;
                Data2@#Creator(this.data1.i) data2 requires this.data1.i; // ERROR intervals.illegal.type.dep(this.data1.i, this.data1.i)
            }
            """
        )
    }
    
    @Test
    def illegalClassInReq
    {
        wf(
            """
            class Data extends #Object {
                #Interval i;
            }
            
            class C extends #Object {
                void mthd(
                    Data@#Creator(readableBy method) m1, 
                    Data@#Creator(readableBy method) m2
                ) 
                requires m1.i hb m2.i
                requires m1 hb m2 // ERROR intervals.expected.subclass.of.any(Data, Array(#Point, #Interval))
                requires m1.i hb m2 // ERROR intervals.expected.subclass.of.any(Data, Array(#Point, #Interval))
                {
                    
                }
            }
            """
        )    
    }
    
    @Test
    def mutableHbRequirements
    {
        tc(
            """
            class Data extends #Object {
                #Interval i;
            }
            
            class C extends #Object {
                void mthd(
                    Data@#Creator(readableBy method) m1, 
                    Data@#Creator(readableBy method) m2
                ) 
                requires m1.i hb m2.i // ERROR intervals.must.be.immutable(m1.i)
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
            class Foo@s(#String) extends #Object {
            }
            
            class Bar@s(#String)@i(#Interval) extends #Object {
                Foo@s(this.s)@#Creator(this.i) f1; 
                Foo@t(this.s)@#Creator(this.i) f2; // ERROR intervals.no.such.ghost(Foo, t)
                Foo@t(this.s)@#Creator(i) f3; // ERROR intervals.no.such.variable(i)
                Foo@t(this.s)@#Creator(method) f4; // ERROR intervals.no.such.variable(method)
            }
            """
        )
    }    
    
    @Test
    def superTypePreservesGhosts()
    {
        tc(
            """
            class C1@l1(#Lock) extends #Object {
            }
            
            class C2@l2(#Lock) extends C1 {
            }
            
            class D@a(#Interval)@b(#Lock)@c(#Lock)@d(#Interval) extends #Object { 
                #Object@#Creator(this.a) creatorA;
                C1@l1(this.b) l1B;
                C1@#Creator(this.a)@l1(this.b) creatorAl1B;
                
                void ok() 
                requires this.#Creator writableBy method
                {
                    obj = new C2 @#Creator(this.a) @l1(this.b) @l2(this.c)();
                    this->creatorA = obj;
                    this->l1B = obj;
                    this->creatorAl1B = obj; 
                }
                
                void creatorWrong() 
                requires this.#Creator writableBy method
                {
                    obj = new C2 @#Creator(this.d) @l1(this.b) @l2(this.c) ();
                    this->creatorA = obj; // ERROR intervals.expected.subtype(obj, @#Creator(this.d) @l1(this.b) @l2(this.c) C2, @#Creator(this.a) #Object)
                    this->l1B = obj; 
                    this->creatorAl1B = obj; // ERROR intervals.expected.subtype(obj, @#Creator(this.d) @l1(this.b) @l2(this.c) C2, @#Creator(this.a) @l1(this.b) C1)
                } 
                
                void l1Wrong() 
                requires this.#Creator writableBy method
                {
                    obj = new C2@#Creator(this.a)@l1(this.c)@l2(this.b)();
                    this->creatorA = obj;
                    this->l1B = obj; // ERROR intervals.expected.subtype(obj, @#Creator(this.a) @l1(this.c) @l2(this.b) C2, @l1(this.b) C1)
                    this->creatorAl1B = obj; // ERROR intervals.expected.subtype(obj, @#Creator(this.a) @l1(this.c) @l2(this.b) C2, @#Creator(this.a) @l1(this.b) C1)
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
            interface class IFoo extends #Object {
            }
            
            interface class IBar extends Ok { // ERROR intervals.superType.not.interface(Ok)
            }
            
            class Ok extends #Object, IFoo {
            }
            
            class BadExtendsInter extends IFoo, #Object { // ERROR intervals.superType.interface(IFoo)
            }
            
            class BadExtendsClass extends Ok, #Object { // ERROR intervals.superType.not.interface(#Object)
            }
            """
        )
    }

    @Test
    def hoh() {
        success(
            """
            class Data@lock(#Lock) extends #Object@#Creator(this.lock) {
                #String fld requires this.lock;
            }
            
            class Link extends #Object@#Creator(this.Constructor) {
                #Lock lock requires this.Constructor;
                
                Data@lock(this.lock) data requires this.lock;
                Link nextLink requires this.lock;
                
                Constructor() {
                    super();
                    lock = new #Lock();
                    this->lock = lock;
                    
                    subinterval x locks lock {
                        data = (Data@lock(this.lock))null;
                        this->data = data;                        
                        
                        nextLink = (Link)null;
                        this->nextLink = nextLink;
                        
                        return;
                    }
                }
            }
            
            class HohLink extends #Interval {
                Link link requires this.Constructor;
                
                Constructor(Link link) {
                    super();
                    this->link = link;  
                    
                    linkLock = link->lock;               
                    this locks linkLock; 
                    return;
                }
                
                Data@lock(this.link.lock) transform(Data@lock(this.link.lock) inData) 
                requires this.Constructor hb method
                requires this.link.lock writableBy method
                {
                    outData = new Data@lock(this.link.lock)();
                    fld = inData->fld;
                    outData->fld = fld;
                    return outData;
                }
                
                void run() 
                requires method subinterval this
                {
                    // Update data:
                    link = this->link;
                    oldData = link->data;
                    newData = this->transform(oldData);
                    link->data = newData;
                    
                    // Start next link:
                    nextLink = link->nextLink;
                    nextInter = new HohLink(nextLink);
                    nextInterStart = nextInter->start;
                    end = this->end;
                    nextInterStart hb end;
                    
                    return;
                }                
            }
            """
        )
    }

    @Test
    def blockBranchCheckIndices() {
        wf(
            """
            class Class extends #Object {
                void badIndex()
                {
                    switch {
                        {
                            break 1(); // ERROR intervals.invalid.stack.index(1, 1)
                        }
                    } => ();
                    return;
                }                
            }
            """
        )
    }
    
    // XXX Turned off the checking of branch arguments because
    //     they lead to duplicate errors being reported (one from
    //     the original assignment, then later from SSA).
    //@Test
    //def blockBranchCheckTypes() {
    //    tc(
    //        """
    //        class Class extends #Object {
    //            // ----------------------------------------------------------------------
    //            #Object@#Creator(this.#Creator> ok(#Object<creator: this.#Creator) a)                                 
    //            {
    //                switch {
    //                    {
    //                        b1 = (#Object@#Creator(this.#Creator))null;
    //                        break 0(b1);
    //                    }
    //                    {
    //                        break 0(a);
    //                    }
    //                } => (#Object@#Creator(this.#Creator) b3);
    //                return b3;
    //            }
    //
    //            // ----------------------------------------------------------------------
    //            #Object@#Creator(this.#Creator) badTypes()
    //            {
    //                switch {
    //                    {
    //                        b1 = (#Object@#Creator(this))null;
    //                        break 0(b1); // ERROR intervals.expected.subtype(b1, @#Creator(this) #Object, @#Creator(this.`#Creator`) #Object)
    //                    }
    //                } => (#Object@#Creator(this.#Creator) b3);
    //            }                
    //        }
    //        """
    //    )
    //}
    
    @Test
    def shadowGhostsInSuperType() {
        wf(
            """
            class Super
                @i(#Interval) 
            extends #Object {
            }
            
            class Sub
                @i(#Interval) // ERROR intervals.shadowed.ghost(Super, i)
            extends Super {
            }
            """            
        )
    }
    
    @Test
    def duplicateGhostsInSameType() {
        wf(
            """
            class Sub
                @i(#Interval)
                @i(#Interval) // ERROR intervals.duplicate.field(i)
            extends #Object {
            }
            """            
        )
    }

    @Test
    def bbpcData() {
        success(
            """
            class Data extends #Object {
                #Object @#Creator(this.#Creator) o;
            }
            
            class ProdData extends #Object {
                Data @#Creator(this.#Creator) data;
                #Interval nextProd;
                ProdData @#Creator(this.nextProd) nextPdata;
            }
            
            class ConsData extends #Object {
                #Interval nextCons;
                ConsData @#Creator(this.nextCons) nextCdata;
            }
            
            class Producer extends #Interval {
                ConsData @#Creator(readableBy this) cdata requires this.Constructor[Producer];
                ProdData @#Creator(this) pdata requires this.Constructor[Producer];
                
                Constructor(#Interval c, ConsData @#Creator(c) cdata)
                {
                    super();
                    c hb this;
                    this->cdata = cdata;
                    pdata = new ProdData @#Creator(this) ();
                    this->pdata = pdata;
                    return;
                }

                void run()
                requires method subinterval this
                {
                    pdata = this->pdata;
                    cdata = this->cdata;
                    
                    data = new Data @#Creator(this) (); // "produce"
                    pdata->data = data;
                    
                    // Note: Non-trivial deduction here that equates
                    // nextCons with cdata.nextCons!
                    nextCons = cdata->nextCons;
                    nextCdata = cdata->nextCdata;                    
                    
                    nextProd = new Producer(nextCons, nextCdata);
                    pdata->nextProd = nextProd;
                    nextPdata = nextProd->pdata;
                    pdata->nextPdata = nextPdata;
                    return;
                }
            }
            
            class Consumer extends #Interval {
                ProdData@#Creator(readableBy this) pdata requires this.Constructor[Consumer];
                ConsData@#Creator(this) cdata requires this.Constructor[Consumer];
                
                Constructor(#Interval p, ProdData@#Creator(p) pdata)
                {
                    super();
                    p hb this;
                    this->pdata = pdata;
                    cdata = new ConsData@#Creator(this)();
                    this->cdata = cdata;
                    return;
                }

                void run()
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
                    return;
                }
            }
            
            class BBPC extends #Interval {
                void run()
                requires method subinterval this
                {
                    d0 = new ConsData@#Creator(this)();
                    d1 = new ConsData@#Creator(this)();
                    d0->nextCons = this;
                    d0->nextCdata = d1;
                    
                    p = new Producer(this, d0);                    
                    pdata = p->pdata;
                    
                    c = new Consumer(p, pdata);
                    d1->nextCons = c; 
                    cdata = c->cdata;
                    d1->nextCdata = cdata;                    
                    return;
                }
            }
            
            """
        )
    }  

}