package ch.ethz.intervals

import org.scalatest.Suite
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import Util._
import ch.ethz.intervals.log.LogDirectory
import ch.ethz.intervals.log.SplitLog
import ch.ethz.intervals.log.LogStack
import scala.util.parsing.input.Position
import java.net.URLEncoder
import java.io.File

case class ExpError(msg: String, args: List[String])

class TestAnalysis extends Suite { 
    import TestAll.DEBUG_DIR
    import TestAll.subst
    
    val logTests: Set[String] = Set("test_methodHbReturn")
    
    // ___ Test running infrastructure ______________________________________
    
    def runTest(errorPhase: String, text0: String) {
        val (invokingMthdName, baseUrl, baseLineNumber) = {
            val exc = new Throwable().fillInStackTrace
            val stelems = exc.getStackTrace
            val stelem = stelems(2)
            val file = new File(stelem.getFileName)            
            (
                stelem.getMethodName,
                URLEncoder.encode(file.getAbsolutePath, "UTF-8"), 
                stelem.getLineNumber
            )
        }
        
        val mainSplitLog = {
            if(logTests(invokingMthdName)) {
                LogDirectory.newLogDirectory(DEBUG_DIR, "TestAnalysis-%s".format(invokingMthdName)).mainSplitLog
            } else {
                SplitLog.devNullSplitLog
            }
        }
        val logStack = new LogStack(mainSplitLog)
        val indexLog = mainSplitLog.indexLog
        try {
            val text = subst(text0)
            
            // Extract errors:
            val tag = "// ERROR "
            val expErrors = text.lines.zipWithIndex.filter(_._1.contains(tag)).map { 
                case ((line, idx)) => (line.substring(line.indexOf(tag) + tag.length), idx+1)
            }.toList
            
            def logError(line: Int, msg: String) = {                
                // skip the url: to get it right we'd have to traverse the subdirectory
                // to find TestAnalysis.scala etc
                val txmtUrl = "txmt://open?line=%d".format(baseLineNumber + line)
                indexLog.linkTo(txmtUrl, "Line %s (%s): %s", line, line+baseLineNumber, msg) 
            }
        
            val text1 = text.replaceAll("//[^\n]*", "")
            val parser = new IrParser()
            val cds = 
                parser.parse(parser.classDecls)(text1) match {
                    case n: parser.NoSuccess =>
                        val pos = n.next.pos
                        val msg = "Parse failure: " + n.toString
                        logError(pos.line, msg)
                        throw new RuntimeException(msg)
                    case parser.Success(cds, _) =>
                        cds
                }            
            val prog = new Prog(logStack, cds, ir.cds_special ++ ir.cds_unit_test)
            
            val failPhase = new CheckAll(prog).check
            
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
                        case Some((msg, _)) if !(msg glob error.toString) =>
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
                System.out.println("Debugging output for \"%s\":".format(invokingMthdName))
                System.out.println(mainSplitLog.uri)
                throw t
        } finally {
            logStack.flush
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

    def test_linkedFields() {
        tc(
            """
            class Linked extends #Object {
                #Interval inter requires this.#Creator;
                #Object@#Creator(this.inter) obj requires this.#Creator;
                
                Constructor() 
                requires method suspends this.#Creator
                {                   
                    super();                    
                    return;
                }
                
                void setBothOkWhenGivenAsParameters(#Interval inter, #Object@#Creator(inter) obj) 
                requires method suspends this.#Creator
                {
                    this->inter = inter;
                    this->obj = obj;
                    return;
                }
                
                void setBothOkWhenOneIsCreated(#Interval inter)
                requires method suspends this.#Creator
                {
                    obj = new #Object@#Creator(inter)();
                    
                    this->inter = inter;
                    this->obj = obj;
                    return;
                }
                
                void setBothWrongOrder(#Interval inter, #Object@#Creator(inter) obj) 
                requires method suspends this.#Creator
                {
                    this->obj = obj; // ERROR intervals.expected.subtype(obj, @#Constructor(hbNow) @#Creator(inter) #Object, @#Constructor(hbNow this.#Constructor) @#Creator(this.inter) #Object)
                    this->inter = inter;
                    return; // ERROR intervals.must.assign.first(this.obj)
                }
                
                void setOneNotOk(#Interval inter) 
                requires method suspends this.#Creator
                {
                    this->inter = inter;
                    return; // ERROR intervals.must.assign.first(this.obj)
                }
                
                void anotherMethod()
                {
                    return;
                }
                
                void invokingAnotherMethodInBetweenNotOk(#Interval inter, #Object@#Creator(inter) obj) 
                requires method suspends this.#Creator
                {
                    this->inter = inter;
                    this->anotherMethod(); // ERROR intervals.must.assign.first(this.obj)
                    this->obj = obj; 
                    return;
                }
                
                void invokingAnotherMethodAfterIsOk(#Interval inter, #Object@#Creator(inter) obj) 
                requires method suspends this.#Creator
                {
                    this->inter = inter;
                    this->obj = obj;                    
                    this->anotherMethod();
                    return;
                }
                
                void creatingObjectsInBetweenNotOk(#Interval inter, #Object@#Creator(inter) obj) 
                requires method suspends this.#Creator
                {
                    this->inter = inter;
                    obj2 = new #Object@#Creator(inter)(); // ERROR intervals.must.assign.first(this.obj)
                    this->obj = obj; 
                    return;
                }
                
                void creatingObjectsAfterIsOk(#Interval inter, #Object@#Creator(inter) obj) 
                requires method suspends this.#Creator
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
    
    def test_neverLinkedToLockWhichGuardsYou() {
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
    
    def test_constructorTypes() {
        tc(
            """
            class Ctor extends #Object@#Creator(this.Constructor) {
                
                #String c requires this.#Constructor;
                
                Ctor @#Constructor(?) unconstructed requires this.#Constructor;
                Ctor dflt requires this.#Constructor;
                Ctor @#Constructor(hbNow) constructed requires this.#Constructor;
                
                Constructor() 
                {         
                    super();                    
                    this->unconstructed = this;
                    this->dflt = this;
                    this->constructed = this; // ERROR intervals.expected.subtype(this, Ctor, @#Constructor(hbNow) Ctor)
                    return;
                }
                
                void built()
                requires this.#Constructor hb method
                {
                }
                
                // This method is invokable from both within and without the
                // constructor.  It cannot read fields like 'c' because that
                // might permit a data race if the 'this' pointer were shared
                // during the constructor. 
                void noRelationToConstructor() 
                {
                    c = this->c; // ERROR intervals.not.readable(this.#Constructor)
                    
                    this->unconstructed = this; // ERROR intervals.not.writable(this.#Constructor)
                    this->dflt = this; // ERROR intervals.not.writable(this.#Constructor)
                    this->constructed = this; // ERROR intervals.not.writable(this.#Constructor)
                    return;
                }
                
                void writeDuringConstructor() 
                requires method suspends this.Constructor
                {
                    c = this->c; 
                    
                    this->unconstructed = this;
                    this->dflt = this;
                    this->constructed = this; // ERROR intervals.expected.subtype(this, Ctor, @#Constructor(hbNow) Ctor)
                    return;
                }
                
                void readDuringConstructor() 
                requires method suspends this.Constructor
                {
                    // Always constructed:
                    constructed = this->constructed;
                    c1 = constructed->built();                    
                    c2 = constructed->c;
                    
                    // Not known to be constructed because this.Constructor in progress:
                    // (Nor do we know that dflt.Constructor is in progess, so can't read)
                    dflt = this->dflt;
                    d1 = dflt->built(); // ERROR intervals.requirement.not.met(built, requires dflt.#Constructor hb <method-call>)
                    d2 = dflt->c; // ERROR intervals.not.readable(dflt.#Constructor)
                    
                    // Not known to be constructed:
                    unconstructed = this->unconstructed;
                    u1 = unconstructed->built(); // ERROR intervals.requirement.not.met(built, requires unconstructed.#Constructor hb <method-call>)
                    u2 = unconstructed->c; // ERROR intervals.not.readable(unconstructed.#Constructor)
                    
                    return;
                }
                
                void method()
                requires this.Constructor hb method
                {
                    // Always constructed:
                    constructed = this->constructed;
                    c1 = constructed->built();                    
                    c2 = constructed->c;
                    
                    // Known to be constructed because this.Constructor is in progress:
                    dflt = this->dflt;
                    d1 = dflt->built();                    
                    d2 = dflt->c;
                    
                    // Not known to be constructed:
                    unconstructed = this->unconstructed;
                    u1 = unconstructed->built(); // ERROR intervals.requirement.not.met(built, requires unconstructed.#Constructor hb <method-call>)
                    u2 = unconstructed->c; // ERROR intervals.not.readable(this.unconstructed.#Constructor)
                    
                    return;
                }
            }
            """
        )
    }    

    def test_inheritedAssumptionsFromCtor() {
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
                requires method suspends this
                requires this.Constructor hb method
                {
                    // this.final hb this, and method is a inlineInterval of this,
                    // so this.final hb method:
                    o->toString();
                    return;
                }
                
                void readFinal1(#Object@#Creator(this.final) o) 
                requires this.Constructor hb method
                {
                    // Here we do not require this method be
                    // invoked with method a inlineInterval of this,
                    // so we don't know that this.final is readable.
                    o->toString(); // ERROR intervals.requirement.not.met(toString, requires o.#Creator readableBy <method-call>)
                    return;
                }
                
                void readFinal2(#Object@#Creator(this.final) o)
                {
                    // Here the type system resolves o.#Creator to this.final,
                    // but this.final is not an immutable path.  
                    o->toString(); // ERROR intervals.requirement.not.met(toString, requires o.#Creator readableBy <method-call>)
                    return;
                }
                
                void readFinal3(#Object@#Creator(this.final) o)
                requires o.#Creator readableBy method
                {
                    o->toString();
                    return;
                }
            }
            """
        )
    }

    def test_inheritedAssumptionsFromSupertypes() {
        tc(
            """
            class Super extends #Interval 
            {
                #Interval inter requires this.Constructor[Super];
                
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
                #Object @#Creator(this.inter) obj requires this.Constructor[Sub1];
                
                Constructor(#Interval inter)
                {
                    super before(inter);
                    return;
                }
                
                void run() 
                requires method suspends this
                {
                    obj = this->obj;
                    obj->toString();
                    return;
                }
            }
            
            class Sub2 extends Super
            {     
                #Object @#Creator(this.inter) obj requires this.Constructor[Sub2];
                
                Constructor(#Interval inter)
                {
                    super unrelated(inter);
                    return;
                }
                
                void run() 
                requires method suspends this
                {
                    obj = this->obj;
                    obj->toString(); // ERROR intervals.requirement.not.met(toString, requires obj.#Creator readableBy <method-call>)
                    return;
                }
            }           
            """
        )
    }
    
    def test_inheritedAssumptionsFromMultipleCtors() {
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
                requires method suspends this
                {
                    o->toString(); // ERROR intervals.requirement.not.met(toString, requires o.#Creator readableBy <method-call>)
                    return;
                }
                
                void mr(#Object@#Creator(this.maybeRelated) o) 
                requires method suspends this
                {
                    o->toString(); // ERROR intervals.requirement.not.met(toString, requires o.#Creator readableBy <method-call>)
                    return;
                }
                
                void mb(#Object@#Creator(this.before) o) 
                requires method suspends this
                {
                    o->toString(); 
                    return;
                }
            }
            """
        )
    }
    
    def test_inheritedAssumptionsOnlyIncludeTemporarilyAliasedFields() {
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
                
                void emptyMethod()
                {                    
                    return;
                }
                
                void readFinal(#Object @#Creator(this.final) o) 
                requires method suspends this
                {
                    o->toString(); // ERROR intervals.requirement.not.met(toString, requires o.#Creator readableBy <method-call>)
                    return;
                }                
            }
            """
        )
    }    

    def test_overriddenMethodsCannotAddRequirements() {
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
                    super->aHbB(); // ERROR intervals.requirement.not.met(aHbB, requires this.a hb this.b)
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
                    super->aHbB(); // ERROR intervals.requirement.not.met(aHbB, requires this.a hb this.b)
                    return;
                }
            }
            
            class UnsupportedReqs1 extends A 
            {
                void aHbB() // ERROR intervals.override.adds.req(requires method suspends this.#Constructor)
                requires method suspends this.Constructor 
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
                void aHbB() // ERROR intervals.override.adds.req(requires this.b readableBy this.a)
                requires this.b readableBy this.a
                {                    
                    return;
                }
            }
            """
        )
    }
    
    def test_superCtors() {
        wf(
            """
            class Z extends #Object {
                Constructor(#String s) {
                    super(s); // ERROR intervals.wrong.number.method.arguments(0, 1) 
                    return;
                }
            }
            
            class A extends #Object {
                #String s requires this.Constructor;
                
                Constructor(#String s) {
                    super();
                    this->s = s;
                    return;
                }
            }
            
            class B1 extends A {
                Constructor(#String t, #String w) {
                    super(t, w); // ERROR intervals.wrong.number.method.arguments(1, 2)
                    return;
                }
            }            
            """
        )
        
        tc(
            """
            class A extends #Object {
                #String s requires this.Constructor;
                
                Constructor(#String s) {
                    super();
                    this->s = s;
                    return;
                }
            }
            
            class B2 extends A {
                Constructor(void v) {
                    super(v); // ERROR intervals.expected.subtype(v, @#Constructor(hbNow) void, @#Constructor(hbNow) #String)
                    return;
                }                
            }            
            """
        )
    }
    
    def test_superInterval() {
        tc(
            """
            class A extends #Object@#Creator(this.Constructor) {
                #String s requires this.Constructor[A];
                
                Constructor(#String s) {
                    super();
                    return;
                }
            }
            
            class B3 extends A {
                Constructor(#String t) {
                    super(t);
                    
                    this->s = t; // ERROR intervals.not.writable(this.Constructor[A])
                    return;
                }
            }     
                   
            class B4 extends A {
                #String t requires this.Constructor[B4];
                
                Constructor(#String s, #String t) {
                    super(s);
                    this->t = t;
                    return;
                }
                
                #String toString() 
                requires this.#Constructor hb method
                {
                    t = this->t;
                    return t;
                }
            }       
                 
            class B5 extends A {
                #String t requires this.Constructor[B5];
                Constructor(#String t) {
                    super(t);
                    
                    s = this->s; // A is readable
                    this->t = s; 
                    return;
                }
            }     
            
            class C extends #Object@#Creator(this.Constructor) {
                Constructor() { 
                    super();
                    return;
                }
                
                void mthdReadB(B5 @Constructor(?) b) 
                {
                    s = b->t; // ERROR intervals.not.readable(b.Constructor[B5])
                    return;
                }
            }
            """
        )
    }
    
    def test_extendedInit() {
        tc(
            """
            class ExtendedInit@init(#Interval) extends #Object@#Creator(this.init) {
                #String f1 requires this.init;
                #String f2 requires this.init;
                
                Constructor(#String f1) 
                requires method suspends this.init
                {
                    super();
                    this->f1 = f1;
                    return;
                }
                
                void additionalInit(#String f2)
                requires method suspends this.init
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
    
    def test_inlineInterval() {
        tc(
            """
            class Monitor extends #Object@#Creator(this.Constructor) {
                #Lock lock requires this.Constructor;
                
                Constructor() {
                    super();
                    lock = new #Lock();
                    this->lock = lock;
                    return;
                }
            }
            
            class scalarRegister extends Monitor {
                #String value requires this.lock;
                
                Constructor() {
                    super();
                    return;
                }

                #String brokenGet() 
                requires this.Constructor hb method
                {
                    v = this->value; // ERROR intervals.not.readable(this.lock)
                    return v; // ERROR intervals.no.such.variable(v)
                }

                void brokenSet(#String v) 
                requires this.Constructor hb method
                {
                    this->value = v; // ERROR intervals.not.writable(this.lock)
                    return;
                }
                
                #String get() 
                requires this.Constructor hb method
                {
                    inlineInterval x {
                        lock = this->lock;
                        x locks lock;
                    }, {
                        v = this->value;                         
                        break 0(v); // 0 == seq, 1 == subinter
                    } => (#String v1);
                    return v1;
                }
                
                void set(#String v) 
                requires this.Constructor hb method
                {
                    inlineInterval x {
                        lock = this->lock;
                        x locks lock;
                    }, {
                        this->value = v;
                        break 0();
                    }
                    return;
                }
                
                #String toString() 
                requires this.Constructor hb method
                {
                    s = this->get();
                    return s;
                }
                
            }
            """
        )
    }
    
    def test_multipleInheritance() {
        tc(
            """
            interface class IFoo extends #Object {
                Constructor() {
                    super();
                }
                
                #Object m1()
                requires this.#Constructor hb method
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
                requires this.#Constructor hb method
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
                requires this.#Constructor hb method
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
                
                void assign() 
                requires this.#Constructor hb method
                {
                    ifoo1 = this->foo1;
                    ifoo2 = this->foo2;                    
                    return;
                }
                
                void invokeThroughInterface() 
                requires this.#Constructor hb method
                {
                    foo2 = this->foo2;
                    ifoo2 = (IFoo @#Constructor(hbNow) @#Creator(this.#Creator))foo2;
                    ifoo2->m1(); // ERROR intervals.requirement.not.met(m1, requires ifoo2.#Creator readableBy <method-call>)
                    return;
                }
                
                void invokeThroughFoo1() 
                requires this.#Constructor hb method
                {
                    foo1 = this->foo1;
                    foo1->m1(); // ERROR intervals.requirement.not.met(m1, requires foo1.#Creator readableBy <method-call>)
                    return;
                }
                
                void invokeThroughFoo2() 
                requires this.#Constructor hb method
                {
                    foo2 = this->foo2;
                    foo2->m1();
                    return;
                }
            }
            """
        )
    }
    
    def test_ghostTypesLocals()
    {
        tc(
            """
            class Foo@s(#String)@l(#Lock)@i(#Interval) extends #Object {
                Constructor() 
                {
                    super();
                    return;
                }                
            }
            
            class Bar extends #Object {
                Constructor() 
                {
                    super();
                    return;
                }
                
                void run() 
                {
                    s = (#String)null;
                    l = (#Lock)null;
                    i = (#Interval)null;
                                        
                    f1 = new Foo@s(s)@l(l)@i(i)@#Creator(i)();
                    f2 = new Foo@s(l)@l(l)@i(i)@#Creator(i)(); // ERROR intervals.must.be.subclass(l, #String)
                    f3 = new Foo@s(s)@l(s)@i(i)@#Creator(i)(); // ERROR intervals.must.be.subclass(s, #Lock)
                    f4 = new Foo@s(s)@l(l)@i(s)@#Creator(i)(); // ERROR intervals.must.be.subclass(s, #Interval)
                    f5 = new Foo@s(s)@l(l)@i(i)@#Creator(s)(); // ERROR intervals.must.be.subclass(s, #Guard)
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
    
    def test_illegalLinkedFields()
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
                Data2@#Creator(this.data1.i) data2; // ERROR intervals.illegal.type.dep(this.data1.i, this.#Creator)
            }
                        
            class Indirect2 extends #Object {
                // It doesn't matter that data1 is only created by the
                // ctor, because we don't know that data2 is only modified after ctor completes!
                // See TODO for more thoughts.
                Data1@#Creator(this.Constructor) data1 requires this.Constructor;
                Data2@#Creator(this.data1.i) data2; // ERROR intervals.illegal.type.dep(this.data1.i, this.#Creator)
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
    
    def test_illegalClassInReq
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
                requires m1 hb m2 // ERROR intervals.expected.subclass.of.any(m1, #Point, #Interval)
                requires m1.i hb m2 // ERROR intervals.expected.subclass.of.any(m2, #Point, #Interval)
                {
                    
                }
            }
            """
        )    
    }
    
    def test_mutableHbRequirements
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
    
    def test_fieldsTypesWf()
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
    
    def test_superTypePreservesGhosts()
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
                    this->creatorA = obj; // ERROR intervals.expected.subtype(obj, @#Creator(this.d) @l1(this.b) @l2(this.c) C2, @#Constructor(hbNow this.#Constructor) @#Creator(this.a) #Object)
                    this->l1B = obj; 
                    this->creatorAl1B = obj; // ERROR intervals.expected.subtype(obj, @#Creator(this.d) @l1(this.b) @l2(this.c) C2, @#Constructor(hbNow this.#Constructor) @#Creator(this.a) @l1(this.b) C1)
                } 
                
                void l1Wrong() 
                requires this.#Creator writableBy method
                {
                    obj = new C2@#Creator(this.a)@l1(this.c)@l2(this.b)();
                    this->creatorA = obj;
                    this->l1B = obj; // ERROR intervals.expected.subtype(obj, @#Creator(this.a) @l1(this.c) @l2(this.b) C2, @#Constructor(hbNow this.#Constructor) @l1(this.b) C1)
                    this->creatorAl1B = obj; // ERROR intervals.expected.subtype(obj, @#Creator(this.a) @l1(this.c) @l2(this.b) C2, @#Constructor(hbNow this.#Constructor) @#Creator(this.a) @l1(this.b) C1)
                }                                         
            }            
            """
        )
    }

    // Checks the basic rules for interface inheritance.  We should
    // add more complete tests, but since these are enforced by javac
    // anyway they are hardly high priority.
    def test_interfaceInheritance() {
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

    def test_hoh() {
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
                    
                    inlineInterval x {
                        x locks lock;
                    }, {
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
                requires method suspends this
                {
                    // Update data:
                    link = this->link;
                    oldData = link->data;
                    newData = this->transform(oldData);
                    link->data = newData;
                    
                    // Start next link:
                    nextLink = link->nextLink;
                    nextInter = new HohLink @#Parent(this.#Parent) (nextLink);
                    nextInterStart = nextInter->start;
                    end = this->end;
                    nextInterStart hb end;
                    
                    return;
                }                
            }
            """
        )
    }

    def test_blockBranchCheckIndices() {
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
    
    def test_shadowGhostsInSuperType() {
        wf(
            """
            class Super
                @i(#Interval) 
            extends #Object {
            }
            
            class Sub
                @i(#Interval) // ERROR intervals.shadowed(Super, i)
            extends Super {
            }
            """            
        )
    }
    
    def test_duplicateGhostsInSameType() {
        wf(
            """
            class Sub
                @i(#Interval) // ERROR intervals.shadowed(Sub, i)
                @i(#Interval) // ERROR intervals.shadowed(Sub, i)
            extends #Object {
            }
            """            
        )
    }

    def test_bbpcData() {
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
                requires method suspends this
                {
                    pdata = this->pdata;
                    cdata = this->cdata;
                    
                    data = new Data @#Creator(this) (); // "produce"
                    pdata->data = data;
                    
                    // Note: Non-trivial deduction here that equates
                    // nextCons with cdata.nextCons!
                    nextCons = cdata->nextCons;
                    nextCdata = cdata->nextCdata;                    
                    
                    nextProd = new Producer @#Parent(this.#Parent) (nextCons, nextCdata);
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
                requires method suspends this
                {
                    pdata = this->pdata;
                    cdata = this->cdata;
                    
                    data = pdata->data; // "consume" 
                    
                    nextProd = pdata->nextProd;
                    nextPdata = pdata->nextPdata;
                    
                    nextCons = new Consumer @#Parent(this.#Parent) (nextProd, nextPdata);
                    cdata->nextCons = nextCons;
                    nextCdata = nextCons->cdata;
                    cdata->nextCdata = nextCdata;
                    return;
                }
            }
            
            class BBPC extends #Interval {
                void run()
                requires method suspends this
                {
                    d0 = new ConsData@#Creator(this)();
                    d1 = new ConsData@#Creator(this)();
                    d0->nextCons = this;
                    d0->nextCdata = d1;
                    
                    p = new Producer @#Parent(this) (this, d0);                    
                    pdata = p->pdata;
                    
                    c = new Consumer @#Parent(this) (p, pdata);
                    d1->nextCons = c; 
                    cdata = c->cdata;
                    d1->nextCdata = cdata;                    
                    return;
                }
            }
            
            """
        )
    }  

    def test_getFromGenericList() {
        tc(
            """
            class List 
                <E <: #Object>
            extends #Object 
            {
                this:E get(scalar idx)
                requires this.Constructor hb method
                requires this.#Creator readableBy method
                {
                    return;
                }
            }
            
            class Data
            extends #Object
            {
                #String read()
                requires this.#Creator readableBy method
                {
                    return;
                }                      
                
                void write()
                requires this.#Creator writableBy method
                {
                    return;
                }                      
            }
            
            class UseList 
            extends #Object
            {
                List @#Creator(this.#Creator) <E: Data @#Creator(this.#Creator)> 
                list requires this.Constructor;
                
                #String readFromIndexWithReadPermission(scalar idx) 
                requires this.#Creator readableBy method
                requires this.Constructor hb method
                {
                    list = this->list;
                    item = list->get(idx);
                    str = item->read();
                    return str;
                }
                
                #String readFromIndexWithWritePermission(scalar idx) 
                requires this.#Creator writableBy method
                requires this.Constructor hb method
                {
                    list = this->list;
                    item = list->get(idx);
                    str = item->read();
                    return str;
                }
                
                void writeToIndexWithReadPermission(scalar idx) 
                requires this.#Creator readableBy method
                requires this.Constructor hb method
                {
                    list = this->list;
                    item = list->get(idx);
                    item->write(); // ERROR intervals.requirement.not.met(write, requires item.#Creator writableBy <method-call>)
                    return;
                }
                
                void writeToIndexWithWritePermission(scalar idx) 
                requires this.#Creator writableBy method
                requires this.Constructor hb method
                {
                    list = this->list;
                    item = list->get(idx);
                    item->write();
                    return;
                }
            }
            """
        )
    }  
    
    def test_addToGenericList() {
        tc(
            """
            class List 
                <E <: #Object>
            extends #Object 
            {
                void add(this:E e)
                requires this.Constructor hb method
                requires this.#Creator writableBy method
                {
                    return;
                }
            }
            
            class Data
            extends #Object
            {
                #String read()
                requires this.#Creator readableBy method
                {
                    return;
                }                      
                
                void write()
                requires this.#Creator writableBy method
                {
                    return;
                }                      
            }
            
            class ExtData
            extends Data
            {
            }
            
            class UseList 
            extends #Object
            {
                List @#Creator(this.#Creator) <E: Data @#Creator(this.#Creator)> 
                dataList requires this.Constructor;
                
                List @#Creator(this.#Creator) <E: ExtData @#Creator(this.#Creator)> 
                extDataList requires this.Constructor;
                
                void addData(
                    Data @#Creator(this.#Creator) extData
                )
                requires this.#Creator writableBy method
                requires this.Constructor hb method
                {
                    dataList = this->dataList;
                    dataList->add(extData);
                    
                    extDataList = this->extDataList;
                    extDataList->add(extData); // ERROR intervals.expected.subtype(extData, @#Constructor(hbNow) @#Creator(this.#Creator) Data, extDataList:E)
                    return;
                }
                
                void addExtData(
                    ExtData @#Creator(this.#Creator) extData
                )
                requires this.#Creator writableBy method
                requires this.Constructor hb method
                {
                    dataList = this->dataList;
                    dataList->add(extData);
                    
                    extDataList = this->extDataList;
                    extDataList->add(extData);
                    return;
                }
                
                void addExtDataWithUnspecifiedCreator(
                    ExtData extData
                )
                requires this.#Creator writableBy method
                requires this.Constructor hb method
                {
                    dataList = this->dataList;
                    dataList->add(extData); // ERROR intervals.expected.subtype(extData, @#Constructor(hbNow) ExtData, dataList:E)
                    
                    extDataList = this->extDataList;
                    extDataList->add(extData); // ERROR intervals.expected.subtype(extData, @#Constructor(hbNow) ExtData, extDataList:E)
                    return;
                }
            }
           """
        )
    }

    def test_readPhaseWithSubintervals() {
        tc(
            """
            class Data
            extends #Object
            {
                scalar field requires this.#Creator;
            }
            
            class TestInterval extends #Interval
            {
                Data @#Creator(readableBy this)
                inData requires this.#Constructor;
                
                Data @#Creator(this) 
                outData requires this.#Constructor;
                
                Constructor(
                    Data @#Creator(readableBy this) inData
                ) {
                    this->inData = inData;
                    outData = new Data @#Creator(this) ();
                    this->outData = outData;
                    return;                    
                }
                
                void run()
                requires method suspends this
                requires this.#Constructor hb method 
                {
                    inData = this->inData;
                    outData = this->outData;

                    field = inData->field;
                    outData->field = field;
                    return;
                }
            }
            
            class TestClass
            extends #Object
            {
                void correctMethod(Data @#Creator(writableBy method) data)
                {
                    // Create two asynchronous subintervals which will
                    // read from data and write to outData[12]:
                    inlineInterval sub {}, {
                        inter1 = new TestInterval @#Parent(sub) (data);
                        inter2 = new TestInterval @#Parent(sub) (data);                        
                        break 0(inter1, inter2);
                    } => (
                        TestInterval @#Parent(sub) inter1, 
                        TestInterval @#Parent(sub) inter2
                    );

                    // "Combine" the results of the two subintervals:
                    outData1 = inter1->outData;
                    result1 = outData1->field;
                    data->field = result1;
                    outData2 = inter2->outData;                    
                    result2 = outData2->field;
                    data->field = result2;
                    return;
                }
                
                void unreadableData(
                    #Interval parent,
                    Data @#Creator(writableBy method) data)
                {
                    // Create two asynchronous subintervals which will
                    // read from data and write to outData[12]:
                    inlineInterval sub {}, {
                        inter1 = new TestInterval @#Parent(parent) (data); // ERROR intervals.expected.subtype(data, @(ch.ethz.intervals.quals.Constructor)(hbNow) @(ch.ethz.intervals.quals.Creator)(writableBy method) Data, @(ch.ethz.intervals.quals.Constructor)(hbNow) @(ch.ethz.intervals.quals.Creator)(readableBy inter1) Data)
                        inter2 = new TestInterval @#Parent(parent) (data); // ERROR intervals.expected.subtype(data, @(ch.ethz.intervals.quals.Constructor)(hbNow) @(ch.ethz.intervals.quals.Creator)(writableBy method) Data, @(ch.ethz.intervals.quals.Constructor)(hbNow) @(ch.ethz.intervals.quals.Creator)(readableBy inter2) Data)                        
                        break 0();
                    }
                    return;
                }
                
                void unwritableData(
                    #Interval parent,
                    Data @#Creator(writableBy parent) data)
                {
                    // Create two asynchronous subintervals which will
                    // read from data and write to outData[12]:
                    inlineInterval sub {}, {
                        inter1 = new TestInterval @#Parent(parent) (data);
                        inter2 = new TestInterval @#Parent(parent) (data);
                        break 0(inter1, inter2);
                    } => (
                        TestInterval @#Parent(parent) inter1, 
                        TestInterval @#Parent(parent) inter2
                    );

                    // "Combine" the results of the two subintervals:
                    outData1 = inter1->outData; // Note: we can read this, it's a final field
                    result1 = outData1->field; // ERROR intervals.not.readable(inter1)
                    outData2 = inter2->outData;                    
                    result2 = outData2->field; // ERROR intervals.not.readable(inter2)
                    return;
                }
            }
            """
        )
    }

    def test_reproducingThread() {
        success(
            """
            class Data
            extends #Object
            {
                scalar field requires this.#Creator;
            }
            
            class TestInterval extends #Interval
            {
                Data @#Creator(readableBy this.#Parent)
                inData requires this.#Constructor;
                
                Constructor(
                    Data @#Creator(readableBy this.#Parent) inData
                ) {
                    this->inData = inData;
                    return;                    
                }
                
                void run()
                requires method suspends this
                requires this.#Constructor hb method 
                {
                    // We can read inData since our parent can:
                    inData = this->inData;
                    field = inData->field;
                    
                    // Create a sibling interval and demonstrate that
                    // we can hand-off inData to it:
                    sibling = new TestInterval @#Parent(this.#Parent) (inData);                    
                    return;
                }
            }
            """
        )
    }
    
    def test_simpleIsAnnots() {
        tc(
            """
            class TestInterval extends #Interval
            {
                @Is(this.#Parent)
                #Interval parent requires this.#Constructor;
                
                Constructor ok(
                    @Is(this.#Parent) #Interval parent
                ) {
                    this->parent = parent;
                    return;                    
                }
                
                Constructor bad(
                    #Interval parent
                ) {
                    this->parent = parent; // ERROR intervals.must.match(parent, this.(ch.ethz.intervals.Parent))
                    return;                    
                }
                
                @Is(this.#Parent) #Interval parent()
                requires this.#Constructor hb method
                {
                    parent = this->parent;
                    return parent;
                }
                
                @Is(this.#Parent) #Interval bogusParent()
                requires this.#Constructor hb method
                {
                    return this; // ERROR intervals.must.match(this, this.#Parent)
                }
                
                void makeWithThisAsParent()
                requires this.#Constructor hb method
                {
                    good = new TestInterval @#Parent(this) ok(this);
                    bad = new TestInterval @#Parent(this.#Parent) ok(this); // ERROR intervals.must.match(this, bad.#Parent)
                }
                
                void makeSiblingWithFieldLoad()
                requires this.#Constructor hb method
                {
                    parent = this->parent;
                    good = new TestInterval @#Parent(this.#Parent) ok(parent);
                    bad = new TestInterval @#Parent(this) ok(parent);  // ERROR intervals.must.match(this.parent, bad.#Parent)
                }
                
                void makeSiblingWithMethodReturn()
                requires this.#Constructor hb method
                {
                    parent = this->parent();
                    good = new TestInterval @#Parent(this.#Parent) ok(parent);
                    bad = new TestInterval @#Parent(this) ok(parent);  // ERROR intervals.must.match(parent, bad.#Parent)
                }
                
            }
            """
        )
    }
    
    def test_methodHbReturn() {
        tc(
            """
            class Test extends #Object
            {
                #Object someMethod()
                {
                    obj = new #Object @#Creator(method) @#Constructor(method) ();
                    return obj;
                }
                
                #Object someMethod(#Interval x)
                {
                    obj = new #Object @#Creator(method) @#Constructor(x) (); // ERROR intervals.ctor.must.encompass.current(x, *)
                    return obj; // ERROR intervals.no.such.variable(obj)
                }
            }
            """
        )
    }    
}