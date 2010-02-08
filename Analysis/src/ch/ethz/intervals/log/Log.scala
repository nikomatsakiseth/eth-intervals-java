package ch.ethz.intervals.log

import Util._
import javax.lang.model.element.Element
import com.sun.source.tree.Tree

abstract class Log {
    // ___ Abstract methods defined by subclass _____________________________
    
    def uri: String
    def ifEnabled(f: => Unit): Unit     // Executes f iff rawWrite() does something.
    def rawStart(html: String): String  // Starts a log message with body 'html'.  Returns the id of this element.
    def rawClose(): Unit                // Finishes a log message.
    def rawLinkTo(uri: String, html: String): Unit // Starts a log message with a link to 'uri'
    def escape(s: String): String       // Escapes into HTML.
    
    // ___ Sublogs __________________________________________________________
    
    def log(name: String): Log            // Creates a new log, inserting a link into this one.
    def splitLog(name: String): SplitLog  // Creates a new split log, inserting a link into this one.

    // ___ Raw methods to add to the log ____________________________________
    
    private def escape(fmt: String, args: List[Any]): String = {
        def toLogString(o: Any) = o match {
            case null => escape("null")
            case elem: Element => escape("%s[%s]".format(elem.getKind, qualName(elem)))
            case tree: Tree => escape("%s[%s]".format(tree.getKind, prefix(tree.toString)))
            case _ => escape(o.toString)
        }

        val strs = args.map(toLogString).toArray[String]
        escape(fmt).format(strs: _*)
    }
    
    def rawWrite(html: String): String = {
        val id = rawStart(html)
        rawClose
        id
    }
    
    // ___ Indentation ______________________________________________________
    def indented[R](fmt: String, arg0: Any, args: Any*)(f: => R): R = {
        try {
            ifEnabled { rawStart(escape(fmt, (arg0 :: args.toList))) }
            f 
        } catch {
            case t: Throwable => 
                apply("Error = %s".format(t))
                throw t
        } finally { 
            rawClose
        }
    }
    
    def indented[R](str: Any)(f: => R): R = indented("%s", str)(f)
    
    def indentedRes[R](fmt: String, arg0: Any, args: Any*)(f: => R): R = {
        try {
            ifEnabled { rawStart(escape(fmt, (arg0 :: args.toList))) }
            val result = f
            apply("Result = %s", result)
            result
        } catch {
            case t: Throwable => 
                apply("Error = %s".format(t))
                throw t
        } finally {
            rawClose
        }
    }
    
    def indentedRes[R](fmt: Any)(f: => R): R = indentedRes("%s", fmt)(f)
    
    // ___ Methods to add to the log ________________________________________
    
    def apply(v: Any): Unit = ifEnabled {
        v match {
            case e: ir.TcEnv => env("Env", e)
            case m: Map[_, _] => map("Map", m)
            case r: Relation[_, _] => map("Relation", r)
            case cd: ir.ClassDecl => classDecl("", cd)
            case md: ir.MethodDecl => methodDecl("", md)
            case _ => rawWrite(escape(v.toString))
        }
    }
    
    def apply(fmt: String, arg0: Any, args: Any*): Unit = ifEnabled {
        rawWrite(escape(fmt, (arg0 :: args.toList)))
    }
    
    def linkTo(uri: String, fmt: String): Unit = ifEnabled {
        rawLinkTo(uri, escape(fmt))
    }
    
    def linkTo(uri: String, fmt: String, arg0: Any, args: Any*): Unit = ifEnabled {
        rawLinkTo(uri, escape(fmt, (arg0 :: args.toList)))
    }
    
    def env(lbl: Any, env: ir.TcEnv): Unit = ifEnabled {
        indented("%s", lbl) {
            apply("ps_cur: %s", env.ps_cur)
            apply("wt_ret: %s", env.wt_ret)
            map("perm:", env.perm)
            flow("flow:", env.flow)
        }
    }
    
    def flow(lbl: Any, flow: FlowEnv): Unit = ifEnabled {
        indented("%s", lbl) {
            map("temp:", flow.temp)
            apply("invalidated: %s", flow.ps_invalidated)
            rel("readable:", "readable by", flow.readable)
            rel("writable:", "writable by", flow.writable)
            rel("hb:", "hb", flow.hb)
            rel("subinterval:", "subinterval of", flow.subinterval)
            rel("locks:", "locks", flow.locks)                    
        }
    }
    
    def map(lbl: Any, m: Iterable[(Any, Any)]): Unit = ifEnabled {
        indented("%s", lbl) {
            for((k, v) <- m)
                apply("%s: %s", k, v)
        }
    }
    
    def rel(lbl: Any, n: String, r: Relation[_, _]): Unit = ifEnabled {
        indented("%s", lbl) {
            for((k, v) <- r.elements)
                apply("%s %s %s", k, n, v)
        }
    }
    
    def classDecl(lbl: Any, cd: ir.ClassDecl): Unit = ifEnabled {
        indented("%s%s", lbl, cd) {
            cd.rfds.foreach(apply)
            cd.ctors.foreach(methodDecl("[Ctor] ", _))
            cd.methods.foreach(methodDecl("[Mthd] ", _))
        }
    }
            
    def methodDecl(lbl: Any, md: ir.MethodDecl): Unit = ifEnabled {
        indented("%s%s", lbl, md) {
            statementSeq("", md.body)
        }
    }
    
    def statementSeq(lbl: Any, seq: ir.StmtSeq): Unit = ifEnabled {
        indented("%s%s", lbl, seq) {
            seq.stmts.foreach(statement(lbl, _))
        }
    }
    
    def statement(lbl: Any, stmt: ir.Stmt): Unit = ifEnabled {
        stmt match {
            case ir.StmtCompound(ir.Loop(args, ps_initial, seq), defines) =>
                indented("%s%s", lbl, "Loop") {
                    indented("Loop Arguments") {
                        args.zip(ps_initial).foreach { case (lvd, p) =>
                            apply("%s = %s", lvd, p)
                        }
                    }
                    statementSeq("Body: ", seq)
                    if(!defines.isEmpty) {
                        indented("Defines") {
                            defines.foreach(apply)
                        }
                    }
                }
                
            case ir.StmtCompound(kind, defines) =>
                indented("%s%s", lbl, kind) {
                    kind.subseqs.foreach(statementSeq("", _))
                    if(!defines.isEmpty) {
                        indented("Defines") {
                            defines.foreach(apply)
                        }
                    }
                }
                
            case _ => 
                apply("%s%s", lbl, stmt)
        }
    }
            
}
