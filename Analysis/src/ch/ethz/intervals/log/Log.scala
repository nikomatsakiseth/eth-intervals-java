package ch.ethz.intervals.log

import Util._
import javax.lang.model.element.Element
import com.sun.source.tree.Tree
import java.io.File

abstract class Log {
    // ___ Abstract methods defined by subclass _____________________________
    
    def uri: String
    def ifEnabled(f: => Unit): Unit   // Executes f iff rawWrite() does something.
    def rawWrite(html: String): Unit  // Prints HTML to the log (if enabled).
    def rawIndent(): Unit             // Indents later messages.
    def rawUndent(): Unit             // Undents later messages.
    def escape(s: String): String     // Escapes into HTML.
    
    // ___ Sublogs __________________________________________________________
    
    def log(name: String): Log            // Creates a new log, inserting a link into this one.
    def splitLog(name: String): SplitLog  // Creates a new split log, inserting a link into this one.

    // ___ Indentation ______________________________________________________
    def indented[R](fmt: String, arg0: Any, args: Any*)(f: => R): R = {
        try {
            apply(fmt, arg0, args: _*)
            rawIndent
            f 
        } catch {
            case t: Throwable => 
                apply("Error = %s".format(t))
                throw t
        } finally { 
            rawUndent
        }
    }
    
    def indented[R](str: Any)(f: => R): R = indented("%s", str)(f)
    
    def indentedRes[R](fmt: String, arg0: Any, args: Any*)(f: => R): R = {
        try {
            apply(fmt, arg0, args: _*)
            rawIndent
            val result = f
            apply("Result = %s", result)
            result
        } catch {
            case t: Throwable => 
                apply("Error = %s".format(t))
                throw t
        } finally {
            rawUndent
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
        def toLogString(o: Any) = o match {
            case null => escape("null")
            case elem: Element => escape("%s[%s]".format(elem.getKind, qualName(elem)))
            case tree: Tree => escape("%s[%s]".format(tree.getKind, prefix(tree.toString)))
            case _ => escape(o.toString)
        }

        val strs = (arg0 :: args.toList).map(toLogString).toArray[String]
        rawWrite(escape(fmt).format(strs: _*))
    }
    
    def env(lbl: Any, env: ir.TcEnv) = ifEnabled {
        indented("%s", lbl) {
            apply("ps_cur: %s", env.ps_cur)
            apply("wt_ret: %s", env.wt_ret)
            map("perm:", env.perm)
            map("temp:", env.temp)
            apply("invalidated: %s", env.ps_invalidated)
            rel("readable:", "readable by", env.readable)
            rel("writable:", "writable by", env.writable)
            rel("hb:", "hb", env.hb)
            rel("subinterval:", "subinterval of", env.subinterval)
            rel("locks:", "locks", env.locks)                    
        }
    }
    
    def map(lbl: Any, m: Iterable[(Any, Any)]) = ifEnabled {
        indented("%s", lbl) {
            for((k, v) <- m)
                apply("%s: %s", k, v)
        }
    }
    
    def rel(lbl: Any, n: String, r: Relation[_, _]) = ifEnabled {
        indented("%s", lbl) {
            for((k, v) <- r.elements)
                apply("%s %s %s", k, n, v)
        }
    }
    
    def classDecl(lbl: Any, cd: ir.ClassDecl) = ifEnabled {
        indented("%s%s", lbl, cd) {
            cd.rfds.foreach(apply)
            cd.ctors.foreach(methodDecl("", _))
            cd.methods.foreach(methodDecl("", _))
        }
    }
            
    def methodDecl(lbl: Any, md: ir.MethodDecl) = ifEnabled {
        indented("%s%s", lbl, md) {
            md.blocks.indices.foreach(b =>
                    block("%s: ".format(b), md.blocks(b)))
        }
    }
            
    def block(lbl: Any, blk: ir.Block) = ifEnabled {
        indented("%s%s", lbl, blk) {
            blk.stmts.foreach(apply)
            blk.gotos.foreach(apply)
        }
    }
    
}

object Log {
    
    class HtmlLog(
        logDirectory: LogDirectory, 
        val outFile: File,
        detailsLog: Option[Log]
    ) extends Log {
        def uri = outFile.toURI.toString
        
        val outWriter = {
            val pw = new java.io.PrintWriter(outFile)
            
            pw.print("""
            <HTML>
            <HEAD>
                <TITLE>Debug log<TITLE>
                <SCRIPT LANGUAGE='JavaScript' SRC='http://smallcultfollowing.com/mktree/mktree.js'></SCRIPT>
                <LINK REL="stylesheet" HREF='http://smallcultfollowing.com/mktree/mktree-debug.css'>
            </HEAD>
            <BODY>
            <a href='#' class='button' onClick='expandTree("tree1");return false;'>Expand All Nodes</a><br>
            <a class='button' href='#' onClick='collapseTree("tree1"); return false;'>Collapse All Nodes</a><br>
            <UL class='mktree' id='tree1'>
            """)
            
            // For the future, if we tag an <LI ID="foo">, we can use:
            // <A class='button' href='#' onClick='expandToItem("tree1","login"); return false;'>Expand to a certain location</A><br>
            pw
        }
        
        private var links = 0
        override def rawWrite(msg: String) {            
            detailsLog match {
                case None =>
                    outWriter.print("<li> ")
                    outWriter.println(msg)
                    
                case Some(l) =>
                    outWriter.print(
                        "<li> <a href='%s#l%d' target='details'>%s</a>".format(
                            l.uri, links, msg))
                    l.rawWrite(
                        "<a name='l%d'> <i>%s</i>".format(
                            links, msg))
                    links = links + 1
            }
            outWriter.flush
        }
        
        def rawIndent {
            outWriter.println("<ul>")
        }
        
        def rawUndent {
            outWriter.println("</ul>")
        }
        
        def escape(s0: String) = {
            var s = s0
            s = s.replace("&", "&amp;")
            s = s.replace("<", "&lt;")
            s
        }

        def ifEnabled(f: => Unit): Unit = f
        
        def log(name: String) = {
            val f = logDirectory.newFile("html")
            rawWrite("<a href='%s' target='_top'>%s</a>".format(f.getName, escape(name)))
            new HtmlLog(logDirectory, f, None)
        }
        
        def splitLog(name: String) = {
            val f = logDirectory.newFile("html")
            val lf = logDirectory.newFile("html")
            val rf = logDirectory.newFile("html")
            
            val pw = new java.io.PrintWriter(f)            
            pw.write("""
            <html>
                <head><title>%s</title></head>
                <body>
                    <frameset cols='30%%,*'>
                        <frame src='%s' name='index'/>
                        <frame src='%s' name='details'/>
                    </frameset>
                </body>
            </html>
            """.format(escape(name), lf.getName, rf.getName))
            pw.close
            
            rawWrite("<a href='%s' target='_top'>%s</a>".format(f.getName, escape(name)))
            
            val detailsLog = new HtmlLog(logDirectory, rf, None)
            val indexLog = new HtmlLog(logDirectory, lf, Some(detailsLog))
            new SplitLog(indexLog, detailsLog)
        }
    }
    
    object DevNullLog extends Log {
        def uri = ""
        def rawWrite(msg: String) { }
        def rawIndent { }
        def rawUndent { }
        def escape(s: String) = s
        def ifEnabled(f: => Unit): Unit = ()
        def log(name: String) = this
        def splitLog(name: String) = new SplitLog(this, this)
    }
    
}