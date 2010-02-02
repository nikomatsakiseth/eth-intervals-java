package ch.ethz.intervals.log

import Util._
import javax.lang.model.element.Element
import com.sun.source.tree.Tree
import java.io.File

abstract class Log {
    // ___ Abstract methods defined by subclass _____________________________
    
    def uri: String
    def ifEnabled(f: => Unit): Unit     // Executes f iff rawWrite() does something.
    def rawStart(html: String): String  // Starts a log message with body 'html'.  Returns the id of this element.
    def rawClose(): Unit                // Finishes a log message.
    def escape(s: String): String       // Escapes into HTML.
    
    // ___ Sublogs __________________________________________________________
    
    def log(name: String): Log            // Creates a new log, inserting a link into this one.
    def splitLog(name: String): SplitLog  // Creates a new split log, inserting a link into this one.

    // ___ Raw methods to add to the log ____________________________________
    
    def rawWrite(html: String): String = {
        val id = rawStart(html)
        rawClose
        id
    }
    
    def rawStart(fmt: String, args: List[Any]): String = {
        def toLogString(o: Any) = o match {
            case null => escape("null")
            case elem: Element => escape("%s[%s]".format(elem.getKind, qualName(elem)))
            case tree: Tree => escape("%s[%s]".format(tree.getKind, prefix(tree.toString)))
            case _ => escape(o.toString)
        }

        val strs = args.map(toLogString).toArray[String]
        rawStart(escape(fmt).format(strs: _*))
    }    
    
    def rawWrite(fmt: String, args: List[Any]): String = {
        val id = rawStart(fmt, args)
        rawClose
        id
    }
    
    // ___ Indentation ______________________________________________________
    def indented[R](fmt: String, arg0: Any, args: Any*)(f: => R): R = {
        try {
            ifEnabled { rawStart(fmt, (arg0 :: args.toList)) }
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
            ifEnabled { rawStart(fmt, (arg0 :: args.toList)) }
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
        rawWrite(fmt, (arg0 :: args.toList))
    }
    
    def env(lbl: Any, env: ir.TcEnv): Unit = ifEnabled {
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
            cd.ctors.foreach(methodDecl("", _))
            cd.methods.foreach(methodDecl("", _))
        }
    }
            
    def methodDecl(lbl: Any, md: ir.MethodDecl): Unit = ifEnabled {
        indented("%s%s", lbl, md) {
            md.blocks.indices.foreach(b =>
                    block("%s: ".format(b), md.blocks(b)))
        }
    }
            
    def block(lbl: Any, blk: ir.Block): Unit = ifEnabled {
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
        
        val backgroundColors = List(
            "CCCCCC", // Gray
            "CC99CC", // Purple
            "BBBBBB", // Gray
            "CC9966", // Tan
            "AAAAAA", // Gray
            "88AAFF", // Blue
            "99FFCC", // Aquamarine
            "CC9999"  // Pink
        )
        var currentColor = -1
        
        def nextColor = {
            currentColor = (currentColor + 1) % backgroundColors.length
            backgroundColors(currentColor)
        }
        
        val outWriter = {
            val pw = new java.io.PrintWriter(outFile)
            
            pw.print("""
            <HTML>
            <HEAD>
                <TITLE>Debug log</TITLE>
                <SCRIPT type='text/javascript'>
                    function toggleId(id)
                    {
                        var target = document.getElementById(id);
                        var kids = target.childNodes;
                        var openedKids = false;
                        var closedKids = false;
                        for(var i = 0; (i < kids.length); i++) {
                            var kid = kids[i];
                            if(kid.className == 'log') {
                                if(kid.style.display == 'none') {
                                    kid.style.display = 'block';
                                    openedKids = true;
                                } else {
                                    kid.style.display = 'none';
                                    closedKids = true;
                                }
                            }
                        }
                        
                        if(openedKids) {
                            target.style.opacity = 1.0;
                        } else if (closedKids) {
                            target.style.opacity = 0.25;                            
                        }
                    }
                </SCRIPT>
                <STYLE>
                    DIV.log {
                        border-width: thin;
                        border-style: solid;
                        margin-top: .1cm;
                        margin-bottom: .1cm;
                        margin-left: .3cm;
                    }
                    A:hover {
                        text-decoration: underline;
                    }
                    A:link {
                        text-decoration: none;
                    }
                    A:visited {
                        text-decoration: none;
                    }
                </STYLE>
            </HEAD>
            <BODY>
            <DIV id='id0'>            
            """)
            
            pw
        }
        
        private var ids = 0
        private var idStack = List("id0")
                
        private def pushId() {
            ids = ids + 1
            val id = "id" + ids
            idStack = id :: idStack            
        }
        
        private def popId() {
            idStack = idStack.tail
        }
        
        override def rawStart(msg: String) = {
            pushId()            
            val parentId = idStack.tail.head
            val id = idStack.head
            
            outWriter.print(
                (
                    "<DIV id='%s' class='log' style='background-color: #%s'>"+
                    "<A href='#%s'>&#8689;</A>&nbsp;"+
                    //"<A href='#%s' class='collapse' onclick='toggleId(\"%s\")'>%s</A>"
                    "<SPAN class='msg' onclick='toggleId(\"%s\")'>%s</SPAN>"
                ).format(
                    id, nextColor, 
                    parentId,
                    id, msg
                ))            
             
            detailsLog.foreach { l =>
                val linkId = l.rawWrite("<I>%s</I>".format(msg))
                outWriter.println("<A href='%s#%s' target='details'>&rarr;</A>".format(l.uri, linkId))
            }
            
            outWriter.flush
            
            id
        }
        
        def rawClose {
            popId()
            outWriter.println("</DIV>")
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
        def rawStart(html: String) = ""
        def rawClose() { }
        def escape(s: String) = s
        def ifEnabled(f: => Unit): Unit = ()
        def log(name: String) = this
        def splitLog(name: String) = new SplitLog(this, this)
    }
    
}