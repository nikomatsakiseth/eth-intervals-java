package ch.ethz.intervals.log

import ch.ethz.intervals.TcEnv
import ch.ethz.intervals.FlowEnv
import ch.ethz.intervals.PathRelation
import ch.ethz.intervals.ir
import ch.ethz.intervals.Util._
import javax.lang.model.element.Element
import com.sun.source.tree.Tree

abstract class Log {
    // ___ Abstract methods defined by subclass _____________________________
    
    def uri: String
    def ifEnabled(f: => Unit): Unit                     // Executes f iff rawWrite() does something.
    def rawStart(open: Boolean, html: String): String   // Starts an initially open log message with body 'html'.  Returns the id of this element.
    def rawClose(): Unit                                // Finishes a log message.
    def rawLinkTo(uri: String, html: String): Unit      // Starts a log message with a link to 'uri'
    def escape(s: String): String                       // Escapes into HTML.
    
    // ___ Sublogs __________________________________________________________
    
    def inlineLog: Log                    // Creates a new log, inserting no links.
    def splitLog(name: String): SplitLog  // Creates a new split log, inserting a link into this one.

    // ___ Raw methods to add to the log ____________________________________
    
    private def escape(fmt: String, args: List[Any]): String = {
        def toLogString(o: Any) = o match {
            case null => escape("null")
            case () => escape("unit")
            case elem: Element => escape("%s[%s]".format(elem.getKind, qualName(elem)))
            case tree: Tree => escape("%s[%s]".format(tree.getKind, prefix(tree.toString)))
            case _ if hasDetailedRepr(o) => inline(_.detailed(o))(o)
            case _ => escape(o.toString)
        }

        val strs = args.map(toLogString).toArray[String]
        try {
            escape(fmt).format(strs: _*)
        } catch {
            case e: java.lang.reflect.InvocationTargetException =>
                e.toString
        }        
    }
    
    def rawWrite(html: String): String = {
        val id = rawStart(true, html)
        rawClose
        id
    }
    
    // ___ Indentation ______________________________________________________
    def indented[R](open: Boolean, fmt: String, arg0: Any, args: Any*)(f: => R): R = {
        try {
            ifEnabled { rawStart(open, escape(fmt, (arg0 :: args.toList))) }
            val result = f
            if(result != ()) // no need to print Unit
                apply("Result: %s", result)
            result
        } catch {
            case t: Throwable => 
                apply("Error = %s".format(t))
                throw t
        } finally { 
            rawClose
        }
    }
    
    def indented[R](fmt: String, arg0: Any, args: Any*)(f: => R): R = indented(true, fmt, arg0, args: _*)(f)
    def indented[R](str: Any)(f: => R): R = indented(true, "%s", str)(f)    
    def indentedClosed[R](fmt: String, arg0: Any, args: Any*)(f: => R): R = indented(false, fmt, arg0, args: _*)(f)
    def indentedClosed[R](str: Any)(f: => R): R = indented(false, "%s", str)(f)
    
    // ___ Methods to add to the log ________________________________________
    
    def apply(v: Any) = detailed(v)
    
    def apply(fmt: String, arg0: Any, args: Any*): Unit = ifEnabled {
        rawWrite(escape(fmt, (arg0 :: args.toList)))
    }
    
    def linkTo(uri: String, fmt: String): Unit = ifEnabled {
        rawLinkTo(uri, escape(fmt))
    }
    
    def linkTo(uri: String, fmt: String, arg0: Any, args: Any*): Unit = ifEnabled {
        rawLinkTo(uri, escape(fmt, (arg0 :: args.toList)))
    }
    
    // ___ Custom ___________________________________________________________
    
    private def hasDetailedRepr(v: Any) = v match {
        case e: TcEnv => true
        case f: FlowEnv => true
        case m: Map[_, _] => true
        case r: PathRelation => true
        case cd: ir.ClassDecl => true
        case rfd: ir.ReifiedFieldDecl => true
        case md: ir.MethodDecl => true
        case cp: ir.CanonPath => true
        case _ => false
    }
    
    private def detailed(v: Any): Unit = ifEnabled {
        v match {
            case e: TcEnv => env(true, "Env", e)
            case f: FlowEnv => flow(true, "Flow", f)
            case m: Map[_, _] => map("Map", m)
            case r: PathRelation => map("PathRelation", r.mmap.map)
            case cd: ir.ClassDecl => classDecl("", cd)
            case rfd: ir.ReifiedFieldDecl => reifiedFieldDecl("", rfd)
            case md: ir.MethodDecl => methodDecl(true, "", md)
            case cp: ir.CanonPath => canonPath(true, "Canonical Path: ", cp)
            case _ => rawWrite(escape(v.toString))                    
        }
    }
    
    def env(open: Boolean, lbl: Any, env: TcEnv): Unit = ifEnabled {
        indented(open, "%s", lbl) {
            apply("o_lv_cur: %s", env.o_lv_cur)
            apply("wt_ret: %s", env.wt_ret)
            map("perm:", env.perm)
            flow(true, "flow:", env.flow)
        }
    }
    
    def flow(open: Boolean, lbl: Any, flow: FlowEnv): Unit = ifEnabled {
        indented(open, "%s", lbl) {
            iterable("nonnull:", flow.nonnull)
            map("temp:", flow.temp)
            apply("invalidated: %s", flow.ps_invalidated)
            rel("readable:", "readable by", flow.readableRel)
            rel("writable:", "writable by", flow.writableRel)
            rel("hb:", "hb", flow.hbRel)
            rel("inlineInterval:", "inlineInterval of", flow.inlineRel)
            rel("locks:", "locks", flow.locksRel)
        }
    }
    
    def iterable(lbl: Any, m: Iterable[Any]): Unit = ifEnabled {
        indented("%s", lbl) {
            for(v <- m) apply(v)
        }
    }
    
    def map(lbl: Any, m: Iterable[(Any, Any)]): Unit = ifEnabled {
        indented("%s", lbl) {
            for((k, v) <- m)
                apply("%s: %s", k, v)
        }
    }
    
    def rel(lbl: Any, n: String, r: PathRelation): Unit = ifEnabled {
        indented("%s", lbl) {
            for((k, v) <- r.mmap)
                apply("%s %s %s", k, n, v)
        }
    }
    
    def classDecl(lbl: Any, cd: ir.ClassDecl): Unit = ifEnabled {
        indented("%s%s", lbl, cd.toString) {
            cd.ghostFieldDecls.foreach(apply)
            cd.typeVarDecls.foreach(apply)
            cd.ghosts.foreach(apply)
            cd.typeArgs.foreach(apply)
            apply("extends %s", ", ".join(cd.superClasses))
            cd.reifiedFieldDecls.foreach(apply)
            cd.ctors.foreach(methodDecl(true, "[Ctor] ", _))
            cd.methods.foreach(methodDecl(true, "[Mthd] ", _))
        }
    }
    
    def reifiedFieldDecl(lbl: Any, rfd: ir.ReifiedFieldDecl): Unit = ifEnabled {
        indented("%s%s", lbl, rfd.toString) {
            apply("attrs: %s", rfd.as)
            apply("wt: %s", rfd.wt)
            apply("p_guard: %s", rfd.p_guard)
            apply("wps_identity: %s", rfd.wps_identity)
        }        
    }
            
    def methodDecl(open: Boolean, lbl: Any, md: ir.MethodDecl): Unit = ifEnabled {
        indented(open, "%s%s", lbl, md.toString) {
            apply("wt_ret: %s", md.wt_ret)
            indented("args:")   { md.args.foreach(apply) }
            indented("reqs:")   { md.reqs.foreach(apply) }            
            statementSeq("body: ", md.body)
        }
    }
    
    def methodSig(open: Boolean, lbl: Any, msig: ir.MethodSig): Unit = ifEnabled {
        indented(open, "%s", lbl) {
            indented("wts_args:")   { msig.wts_args.foreach(apply) }
            indented("reqs:")       { msig.reqs.foreach(apply) }
            apply("wt_ret: %s", msig.wt_ret)
        }
    }
    
    def statementSeq(lbl: Any, seq: ir.StmtSeq): Unit = ifEnabled {
        indented("%s%s", lbl, seq.toString) {
            seq.stmts.foreach(statement("", _))
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

    def canonPathComponent(open: Boolean, lbl: Any, comp: ir.CanonPathComponent): Unit = ifEnabled {
        comp match {
            case comp @ ir.CpcReifiedField(cpc_base, rfd) => 
                indented(open, "%s: %s", comp.p, comp.wt) { 
                    canonPathComponent(true, "base", cpc_base)
                    reifiedFieldDecl("rfd", rfd)
                }
            case comp @ ir.CpcReifiedLv(lv, wt, wps_identity) =>
                indented(open, "%s: %s", comp.p, comp.wt) { 
                    apply("lv: %s", lv)
                    apply("wt: %s", wt)
                    wps_identity.foreach { wp => apply("identity: %s", wp) }
                }
            case comp @ ir.CpcGhostField(cpc_base, f, c, wps_identity) =>
                indented(open, "%s: %s", comp.p, c) { 
                    canonPathComponent(true, "base", cpc_base)
                    apply("f: %s", f)                    
                    wps_identity.foreach { wp => apply("identity: %s", wp) }                    
                }
            case comp @ ir.CpcGhostLv(lv, c, wps_identity) => 
                indented(open, "%s: %s", comp.p, c) { 
                    apply("lv: %s", lv)
                    wps_identity.foreach { wp => apply("identity: %s", wp) }
                }
        }
    }
    
    def canonPath(open: Boolean, lbl: Any, cp: ir.CanonPath) = {
        indented(open, "%s%s", lbl, cp.reprPath) {
            cp.components.foreach(canonPathComponent(false, "", _))
        }
    }
    
    private[this] def inline(func: (Log => Unit))(lbl: Any) = {
        val ilog = inlineLog
        func(ilog)
        "<a href=\"%s\" onMouseover=\"preview(this);\" onMouseout=\"closePreview(this);\">%s</a>".format(ilog.uri, escape(lbl.toString))
    }
    
    def inlineCanonPath(cp: ir.CanonPath) = inline(_.canonPath(true, "Canonical Path: ", cp))(cp)
    
}
