package ch.ethz.intervals

import Util._
import javax.lang.model.element.Element
import com.sun.source.tree.Tree

abstract class Log {
  // ______________________________________________________________________
  // Various versions of apply()
  
  def apply(fmt: String, arg0: Any, args: Any*): Unit = rawWrite {
      def toLogString(o: Any) = o match {
        case null => escape("null")
        case elem: Element => escape("%s[%s]".format(elem.getKind, qualName(elem)))
        case tree: Tree => escape("%s[%s]".format(tree.getKind, prefix(tree.toString)))
        case _ => escape(o.toString)
      }

      val strs = (arg0 :: args.toList).map(toLogString).toArray[String]
      escape(fmt).format(strs: _*)      
  }
  
  // ______________________________________________________________________
  // Special Log Methods
  
  def apply(v: Any): Unit = v match {
      case e: ir.TcEnv => env("Env", e)
      case m: Map[_, _] => map("Map", m)
      case r: Relation[_, _] => map("Relation", r)
      case cd: ir.ClassDecl => classDecl("", cd)
      case md: ir.MethodDecl => methodDecl("", md)
      case _ => rawWrite(escape(v.toString))
  }
  
  def env(lbl: Any, env: ir.TcEnv) = {
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
  
  def map(lbl: Any, m: Iterable[(Any, Any)]) = {
      indented("%s", lbl) {
          for((k, v) <- m)
            apply("%s: %s", k, v)
      }
  }
  
  def rel(lbl: Any, n: String, r: Relation[_, _]) = {
      indented("%s", lbl) {
          for((k, v) <- r.elements)
            apply("%s %s %s", k, n, v)
      }
  }
  
  def classDecl(lbl: Any, cd: ir.ClassDecl) =
      indented("%s%s", lbl, cd) {
          cd.rfds.foreach(apply)
          cd.ctors.foreach(methodDecl("", _))
          cd.methods.foreach(methodDecl("", _))
      }
      
  def methodDecl(lbl: Any, md: ir.MethodDecl) =
    indented("%s%s", lbl, md) {
        md.blocks.indices.foreach(b =>
            block("%s: ".format(b), md.blocks(b)))
    }
      
  def block(lbl: Any, blk: ir.Block) =
    indented("%s%s", lbl, blk) {
        blk.stmts.foreach(apply)
        blk.gotos.foreach(apply)
    }
      
  // ______________________________________________________________________
  // Indentation

  def indented[R](fmt: String, arg0: Any, args: Any*)(f: => R): R = {
    try {
      apply(fmt, arg0, args: _*)
      rawIndent
      f 
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
    } finally { 
      rawUndent
    }
  }
  
  def indentedRes[R](fmt: Any)(f: => R): R = indentedRes("%s", fmt)(f)
  
  // ______________________________________________________________________
  // Links

/*  
  def link(hb: ir.HB, desc: String) = {
    // Generate a temporary file with the HB data and print out a link to it.
    val tmpFile = java.io.File.createTempFile("jpart.aux.", ".txt")
    val tmpWriter = new java.io.PrintWriter(tmpFile)    
    tmpWriter.println("Description of HB: " + desc)
    tmpWriter.println("Keys: " + hb.edges.keySet)
    for(k <- hb.edges.keySet) {
      tmpWriter.println("%s => %s".format(k, hb.edges(k)))
    }
    tmpWriter.close()
    //"<a href=\"txmt://open/?url=%s\">%s</a>".format(tmpFile.toURI(), desc)
    linkToFile(tmpFile) // Hack: triggers a link in good ol' linkfiles.py
  }
*/  
  
  // ______________________________________________________________________
  // HTML Escaping
  
  def rawWrite(msg: => String): Unit
  def rawIndent: Unit
  def rawUndent: Unit    
  def escape(s: String): String  
  def linkToFile(f: java.io.File): String   
  
}

object Log {
  
  abstract trait WritingText extends Log {
    private var indent: Int = 0    
    abstract override def rawWrite(msg: => String) {
      val msg1 = ("  " * indent) + msg
      super.rawWrite(msg1)
    }
    def rawIndent = indent += 1
    def rawUndent = indent -= 1
    def escape(s: String) = s
    def linkToFile(f: java.io.File) = {
      f.getAbsolutePath+":1" // Hack: triggers a link in good ol' linkfiles.py
    }
    
  }
  
  abstract trait WritingHtml extends Log {
    abstract override def rawWrite(msg: => String) {
      super.rawWrite("<li> " + msg)
    }
    def rawIndent = super.rawWrite("<ul>")
    def rawUndent = super.rawWrite("</ul>")
    def escape(s0: String) = {
      var s = s0
      s = s.replace("&", "&amp;")
      s = s.replace("<", "&lt;")
      s
    }
    def linkToFile(f: java.io.File) =
      "<a href=\"%s\">%s</a>".format(f.toURI, f.getAbsolutePath)
      
    def rawInit = {
      super.rawWrite("""
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
    }
  }
  
  abstract trait WritingToTmpFile extends Log {
    val (outFile, outURI) = {
      val tmpFile = java.io.File.createTempFile("jpart.scala.", ".html")
      (new java.io.PrintWriter(tmpFile), tmpFile.toURI)
    }
    
    override def rawWrite(msg: => String) {
      outFile.println(msg)
      outFile.flush
    }
  }
  
  trait WritingToMemoryBuffer extends Log {
    val buffer = new StringBuffer()    
    override def rawWrite(msg: => String) {
      buffer.append(msg).append("\n")
    }    
  }
  
  trait WritingToStdOut extends Log {
    override def rawWrite(msg: => String) {
      println(msg)
    }
  }
  
  class BufferLog extends Log with WritingToMemoryBuffer with WritingText
  class TmpHtmlLog extends Log with WritingToTmpFile with WritingHtml {
    rawInit // not sure how to do this properly...
  }
  object StdOutLog extends Log with WritingToStdOut with WritingText
  
  object DevNullLog extends Log {
    def rawWrite(msg: => String) { }
    def rawIndent { }
    def rawUndent { }
    def escape(s: String) = s
    def linkToFile(f: java.io.File) = ""
  }
  
}