package inter.compiler

import java.io.File
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader
import scala.util.parsing.input.OffsetPosition
import scala.util.parsing.input.Position

object ParsePass {
    
    class FileReader(interFile: File, seq: PagedSeq[Char], off: Int) extends PagedSeqReader(seq, off) {
        
        override def rest: FileReader =
            if (seq.isDefinedAt(offset)) new FileReader(interFile, seq, offset + 1)
            else this
            
        override def drop(n: Int): FileReader = 
            new FileReader(interFile, seq, offset + n)
            
        override def pos = new OffsetPosition(source, offset) with InterPosition {
            def file = interFile
        }
        
    }

    class FilePosition(interFile: File) extends Position with InterPosition {
        def file = interFile
        def line = 1
        def column = 1
        override def lineContents = ""
    }
    
    def apply(state: CompilationState, interFile: File, expClass: Option[Name.Qual]) {
        val javaReader = Util.javaReaderFromFile(interFile)
        val parser = new HlParser()
        
        val reader = new FileReader(interFile, PagedSeq.fromReader(javaReader), 0)
        val tokens = new parser.lexical.Scanner(reader)
        parser.phrase(parser.compUnit)(tokens) match {
            case n: parser.NoSuccess => {
                state.reporter.report(n.next.pos, "parse.error", n.msg)
            }
            case parser.Success(compUnit, _) => {
                if(state.config.dumpParsedTrees) {
                    compUnit.println(PrettyPrinter.stdout)
                }
                
                // Check that we got (at least) what we expected to get:
                expClass.foreach { expName =>
                    compUnit.classPairs.find(_._1 == expName) match {
                        case Some(_) =>
                        case None => state.reporter.report(
                            new FilePosition(interFile),
                            "expected.to.find.class", 
                            expName.toString
                        )
                    }
                }
                
                // Add entries to the symbol table:
                compUnit.classPairs.foreach { case (qn, clsdefn) =>
                    if(state.symtab.classes.isDefinedAt(qn))
                        state.reporter.report(clsdefn.pos, "class.already.defined", qn.toString)
                    else
                        state.symtab.classes(qn) = new Symbol.Class(qn)
                }

                state.toBeResolved += compUnit
            }
        }                
    }
}