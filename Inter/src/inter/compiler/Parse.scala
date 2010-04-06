package inter.compiler

import Hl.{P => out}
import java.io.File
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader
import scala.util.parsing.input.OffsetPosition
import scala.util.parsing.input.Position

object Parse {
    
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

    def apply(state: CompilationState, interFile: File) = {
        val javaReader = Util.javaReaderFromFile(interFile)
        val parser = new HlParser()
        
        val reader = new FileReader(interFile, PagedSeq.fromReader(javaReader), 0)
        val tokens = new parser.lexical.Scanner(reader)
        parser.phrase(parser.compUnit)(tokens) match {
            case n: parser.NoSuccess => {
                state.reporter.report(n.next.pos, "parse.error", n.msg)
                None
            }
            case parser.Success(compUnit, _) => {
                if(state.config.dumpParsedTrees) {
                    compUnit.println(PrettyPrinter.stdout)
                }
                
                Some(compUnit)
            }
        }                
    }
}