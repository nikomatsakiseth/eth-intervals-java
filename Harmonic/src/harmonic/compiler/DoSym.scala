package harmonic.compiler

import ch.ethz.intervals._

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

import Ast.{Parse => in}
import Ast.{Lower => out}
import Util._

/** The first pass in the Harmonic compiler.
  * Processes a class or method body and creates
  * symbols for each of the declared variables as
  * well as intervals to process those symbols and/or
  * any statements found within.
  */
class DoSym(
    global: Global,
    compUnit: in.CompUnit
) {
    def cls(
        csym: ClassFromSource,
        cdecl: in.ClassDecl
    ) = {
        
    }
    
    def method(
        csym: ClassFromSource,
        msym: MethodSymbol,
        
    ) = {
        
    }
}